#!/usr/bin/env node
/**
 * One-shot image optimization pipeline.
 *
 * Reads data.json from the live CDN, then for every unique fileName
 * referenced in any section.items / tag.items:
 *   1. Downloads the original from https://data.palavara.com/img/<fileName>
 *      (cached locally so re-runs skip already-fetched files)
 *   2. Generates AVIF + WebP + JPEG variants at mobile-focused widths
 *      (320 / 480 / 640 / 768 / 1024 / 1280 px). Original kept as-is
 *      for above-1280 fallback.
 *   3. Generates a 20-px-wide JPEG LQIP (base64 data URI), matching the
 *      admin tool's client-side LQIP convention so newly-uploaded images
 *      and bulk-optimised images use the same format.
 *
 * Variants land in .optimize-cache/variants/<basename>-<width>.<ext>.
 * LQIPs are written to .optimize-cache/lqips.json mapping fileName ->
 * data:image/jpeg;base64,...
 *
 * A separate script (scripts/optimize-images-deploy.sh) uploads the
 * cache to S3 and merges LQIPs back into the live data.json. We split
 * those concerns so this step can run anywhere (no AWS perms needed)
 * and the deploy step is a thin shell wrapper around the AWS CLI.
 *
 * Usage: node scripts/optimize-images.js
 *   --force        Re-generate variants even if cached
 *   --no-download  Skip the originals fetch (use whatever's cached)
 *   --concurrency  Parallel image processing limit (default 4)
 *
 * Idempotent: re-runs skip variants that already exist on disk.
 */

const fs = require('fs');
const path = require('path');
const https = require('https');
const sharp = require('sharp');

const APP_DATA_URL = 'https://data.palavara.com/data';
const IMG_BASE_URL = 'https://data.palavara.com/img/';

const ROOT = path.join(__dirname, '..');
const CACHE_DIR = path.join(ROOT, '.optimize-cache');
const ORIGINALS_DIR = path.join(CACHE_DIR, 'originals');
const VARIANTS_DIR = path.join(CACHE_DIR, 'variants');
const LQIPS_PATH = path.join(CACHE_DIR, 'lqips.json');
const DATA_OUT_PATH = path.join(CACHE_DIR, 'data.json');

// Mobile-focused width ladder. The site shows hero images at 60vh tall
// with width: 100% in a flexible container, so on a 390-px viewport
// with DPR 2-3 we want ~780-1170 device pixels — 768 covers DPR ≤ 2,
// 1280 covers DPR 3 and tablet. Adding 320 / 480 / 640 / 1024 fills
// the gap so the browser always finds a closer match than the original.
const WIDTHS = [320, 480, 640, 768, 1024, 1280];

// Quality settings tuned per format. AVIF is the smallest at the same
// perceptual quality so we can push lower. WebP slightly higher.
// JPEG kept moderate so the original-quality fallback also works.
const QUALITY = { avif: 50, webp: 75, jpeg: 80 };

// LQIP: 20 px wide, JPEG quality matching what the admin's client-side
// LQIP uses (~0.2 in canvas → roughly q=20 in sharp). Slight bump to 25
// so they look acceptable when rendered at large sizes.
const LQIP_WIDTH = 20;
const LQIP_QUALITY = 25;

const args = new Set(process.argv.slice(2));
const FORCE = args.has('--force');
const NO_DOWNLOAD = args.has('--no-download');
const CONCURRENCY = parseInt(
  (process.argv.find((a) => a.startsWith('--concurrency=')) || '').split('=')[1] || '4',
  10,
);

// ---- Utils -----------------------------------------------------------

function ensureDir(p) {
  fs.mkdirSync(p, { recursive: true });
}

function fetchBuffer(url) {
  return new Promise((resolve, reject) => {
    https.get(url, { headers: { 'User-Agent': 'palavara-optimize/1.0' } }, (res) => {
      if (res.statusCode !== 200) {
        reject(new Error(`HTTP ${res.statusCode} for ${url}`));
        return;
      }
      const chunks = [];
      res.on('data', (c) => chunks.push(c));
      res.on('end', () => resolve(Buffer.concat(chunks)));
      res.on('error', reject);
    }).on('error', reject);
  });
}

function fetchJson(url) {
  return fetchBuffer(url).then((buf) => JSON.parse(buf.toString()));
}

function basenameNoExt(fileName) {
  const ext = path.extname(fileName);
  return fileName.slice(0, fileName.length - ext.length);
}

function variantPath(fileName, width, ext) {
  return path.join(VARIANTS_DIR, `${basenameNoExt(fileName)}-${width}.${ext}`);
}

async function pLimit(items, concurrency, fn) {
  const out = new Array(items.length);
  let i = 0;
  await Promise.all(
    Array(concurrency).fill(0).map(async () => {
      while (i < items.length) {
        const my = i++;
        out[my] = await fn(items[my], my);
      }
    }),
  );
  return out;
}

// ---- Pipeline -------------------------------------------------------

function collectFileNames(data) {
  const set = new Set();
  for (const s of data.sections || []) {
    for (const it of s.items || []) if (it.fileName) set.add(it.fileName);
    for (const t of s.tags || []) {
      for (const it of t.items || []) if (it.fileName) set.add(it.fileName);
    }
    if (s.imageId) set.add(s.imageId);
  }
  return Array.from(set);
}

async function ensureOriginal(fileName) {
  const target = path.join(ORIGINALS_DIR, fileName);
  if (fs.existsSync(target) && !FORCE) return target;
  if (NO_DOWNLOAD) {
    throw new Error(`Original missing and --no-download: ${fileName}`);
  }
  // Encode each path segment so spaces / specials don't break the URL.
  // (We've seen "Screenshot 2026-... .png" filenames in the data — those
  // are broken upstream but skip cleanly here rather than crash.)
  const url = IMG_BASE_URL + encodeURIComponent(fileName);
  const buf = await fetchBuffer(url);
  ensureDir(path.dirname(target));
  fs.writeFileSync(target, buf);
  return target;
}

async function generateVariantsFor(fileName) {
  const originalPath = path.join(ORIGINALS_DIR, fileName);
  const ext = path.extname(fileName).toLowerCase().slice(1);
  if (!['jpg', 'jpeg', 'png', 'webp'].includes(ext)) {
    return { fileName, skipped: true, reason: `unsupported extension: ${ext}` };
  }

  const meta = await sharp(originalPath).metadata();
  const widths = WIDTHS.filter((w) => w < (meta.width || Infinity));

  let made = 0;
  for (const w of widths) {
    for (const fmt of ['avif', 'webp', 'jpeg']) {
      const outExt = fmt === 'jpeg' ? 'jpg' : fmt;
      const outPath = variantPath(fileName, w, outExt);
      if (fs.existsSync(outPath) && !FORCE) continue;
      ensureDir(path.dirname(outPath));
      const pipe = sharp(originalPath).resize(w, null, { withoutEnlargement: true });
      if (fmt === 'avif') await pipe.avif({ quality: QUALITY.avif }).toFile(outPath);
      else if (fmt === 'webp') await pipe.webp({ quality: QUALITY.webp }).toFile(outPath);
      else await pipe.jpeg({ quality: QUALITY.jpeg, progressive: true }).toFile(outPath);
      made++;
    }
  }
  return { fileName, made, dimensions: `${meta.width}x${meta.height}` };
}

async function generateLqip(fileName) {
  const originalPath = path.join(ORIGINALS_DIR, fileName);
  const buf = await sharp(originalPath)
    .resize(LQIP_WIDTH, null, { withoutEnlargement: true })
    .jpeg({ quality: LQIP_QUALITY, progressive: false })
    .toBuffer();
  return `data:image/jpeg;base64,${buf.toString('base64')}`;
}

async function main() {
  ensureDir(CACHE_DIR);
  ensureDir(ORIGINALS_DIR);
  ensureDir(VARIANTS_DIR);

  console.log('▸ fetching data.json from CDN...');
  const data = await fetchJson(APP_DATA_URL);
  fs.writeFileSync(DATA_OUT_PATH, JSON.stringify(data, null, 2));

  const fileNames = collectFileNames(data);
  console.log(`▸ ${fileNames.length} unique fileNames referenced in data`);

  // 1. Download originals
  console.log(`▸ ensuring originals (concurrency=${CONCURRENCY})...`);
  const downloadResults = await pLimit(fileNames, CONCURRENCY, async (fn) => {
    try {
      await ensureOriginal(fn);
      return { fn, ok: true };
    } catch (e) {
      return { fn, ok: false, error: e.message };
    }
  });
  const downloadFailed = downloadResults.filter((r) => !r.ok);
  if (downloadFailed.length) {
    console.warn(`  ${downloadFailed.length} originals could not be fetched (will be skipped):`);
    for (const f of downloadFailed) console.warn(`    ${f.fn}: ${f.error}`);
  }
  const usableFileNames = downloadResults.filter((r) => r.ok).map((r) => r.fn);

  // 2. Generate variants
  console.log(`▸ generating variants for ${usableFileNames.length} images...`);
  let totalMade = 0;
  await pLimit(usableFileNames, CONCURRENCY, async (fn, i) => {
    try {
      const r = await generateVariantsFor(fn);
      if (r.made) totalMade += r.made;
      if ((i + 1) % 25 === 0 || i + 1 === usableFileNames.length) {
        process.stdout.write(`  ${i + 1}/${usableFileNames.length}\r`);
      }
    } catch (e) {
      console.error(`  ${fn} variants failed: ${e.message}`);
    }
  });
  console.log(`\n  wrote ${totalMade} new variant files`);

  // 3. Generate LQIPs
  console.log(`▸ generating LQIPs...`);
  const lqips = {};
  await pLimit(usableFileNames, CONCURRENCY, async (fn) => {
    try {
      lqips[fn] = await generateLqip(fn);
    } catch (e) {
      console.error(`  ${fn} lqip failed: ${e.message}`);
    }
  });
  fs.writeFileSync(LQIPS_PATH, JSON.stringify(lqips, null, 2));
  console.log(`  wrote LQIPs for ${Object.keys(lqips).length} files`);

  // 4. Merge LQIPs into data.json copy. The deploy script uploads this
  //    file to s3://palavara-front-api/data.json so the Elm app's
  //    runtime fetch picks them up.
  let updated = 0;
  for (const s of data.sections || []) {
    for (const it of s.items || []) {
      if (it.fileName && lqips[it.fileName] && it.lqip !== lqips[it.fileName]) {
        it.lqip = lqips[it.fileName];
        updated++;
      }
    }
    for (const t of s.tags || []) {
      for (const it of t.items || []) {
        if (it.fileName && lqips[it.fileName] && it.lqip !== lqips[it.fileName]) {
          it.lqip = lqips[it.fileName];
          updated++;
        }
      }
    }
  }
  fs.writeFileSync(DATA_OUT_PATH, JSON.stringify(data, null, 2));
  console.log(`  merged LQIPs into ${updated} item entries`);

  console.log(`\n✓ done. Cache:`);
  console.log(`  originals: ${ORIGINALS_DIR}`);
  console.log(`  variants:  ${VARIANTS_DIR}`);
  console.log(`  lqips:     ${LQIPS_PATH}`);
  console.log(`  data.json: ${DATA_OUT_PATH}`);
  console.log(`\nNext: bash scripts/optimize-images-deploy.sh`);
}

main().catch((err) => {
  console.error('FATAL', err);
  process.exit(1);
});
