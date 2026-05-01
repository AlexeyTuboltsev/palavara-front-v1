#!/usr/bin/env node
/**
 * Generate deterministic mock images for visual regression.
 *
 * Three aspect ratios with distinct colour + label so screenshots are
 * easy to eyeball:
 *   - test-landscape  (4:3,  blue,    1600×1200)
 *   - test-portrait   (3:4,  red,     1200×1600)
 *   - test-square     (1:1,  green,    600× 600)  ← also < 1280 so it
 *     exercises the originalWidth fallback in pictureFor.
 *
 * Each is produced in AVIF + WebP + JPEG so the <picture> source-
 * negotiation path picks a valid format on every browser. The route
 * handler in visual-regression.spec.ts serves these for any /img/<base>
 * (or /img/<base>-<width>.<ext>) request — variant URLs all map back to
 * the same source mock, since the suite tests layout, not bandwidth.
 *
 * Run on demand: `node e2e/fixtures/images/generate.js`. Outputs are
 * checked in alongside this script.
 */

const sharp = require('sharp');
const path = require('path');

const OUT_DIR = __dirname;

const fixtures = [
  {
    name: 'test-landscape',
    width: 1600,
    height: 1200,
    color: '#2563eb',
    label: 'LANDSCAPE 4:3',
  },
  {
    name: 'test-portrait',
    width: 1200,
    height: 1600,
    color: '#dc2626',
    label: 'PORTRAIT 3:4',
  },
  {
    name: 'test-square',
    width: 600,
    height: 600,
    color: '#059669',
    label: 'SQUARE 1:1',
  },
];

function svgFor({ width, height, color, label }) {
  const labelSize = Math.round(Math.min(width, height) / 10);
  const dimSize = Math.round(Math.min(width, height) / 16);
  return `<svg xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}">
  <rect width="100%" height="100%" fill="${color}"/>
  <rect x="2%" y="2%" width="96%" height="96%" fill="none" stroke="#ffffff" stroke-width="${Math.round(Math.min(width, height) / 200)}"/>
  <text x="50%" y="46%" font-family="sans-serif" font-size="${labelSize}" font-weight="700" fill="#ffffff" text-anchor="middle" dominant-baseline="middle">${label}</text>
  <text x="50%" y="58%" font-family="sans-serif" font-size="${dimSize}" fill="#ffffff" text-anchor="middle" dominant-baseline="middle">${width}×${height}</text>
</svg>`;
}

async function main() {
  for (const f of fixtures) {
    const svgBuf = Buffer.from(svgFor(f));
    const base = path.join(OUT_DIR, f.name);
    // Sharp pipes from SVG → raster output. quality settings are
    // deliberately moderate; this isn't artwork, just identifiable
    // markers in screenshots.
    await sharp(svgBuf).avif({ quality: 70 }).toFile(`${base}.avif`);
    await sharp(svgBuf).webp({ quality: 80 }).toFile(`${base}.webp`);
    await sharp(svgBuf).jpeg({ quality: 85, progressive: true }).toFile(`${base}.jpg`);
    console.log(`✓ ${f.name}.{avif,webp,jpg} (${f.width}×${f.height})`);
  }
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
