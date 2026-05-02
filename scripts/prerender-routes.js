#!/usr/bin/env node
/**
 * Post-webpack prerender for SEO.
 *
 * The Elm SPA serves the same build/index.html for every route, so
 * non-JS clients (Googlebot's first pass, Telegram/WhatsApp/Slack/FB
 * previews, Bing) see the home page's meta tags on every URL —
 * making them all look like duplicates of /. This script writes a
 * copy of index.html for each route with the route-specific tags
 * substituted, so crawlers see the right title/description/canonical
 * on the first byte.
 *
 * Routes covered:
 *   - /                         (home; left as index.html — webpack already wrote it)
 *   - /info
 *   - /<sectionId>              (3: illustrations, graphics, ceramics)
 *   - /<sectionId>/<tagId>      (14 tag pages)
 *   - /<sectionId>/<itemId>     (per-artwork canonical URLs)
 *   - /<sectionId>/<tagId>/<itemId> (per-artwork via tag; canonical points at the
 *                                    section-image variant when that exists)
 *
 * The deploy step (scripts/deploy-prod.sh) uploads each as a key
 * without the .html extension so CloudFront serves it directly when
 * the user-facing URL is requested. React still hydrates on top.
 *
 * AppData is fetched once at build time. If the network call fails
 * the build keeps going — the static index.html is still written by
 * webpack and the SPA fallback at runtime will fetch the latest data
 * itself.
 */

const fs = require('fs');
const path = require('path');
const https = require('https');

const SITE_URL = 'https://palavara.com';
const STUDIO_ID = 'https://studio.palavara.com/#studio';
const VARYA_ID = `${SITE_URL}/#varvara`;
const DEFAULT_OG_IMAGE = 'https://data.palavara.com/img/0.jpg';
const APP_DATA_URL = 'https://data.palavara.com/data';

const buildDir = path.join(__dirname, '..', 'build');
const indexPath = path.join(buildDir, 'index.html');

// Section.label is plural ("illustrations"). For per-artwork titles
// we want the singular ("Illustration by Varvara Polyakova").
const SECTION_SINGULAR = {
  illustrations: 'Illustration',
  graphics: 'Graphic',
  ceramics: 'Ceramic',
};

function fetchJson(url) {
  return new Promise((resolve, reject) => {
    https.get(url, { headers: { 'User-Agent': 'palavara-prerender/1.0' } }, (res) => {
      if (res.statusCode !== 200) {
        reject(new Error(`HTTP ${res.statusCode} for ${url}`));
        return;
      }
      const chunks = [];
      res.on('data', (c) => chunks.push(c));
      res.on('end', () => {
        try {
          resolve(JSON.parse(Buffer.concat(chunks).toString()));
        } catch (e) {
          reject(e);
        }
      });
      res.on('error', reject);
    }).on('error', reject);
  });
}

function escapeAttr(s) {
  return String(s)
    .replace(/&/g, '&amp;')
    .replace(/"/g, '&quot;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;');
}
function escapeText(s) {
  return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}
function capitalize(s) {
  return s.charAt(0).toUpperCase() + s.slice(1);
}

/**
 * Replace meta tags + canonical and inject a per-route JSON-LD block.
 * Fails loudly if any expected pattern doesn't match — silent half-
 * rewritten files are worse than a build error.
 */
// Set to true after inlineLatinFont() succeeds — used to suppress the
// now-redundant <link rel="preload" as="font"> for latin (the bytes
// are already in the CSS data URI, so a preload would just trigger an
// unused second fetch).
let LATIN_FONT_INLINED = false;

/**
 * Inline the didact-gothic-latin woff2 directly into the built CSS as
 * a base64 data: URI. Body text uses this font and Lighthouse's
 * network dependency tree on /illustrations showed it as the longest
 * critical-path leg (~2,800 ms): the browser only discovers the
 * @font-face URL after CSS parses, then has to fetch the font as a
 * separate request. With the bytes embedded in the CSS, the font is
 * "available" the instant CSS arrives — no separate request, no extra
 * round trip.
 *
 * ~17.8 KB woff2 → ~24 KB base64 → ~6-10 KB once brotli is applied to
 * the CSS at the CDN. CSS is cached `max-age=1y immutable` so this
 * extra weight is paid once per CSS hash bump, not per page.
 *
 * Only the latin font is inlined — cyrillic and latin-ext are only
 * used when the page renders text in those scripts (rare; the about
 * page mentions a few names) and inlining them all would add ~25 KB
 * of base64 weight to every CSS download for marginal LCP benefit.
 */
function inlineLatinFont() {
  let cssEntries;
  try {
    cssEntries = fs.readdirSync(buildDir).filter((f) => /^main-.*\.css$/.test(f));
  } catch {
    return;
  }
  if (!cssEntries.length) {
    console.warn('inlineLatinFont: no main-*.css found, skipping');
    return;
  }
  const fontUrlRe = /url\((\/fonts\/didact-gothic-latin\.[a-z0-9]+\.woff2)\)/g;
  for (const cssFile of cssEntries) {
    const cssPath = path.join(buildDir, cssFile);
    let css = fs.readFileSync(cssPath, 'utf8');
    let count = 0;
    css = css.replace(fontUrlRe, (match, urlPath) => {
      const fontPath = path.join(buildDir, urlPath.replace(/^\//, ''));
      try {
        const buf = fs.readFileSync(fontPath);
        const b64 = buf.toString('base64');
        count++;
        return `url(data:font/woff2;base64,${b64})`;
      } catch (e) {
        console.warn(`inlineLatinFont: failed to read ${fontPath}: ${e.message}`);
        return match;
      }
    });
    if (count > 0) {
      fs.writeFileSync(cssPath, css);
      console.log(`✓ inlined didact-gothic-latin into ${cssFile} (${count} occurrence${count > 1 ? 's' : ''})`);
      LATIN_FONT_INLINED = true;
    }
  }
}

/**
 * Build a `<link rel="preload" as="font">` for the hashed
 * didact-gothic-latin file. Returns "" when the font is already
 * inlined into CSS (preload would just trigger a redundant second
 * fetch) or when build/fonts/ doesn't exist (dev or test envs).
 */
function findFontPreload() {
  if (LATIN_FONT_INLINED) return '';
  const fontsDir = path.join(buildDir, 'fonts');
  let entries;
  try {
    entries = fs.readdirSync(fontsDir);
  } catch {
    return '';
  }
  const file = entries.find((f) => /^didact-gothic-latin\.[a-z0-9]+\.woff2$/.test(f));
  if (!file) return '';
  return `<link rel="preload" as="font" type="font/woff2" href="/fonts/${file}" crossorigin>`;
}

/**
 * Build a static <picture> for the LCP candidate, mirroring exactly
 * what Main.elm's pictureFor + buildSectionPicture would emit. Goes
 * inside <body> so the browser parses it during HTML parse, fetches
 * the AVIF variant, and paints it BEFORE the Elm bundle finishes
 * booting — closing the LCP "element render delay" window that
 * Lighthouse measures at ~1.5 s on /illustrations (Elm
 * Browser.application boot + first render time on a Moto G).
 *
 * Caveat: Elm's Browser.application owns <body>; when it mounts, the
 * existing body content is replaced by the first render. Whether the
 * static element survives matters for LCP detachment behaviour. Even
 * in the worst case (Elm wipes and re-renders) the image is already
 * fetched + decoded, so Elm's <picture> paints instantly from cache.
 *
 * Returns "" for kinds without a clear LCP candidate (info, home).
 */
function staticLcpFor(kind, ctx) {
  const { section, tag, item } = ctx;
  let lcpItem;
  if (kind === 'item' || kind === 'tagItem') lcpItem = item;
  else if (kind === 'section') lcpItem = (section.items || [])[0];
  else if (kind === 'tag') lcpItem = (tag.items || [])[0];
  else return '';
  if (!lcpItem || !lcpItem.fileName || !Array.isArray(lcpItem.widths) || !lcpItem.widths.length) {
    return '';
  }
  const dotIdx = lcpItem.fileName.lastIndexOf('.');
  const base = dotIdx >= 0 ? lcpItem.fileName.slice(0, dotIdx) : lcpItem.fileName;
  const prefix = `https://data.palavara.com/img/${base}`;
  const srcsetFor = (ext) =>
    lcpItem.widths.map((w) => `${prefix}-${w}.${ext} ${w}w`).join(', ');
  const sizes = '(max-width: 1024px) 100vw, 50vw';
  const w = lcpItem.originalWidth;
  const h = lcpItem.originalHeight;
  const dims = (w && h) ? ` width="${w}" height="${h}"` : '';
  const fullJpg = `https://data.palavara.com/img/${lcpItem.fileName}`;
  return (
    `<div class="layout">` +
      `<div class="image-group">` +
        `<a class="image" id="${escapeAttr(lcpItem.itemId)}">` +
          `<picture>` +
            `<source type="image/avif" srcset="${escapeAttr(srcsetFor('avif'))}" sizes="${escapeAttr(sizes)}">` +
            `<source type="image/webp" srcset="${escapeAttr(srcsetFor('webp'))}" sizes="${escapeAttr(sizes)}">` +
            `<img src="${escapeAttr(fullJpg)}" srcset="${escapeAttr(srcsetFor('jpg'))}" sizes="${escapeAttr(sizes)}"${dims} fetchpriority="high" decoding="async" alt="">` +
          `</picture>` +
        `</a>` +
      `</div>` +
    `</div>`
  );
}

function rewriteHtml(template, { title, description, canonical, ogImage, jsonLd, lcpPreload, fontPreload, staticLcp }) {
  const titleText = escapeText(title);
  const titleAttr = escapeAttr(title);
  const descAttr = escapeAttr(description);
  const ogImageAttr = escapeAttr(ogImage);

  const replacements = [
    [/<title>[^<]*<\/title>/, `<title>${titleText}</title>`],
    [/<meta name="description" content="[^"]*"\s*\/?>/, `<meta name="description" content="${descAttr}"/>`],
    [/<link rel="canonical" href="[^"]*"\s*\/?>/, `<link rel="canonical" href="${escapeAttr(canonical)}"/>`],
    [/<meta property="og:title" content="[^"]*"\s*\/?>/, `<meta property="og:title" content="${titleAttr}">`],
    [/<meta property="og:description" content="[^"]*"\s*\/?>/, `<meta property="og:description" content="${descAttr}">`],
    [/<meta property="og:url" content="[^"]*"\s*\/?>/, `<meta property="og:url" content="${escapeAttr(canonical)}">`],
    [/<meta property="og:image" content="[^"]*"\s*\/?>/, `<meta property="og:image" content="${ogImageAttr}">`],
    [/<meta name="twitter:title" content="[^"]*"\s*\/?>/, `<meta name="twitter:title" content="${titleAttr}">`],
    [/<meta name="twitter:description" content="[^"]*"\s*\/?>/, `<meta name="twitter:description" content="${descAttr}">`],
    [/<meta name="twitter:image" content="[^"]*"\s*\/?>/, `<meta name="twitter:image" content="${ogImageAttr}">`],
  ];

  let html = template;
  for (const [pattern, replacement] of replacements) {
    if (!pattern.test(html)) {
      throw new Error(`Pattern ${pattern} did not match build/index.html — schema drift?`);
    }
    html = html.replace(pattern, replacement);
  }

  // Insert the per-route JSON-LD before </head>. The existing Person
  // schema in index.html stays — multiple JSON-LD blocks are valid.
  if (jsonLd) {
    const tag = `<script type="application/ld+json">${JSON.stringify(jsonLd)}</script>`;
    html = html.replace('</head>', `${tag}</head>`);
  }

  // LCP image preload — placed early in <head> (just before </head>
  // for simplicity; preload-discovery doesn't need a specific position
  // since the parser scans the whole head before fetching).
  if (lcpPreload) {
    html = html.replace('</head>', `${lcpPreload}</head>`);
  }

  // Font preload — same placement as LCP preload, same reasoning.
  if (fontPreload) {
    html = html.replace('</head>', `${fontPreload}</head>`);
  }

  // Static LCP element — injected into the empty <body> placeholder
  // so the browser paints the LCP image before Elm even boots. Elm
  // owns body once it mounts; whatever static markup is here gets
  // reconciled (or replaced) by Elm's first render. The image bytes
  // are cached by then so even a hard replace repaints instantly.
  if (staticLcp) {
    html = html.replace(
      /<body>(\s*)<noscript>/,
      `<body>$1${staticLcp}<noscript>`,
    );
  }

  return html;
}

function writeRoute(urlPath, html) {
  // urlPath: '/illustrations/portraits/0ddbes65'
  // file:    build/illustrations/portraits/0ddbes65.html
  const slug = urlPath.replace(/^\//, '');
  const filePath = path.join(buildDir, `${slug}.html`);
  fs.mkdirSync(path.dirname(filePath), { recursive: true });
  fs.writeFileSync(filePath, html);
}

function imageUrlFor(item) {
  // The Elm app builds image URLs as `${apiUrl}${fileName}` (see
  // src/Page.elm: `apiUrl ++ fileName`). urlString is a separate
  // hash-like id used for routing/deep-link matching, NOT for image
  // URLs — uploading via the admin tool puts files at /img/<fileName>.
  return `https://data.palavara.com/img/${item.fileName}`;
}

/**
 * Build a `<link rel="preload" as="image">` for the route's LCP
 * candidate. The Elm app eventually adds `fetchpriority="high"` to
 * the rendered <img>, but the request still has to wait for the JS
 * bundle to parse and React to mount. Preloading from the HTML kicks
 * the fetch off during HTML parse, in parallel with CSS/JS — closes
 * the LCP "resource load delay" window from ~1.4 s to ~200 ms.
 *
 * Uses imagesrcset/imagesizes so the browser picks the same variant
 * the rendered <picture> would. The variant list mirrors variantWidths
 * in src/Main.elm, filtered to whatever's actually been generated for
 * this item (data.json's `widths` array). AVIF is preferred — browsers
 * without AVIF support simply ignore an unsupported preload.
 *
 * Returns "" for routes whose LCP item has no widths recorded yet
 * (e.g. items that haven't been through the optimize pipeline).
 */
function lcpPreloadFor(kind, ctx) {
  const { section, tag, item } = ctx;
  let lcpItem;
  if (kind === 'item' || kind === 'tagItem') {
    lcpItem = item;
  } else if (kind === 'section') {
    lcpItem = (section.items || [])[0];
  } else if (kind === 'tag') {
    lcpItem = (tag.items || [])[0];
  } else {
    return ''; // info page's image is below the fold; not the LCP
  }
  if (!lcpItem || !lcpItem.fileName || !Array.isArray(lcpItem.widths) || !lcpItem.widths.length) {
    return '';
  }
  const dotIdx = lcpItem.fileName.lastIndexOf('.');
  const base = dotIdx >= 0 ? lcpItem.fileName.slice(0, dotIdx) : lcpItem.fileName;
  const prefix = `https://data.palavara.com/img/${base}`;
  const srcset = lcpItem.widths
    .map((w) => `${prefix}-${w}.avif ${w}w`)
    .join(', ');
  // Sizes attribute from Main.elm's variantSizesAttr — keep in sync.
  const sizes = '(max-width: 1024px) 100vw, 50vw';
  return `<link rel="preload" as="image" type="image/avif" fetchpriority="high" imagesrcset="${escapeAttr(srcset)}" imagesizes="${escapeAttr(sizes)}">`;
}

/**
 * Build per-route metadata for each kind of page.
 */
function metaFor(kind, ctx) {
  const { section, tag, item } = ctx;
  switch (kind) {
    case 'info':
      return {
        title: 'About Varya — Palavara',
        description:
          'Varvara Polyakova — visual artist, illustrator and ceramicist based in Berlin. Book illustrations, graphic art, woodcuts and pottery.',
        canonical: `${SITE_URL}/info`,
        ogImage: DEFAULT_OG_IMAGE,
      };
    case 'section':
      return {
        title: `${capitalize(section.label)} by Varvara Polyakova`,
        description: `${capitalize(section.label)} by Varvara Polyakova — Palavara, Berlin. Portfolio of original work across ${section.label}.`,
        canonical: `${SITE_URL}/${section.sectionId}`,
        ogImage: section.items?.[0] ? imageUrlFor(section.items[0]) : DEFAULT_OG_IMAGE,
      };
    case 'tag':
      return {
        title: `${capitalize(tag.label)} ${section.label} by Varvara Polyakova`,
        description: `${capitalize(tag.label)} ${section.label} by Varvara Polyakova — Palavara, Berlin.`,
        canonical: `${SITE_URL}/${section.sectionId}/${tag.tagId}`,
        ogImage: tag.items?.[0] ? imageUrlFor(tag.items[0]) : DEFAULT_OG_IMAGE,
      };
    case 'item': {
      const singular = SECTION_SINGULAR[section.sectionId] || capitalize(section.label);
      return {
        title: `${singular} by Varvara Polyakova`,
        description: `${singular} by Varvara Polyakova — Palavara, Berlin.`,
        canonical: `${SITE_URL}/${section.sectionId}/${item.itemId}`,
        ogImage: imageUrlFor(item),
      };
    }
    case 'tagItem': {
      const singular = SECTION_SINGULAR[section.sectionId] || capitalize(section.label);
      // If the same item also exists in section.items, the canonical is
      // /<section>/<itemId>. Otherwise the tag-image URL is itself canonical.
      const inSection = (section.items || []).some((i) => i.itemId === item.itemId);
      const canonical = inSection
        ? `${SITE_URL}/${section.sectionId}/${item.itemId}`
        : `${SITE_URL}/${section.sectionId}/${tag.tagId}/${item.itemId}`;
      return {
        title: `${singular} by Varvara Polyakova`,
        description: `${singular} by Varvara Polyakova — Palavara, Berlin.`,
        canonical,
        ogImage: imageUrlFor(item),
      };
    }
    default:
      throw new Error(`Unknown route kind: ${kind}`);
  }
}

/**
 * JSON-LD per route. CreativeWork for individual artworks linked to
 * Varvara via creator @id; sections/tags are CollectionPage with
 * Varvara as creator.
 */
function jsonLdFor(kind, ctx, meta) {
  const { section, item } = ctx;
  if (kind === 'item' || kind === 'tagItem') {
    return {
      '@context': 'https://schema.org',
      '@type': 'CreativeWork',
      name: meta.title.replace(' by Varvara Polyakova', ''),
      url: meta.canonical,
      image: meta.ogImage,
      creator: { '@id': VARYA_ID },
      genre: section.label,
      isPartOf: { '@type': 'CollectionPage', url: `${SITE_URL}/${section.sectionId}` },
    };
  }
  if (kind === 'section' || kind === 'tag') {
    return {
      '@context': 'https://schema.org',
      '@type': 'CollectionPage',
      name: meta.title,
      description: meta.description,
      url: meta.canonical,
      creator: { '@id': VARYA_ID },
      isPartOf: { '@type': 'WebSite', url: `${SITE_URL}/` },
    };
  }
  return null; // info uses the existing Person schema in index.html
}

function writeSitemap(canonicalUrls) {
  // canonicalUrls is an array of { path, priority, changefreq } in the
  // order we want them to appear in the sitemap. We deliberately omit
  // tag-item routes (/<section>/<tagId>/<itemId>) — those carry a
  // canonical tag pointing back at the section-image URL, so listing
  // them in the sitemap would just duplicate signal and hand Google
  // "Page with redirect" entries it has to dedupe.
  const lines = [
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">',
  ];
  for (const u of canonicalUrls) {
    lines.push(
      `  <url><loc>${SITE_URL}${u.path}</loc><changefreq>${u.changefreq}</changefreq><priority>${u.priority}</priority></url>`,
    );
  }
  lines.push('</urlset>', '');
  const out = path.join(buildDir, 'sitemap.xml');
  fs.writeFileSync(out, lines.join('\n'));
  console.log(`✓ sitemap: wrote ${canonicalUrls.length} URLs to ${out}`);
}

async function main() {
  if (!fs.existsSync(indexPath)) {
    console.error(`ERROR: ${indexPath} not found. Did webpack build run first?`);
    process.exit(1);
  }
  let template = fs.readFileSync(indexPath, 'utf8');

  // Add `defer` to the main.js <script>. html-webpack-plugin v4-beta
  // doesn't honour `scriptLoading: 'defer'`, so the bundle ships as
  // a render-blocking <script src="/main.<hash>.js"></script> by
  // default. Defer keeps the parser unblocked through HTML; Elm's
  // first paint is data-driven anyway (waits on /data fetch), so
  // there's no TTI cost — only an FCP win. Lighthouse measured
  // ~1.2 s of render-blocking on the pre-defer build.
  const scriptRe = /<script src="\/main\.[a-z0-9]+\.js"><\/script>/;
  if (!scriptRe.test(template)) {
    throw new Error('Could not find main.js <script> in build/index.html — schema drift?');
  }
  template = template.replace(scriptRe, (m) => m.replace('<script ', '<script defer '));
  fs.writeFileSync(indexPath, template);

  // Inline the latin font into CSS before reading per-route preload
  // tags — once it succeeds, findFontPreload() returns "" so we don't
  // emit a redundant <link rel="preload" as="font">.
  inlineLatinFont();

  let appData;
  try {
    appData = await fetchJson(APP_DATA_URL);
  } catch (e) {
    console.warn(`Prerender: AppData fetch failed (${e.message}). Skipping per-route HTML — only index.html ships.`);
    return;
  }

  // Font preload string is the same on every route — compute once.
  const fontPreload = findFontPreload();

  let count = 0;
  const sitemapUrls = [];
  const generate = (urlPath, kind, ctx) => {
    const meta = metaFor(kind, ctx);
    const ld = jsonLdFor(kind, ctx, meta);
    const lcpPreload = lcpPreloadFor(kind, ctx);
    const staticLcp = staticLcpFor(kind, ctx);
    const html = rewriteHtml(template, {
      ...meta,
      jsonLd: ld,
      lcpPreload,
      fontPreload,
      staticLcp,
    });
    writeRoute(urlPath, html);
    count++;
  };

  // / (home)
  sitemapUrls.push({ path: '/', priority: '1.0', changefreq: 'monthly' });

  // /info
  generate('/info', 'info', {});
  sitemapUrls.push({ path: '/info', priority: '0.6', changefreq: 'yearly' });

  for (const section of appData.sections || []) {
    if (section.sectionId === 'info' || !section.sectionId) continue;

    // /<sectionId>
    generate(`/${section.sectionId}`, 'section', { section });
    sitemapUrls.push({
      path: `/${section.sectionId}`,
      priority: '0.9',
      changefreq: 'monthly',
    });

    // /<sectionId>/<tagId>
    for (const tag of section.tags || []) {
      generate(`/${section.sectionId}/${tag.tagId}`, 'tag', { section, tag });
      sitemapUrls.push({
        path: `/${section.sectionId}/${tag.tagId}`,
        priority: '0.7',
        changefreq: 'monthly',
      });

      // /<sectionId>/<tagId>/<itemId> — prerendered for direct-link
      // freshness and crawler reachability, but NOT in sitemap (their
      // canonical points at /<section>/<itemId> below).
      for (const item of tag.items || []) {
        generate(`/${section.sectionId}/${tag.tagId}/${item.itemId}`, 'tagItem', { section, tag, item });
      }
    }

    // /<sectionId>/<itemId> — canonical per-artwork URL
    for (const item of section.items || []) {
      generate(`/${section.sectionId}/${item.itemId}`, 'item', { section, item });
      sitemapUrls.push({
        path: `/${section.sectionId}/${item.itemId}`,
        priority: '0.5',
        changefreq: 'monthly',
      });
    }
  }

  console.log(`✓ prerender: wrote ${count} per-route HTML files`);
  writeSitemap(sitemapUrls);
}

main().catch((err) => {
  console.error('prerender failed:', err);
  process.exit(1);
});
