import { test, expect, Page } from '@playwright/test';
import * as fs from 'fs';
import * as path from 'path';

/**
 * Visual regression for the artist site.
 *
 * AppData is mocked so the rendered structure is deterministic
 * regardless of what the live data.palavara.com/data backend returns.
 *
 * Images are also mocked: every fixture item's fileName resolves to one
 * of three labelled aspect-ratio mocks (test-landscape / test-portrait
 * / test-square — see fixtures/images/generate.js). The route handler
 * intercepts any /img/<base>(-<width>)?.(avif|webp|jpg) request and
 * serves the corresponding mock file from disk, so the suite never hits
 * the live CDN and screenshots are byte-for-byte stable across runs.
 */

const fixture = JSON.parse(
  fs.readFileSync(path.join(__dirname, 'fixtures', 'appdata.json'), 'utf8'),
);

async function setupMocks(page: Page) {
  // The Elm app's URL builder is `apiProtocol://apiBaseUrl:apiPort/`
  // with apiPort='' in production config, producing
  // `https://data.palavara.com:/data` (note the `:`). Browsers
  // normalise that to `https://data.palavara.com/data` before fetch,
  // but Playwright's route glob sees the original form. Match both
  // shapes via a regex so the data mock never silently misses.
  await page.route(/^https?:\/\/data\.palavara\.com:?\/data(\?.*)?$/, (route) =>
    route.fulfill({
      status: 200,
      contentType: 'application/json',
      headers: { 'access-control-allow-origin': '*' },
      body: JSON.stringify(fixture),
    }),
  );

  // Image route handler. The Elm app renders <picture> elements with
  // srcset entries for variant URLs (foo-320.avif, foo-768.webp, …).
  // Browser's <picture> algorithm picks one URL based on viewport+DPR
  // and shows a broken image if it fails — it does NOT fall through to
  // the next <source> on a 4xx/5xx.
  //
  // The fixture's items all reference one of the three test-<shape>
  // mocks (test-landscape / test-portrait / test-square, see
  // fixtures/images/generate.js). For any /img/<mock>(-<width>)?.<ext>
  // request, this handler serves the corresponding mock file. Variant
  // widths all map to the same source mock, since the suite tests
  // layout, not bandwidth.
  const mockDir = path.join(__dirname, 'fixtures', 'images');
  const mimeFor: Record<string, string> = {
    avif: 'image/avif',
    webp: 'image/webp',
    jpg: 'image/jpeg',
  };
  // The home page's hero image is a CSS background-image hardcoded to
  // /img/0.jpg in styles.scss (the "trees" photo in production). The
  // fixture appdata can't redirect that — it's compiled into CSS — so
  // the route handler maps it to the home-hero mock at the original
  // 1300×976 dimensions.
  const hardcodedAliases: Record<string, string> = {
    '0.jpg': 'test-home-hero',
    '0.jpeg': 'test-home-hero',
    '0.webp': 'test-home-hero',
    '0.avif': 'test-home-hero',
  };

  await page.route('**/data.palavara.com/**/img/**', async (route) => {
    const url = route.request().url();
    // Strip variant suffix and query string, then match either:
    //   - one of our /img/test-<name>(-<width>)?.<ext> mocks, or
    //   - a hardcoded alias like /img/0.jpg → home-hero.
    const m = url.match(/\/img\/([^/?#]+?)(?:-\d+)?\.(avif|webp|jpg)(?:\?.*)?$/);
    if (!m) return route.fulfill({ status: 404 });
    const [, basenameRaw, ext] = m;
    const aliasKey = `${basenameRaw}.${ext}`;
    let base: string | undefined;
    if (basenameRaw.startsWith('test-')) {
      base = basenameRaw;
    } else if (hardcodedAliases[aliasKey]) {
      base = hardcodedAliases[aliasKey];
    }
    if (!base) return route.fulfill({ status: 404 });
    const file = path.join(mockDir, `${base}.${ext}`);
    try {
      const body = fs.readFileSync(file);
      return route.fulfill({ status: 200, contentType: mimeFor[ext], body });
    } catch {
      return route.fulfill({ status: 404 });
    }
  });

  // Block analytics so they don't add load-timing variance. They're
  // fired 4 s after `load` per index.html, usually past our screenshot
  // window — but a slow run could catch them.
  await page.route('**/googletagmanager.com/**', (route) => route.abort());
  await page.route('**/google-analytics.com/**', (route) => route.abort());
}

type RouteSpec = { path: string; name: string };
const routes: RouteSpec[] = [
  { path: '/', name: 'home' },
  { path: '/info', name: 'info' },
  // Section pages (galleryWithTags) — show 12 items in a grid.
  { path: '/illustrations', name: 'section-illustrations' },
  { path: '/graphics', name: 'section-graphics' },
  { path: '/ceramics', name: 'section-ceramics' },
  // Tag pages — gallery view filtered to a tag's items.
  { path: '/illustrations/black_and_white', name: 'tag-black-and-white' },
  { path: '/ceramics/things', name: 'tag-ceramics-things' },
  // Item pages — exercise pictureFor across aspect ratios. Pinned
  // itemIds (see fixtures/appdata.json) so each name maps to a known
  // source mock.
  { path: '/illustrations/0ddbes65', name: 'item-landscape' },     // pinned to test-medium-landscape (1200×800)
  { path: '/illustrations/84s4ewzf', name: 'item-portrait' },      // pinned to test-medium-portrait (800×1200)
  { path: '/illustrations/ogjun7iq', name: 'item-tiny' },          // pinned to test-tiny-square (240×240) — exercises originalWidth fallback (source < 320)
  // Item-via-tag — same UX as the bare item route but the URL goes
  // through a tag, exercising TagImageRoute / TagImage parsers.
  { path: '/illustrations/black_and_white/0ddbes65', name: 'tag-item-landscape' },
];

async function settleImages(page: Page) {
  // Force-eager every <img> in the DOM. Production uses loading="lazy"
  // which defers off-screen images until they intersect the viewport,
  // and IntersectionObserver does NOT fire when Playwright resizes the
  // viewport for a fullPage screenshot. Combined with the desktop
  // gallery's own scroll container (.image-group { overflow: auto })
  // there's no reliable way to scroll all thumbs into view.
  //
  // Removing the loading attribute promotes them to default ("eager")
  // and the browser kicks off the requests immediately. We then wait
  // for every img.complete with a naturalWidth — anything still false
  // after that means an actual broken image, which the suite should
  // catch.
  await page.evaluate(() => {
    document.querySelectorAll('img[loading]').forEach((img) => {
      img.removeAttribute('loading');
    });
  });
  // Only wait on imgs whose src actually points at the image CDN —
  // Main.elm emits a placeholder `<img src="">` in the "main-image
  // off" state, which the browser resolves to the page URL itself
  // (HTML response → naturalWidth=0 → never settles). Filtering by
  // the /img/ path skips that and any other empty-src placeholders.
  await page.waitForFunction(
    () => {
      const imgs = Array.from(document.querySelectorAll('img')).filter(
        (img) => {
          const src = img.getAttribute('src') || '';
          return /\/img\//.test(src);
        },
      );
      return imgs.every((img) => img.complete && img.naturalWidth > 0);
    },
    undefined,
    { timeout: 10000 },
  );
  // Force decode on every image so the compositor has the bitmap
  // ready by screenshot time. img.complete only means "fetch done",
  // not "decoded and ready to paint" — that gap is what produced
  // the run-to-run variance on individual tiles.
  await page.evaluate(async () => {
    const imgs = Array.from(document.querySelectorAll('img')).filter(
      (img) => {
        const src = img.getAttribute('src') || '';
        return /\/img\//.test(src);
      },
    );
    await Promise.all(imgs.map((img) => img.decode().catch(() => undefined)));
  });
}

for (const route of routes) {
  test(`${route.name} - visual regression`, async ({ page }, testInfo) => {
    await setupMocks(page);
    await page.goto(route.path);
    await page.waitForLoadState('networkidle');
    // Wait for Elm to actually render the route. networkidle fires
    // when no requests are in flight for 500 ms, but the data fetch
    // and Elm's first paint can land just before / after that window
    // — sometimes the screenshot is taken before Elm has produced
    // any content. `.menu-wrapper` is in every rendered page (start,
    // info, gallery) so its presence is the "Elm has finished its
    // first render" signal we need.
    await page.waitForSelector('.menu-wrapper', { timeout: 5000 });
    await settleImages(page);
    // Wait for two animation frames after image decode — layout
    // sometimes shifts one frame after the bitmap lands as the cell's
    // aspect ratio settles. Two RAFs is the standard "layout stable"
    // signal in browser tests.
    await page.evaluate(
      () =>
        new Promise<void>((resolve) =>
          requestAnimationFrame(() => requestAnimationFrame(() => resolve())),
        ),
    );

    // Save the actual screenshot to a stable location regardless of
    // whether toHaveScreenshot passes or fails. The CI report renders
    // expected/actual/diff for every route × viewport combo, so the
    // reviewer can eyeball even passing routes — not just the failing
    // ones. By default Playwright only writes -actual.png on failure;
    // this extra screenshot covers the success case too. Run before
    // toHaveScreenshot so a thrown assertion doesn't skip the save.
    const actualDir = path.join('test-results', 'visual-actuals', testInfo.project.name);
    fs.mkdirSync(actualDir, { recursive: true });
    await page.screenshot({
      path: path.join(actualDir, `${route.name}.png`),
      fullPage: true,
      animations: 'disabled',
    });

    await expect(page).toHaveScreenshot(`${route.name}.png`, {
      fullPage: true,
      animations: 'disabled',
      // Per-pixel YIQ tolerance. JPEG decode is bit-stable but the
      // browser composites images through colour management and
      // sub-pixel anti-aliasing that drift slightly between runs
      // (visible as faint outlines on the mocks' high-contrast
      // borders). 0.2 absorbs that without letting real colour /
      // layout regressions slip through.
      threshold: 0.2,
      maxDiffPixels: 25000,
    });
  });
}
