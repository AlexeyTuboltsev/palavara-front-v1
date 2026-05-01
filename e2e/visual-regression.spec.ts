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
  await page.route('**/data.palavara.com/data', (route) =>
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

for (const route of routes) {
  test(`${route.name} - visual regression`, async ({ page }) => {
    await setupMocks(page);
    await page.goto(route.path);
    await page.waitForLoadState('networkidle');
    // Elm boots after the AppData fetch resolves; give it a moment to
    // paint the rendered route before snapshotting.
    await page.waitForTimeout(500);
    await expect(page).toHaveScreenshot(`${route.name}.png`, {
      fullPage: true,
      animations: 'disabled',
      maxDiffPixels: 100,
    });
  });
}
