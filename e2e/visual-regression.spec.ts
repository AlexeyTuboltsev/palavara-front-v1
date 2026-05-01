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
  await page.route('**/data.palavara.com/**/img/**', async (route) => {
    const url = route.request().url();
    const m = url.match(/\/img\/(test-(?:landscape|portrait|square))(?:-\d+)?\.(avif|webp|jpg)(?:\?.*)?$/);
    if (!m) return route.fulfill({ status: 404 });
    const [, base, ext] = m;
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
  { path: '/illustrations', name: 'illustrations' },
  { path: '/graphics', name: 'graphics' },
  { path: '/ceramics', name: 'ceramics' },
  // Item pages — exercise pictureFor across aspect ratios so changes
  // to the <picture> wrapping CSS (display, sizing, srcset originalWidth
  // fallback, …) get caught by the suite.
  { path: '/illustrations/0ddbes65', name: 'item-landscape' },     // 7-1.jpg, 1853×1361 (~1.36)
  { path: '/illustrations/84s4ewzf', name: 'item-portrait' },      // 7-2.jpg, 1603×1950 (~0.82)
  { path: '/illustrations/ogjun7iq', name: 'item-small-square' },  // 7-3.jpg, 591×566 — also exercises originalWidth fallback (source < 1280)
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
