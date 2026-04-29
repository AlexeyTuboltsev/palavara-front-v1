import { test, expect, Page } from '@playwright/test';
import * as fs from 'fs';
import * as path from 'path';

/**
 * Visual regression for the artist site.
 *
 * The app makes two kinds of network requests we need to control:
 *  - One JSON GET to https://data.palavara.com/data (AppData).
 *  - Many image GETs to https://data.palavara.com/img/<filename>.
 *
 * Both are intercepted via page.route() so tests run with a fixed
 * fixture and don't depend on remote state. Images are returned as
 * a tiny gray PNG of fixed dimensions — actual artwork variance
 * isn't what we're regression-testing here, layout and chrome are.
 */

const fixture = JSON.parse(
  fs.readFileSync(path.join(__dirname, 'fixtures', 'appdata.json'), 'utf8'),
);

// 50×50 solid #e0e0e0 PNG. Small, deterministic, intrinsic
// dimensions roughly match a thumbnail tile so layout doesn't
// cave when an artwork-shaped slot is filled with this image.
const GRAY_PNG = Buffer.from(
  'iVBORw0KGgoAAAANSUhEUgAAADIAAAAyCAIAAACRXR/mAAAAOklEQVR42u3OAQ0AAAjDMOZf9DDB' +
  'IZBIaXfTpEgEEUQQQQQRRBBBBEEEEUQQQQQRRBBBBBFEkBcLRgABaWshPwAAAABJRU5ErkJggg==',
  'base64',
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

  await page.route('**/data.palavara.com/img/**', (route) =>
    route.fulfill({
      status: 200,
      contentType: 'image/png',
      headers: { 'access-control-allow-origin': '*' },
      body: GRAY_PNG,
    }),
  );

  // Also block analytics so they don't add load-timing variance to the
  // snapshot. They're fired 4 s after `load` per index.html, so usually
  // already past our screenshot window — but a slow run could catch
  // them.
  await page.route('**/googletagmanager.com/**', (route) => route.abort());
  await page.route('**/google-analytics.com/**', (route) => route.abort());
}

const routes = [
  { path: '/', name: 'home' },
  { path: '/info', name: 'info' },
  { path: '/illustrations', name: 'illustrations' },
  { path: '/graphics', name: 'graphics' },
  { path: '/ceramics', name: 'ceramics' },
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
