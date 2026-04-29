import { test, expect, Page } from '@playwright/test';
import * as fs from 'fs';
import * as path from 'path';

/**
 * Visual regression for the artist site.
 *
 * AppData is mocked so the rendered structure is deterministic
 * regardless of what the live data.palavara.com/data backend returns.
 * Images are allowed through and load from the CDN as in production —
 * artwork rarely changes and the variance is acceptable.
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

  // Block analytics so they don't add load-timing variance. They're
  // fired 4 s after `load` per index.html, usually past our screenshot
  // window — but a slow run could catch them.
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
