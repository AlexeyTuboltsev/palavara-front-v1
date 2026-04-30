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

  // Image route handler. The Elm app renders <picture> elements with
  // srcset entries for variant URLs (foo-320.avif, foo-768.webp, …).
  // Those variants don't exist on the CDN until the operator runs
  // `yarn optimize-images:deploy`. The browser's <picture> algorithm
  // picks one URL based on viewport+DPR and shows broken image if it
  // fails — it does NOT fall through to the next <source> on a 4xx/5xx.
  //
  // To exercise the picture-element render path in tests without a
  // CDN dependency, intercept any URL that looks like a variant
  // (basename-<width>.<ext>) and serve the bytes of the original
  // (basename.<ext>) instead. Layout / format / aspect ratio match
  // what production renders post-optimisation; only the byte size is
  // "as-if-not-optimised". Good enough for visual regression.
  await page.route('**/data.palavara.com/img/*', async (route) => {
    const url = route.request().url();
    const variantMatch = url.match(/^(.+\/img\/)(.+)-\d+\.(avif|webp|jpg)$/);
    if (variantMatch) {
      const [, prefix, base] = variantMatch;
      // Try plausible original extensions in order. The fixture is
      // dominated by .jpg; .png appears for some screenshots.
      for (const ext of ['jpg', 'jpeg', 'png']) {
        const originalUrl = `${prefix}${base}.${ext}`;
        try {
          const res = await fetch(originalUrl);
          if (res.ok) {
            const buf = Buffer.from(await res.arrayBuffer());
            return route.fulfill({
              status: 200,
              contentType: res.headers.get('content-type') || 'image/jpeg',
              body: buf,
            });
          }
        } catch {}
      }
      return route.fulfill({ status: 404 });
    }
    return route.continue();
  });

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
