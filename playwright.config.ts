import { defineConfig, devices } from '@playwright/test';

/**
 * Visual regression for the artist site.
 *
 * Determinism:
 *  - Runs in mcr.microsoft.com/playwright:v1.58.0-noble Docker so font
 *    rendering is identical across machines (yarn test:visual / :update
 *    wrap that). Same image used for studio.palavara.com.
 *  - The Elm app fetches AppData from https://data.palavara.com/data at
 *    runtime, and remote images from data.palavara.com/img/*. Both are
 *    intercepted in the test (see e2e/visual-regression.spec.ts) so a
 *    test run never depends on remote state.
 */
export default defineConfig({
  testDir: './e2e',
  timeout: 30 * 1000,
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  reporter: [['html', { outputFolder: 'playwright-report' }], ['list']],

  use: {
    baseURL: 'http://127.0.0.1:4321',
    trace: 'on-first-retry',
    screenshot: 'only-on-failure',
  },

  projects: [
    {
      name: 'chromium-desktop',
      use: { ...devices['Desktop Chrome'], viewport: { width: 1920, height: 1080 } },
    },
    {
      name: 'chromium-tablet',
      use: { ...devices['iPad Pro'], viewport: { width: 1024, height: 768 } },
    },
    {
      name: 'chromium-mobile',
      use: {
        ...devices['Desktop Chrome'],
        viewport: { width: 390, height: 844 },
        isMobile: true,
        hasTouch: true,
      },
    },
  ],

  // Build once, then serve build/ statically. -s / --single is the
  // SPA fallback flag — without it, /info, /illustrations etc. would
  // return 404 because they don't exist as files in build/. With it,
  // index.html is served for every unmatched path and the Elm app
  // handles routing client-side.
  webServer: {
    command: 'NODE_OPTIONS=--openssl-legacy-provider yarn build && npx --yes serve -s -l 4321 build',
    url: 'http://127.0.0.1:4321',
    reuseExistingServer: !process.env.CI,
    timeout: 180 * 1000,
    stdout: 'pipe',
    stderr: 'pipe',
  },
});
