#!/usr/bin/env node
/**
 * Build a static HTML report of the visual regression run, suitable
 * for hosting on GitHub Pages and linking from a PR comment.
 *
 * For every route × viewport-project the suite covers, the report
 * shows three side-by-side panels: the committed baseline, the actual
 * screenshot from this run, and (when present) the diff Playwright
 * generated on a failure. Tests that pass have only baseline + actual
 * — the diff column is empty, but the row still renders so a reviewer
 * can sanity-check the screenshot visually.
 *
 * Inputs (all relative to the repo root):
 *   e2e/visual-regression.spec.ts-snapshots/<route>-<project>-linux.png
 *     Baseline images committed to the repo.
 *   test-results/visual-actuals/<project>/<route>.png
 *     Actuals saved by visual-regression.spec.ts on every run.
 *   test-results/<test-id>/<route>-diff.png
 *     Diffs Playwright writes on failed assertions. The <test-id>
 *     directory name encodes route + project; we extract them from the
 *     JSON reporter (test-results/results.json) so we don't have to
 *     parse the directory naming convention.
 *   test-results/results.json
 *     Playwright JSON reporter output. Tells us per-test status
 *     (passed/failed) and the path to attached diffs/actuals.
 *
 * Output:
 *   visual-report/index.html
 *   visual-report/img/<project>/<route>/{baseline,actual,diff}.png
 *
 * The CI workflow then deploys visual-report/ under
 * pr-<N>/<sha>/ on the gh-pages branch.
 */

const fs = require('fs');
const path = require('path');

const ROOT = path.join(__dirname, '..');
const SNAPSHOTS_DIR = path.join(ROOT, 'e2e', 'visual-regression.spec.ts-snapshots');
const ACTUALS_DIR = path.join(ROOT, 'test-results', 'visual-actuals');
const RESULTS_JSON = path.join(ROOT, 'test-results', 'results.json');
const OUT_DIR = path.join(ROOT, 'visual-report');
const OUT_IMG_DIR = path.join(OUT_DIR, 'img');

function ensureDir(p) {
  fs.mkdirSync(p, { recursive: true });
}

function safeRead(p) {
  try { return fs.readFileSync(p); } catch { return null; }
}

function safeReadJson(p) {
  try { return JSON.parse(fs.readFileSync(p, 'utf8')); } catch { return null; }
}

/**
 * Parse the Playwright JSON reporter output into a flat list of
 * { route, project, status, diffPath, actualPath } records.
 *
 * Each test in the JSON has a `title` like
 *   "info - visual regression"
 * and lives inside a project block whose `projectName` is one of
 * "chromium-desktop" / "chromium-tablet" / "chromium-mobile".
 *
 * Test attachments include the diff and (on failure) the actual that
 * Playwright generated. We pull the diff path from there since the
 * test-id directory name is opaque.
 */
function parseResults(json) {
  const out = [];
  if (!json || !json.suites) return out;
  const walk = (suite) => {
    for (const s of suite.suites || []) walk(s);
    for (const spec of suite.specs || []) {
      for (const t of spec.tests || []) {
        const project = t.projectName || 'unknown';
        const titleMatch = spec.title.match(/^(.+?)\s*-\s*visual regression$/);
        const route = titleMatch ? titleMatch[1] : spec.title;
        // A test can have multiple result entries (retries). The last
        // one is the final outcome.
        const result = (t.results || [])[t.results.length - 1];
        const status = result ? result.status : 'unknown';
        let diffPath = null;
        let plActualPath = null;
        for (const att of (result && result.attachments) || []) {
          if (!att.path) continue;
          if (/-diff\.png$/.test(att.name) || /-diff\.png$/.test(att.path)) diffPath = att.path;
          if (/-actual\.png$/.test(att.name) || /-actual\.png$/.test(att.path)) plActualPath = att.path;
        }
        out.push({ route, project, status, diffPath, plActualPath });
      }
    }
  };
  for (const s of json.suites) walk(s);
  return out;
}

function copyIfExists(src, dst) {
  const buf = safeRead(src);
  if (!buf) return false;
  ensureDir(path.dirname(dst));
  fs.writeFileSync(dst, buf);
  return true;
}

function htmlEscape(s) {
  return String(s).replace(/[&<>"']/g, (c) => ({
    '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;',
  }[c]));
}

function statusBadge(status) {
  const colors = { passed: '#1a7f37', failed: '#cf222e', timedOut: '#cf222e', skipped: '#6e7781' };
  const color = colors[status] || '#6e7781';
  return `<span style="background:${color};color:#fff;padding:2px 8px;border-radius:10px;font-size:12px;">${htmlEscape(status)}</span>`;
}

function buildIndexHtml({ commit, label, ts, rows, passed, failed, total }) {
  const panel = (caption, src) => src
    ? `<figure><figcaption>${caption}</figcaption><a href="${src}" target="_blank"><img src="${src}" loading="lazy" alt="${caption}"></a></figure>`
    : `<figure class="empty"><figcaption>${caption}</figcaption><div class="placeholder">—</div></figure>`;

  const rowHtml = rows.map((r) => `
    <article class="row" id="${htmlEscape(r.project + '-' + r.route)}">
      <header>
        <h2>${htmlEscape(r.route)}</h2>
        <span class="project">${htmlEscape(r.project)}</span>
        ${statusBadge(r.status)}
      </header>
      <div class="panels">
        ${panel('expected', r.expected)}
        ${panel('actual', r.actual)}
        ${panel('diff', r.diff)}
      </div>
    </article>
  `).join('\n');

  return `<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>Visual regression — ${htmlEscape(label)} @ ${htmlEscape(commit.slice(0, 7))}</title>
  <meta name="viewport" content="width=device-width,initial-scale=1">
  <style>
    body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif; margin: 0; padding: 16px; max-width: 1400px; margin-inline: auto; color: #1f2328; }
    h1 { margin-top: 0; }
    .summary { background: #f6f8fa; border: 1px solid #d0d7de; border-radius: 6px; padding: 12px 16px; margin-bottom: 24px; }
    .summary p { margin: 4px 0; }
    .summary .stats { font-weight: 600; }
    .row { border: 1px solid #d0d7de; border-radius: 6px; padding: 12px; margin-bottom: 16px; }
    .row header { display: flex; align-items: center; gap: 12px; margin-bottom: 8px; }
    .row header h2 { margin: 0; font-size: 16px; flex: 1; }
    .project { color: #6e7781; font-size: 13px; }
    .panels { display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 12px; }
    figure { margin: 0; }
    figcaption { font-size: 12px; color: #6e7781; margin-bottom: 4px; text-align: center; }
    figure img { width: 100%; height: auto; display: block; border: 1px solid #d0d7de; border-radius: 4px; cursor: zoom-in; }
    figure.empty .placeholder { width: 100%; aspect-ratio: 4 / 3; display: flex; align-items: center; justify-content: center; color: #afb8c1; border: 1px dashed #d0d7de; border-radius: 4px; }
    @media (max-width: 800px) { .panels { grid-template-columns: 1fr; } }
  </style>
</head>
<body>
  <h1>Visual regression report</h1>
  <section class="summary">
    <p><strong>${htmlEscape(label)}</strong> — commit <code>${htmlEscape(commit.slice(0, 7))}</code></p>
    <p class="stats">${passed} passed · ${failed} failed · ${total} total</p>
    <p>Generated ${htmlEscape(ts)}</p>
  </section>
  ${rowHtml}
</body>
</html>
`;
}

function main() {
  ensureDir(OUT_DIR);
  ensureDir(OUT_IMG_DIR);

  const json = safeReadJson(RESULTS_JSON);
  const records = parseResults(json);
  if (!records.length) {
    console.warn('[generate-report] no test results found in', RESULTS_JSON);
  }

  const rows = [];
  let passed = 0;
  let failed = 0;

  for (const r of records) {
    const slug = `${r.project}/${r.route}`;
    const baselineSrc = path.join(SNAPSHOTS_DIR, `${r.route}-${r.project}-linux.png`);
    const actualSrc = path.join(ACTUALS_DIR, r.project, `${r.route}.png`);

    const baselineDst = path.join(OUT_IMG_DIR, slug, 'baseline.png');
    const actualDst = path.join(OUT_IMG_DIR, slug, 'actual.png');
    const diffDst = path.join(OUT_IMG_DIR, slug, 'diff.png');

    const haveBaseline = copyIfExists(baselineSrc, baselineDst);
    const haveActual = copyIfExists(actualSrc, actualDst);
    const haveDiff = r.diffPath ? copyIfExists(r.diffPath, diffDst) : false;

    rows.push({
      route: r.route,
      project: r.project,
      status: r.status,
      expected: haveBaseline ? `img/${slug}/baseline.png` : null,
      actual: haveActual ? `img/${slug}/actual.png` : null,
      diff: haveDiff ? `img/${slug}/diff.png` : null,
    });

    if (r.status === 'passed') passed++;
    else if (r.status === 'failed' || r.status === 'timedOut') failed++;
  }

  // Stable order: failures first (so they're at the top of the page),
  // then by route name, then by viewport (desktop / tablet / mobile).
  const projectOrder = ['chromium-desktop', 'chromium-tablet', 'chromium-mobile'];
  rows.sort((a, b) => {
    if (a.status !== b.status) {
      if (a.status === 'failed' || a.status === 'timedOut') return -1;
      if (b.status === 'failed' || b.status === 'timedOut') return 1;
    }
    if (a.route !== b.route) return a.route.localeCompare(b.route);
    return projectOrder.indexOf(a.project) - projectOrder.indexOf(b.project);
  });

  const commit = process.env.GITHUB_SHA || process.env.COMMIT_SHA || 'local';
  const label = process.env.REPORT_LABEL || (process.env.GITHUB_REF_NAME ? `branch ${process.env.GITHUB_REF_NAME}` : 'local run');
  const ts = new Date().toISOString();

  const html = buildIndexHtml({
    commit,
    label,
    ts,
    rows,
    passed,
    failed,
    total: rows.length,
  });
  fs.writeFileSync(path.join(OUT_DIR, 'index.html'), html);

  console.log(`[generate-report] wrote ${path.join(OUT_DIR, 'index.html')}`);
  console.log(`[generate-report] ${passed} passed, ${failed} failed, ${rows.length} total`);
}

main();
