#!/usr/bin/env bash
set -e

# Resolve config. Locally: read from .env via dotenv. In CI: read from
# environment variables populated from GitHub Secrets. Env wins when set
# so CI doesn't depend on a checked-in .env (which is gitignored anyway).
bucketName="${BUCKET_NAME_PROD:-$(dotenv get BUCKET_NAME_PROD 2>/dev/null || true)}"
distributionId="${DISTRIBUTION_ID_PROD:-$(dotenv get DISTRIBUTION_ID_PROD 2>/dev/null || true)}"
# Export so the parallel xargs subshells below can see it.
export bucketName

if [ -z "$bucketName" ] || [ -z "$distributionId" ]; then
  echo "ERROR: BUCKET_NAME_PROD and DISTRIBUTION_ID_PROD must be set (.env locally, secrets in CI)" >&2
  exit 1
fi

echo "--deploying to PRODUCTION: $bucketName --"

# Two-pass sync. Without this, index.html (and meta files like robots.txt
# / sitemap.xml) inherit the same long cache as hashed assets, so a deploy
# only refreshes CloudFront's edge — clients keep loading stale HTML for
# whatever the bucket-level cache TTL is. Same fix shipped on the studio.

# 1. Hashed webpack outputs (main.<hash>.js, main-<hash>.css, /static/*)
#    — content-addressed, safe for long immutable cache.
aws s3 sync build/ "s3://$bucketName" \
  --cache-control "public, max-age=31536000, immutable" \
  --exclude "index.html" \
  --exclude "*.html" \
  --exclude "robots.txt" \
  --exclude "sitemap.xml" \
  --exclude "manifest.json" \
  --exclude "asset-manifest.json"

# 2. SPA shell + meta — short cache, must revalidate.
aws s3 cp build/index.html "s3://$bucketName/index.html" \
  --cache-control "public, max-age=300, must-revalidate" \
  --content-type "text/html; charset=utf-8"

for f in robots.txt sitemap.xml manifest.json asset-manifest.json; do
  if [ -f "build/$f" ]; then
    aws s3 cp "build/$f" "s3://$bucketName/$f" \
      --cache-control "public, max-age=300, must-revalidate"
  fi
done

# 3. Per-route prerendered HTML (from scripts/prerender-routes.js).
#    Uploaded as s3://.../<slug> with no .html extension and explicit
#    text/html content-type so CloudFront serves them directly when
#    the user-facing URL is requested. The SPA's normal 404 →
#    index.html fallback still handles unrouted URLs at runtime.
#
#    Parallelized via xargs -P. Sequential cp with ~676 files takes
#    ~8 min in CI (each AWS API call has ~0.5-1s of TLS + auth
#    overhead); 16-way fan-out brings it to ~30s. xargs propagates
#    non-zero exit from any child, which set -e catches.
find build -type f -name "*.html" ! -name "index.html" -print0 | \
  xargs -0 -n 1 -P 16 bash -c '
    f="$0"
    rel="${f#build/}"
    key="${rel%.html}"
    aws s3 cp "$f" "s3://$bucketName/$key" \
      --cache-control "public, max-age=300, must-revalidate" \
      --content-type "text/html; charset=utf-8" >/dev/null
  '
echo "uploaded prerendered routes"

aws cloudfront create-invalidation --distribution-id "$distributionId" --paths "/*"
