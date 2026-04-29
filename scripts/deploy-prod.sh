#!/usr/bin/env bash
set -e

bucketName="$(dotenv get BUCKET_NAME_PROD)"
distributionId="$(dotenv get DISTRIBUTION_ID_PROD)"

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
find build -type f -name "*.html" ! -name "index.html" | while read -r f; do
  rel="${f#build/}"           # illustrations/portraits/0ddbes65.html
  key="${rel%.html}"          # illustrations/portraits/0ddbes65
  aws s3 cp "$f" "s3://$bucketName/$key" \
    --cache-control "public, max-age=300, must-revalidate" \
    --content-type "text/html; charset=utf-8" >/dev/null
done
echo "uploaded prerendered routes"

aws cloudfront create-invalidation --distribution-id "$distributionId" --paths "/*"
