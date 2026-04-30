#!/usr/bin/env bash
#
# Pushes the .optimize-cache/ output of scripts/optimize-images.js to
# the production CDN and updates data.json with the generated LQIPs.
# Run this after `node scripts/optimize-images.js`.
#
set -euo pipefail

CACHE_DIR=".optimize-cache"
VARIANTS_DIR="$CACHE_DIR/variants"
DATA_FILE="$CACHE_DIR/data.json"

# S3 bucket layout (artist site):
#   s3://palavara-front-api/data.json     ← single source of truth
#   s3://palavara-front-api/img/<file>    ← originals
#                          /img/<file>-W.{avif,webp,jpg}  ← variants we add here
S3_BUCKET="palavara-front-api"
S3_REGION="eu-central-1"
# Distribution serving https://data.palavara.com — invalidated below.
DATA_DISTRIBUTION_ID="${DATA_DISTRIBUTION_ID:-$(dotenv get DATA_DISTRIBUTION_ID 2>/dev/null || true)}"

if [ ! -d "$VARIANTS_DIR" ]; then
  echo "ERROR: $VARIANTS_DIR not found. Run \`node scripts/optimize-images.js\` first." >&2
  exit 1
fi
if [ ! -f "$DATA_FILE" ]; then
  echo "ERROR: $DATA_FILE not found. Run \`node scripts/optimize-images.js\` first." >&2
  exit 1
fi

echo "▸ syncing variants to s3://$S3_BUCKET/img/"
aws s3 sync "$VARIANTS_DIR/" "s3://$S3_BUCKET/img/" \
  --region "$S3_REGION" \
  --cache-control "public, max-age=31536000, immutable" \
  --size-only

echo "▸ uploading data.json (with merged LQIPs)"
aws s3 cp "$DATA_FILE" "s3://$S3_BUCKET/data.json" \
  --region "$S3_REGION" \
  --cache-control "public, max-age=300, must-revalidate" \
  --content-type "application/json"

if [ -n "$DATA_DISTRIBUTION_ID" ]; then
  echo "▸ invalidating CloudFront /data + /img/* on $DATA_DISTRIBUTION_ID"
  aws cloudfront create-invalidation \
    --distribution-id "$DATA_DISTRIBUTION_ID" \
    --paths "/data" "/img/*" >/dev/null
else
  echo "WARN: DATA_DISTRIBUTION_ID not set — skipping CloudFront invalidation."
  echo "      Set it in .env or as an env var to invalidate automatically."
fi

echo "✓ optimize-images deploy complete"
