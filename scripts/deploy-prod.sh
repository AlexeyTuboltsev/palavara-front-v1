bucketName="$(dotenv get BUCKET_NAME_PROD)"
distributionId="$(dotenv get DISTRIBUTION_ID_PROD)"

echo "--deploying to PRODUCTION: $bucketName --"

aws s3 sync build/ s3://$bucketName
aws cloudfront create-invalidation --distribution-id $distributionId --invalidation-batch file://scripts/invalidation-batch.json