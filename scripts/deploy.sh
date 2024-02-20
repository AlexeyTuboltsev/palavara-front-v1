bucketName="$(dotenv get BUCKET_NAME)"
distributionId="$(dotenv get DISTRIBUTION_ID)"

echo "--deploying to $bucketName --"

aws s3 sync build/ s3://$bucketName
aws cloudfront create-invalidation --distribution-id $distributionId --paths "/*"