import { S3Client, GetObjectCommand, PutObjectCommand, DeleteObjectCommand } from '@aws-sdk/client-s3';
import { CloudFrontClient, CreateInvalidationCommand } from '@aws-sdk/client-cloudfront';
import { getSignedUrl } from '@aws-sdk/s3-request-presigner';

const s3 = new S3Client({ region: process.env.S3_REGION });
const cf = new CloudFrontClient({ region: 'us-east-1' });

const {
  ADMIN_API_KEY,
  S3_BUCKET,
  DATA_KEY = 'data.json',
  IMG_PREFIX = 'img/',
  CLOUDFRONT_DISTRIBUTION_ID,
} = process.env;

const CORS_HEADERS = {
  'Access-Control-Allow-Origin': '*',
  'Access-Control-Allow-Headers': 'Content-Type, X-Admin-Key',
  'Access-Control-Allow-Methods': 'GET, PUT, POST, DELETE, OPTIONS',
};

function respond(statusCode, body, extra = {}) {
  return {
    statusCode,
    headers: { 'Content-Type': 'application/json', ...CORS_HEADERS, ...extra },
    body: typeof body === 'string' ? body : JSON.stringify(body),
  };
}

function isAuth(event) {
  const headers = event.headers || {};
  const key = headers['x-admin-key'] || headers['X-Admin-Key'];
  return key === ADMIN_API_KEY;
}

const FILENAME_RE = /^[\w\-. ]+\.(jpg|jpeg|png|gif|webp)$/i;

export async function handler(event) {
  const method = event.httpMethod || event.requestContext?.http?.method;
  const path = event.path || event.rawPath || '';
  const rawBody = event.isBase64Encoded
    ? Buffer.from(event.body || '', 'base64').toString('utf-8')
    : (event.body || '');

  if (method === 'OPTIONS') return respond(200, '');
  if (!isAuth(event)) return respond(401, { error: 'Unauthorized' });

  try {
    // GET /admin/data
    if (method === 'GET' && path === '/admin/data') {
      const res = await s3.send(new GetObjectCommand({ Bucket: S3_BUCKET, Key: DATA_KEY }));
      const body = await res.Body.transformToString();
      return respond(200, body, { 'Content-Type': 'application/json' });
    }

    // PUT /admin/data
    if (method === 'PUT' && path === '/admin/data') {
      const data = JSON.parse(rawBody);
      if (!Array.isArray(data.sections)) {
        return respond(400, { error: 'Invalid data: sections must be an array' });
      }
      await s3.send(new PutObjectCommand({
        Bucket: S3_BUCKET,
        Key: DATA_KEY,
        Body: JSON.stringify(data),
        ContentType: 'application/json',
        CacheControl: 'no-cache',
      }));
      if (CLOUDFRONT_DISTRIBUTION_ID) {
        await cf.send(new CreateInvalidationCommand({
          DistributionId: CLOUDFRONT_DISTRIBUTION_ID,
          InvalidationBatch: {
            CallerReference: `admin-${Date.now()}`,
            Paths: { Quantity: 1, Items: ['/data'] },
          },
        }));
      }
      return respond(200, { ok: true });
    }

    // POST /admin/presign
    if (method === 'POST' && path === '/admin/presign') {
      const { fileName } = JSON.parse(rawBody);
      if (!fileName || !FILENAME_RE.test(fileName) || fileName.includes('/')) {
        return respond(400, { error: 'Invalid fileName' });
      }
      const key = IMG_PREFIX + fileName;
      const url = await getSignedUrl(
        s3,
        new PutObjectCommand({ Bucket: S3_BUCKET, Key: key }),
        { expiresIn: 300 }
      );
      return respond(200, { url, key });
    }

    // DELETE /admin/image/{fileName}
    if (method === 'DELETE' && path.startsWith('/admin/image/')) {
      const fileName = decodeURIComponent(
        event.pathParameters?.fileName || path.slice('/admin/image/'.length)
      );
      if (!FILENAME_RE.test(fileName) || fileName.includes('/')) {
        return respond(400, { error: 'Invalid fileName' });
      }
      await s3.send(new DeleteObjectCommand({ Bucket: S3_BUCKET, Key: IMG_PREFIX + fileName }));
      return respond(200, { ok: true });
    }

    return respond(404, { error: 'Not found' });
  } catch (e) {
    console.error(e);
    return respond(500, { error: e.message });
  }
}
