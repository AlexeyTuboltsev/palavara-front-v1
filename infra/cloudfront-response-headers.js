// CloudFront Function: palavara-front-response-headers
// Runtime: cloudfront-js-2.0
// Event type: viewer-response
// Distribution: E36KAZ5AXER0B0 (palavara.com / www.palavara.com)
//
// Replaces the previous Lambda@Edge palavara_front_prod (viewer-
// response) with a CloudFront Function. Functionally identical: sets
// the same security headers on every response, and copies the S3
// x-amz-meta-last-modified into Last-Modified for the prerendered
// HTML routes.
//
// Why migrate? Cache-behavior constraint: a viewer-request CloudFront
// Function (used here for www→apex + trailing-slash redirects) can't
// coexist with a viewer-response Lambda@Edge. CF Functions can — and
// they're cheaper, faster, and don't require regional replication.
//
// Keep the CSP / HSTS values in sync with whatever inline scripts and
// CDN origins the site actually uses. The script-src hash currently
// matches the GA bootstrap inline <script> in public/index.html.

function handler(event) {
    var response = event.response;
    var headers = response.headers;

    // S3 stores the original modification time as a custom metadata
    // header; promote it to a real Last-Modified so caches and
    // crawlers can use it.
    if (headers['x-amz-meta-last-modified']) {
        headers['last-modified'] = {
            value: headers['x-amz-meta-last-modified'].value,
        };
    }

    headers['strict-transport-security'] = { value: 'max-age=31536000' };

    headers['content-security-policy'] = {
        value: [
            "default-src 'self' *.palavara.com",
            "img-src 'self' *.palavara.com data: https://www.google-analytics.com",
            "script-src 'self' *.palavara.com 'sha256-77XoCGsFb5hDuCajDwYR/6Otip4fiyKsLsKRActYJO0=' https://www.googletagmanager.com https://www.google-analytics.com",
            "style-src 'self' *.palavara.com",
            "font-src 'self' *.palavara.com data:",
            "connect-src 'self' *.palavara.com https://www.google-analytics.com https://*.google-analytics.com https://*.analytics.google.com https://*.googletagmanager.com",
        ].join('; '),
    };

    headers['x-frame-options'] = { value: 'DENY' };
    headers['x-content-type-options'] = { value: 'nosniff' };

    return response;
}
