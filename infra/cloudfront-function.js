// CloudFront Function: palavara-front-routing
// Runtime: cloudfront-js-2.0
// Event type: viewer-request
// Distribution: E36KAZ5AXER0B0 (palavara.com / www.palavara.com)
//
// Handles two SEO-affecting URL hygiene cases that were causing
// Search Console to report "Page with redirect" / "Not found (404)"
// entries in the indexing report:
//
//   1. www.palavara.com → palavara.com  (301)
//      Both aliases resolve to the same S3 origin and serve identical
//      bytes today. Without a redirect Google can index either form;
//      with the redirect the apex is the only canonical entry.
//
//   2. /<path>/ → /<path>  (301)
//      Static prerendered routes are uploaded as keys without a
//      trailing slash and without an /index.html suffix (e.g.
//      `s3://palavara.com/illustrations`). Requests with a trailing
//      slash hit S3 looking for `illustrations/index.html`, which
//      doesn't exist → 404. Strip the trailing slash and 301 to the
//      canonical path. The root `/` is preserved as-is.
//
// Anything not matching the above is passed through unchanged so
// existing behaviour is not affected.

function handler(event) {
    var request = event.request;
    var host = request.headers.host && request.headers.host.value;
    var uri = request.uri;
    var qs = request.querystring;

    // Reconstruct the query string suffix for the redirect Location.
    var qsSuffix = '';
    if (qs) {
        var parts = [];
        for (var k in qs) {
            if (qs[k].multiValue) {
                for (var i = 0; i < qs[k].multiValue.length; i++) {
                    parts.push(encodeURIComponent(k) + '=' + encodeURIComponent(qs[k].multiValue[i].value));
                }
            } else {
                parts.push(encodeURIComponent(k) + '=' + encodeURIComponent(qs[k].value));
            }
        }
        if (parts.length) qsSuffix = '?' + parts.join('&');
    }

    // 1) www → apex
    if (host === 'www.palavara.com') {
        return {
            statusCode: 301,
            statusDescription: 'Moved Permanently',
            headers: {
                'location': { value: 'https://palavara.com' + uri + qsSuffix },
                'cache-control': { value: 'public, max-age=86400' },
            },
        };
    }

    // 2) trailing slash → no slash (skip root "/")
    if (uri.length > 1 && uri.charAt(uri.length - 1) === '/') {
        var stripped = uri.replace(/\/+$/, '');
        return {
            statusCode: 301,
            statusDescription: 'Moved Permanently',
            headers: {
                'location': { value: 'https://palavara.com' + stripped + qsSuffix },
                'cache-control': { value: 'public, max-age=86400' },
            },
        };
    }

    return request;
}
