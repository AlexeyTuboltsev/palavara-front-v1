#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const https = require('https');
const sharp = require('sharp');

const DATA_PATH = path.join(__dirname, '..', 'mock-data', 'data.json');
const API_BASE = 'https://data.palavara.com/img/';
const BATCH_SIZE = 5;

function fetchBuffer(url) {
    return new Promise((resolve, reject) => {
        https.get(url, (res) => {
            if (res.statusCode !== 200) {
                reject(new Error(`HTTP ${res.statusCode} for ${url}`));
                return;
            }
            const chunks = [];
            res.on('data', (chunk) => chunks.push(chunk));
            res.on('end', () => resolve(Buffer.concat(chunks)));
            res.on('error', reject);
        }).on('error', reject);
    });
}

async function generateLqip(fileName) {
    const url = API_BASE + fileName;
    const buf = await fetchBuffer(url);
    const lqipBuf = await sharp(buf)
        .resize(20, null, { fit: 'inside' })
        .blur(3)
        .jpeg({ quality: 20 })
        .toBuffer();
    return 'data:image/jpeg;base64,' + lqipBuf.toString('base64');
}

async function processBatch(fileNames) {
    const results = {};
    for (let i = 0; i < fileNames.length; i += BATCH_SIZE) {
        const batch = fileNames.slice(i, i + BATCH_SIZE);
        console.log(`Processing batch ${Math.floor(i / BATCH_SIZE) + 1}: ${batch.join(', ')}`);
        const batchResults = await Promise.all(
            batch.map(async (fileName) => {
                try {
                    const lqip = await generateLqip(fileName);
                    return [fileName, lqip];
                } catch (err) {
                    console.error(`  Failed for ${fileName}: ${err.message}`);
                    return [fileName, ''];
                }
            })
        );
        for (const [fileName, lqip] of batchResults) {
            results[fileName] = lqip;
        }
    }
    return results;
}

function collectFileNames(data) {
    const fileNames = new Set();
    const imageIds = new Set();

    for (const section of data.sections) {
        if (section.type === 'galleryWithTags') {
            for (const item of (section.items || [])) {
                if (item.fileName) fileNames.add(item.fileName);
            }
            for (const tag of (section.tags || [])) {
                for (const item of (tag.items || [])) {
                    if (item.fileName) fileNames.add(item.fileName);
                }
            }
        } else if (section.type === 'gallery') {
            for (const item of (section.items || [])) {
                if (item.fileName) fileNames.add(item.fileName);
            }
        } else if (section.type === 'info') {
            if (section.imageId) imageIds.add(section.imageId);
        }
    }

    return { fileNames: [...fileNames], imageIds: [...imageIds] };
}

function injectLqip(data, lqipMap) {
    for (const section of data.sections) {
        if (section.type === 'galleryWithTags') {
            for (const item of (section.items || [])) {
                if (item.fileName && lqipMap[item.fileName]) {
                    item.lqip = lqipMap[item.fileName];
                }
            }
            for (const tag of (section.tags || [])) {
                for (const item of (tag.items || [])) {
                    if (item.fileName && lqipMap[item.fileName]) {
                        item.lqip = lqipMap[item.fileName];
                    }
                }
            }
        } else if (section.type === 'gallery') {
            for (const item of (section.items || [])) {
                if (item.fileName && lqipMap[item.fileName]) {
                    item.lqip = lqipMap[item.fileName];
                }
            }
        } else if (section.type === 'info') {
            if (section.imageId && lqipMap[section.imageId]) {
                section.lqip = lqipMap[section.imageId];
            }
        }
    }
    return data;
}

async function main() {
    const raw = fs.readFileSync(DATA_PATH, 'utf8');
    const data = JSON.parse(raw);

    const { fileNames, imageIds } = collectFileNames(data);
    const allFiles = [...fileNames, ...imageIds];

    console.log(`Found ${fileNames.length} item images and ${imageIds.length} info images`);
    console.log(`Total to process: ${allFiles.length}`);

    const lqipMap = await processBatch(allFiles);

    injectLqip(data, lqipMap);

    fs.writeFileSync(DATA_PATH, JSON.stringify(data, null, 2));
    console.log(`Done. Written to ${DATA_PATH}`);
}

main().catch((err) => {
    console.error(err);
    process.exit(1);
});
