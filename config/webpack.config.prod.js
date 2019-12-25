'use strict';

const SWPrecacheWebpackPlugin = require('sw-precache-webpack-plugin');
const DefinePlugin = require('webpack/lib/DefinePlugin');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const AssetsPlugin = require('assets-webpack-plugin');
const InterpolateHtmlPlugin = require('react-dev-utils/InterpolateHtmlPlugin');
const getClientEnvironment = require('./env');
const paths = require('../config/paths');
const prodConfig = require('./prodConfig')
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const TerserPlugin = require('terser-webpack-plugin')

// Webpack uses `publicPath` to determine where the app is being served from.
// It requires a trailing slash, or the file assets will get an incorrect path.
const publicPath = paths.servedPath;
// Some apps do not use client-side routing with pushState.
// For these, "homepage" can be set to "." to enable relative asset paths.
const publicUrl = publicPath.slice(0, -1);

// Get environment variables to inject into our app.
const env = getClientEnvironment(publicUrl);

module.exports = {
  // Don't attempt to continue if there are any errors.
  bail: true,
  mode: 'production',
  entry: [paths.appIndexJs],

  output: {
    // The build folder.
    path: paths.appBuild,

    // Append leading slash when production assets are referenced in the html.
    publicPath: publicPath,

    // Add /* filename */ comments to generated require()s in the output.
    pathinfo: true,

    // Generated JS files.
    filename: '[name].[chunkhash:8].js'
  },

  resolve: {
    modules: ['node_modules'],
    extensions: ['.js', '.elm',]
  },

  module: {
    noParse: /\.elm$/,

    rules: [
      {
        test: /\.js$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: require.resolve('babel-loader'),
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: "elm-webpack-loader",
          options: {
            optimize: true,
            files: [
              paths.elmEntry
            ]
          }
        }
      },
      {
        test: /\.s(a|c)ss$/,
          use: [
            {loader: MiniCssExtractPlugin.loader},
            {loader: 'css-loader'},
            {
              loader: 'sass-loader',
              options: {
                sourceMap: false
              }
          }
        ]
      },
      {
        test: /\.(eot|ttf|woff|woff2)$/i,
        loader: require.resolve('url-loader'),
        options: {
          limit: 10000,
          name: 'fonts/[name].[hash:8].[ext]'
        }
      },
      {
        test: /\.(png|jpe?g|gif|svg)$/i,
        loader: require.resolve('url-loader'),
        options: {
          limit: 10000,
          name: 'img/[name].[hash:8].[ext]'
        }
      },
    ]
  },
  optimization: {
    minimizer: [
      new TerserPlugin({
        cache: true,
        parallel: true,
        sourceMap: true, // Must be set to true if using source-maps in production
        terserOptions: {
          // https://github.com/webpack-contrib/terser-webpack-plugin#terseroptions
        }
      }),
    ]
  },
  plugins: [
    // new AssetsPlugin({ path: paths.appBuild, prettyPrint:true }),

    new DefinePlugin({
      ...env.stringified,
      ...prodConfig
    }),
    new HtmlWebpackPlugin({
      inject: true,
      template: paths.appHtml,
      minify: {
        removeComments: true,
        collapseWhitespace: true,
        removeRedundantAttributes: true,
        useShortDoctype: true,
        removeEmptyAttributes: true,
        removeStyleLinkTypeAttributes: true,
        keepClosingSlash: true,
        minifyJS: true,
        minifyCSS: true,
        minifyURLs: true
      }
    }),
    new InterpolateHtmlPlugin(HtmlWebpackPlugin,env.raw),

    new MiniCssExtractPlugin({
      // Options similar to the same options in webpackOptions.output
      // both options are optional
      filename: "[name]-[hash].css",
      chunkFilename: '[id].[hash].css'
    }),

    // Generate a service worker script that will precache, and keep up to date,
    // the HTML & assets that are part of the Webpack build.
    new SWPrecacheWebpackPlugin({
      // By default, a cache-busting query parameter is appended to requests
      // used to populate the caches, to ensure the responses are fresh.
      // If a URL is already hashed by Webpack, then there is no concern
      // about it being stale, and the cache-busting can be skipped.
      dontCacheBustUrlsMatching: /\.\w{8}\./,
      filename: 'service-worker.js',
      logger(message) {
        if (message.indexOf('Total precache size is') === 0) {
          // This message occurs for every build and is a bit too noisy.
          return;
        }
        if (message.indexOf('Skipping static resource') === 0) {
          // This message obscures real errors so we ignore it.
          // https://github.com/facebookincubator/create-react-app/issues/2612
          return;
        }
        console.log(message);
      },
      minify: true,
      // For unknown URLs, fallback to the index page
      navigateFallback: publicUrl + '/index.html',
      // Ignores URLs starting from /__ (useful for Firebase):
      // https://github.com/facebookincubator/create-react-app/issues/2237#issuecomment-302693219
      navigateFallbackWhitelist: [/^(?!\/__).*/],
      // Don't precache sourcemaps (they're large) and build asset manifest:
      staticFileGlobsIgnorePatterns: [/\.map$/, /asset-manifest\.json$/]
    })
  ],
  // Some libraries import Node modules but don't use them in the browser.
  // Tell Webpack to provide empty mocks for them so importing them works.
  node: {
    dgram: 'empty',
    fs: 'empty',
    net: 'empty',
    tls: 'empty',
    child_process: 'empty'
  }
};
