'use strict'

const path = require('path')
const autoprefixer = require('autoprefixer')
const HotModuleReplacementPlugin = require('webpack/lib/HotModuleReplacementPlugin')
const DefinePlugin = require('webpack/lib/DefinePlugin')
const NamedModulesPlugin = require('webpack/lib/NamedModulesPlugin')
const InterpolateHtmlPlugin = require('react-dev-utils/InterpolateHtmlPlugin')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const getClientEnvironment = require('./env')
const paths = require('../config/paths')
const devConfig = require('./devConfig')

const publicPath = '/'
const publicUrl = ''
const env = getClientEnvironment(publicUrl)
var withDebug = !process.env["npm_config_nodebug"];

module.exports = {
  mode: 'development',
  devtool: 'cheap-module-source-map',

  entry: [
    require.resolve('../scripts/utils/webpackHotDevClient'),
    require.resolve('react-error-overlay'),
    paths.appIndexJs
  ],
  output: {
    pathinfo: true,
    path: paths.appBuild,
    filename: "index.js",
    publicPath: publicPath,
    devtoolModuleFilenameTemplate: info =>
      path.resolve(info.absoluteResourcePath).replace(/\\/g, '/')
  },
  resolve: {
    modules: ['node_modules'],
    extensions: [".js", ".elm"]
  },
  module: {
    // noParse: /\.elm$/,
    rules: [
      {
        test: /\.js$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: require.resolve('babel-loader'),
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          { loader: "elm-hot-webpack-loader" },
          {
            loader: "elm-webpack-loader",
            options: {
              // add Elm's debug overlay to output
              debug: withDebug,
              //
              forceWatch: true
            }
          }
        ]
      },
      {
        test: /\.scss$/,
        exclude: [/elm-stuff/, /node_modules/],
        // see https://github.com/webpack-contrib/css-loader#url
        use: [
          {loader:"style-loader"},
          {loader: "css-loader"},
          {loader: "sass-loader"}
        ]
      },
      {
        test: /\.css$/,
        use: [
          require.resolve('style-loader'),
          {
            loader: require.resolve('css-loader'),
            options: {
              importLoaders: 1
            }
          },
          {
            loader: require.resolve('postcss-loader'),
            options: {
              ident: 'postcss', // https://webpack.js.org/guides/migrating/#complex-options
              plugins: () => [
                autoprefixer({
                  browsers: [
                    '>1%',
                    'last 4 versions',
                    'Firefox ESR',
                    'not ie < 9'
                  ]
                })
              ]
            }
          }
        ]
      },

      {
        exclude: [/\.html$/, /\.js$/, /\.elm$/, /\.scss$/, /\.css$/, /\.json$/, /\.svg$/],
        loader: require.resolve('url-loader'),
        options: {
          limit: 10000,
          name: '[name].[hash:8].[ext]'
        }
      },
      {
        test: /\.svg$/,
        loader: require.resolve('file-loader'),
        options: {
          name: '[name].[hash:8].[ext]'
        }
      }
    ]
  },

  plugins: [
    new DefinePlugin({
      ...env.stringified,
      ...devConfig
    }),
    new HtmlWebpackPlugin({
      inject: "body",
      template: paths.appHtml
    }),
    new InterpolateHtmlPlugin(HtmlWebpackPlugin,env.raw),
    new HotModuleReplacementPlugin(),
    new NamedModulesPlugin()
  ],
  node: {
    dgram: 'empty',
    fs: 'empty',
    net: 'empty',
    tls: 'empty',
    child_process: 'empty'
  }
}
