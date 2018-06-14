const path = require('path')
const UglifyJsPlugin = require('uglifyjs-webpack-plugin')

const isProd = process.env.NODE_ENV === 'production';

module.exports = {
  entry: './frontend/main.re',
  output: {
        filename: 'bundle.js',
        path: path.resolve(__dirname, '.'),
  },
  mode: isProd ? 'production' : 'development',
  module: {
      rules: [{
          test: /\.(re|ml)$/,
          use: {
            loader: 'bs-loader',
            options: {
                module: 'es6',
                inSource: false
            }
          }
      }],
  },
  resolve: {
      extensions: ['.re', '.ml', '.js']
  }
};