var path = require('path');
const webpack = require('webpack'); //to access built-in plugins

module.exports = {
  entry: './src/rclustergrammerApp.js',
  plugins: [
    new webpack.ProvidePlugin({
      $: "jquery",
      jQuery: "jquery",
      Clustergrammer: "clustergrammer",
      d3: "d3",
      _: "underscore"
      })],
  module: {
    rules:[{
       test: /\.css$/,
        use: [ 'style-loader', 'css-loader']
        },
      {
        test: /\.exec\.js$/,
        use: [ 'script-loader' ]
      },
      {test: /\.png$/,
       loader: 'url-loader?limit=100000'
       },
      {test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
       loader: 'url-loader?limit=10000&mimetype=application/font-woff'
       },
      //{test: /\.(ttf|otf|eot|woff|woff2|svg)$/,
      // loader: loader: 'file?name=public/fonts/[name].[ext]'
      // }
      {test: /\.(ttf|otf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?|(jpg|gif)$/,
       loader: 'file-loader'
       }
          ]},
  output: {
   path: path.resolve(__dirname, 'dist'),
   filename: 'bundle.js'
   }
};
