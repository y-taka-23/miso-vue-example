const path = require('path');

var common = {
    mode: 'none',
    module: {
        rules: [
            {
                test: /\.html$/,
                exclude: /node_modules/,
                loader: 'file-loader?name=[name].[ext]'
            },
            {
                test: /\.css$/,
                exclude: /node_modules/,
                use: [
                    'style-loader',
                    'css-loader'
                ]
            },
            {
                test: /\.hs$/,
                exclude: /node_modules/,
                loader: 'ghcjs-loader'
            }
        ]
    }
};

var svgGraph = Object.assign({}, common, {
    entry: './svg-graph/index.js',
    output: {
        path: path.resolve(__dirname, 'docs/svg-graph'),
        filename: 'app.js'
    }
});

module.exports = [
    svgGraph
];
