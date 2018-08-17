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

const exampleOf = (name) => Object.assign({}, common, {
    entry: path.join(__dirname, name, 'index.js'),
    output: {
        path: path.resolve(path.join(__dirname, 'docs', name)),
        filename: 'app.js'
    }
});

module.exports = [
    exampleOf('github-commits'),
    exampleOf('grid-component'),
    exampleOf('tree-view'),
    exampleOf('svg-graph'),
    exampleOf('modal-component'),
    exampleOf('elastic-header'),
    exampleOf('wrapper-component'),
    exampleOf('firebase-validation'),
];
