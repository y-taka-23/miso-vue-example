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

var githubCommits = Object.assign({}, common, {
    entry: './github-commits/index.js',
    output: {
        path: path.resolve(__dirname, 'docs/github-commits'),
        filename: 'app.js'
    }
});

var gridComponent = Object.assign({}, common, {
    entry: './grid-component/index.js',
    output: {
        path: path.resolve(__dirname, 'docs/grid-component'),
        filename: 'app.js'
    }
});

var svgGraph = Object.assign({}, common, {
    entry: './svg-graph/index.js',
    output: {
        path: path.resolve(__dirname, 'docs/svg-graph'),
        filename: 'app.js'
    }
});

var modalComponent = Object.assign({}, common, {
    entry: './modal-component/index.js',
    output: {
        path: path.resolve(__dirname, 'docs/modal-component'),
        filename: 'app.js'
    }
});

module.exports = [
    githubCommits,
    gridComponent,
    svgGraph,
    modalComponent
];
