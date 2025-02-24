const express = require('express');

const PORT = 1338;

function main() {
    const app = express();
    app.use((_, res, next) => {
        res.set('Access-Control-Allow-Origin', '*');
        next();
    });
    app.use('/', express.static('../data/package-registry'));
    app.listen(PORT, () => console.log(`Started development package registry server on http://localhost:${PORT}`));
}

main();
