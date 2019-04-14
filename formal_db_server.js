const express = require('express');
const path = require('path');
var app = express();
app.get('/', (req, res) => {
    res.sendFile(path.join(__dirname + '/index.html'));
});
app.listen(process.env.PORT || 3021)
