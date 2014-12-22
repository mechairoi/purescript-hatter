#!/usr/bin/env node

"use strict;"
var hatter = require('output/node_modules/index.js');
var input = "";
process.stdin.on('data', function(chunk) { input += chunk; });
process.stdin.on('end', function() { console.log(hatter([])(input).value0); });
