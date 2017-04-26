/* global exports, process, console, require */
"use strict";
/*eslint no-console: "off"*/ /*XXX*/

var glob = require("glob");
var path = require("path");
var fs = require("fs");

function run(hatter) {
  var target = process.argv[2];
  var files = glob.sync(target + "/**/*.hat");
  files.forEach(function (inpath) {
    var p = path.parse(inpath);
    var outpath = path.format({ dir: p.dir, base: p.name + ".purs" });
    console.log("Generating `" + outpath + "` from `" + inpath  + "`");
    var input = fs.readFileSync(inpath, "utf8");
    var output = hatter(input);
    fs.writeFileSync(outpath, output);
  });
}

exports.mainImpl = function (hatter) {
  return function (isRight) {
    return function () {
      var h = function (input) {
        var result = hatter([])(input);
        if (isRight(result)) {
          return result.value0;
        } else {
          console.log(result.value0);
          throw result.value0;
        }
      };
      run(h);
    };
  };
};
