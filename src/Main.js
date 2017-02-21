/* global exports, process, console */
"use strict";

exports.mainImpl = function (hatter) {
  return function (isRight) {
    return function () {
      process.stdin.resume();
      var input = "";
      process.stdin.on("data", function (chunk) {
        input += chunk;
      });
      process.stdin.on("end", function () {
        var result = hatter([])(input);
        if (isRight(result)) {
          process.stdout.write(result.value0);
        } else {
          console.log(result.value0);
        }
      });
    };
  };
};
