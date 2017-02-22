/* global exports */
"use strict";

exports.consoleLog = function consoleLog(x) {
  console.log(JSON.stringify(x)); return x;
};

exports.isEmptyPatchObject = function isEmptyPatchObject(patches) {
  for (var key in patches) {
    if (key !== "a") {
      return false;
    }
  }
  return true;
};
