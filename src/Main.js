"use strict";

exports.write = function(s) {
  return function () {
    process.stdout.write(s);
  }
};
