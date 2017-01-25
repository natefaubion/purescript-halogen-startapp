"use strict";

exports.requestAnimationFrame = function (run) {
  return function() {
    if (window.requestAnimationFrame) {
      window.requestAnimationFrame(run);
    } else {
      window.setTimeout(run, 0);
    }
  };
};
