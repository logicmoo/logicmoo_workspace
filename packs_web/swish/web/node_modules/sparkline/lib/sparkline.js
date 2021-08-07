/*
 * sparkline
 * https://github.com/shiwano/sparkline
 *
 * Copyright (c) 2013 Shogo Iwano
 * Licensed under the MIT license.
 */

(function(window) {
  'use strict';

  var sparkline,
      ticks = ['▁', '▂', '▃', '▄', '▅', '▆', '▇', '█'];

  function lshift(n, bits) {
    return Math.floor(n) * Math.pow(2, bits);
  }

  sparkline = function(numbers, options) {
    options = options || {};
    var max = typeof options.max === 'number' ? options.max : Math.max.apply(null, numbers),
        min = typeof options.min === 'number' ? options.min : Math.min.apply(null, numbers),
        html = typeof options.html === 'boolean' ? options.html : false,
        results = [],
        f, i;

    f = Math.floor(lshift(max - min, 8) / (ticks.length - 1));
    if (f < 1) { f = 1; }

    for (i = 0; i < numbers.length; i++) {
      var value = ticks[Math.floor(lshift(numbers[i] - min, 8) / f)];

      if (html) {
        value = '<span title="' + numbers[i] + '">' + value + '</span>';
      }

      results.push(value);
    }

    return results.join('');
  };

  if (typeof module === 'object' && typeof module.exports === 'object') {
    module.exports = sparkline;
  } else {
    window.sparkline = sparkline;

    if (typeof define === 'function' && define.amd) {
      define('sparkline', [], function () { return sparkline; });
    }
  }
})(this);
