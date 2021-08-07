/**
 * Created by cshao on 2019-05-23.
 */

'use strict';

const SuperClassA = require('./SuperClassA');

class ClassA extends SuperClassA {
  constructor(options) {
    super({zVal: 42});
    Object.assign(this.options, options);
    this._height = null;
  }

  get height() {
    return this._height;
  }

  set height(h) {
    this._height = h;
  }

  getOptions() {
    return this.options;
  }

  setOptions(options) {
    this.options = options;
  }

  methodOfClassA() {
    console.log('ClassA');
  }

  static staticMethodOfClassA() {
    console.log('Static of ClassA');
  }
}

module.exports = ClassA;