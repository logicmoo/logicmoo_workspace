/**
 * Created by cshao on 2019-05-23.
 */

'use strict';

class SuperClassA {
  constructor(options) {
    this.options = (options && (typeof options === 'object')) ? options : {};
  }

  getOptions() {
    return this.options;
  }

  setOptions(options) {
    this.options = options;
  }

  methodOfSuperClassA() {
    console.log('SuperClassA');
  }
}

module.exports = SuperClassA;