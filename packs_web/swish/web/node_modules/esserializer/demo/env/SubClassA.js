/**
 * Created by cshao on 2019-05-23.
 */

'use strict';

const ClassA = require('./ClassA');

class SubClassA extends ClassA {
  constructor(options) {
    super({xVal: 88, yVal: 99});
    Object.assign(this.options, options);
    this._age = 131;
    this.weight = 129;
  }

  get age() {
    return this._age;
  }

  set age(age) {
    this._age = age;
  }

  getSumOfAgeAndWeight() {
    return this._age + this.weight;
  }

  getOptions() {
    return this.options;
  }

  setOptions(options) {
    this.options = options;
  }

  methodOfSubClassA() {
    console.log('SubClassA');
  }

  staticMethodOfSubClassA() {
    console.log('staticMethodOfSubClassA');
  }
}

module.exports = SubClassA;