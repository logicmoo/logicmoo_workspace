/**
 * Created by cshao on 2021-03-09.
 */

'use strict';

function MyObject() { this.init(); }
MyObject.prototype = {
  property1: "",
  property2: "",

  init: function () {
    this.property1 = "First";
    this.property2 = "Second";
  },

  test: function() {
    console.log("Executing test!");
  }
};
MyObject.prototype.constructor=MyObject;

module.exports = MyObject;