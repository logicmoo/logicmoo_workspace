/**
 * Created by cshao on 2021-03-01.
 */

'use strict';

function Person(age) {
  this.age = age;
  this.isOld = function (){
    return this.age > 60;
  }
}

module.exports = Person;