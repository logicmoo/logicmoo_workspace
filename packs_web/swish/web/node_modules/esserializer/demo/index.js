/**
 * Created by cshao on 2019-05-23.
 */

'use strict';

const ESSerializer = require('../dist/bundle');

const SuperClassA = require('./env/SuperClassA');
const ClassA = require('./env/ClassA');
const SubClassA = require('./env/SubClassA');
let classes = [SuperClassA, ClassA, SubClassA];

let subAObj = new SubClassA({xVal: 666, zVal: 231});
subAObj.age = 42;
subAObj.weight = 88;
subAObj.height = 90;
subAObj.staticMethodOfSubClassA();
console.log(subAObj.getSumOfAgeAndWeight());
console.log(subAObj);

let serializedString = ESSerializer.serialize(subAObj);
console.log(serializedString);

console.log('--------');

let deserializedObj = ESSerializer.deserialize(serializedString, classes);
console.log(deserializedObj);
console.log(deserializedObj instanceof SubClassA);

console.log(deserializedObj.age);
console.log(deserializedObj.height);

deserializedObj.methodOfSuperClassA();
deserializedObj.staticMethodOfSubClassA();