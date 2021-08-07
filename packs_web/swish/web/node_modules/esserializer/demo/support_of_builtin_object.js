/**
 * Created by cshao on 2021-02-23.
 * Node.js v15.0.0 is required to run all the demo program here.
 */

'use strict';

const ESSerializer = require('../dist/bundle');

// Support Date
const objWithDate = {id: 1, date: new Date()};
console.log(objWithDate);
let serializedString = ESSerializer.serialize(objWithDate);
console.log(serializedString);
let deserializedObj = ESSerializer.deserialize(serializedString);
console.log(deserializedObj);