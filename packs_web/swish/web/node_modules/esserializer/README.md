![CircleCI](https://circleci.com/gh/shaochuancs/esserializer.svg?style=shield)
[![Coverage Status](https://coveralls.io/repos/github/shaochuancs/esserializer/badge.svg?branch=master)](https://coveralls.io/github/shaochuancs/esserializer?branch=master)
[![Maintainability](https://api.codeclimate.com/v1/badges/dc4d2ca88c7cc8467b81/maintainability)](https://codeclimate.com/github/shaochuancs/esserializer/maintainability)

# esserializer
ESSerializer is an ECMAScript serialization and deserialization utility.

With ESSerializer, you can serialize JavaScript class instance in JSON format, and later on deserialize it into an instance object, 
with all Class/Property/Method etc. retained. This works in both browser and Node.js environment.

ESSerializer support following features:
* Retain class information of instance field value
* Retain class extension structure
* Support both ES6 class and old function style class
* Support following operator: instanceof, getter & setter
* Support following JavaScript builtin object: Date

## Installation
```sh
$ npm install esserializer --save
```

## Usage
To serialize JavaScript object, invoke ESSerializer's `serialize` method:
```js
const ESSerializer = require('esserializer');
const SomeClass = require('./SomeClass');

let obj = new SomeClass();
// do something here...
let serializedString = ESSerializer.serialize(obj);
console.log(serializedString);
```

To deserialize text and turn it into an corresponding instance object, invoke ESSerializer's `deserialize` method, 
with all involved custom classes as parameter (you don't need to include builtin classes such as `Date` in this parameter):

```js
const ESSerializer = require('esserializer');
const SomeClass = require('./SomeClass');
const AnotherInvolvedClass = require('./AnotherInvolvedClass');

let classes = [SomeClass, AnotherInvolvedClass];
let deserializedObj = ESSerializer.deserialize(serializedString, classes);
console.log(deserializedObj);
```

Please check the `/demo` directory in source code for all examples.

## License
(The MIT License)

Copyright (c) 2019-2021 Chuan Shao &lt;shaochuancs@gmail.com&gt;
