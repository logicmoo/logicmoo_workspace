#!/usr/bin/env node

import Greeter from './hello-world';

var greeter = new Greeter("Hello, World!");
var str = greeter.greet();
console.log(str);
