let fs = require('fs');
let debug_logger = require("../../jsclass-logger")({
  "debug": true,
  "rotate": (file) => fs.statSync(file).size > 50000,
  "generation": 3,
  "toFile": true
});
let assert = require("assert");

describe('Logger', function() {
  describe('Basic Functions', function() {
    it('#debug()', function() {
      class TestClass {
        constructor() {
          this.a = 1;
          this.b = 2;
          this.c = 3;
          this.testArray = [1, 2, 3];
          this.testObject = { "a": 1, "b": 2, "c": 3 };
        }
      }

      let cstring = "test";
      let carray = [1, 2, 3];
      let cobject = new TestClass();
      let cfunction = function testFunc(a, b, c) {};

      debug_logger.debug(cstring);
      debug_logger.info(cstring);
      debug_logger.warn(cstring);
      debug_logger.error(cstring);

      debug_logger.debug(carray);
      debug_logger.info(carray);
      debug_logger.warn(carray);
      debug_logger.error(carray);

      debug_logger.debug(cobject);
      debug_logger.info(cobject);
      debug_logger.warn(cobject);
      debug_logger.error(cobject);

      debug_logger.debug(cfunction);
      debug_logger.info(cfunction);
      debug_logger.warn(cfunction);
      debug_logger.error(cfunction);

      debug_logger.assert(false).info("DONT OUTPUT THIS");
      debug_logger.assert(true).info("OUTPUT THIS");
    })
  })
})