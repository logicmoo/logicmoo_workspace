'use strict'

const assert = require('assert');
const Serializable = require('../../jsclass-serializer')
const mix = require('jsclass-mixin')
const fs = require("fs")

describe('Serializable', function() {
  describe('Create sub class', function() {
    it('#constructor()', function() {
      let sc = class SubClass extends Serializable {};
      let o = new sc();

      assert.equal(o.classname, 'SubClass');
      assert.equal(typeof o.serialize, 'function');
      assert.equal(typeof o.deserialize, 'function');
    })
  })

  describe('serialize/deserialize using static methods', function() {
    it('#Serializable.serialize, deserialize()', function() {
      let nc = class NestedSubClass extends Serializable {};

      let sc = class SubClass extends Serializable {
        constructor() {
          super();

          this._number = 1;
          this._string = "test";
          this._boolean = true;
          this._array = [1, 2, 3];
          this._object = new nc();
          this._date = new Date('1995-12-17T03:24:00.000Z');
        }
      };
      let source = new sc();

      let json = Serializable.serialize(source);
      let target = Serializable.deserialize(json);

      //check type
      assert.equal(target.classname, 'SubClass');
      assert.equal(typeof target._number, 'number');
      assert.equal(typeof target._string, 'string');
      assert.equal(typeof target._boolean, 'boolean');
      assert.equal(Array.isArray(target._array), true);
      assert.equal(target._object instanceof nc, true);
      assert.equal(target._date instanceof Date, true);

      //check value
      assert.equal(target._number, 1);
      assert.equal(target._string, 'test');
      assert.equal(target._boolean, true);
      assert.equal(target._array.toString(), [1, 2, 3].toString());
      assert.equal(target._object.classname, "NestedSubClass");
      assert.equal(target._date.toISOString(), '1995-12-17T03:24:00.000Z');
    })
  })

  describe('serialize/deserialize using object`s methods', function() {
    it('#serialize, deserialize()', function() {
      let nc = class NestedSubClass extends Serializable {};

      let sc = class SubClass extends Serializable {
        constructor() {
          super();

          this._number = 1;
          this._string = "test";
          this._boolean = true;
          this._array = [1, 2, 3];
          this._object = new nc();
          this._date = new Date('1995-12-17T03:24:00.000Z');
        }
      };


      let source = new sc();

      source._number = 100;
      source._string = "changed";
      source._boolean = false;
      source._array = ["a", "b", "c"];
      source._object = {};
      source._date = new Date('2018-12-17T03:24:00.000Z');

      let json = source.serialize();

      let target = new sc();
      target.deserialize(json);

      //check type
      assert.equal(typeof target._number, 'number');
      assert.equal(typeof target._string, 'string');
      assert.equal(typeof target._boolean, 'boolean');
      assert.equal(Array.isArray(target._array), true);
      assert.equal(target._object instanceof nc, true);
      assert.equal(target._date instanceof Date, true);

      //check value
      assert.equal(target._number, 1);
      assert.equal(target._string, 'test');
      assert.equal(target._boolean, true);
      assert.equal(target._array.toString(), ["1", "2", "3"].toString());
      assert.equal(target._object.classname, "NestedSubClass");
      assert.equal(target._date.toISOString(), '1995-12-17T03:24:00.000Z');
    })
  })

  describe('deserialize to unmatched class based object, using object`s methods', function() {
    it('#serialize, deserialize()', function() {
      let nc = class NestedSubClass extends Serializable {};

      let sc = class SubClass extends Serializable {
        constructor() {
          super();

          this._number = 1;
          this._string = "test";
          this._boolean = true;
          this._array = [1, 2, 3];
          this._object = new nc();
        }
      };


      let source = new sc();
      let json = source.serialize();
      let target = new nc();
      target.deserialize(json);
    })
  })

  describe('write to file test', function() {
    it('#serializeToFile()', function() {
      let sc = class SubClass extends Serializable {
        constructor() {
          super(null, './data/');

          this._number = 1;
          this._string = "test";
          this._boolean = true;
          this._array = [1, 2, 3];
          this._date = new Date('1995-12-17T03:24:00.000Z');
        }
      };

      let source = new sc(null, './data/');
      console.log(source.uuid);

      source.saveToFile();

      let target = new sc(null, './data/');
      //target.loadFromFile(source.uuid);

      //check type
      assert.equal(target.classname, 'SubClass');
      assert.equal(typeof target._number, 'number');
      assert.equal(typeof target._string, 'string');
      assert.equal(typeof target._boolean, 'boolean');
      assert.equal(Array.isArray(target._array), true);
      assert.equal(target._date instanceof Date, true);

      //check value
      assert.equal(target._number, 1);
      assert.equal(target._string, 'test');
      assert.equal(target._boolean, true);
      assert.equal(target._array.toString(), [1, 2, 3].toString());
      assert.equal(target._date.toISOString(), '1995-12-17T03:24:00.000Z');
    })
  })

  describe('load all files under storage path', function() {
    it('#loadAll()', function() {
      let dirname = "./data/";
      Serializable.setStoragePath(dirname);

      var targetRemoveFiles = fs.readdirSync(dirname);
      for (var file in targetRemoveFiles) {
        fs.unlinkSync(dirname + targetRemoveFiles[file]);
      }

      // 消したいフォルダを削除
      fs.rmdirSync(dirname);



      let nc = class NestedSubClass extends Serializable {};

      let nc1 = new nc();
      let nc2 = new nc();
      let nc3 = new nc();

      nc1.saveToFile();
      nc2.saveToFile();
      nc3.saveToFile();

      let a = Serializable.loadAll();

      assert.equal(a.length, 3);

      nc3.flag = true;
      nc3.saveToFile();

      a = Serializable.loadAll(o => !o.flag);

      assert.equal(a.length, 2);

    })

  })

  describe('use serializer with jsclass-mixin', function() {
    it('#serialize, deserialize()', function() {
      class B {};

      class A extends mix(Serializable, B) {
        constructor() {
          super();
        }
      };


      let source = new A();
      let json = source.serialize();
      let target = Serializable.deserialize(json);

      assert(target instanceof A, true);
    })
  })
})