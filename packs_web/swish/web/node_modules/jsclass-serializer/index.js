const fs = require("fs");
const uuid = require("uuid/v4");

let environment = Symbol();
let srializable_classes = Symbol();

class DateProxyForClassSerializer {
  constructor(date) {
    this.classname = "DateProxyForClassSerializer";
    this.value = date;
  }
}

/**
 * Serializable object and deserialize back to its original class instance.
 * Also supports save/load to/from file system.
 * @class Serializable
 */
class Serializable {
  /**
   * Set directory path to save/load serialized information to/from file.
   * @method setStoragePath
   * @param  {string}       p Absolute or relative directory path
   * @static
   * @memberof Serializable
   */
  static setStoragePath(p) {
    Serializable[environment].storage_path = p;
  }

  /**
   * Save serialized json object to file.  Where directory path would be the
   * path previously set by setStoragePath().
   * This method can serialize any type of object.
   * @method saveToFile
   * @param  {any}   o Object to serialize.
   * @param  {string}   filename Filename to save object.
   * @return {json}     Json text.
   * @static
   * @memberof Serializable
   */
  static saveToFile(o) {
    let json = Serializable.serialize(o);
    // if storage_path is not set, throw exception
    if (!o.dir) {
      throw new Error("PathNotDefined");
    }

    // check if storage path exists, if it doesnt create one
    let dirname = o.dir
    if (!fs.existsSync(dirname)) {
      fs.mkdirSync(dirname);
    }

    // overwrite file
    let filename = o.uuid;
    let filepath = dirname + filename;
    fs.writeFileSync(filepath, json, "utf8", (error) => {});

    return json;
  }

  /**
   * Load json text from file and convert to object. Where directory path would
   * be the path previously set by setStoragePath().
   * This method can deserialize any file with json text.
   * @method loadFromFile
   * @param  {string}     filename Filename to load json from.
   * @return {any}          Deserialized object.
   * @static
   * @memberof Serializable
   */
  static loadFromFile(filename) {
    let dirname = Serializable[environment].storage_path;
    let filepath = dirname + filename;
    let json = fs.readFileSync(filepath);

    return Serializable.deserialize(json);
  }

  /**
   * Load all json files under storage directory.
   * @method loadAll
   * @param  {function(object)} Callback function applies to retrieved objects.
   * @return {Array} Retrieved objects.
   * @static
   * @memberof Serializable
   */
  static loadAll(callback) {
    let dirname = Serializable[environment].storage_path;
    let retval = [];
    fs.readdirSync(dirname).forEach(file => {
      let o = Serializable.loadFromFile(file);
      let callback_returns = true;
      if (callback) {
        callback_returns = callback(o);
      }

      if (!callback_returns === false) {
        retval.push(o);
      }
    });
    return retval;
  }
  /**
   * Serialize object to json format.
   * This method can serialize any type of object.
   * @method serialize
   * @param  {any}   o Object to serialize.
   * @return {json}    Json text.
   * @static
   * @memberof Serializable
   */
  static serialize(o) {
    let r = function(k, v) {
      if (o[k] instanceof Date) {
        v = new DateProxyForClassSerializer(v);
      }
      return v;
    }
    return JSON.stringify(o, r);
  }

  /**
   * Deserialize json text to object
   * This method can deserialize any file with json text.
   * @method deserialize
   * @param  {json}    json [description]
   * @static
   * @memberof Serializable
   */
  static deserialize(json) {
    let r = function(k, v) {
      if (v instanceof Object && v.classname) {
        if (Serializable[srializable_classes][v.classname]) {
          let c = Serializable[srializable_classes][v.classname];

          v = Object.assign(new c(), v);
        } else if (v.classname === "DateProxyForClassSerializer") {
          return new Date(v.value);
        }
      }
      return v;
    }

    return JSON.parse(json, r);
  }

  /**
   * "jsclass-serializer" provides features to serialize and deserialize to memory
   * and to file in json format.  Deserializing returns instance of original class.
   * @method constructor
   * @param  {String}    dir Path to save / load from.
   */
  constructor(id, dir) {
    this.dir = dir || Serializable[environment].storage_path;
    let classname = this.constructor.name;
    Serializable[srializable_classes][classname] = this.constructor;
    this.classname = classname;
    this.uuid = id || uuid();
    Object.defineProperty(this, "classname", {
      configurable: false,
      writable: true,
    });
  }

  /**
   * Serialize object to json format.
   * @method serialize
   * @return {json}    Json text.
   * @instance
   * @memberof Serializable
   */
  serialize() {
    return Serializable.serialize(this);
  }

  /**
   * Save serialized json object to file.  Where directory path would be the
   * path previously set by setStoragePath(), and file name would be set equally
   * to given objects uuid.
   * @method saveToFile
   * @return {json}     Json text.
   * @instance
   * @memberof Serializable
   */
  saveToFile() {
    return Serializable.saveToFile(this);
  }

  /**
   * Deserialize json text to object
   * @method deserialize
   * @param  {json}    json [description]
   * @instance
   * @memberof Serializable
   */
  deserialize(json) {
    let r = function(k, v) {
      if (v.classname === "DateProxyForClassSerializer") {
        return new Date(v.value);
      }
      return v;
    }

    let o = JSON.parse(json, r);
    if (!this instanceof o.constructor) {
      throw "Type unmatch: [" + this.classname + "] is not instance of [" + o.constructor.name + "]";
    }

    return o;
  }

  /**
   * Load json text from file and convert to object. Where directory path would
   * be the path previously set by setStoragePath().
   * @method loadFromFile
   * @param  {string}     uuid Unique identifier to specify the file to load from.
   * @instance
   * @memberof Serializable
   */
  loadFromFile(uuid) {
    let dirname = this.dir;
    let filename = uuid || this.uuid;
    let filepath = dirname + filename;
    let json = fs.readFileSync(filepath);

    let o = Serializable.deserialize(json);

    Object.assign(this, o);
  }
}

if (!Serializable[srializable_classes]) {
  Serializable[srializable_classes] = {};
};

if (!Serializable[environment]) {
  Serializable[environment] = {};
};

/**
 * A module for de/serializing objects.
 * @module jsclass-serializer
 */
module.exports = Serializable;