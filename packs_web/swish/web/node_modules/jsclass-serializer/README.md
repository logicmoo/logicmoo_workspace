[![Build Status](https://travis-ci.org/kojiy7214/jsclass-serializer.svg?branch=master)](https://travis-ci.org/kojiy7214/jsclass-serializer)

# jsclass-serializer
Yet another serialize module for Node.js, with unique feature that deserialize to its original class based Object.

## What Makes class-serializer Unique
Amoung many serializers, jsclass-serializer module is unique for its class
restore ability.  That is, deserializing does not result in creating
Object object, but original class-based object are restored.

## How to Use
Just extend your class based on "Serializable" class.  And everything works.

## Save and/or Load from File (new @0.2.0)
You can save your serialized json text to file, and load it back easily.
You can also load all objects under storage path.
```
const Serializable = require('jsclass-serializer');

// Save and load to/from file
class SampleClass extends Serializable{};

let source = new SampleClass();

Serializable.setStoragePath('./data/');
source.saveToFile();

let target = new SampleClass();
target.loadFromFile(source.uuid);

//load all objects under storage path
let array_of_object = Serializable.loadAll();

```

## Date Ready
"jsclass-serializer" can de/serialize built-in Date type object.

## Works with "jsclass-mixin"
When developer decide to extend his/her class from Serializable, developer
should give up to extend from other classes.  That is a huge limitation, while
Java Script allows only single inheritance.
Now its not a limitation any more!  "jsclass-serializable" can be used with
["jsclass-mixin"](https://www.npmjs.com/package/jsclass-mixin)!  
Check out the code below!
```
// Serializable with jsclass-mixin
const Serializable = require('jsclass-serializer');
const mix = require('jsclass-mixin');

class B {};

class A extends mix(B, Serializable) {
  constructor() {
    super();

    //call Serializable constructor with "this" makes class A serializable
    Serializable.new(this);
  }
};


let source = new A();
let json = source.serialize();
let target = Serializable.deserialize(json);

//below code return true!
console.log(target instanceof A);
```

## Some Notes
### Use of Global Namespace
Class serializer consumes global with namespace "__serializable_classes__".
Within this name space, every constructor of Serializable subclasses are
stored.  This is because of its module-scoped nature of node, by default
class-serializer can not recognize target class definition.  Please note that
this results in extra node instance memory space
consuming.

### Property "classname" and "uuid" Are Attached Automatically
Serializable attaches property "classname" and "uuid" to its subclasses.  
"classname" is to retrieve original class info, and "uuid" is use as filename
when you save/load to/from files.

## Code Usage
Please take a look at  test/test.js for more sample codes.

### usage1:
```
const Serializable = require('jsclass-serializer');

// Serialize and Deserialize Using Static Methods

class SampleClass extends Serializable{};
let c = new SampleClass();

let json = Serializable.serialize(c);
let o = Serializable.deserialize(json);

//o is not a [Object object], but an instance of SampleClass
//below code returns, true.
console.log(o instanceof SampleClass);
```

### usage2:
```
const Serializable = require('jsclass-serializer');

// Serialize and Deserialize Using Normal Methods
//
class SampleClass extends Serializable{};
let c1 = new SampleClass();
c1.a = "A";

let json = c1.serialize();

let c2 = new SampleClass();
c2.deserialize(json);

//You can deserialize back to its original class.
//below code returns "A"
console.log(c2.a);
```

## API
## Modules

<dl>
<dt><a href="#module_jsclass-serializer">jsclass-serializer</a></dt>
<dd><p>A module for de/serializing objects.</p>
</dd>
</dl>

## Classes

<dl>
<dt><a href="#Serializable">Serializable</a></dt>
<dd></dd>
</dl>

## Functions

<dl>
<dt><a href="#constructor">constructor(baseclass)</a></dt>
<dd><p>&quot;jsclass-serializer&quot; provides features to serialize and deserialize to memory
and to file in json format.  Deserializing returns instance of original class.</p>
</dd>
</dl>

<a name="module_jsclass-serializer"></a>

## jsclass-serializer
A module for de/serializing objects.

<a name="Serializable"></a>

## Serializable
**Kind**: global class  

* [Serializable](#Serializable)
    * [new Serializable()](#new_Serializable_new)
    * _instance_
        * [.serialize()](#Serializable+serialize) ⇒ <code>json</code>
        * [.saveToFile()](#Serializable+saveToFile) ⇒ <code>json</code>
        * [.deserialize(json)](#Serializable+deserialize)
        * [.loadFromFile(uuid)](#Serializable+loadFromFile)
    * _static_
        * [.setStoragePath(p)](#Serializable.setStoragePath)
        * [.saveToFile(o, filename)](#Serializable.saveToFile) ⇒ <code>json</code>
        * [.loadFromFile(filename)](#Serializable.loadFromFile) ⇒ <code>any</code>
        * [.loadAll(Callback)](#Serializable.loadAll) ⇒ <code>Array</code>
        * [.serialize(o)](#Serializable.serialize) ⇒ <code>json</code>
        * [.deserialize(json)](#Serializable.deserialize)

<a name="new_Serializable_new"></a>

### new Serializable()
Serializable object and deserialize back to its original class instance.
Also supports save/load to/from file system.

<a name="Serializable+serialize"></a>

### serializable.serialize() ⇒ <code>json</code>
Serialize object to json format.

**Kind**: instance method of [<code>Serializable</code>](#Serializable)  
**Returns**: <code>json</code> - Json text.  
<a name="Serializable+saveToFile"></a>

### serializable.saveToFile() ⇒ <code>json</code>
Save serialized json object to file.  Where directory path would be the
path previously set by setStoragePath(), and file name would be set equally
to given objects uuid.

**Kind**: instance method of [<code>Serializable</code>](#Serializable)  
**Returns**: <code>json</code> - Json text.  
<a name="Serializable+deserialize"></a>

### serializable.deserialize(json)
Deserialize json text to object

**Kind**: instance method of [<code>Serializable</code>](#Serializable)  

| Param | Type | Description |
| --- | --- | --- |
| json | <code>json</code> | [description] |

<a name="Serializable+loadFromFile"></a>

### serializable.loadFromFile(uuid)
Load json text from file and convert to object. Where directory path would
be the path previously set by setStoragePath().

**Kind**: instance method of [<code>Serializable</code>](#Serializable)  

| Param | Type | Description |
| --- | --- | --- |
| uuid | <code>string</code> | Unique identifier to specify the file to load from. |

<a name="Serializable.setStoragePath"></a>

### Serializable.setStoragePath(p)
Set directory path to save/load serialized information to/from file.

**Kind**: static method of [<code>Serializable</code>](#Serializable)  

| Param | Type | Description |
| --- | --- | --- |
| p | <code>string</code> | Absolute or relative directory path |

<a name="Serializable.saveToFile"></a>

### Serializable.saveToFile(o, filename) ⇒ <code>json</code>
Save serialized json object to file.  Where directory path would be the
path previously set by setStoragePath().
This method can serialize any type of object.

**Kind**: static method of [<code>Serializable</code>](#Serializable)  
**Returns**: <code>json</code> - Json text.  

| Param | Type | Description |
| --- | --- | --- |
| o | <code>any</code> | Object to serialize. |
| filename | <code>string</code> | Filename to save object. |

<a name="Serializable.loadFromFile"></a>

### Serializable.loadFromFile(filename) ⇒ <code>any</code>
Load json text from file and convert to object. Where directory path would
be the path previously set by setStoragePath().
This method can deserialize any file with json text.

**Kind**: static method of [<code>Serializable</code>](#Serializable)  
**Returns**: <code>any</code> - Deserialized object.  

| Param | Type | Description |
| --- | --- | --- |
| filename | <code>string</code> | Filename to load json from. |

<a name="Serializable.loadAll"></a>

### Serializable.loadAll(Callback) ⇒ <code>Array</code>
Load all json files under storage directory.

**Kind**: static method of [<code>Serializable</code>](#Serializable)  
**Returns**: <code>Array</code> - Retrieved objects.  

| Param | Type | Description |
| --- | --- | --- |
| Callback | <code>function</code> | function applies to retrieved objects. |

<a name="Serializable.serialize"></a>

### Serializable.serialize(o) ⇒ <code>json</code>
Serialize object to json format.
This method can serialize any type of object.

**Kind**: static method of [<code>Serializable</code>](#Serializable)  
**Returns**: <code>json</code> - Json text.  

| Param | Type | Description |
| --- | --- | --- |
| o | <code>any</code> | Object to serialize. |

<a name="Serializable.deserialize"></a>

### Serializable.deserialize(json)
Deserialize json text to object
This method can deserialize any file with json text.

**Kind**: static method of [<code>Serializable</code>](#Serializable)  

| Param | Type | Description |
| --- | --- | --- |
| json | <code>json</code> | [description] |

<a name="constructor"></a>

## constructor(baseclass)
"jsclass-serializer" provides features to serialize and deserialize to memory
and to file in json format.  Deserializing returns instance of original class.

**Kind**: global function  

| Param | Type | Description |
| --- | --- | --- |
| baseclass | <code>any</code> | Set "this", when use with jsclass-mixin. |
