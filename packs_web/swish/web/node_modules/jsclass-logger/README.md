[![Build Status](https://travis-ci.org/kojiy7214/jsclass-logger.svg?branch=master)](https://travis-ci.org/kojiy7214/jsclass-logger)

# jsclass-logger
Logger which works perfectly both in production/debug environment.

## What Makes "jsclass-logger" Unique
While there are many proven logging libraries, including famous "Winston",
"jsclass-logger" focuses on ease of use.
Check out the code below.

```
let logger;

// setup logger for production output
logger = require("jsclass-logger")();

// setup logger to output log with module name
logger = require("jsclass-logger")({}, "your_module_name");

// setup logger for development output
logger = require("jsclass-logger")({"debug":true});

// setup logger for file output
logger = require("jsclass-logger")({"toFile":true});
```  

Here is an output sample.
```
// normal message
logger.info("log message");

> 2018-10-16 00:17:17.008 INFO  (test.js:34:20) log message

// object dump
logger.debug(object);

> 2018-10-16 00:17:17.011 DEBUG (test.js:34:21) *** OBJECT DUMP ***
>   TestClass {
>     "a": 1,
>     "b": 2,
>     "c": 3
>   }

// function dump
logger.debug(function);

> 2018-10-16 00:17:17.014 DEBUG (test.js:34:22) *** FUNCTION DUMP ***
>   function(a, b, c){return a+b+c};
```

## Supports Log Rotation
You can specify when to rotate log and how many generations you want to keep
easily.

```
// setup logger to rotate log file when file size exceed 100K
// And keep 3 generations on disk.
logger = require("jsclass-logger")({
  "toFile": true,
  "rotate": (file) => fs.statSync(file).size > 100000,
  "generation": 3
  });
```

## Supports Timestamp Format Customize
You can customize timestamp format to meet your locale.

```
// setup logger to output customized timestamp
logger = require("jsclass-logger")({
  "ts": "DD/MM/YYYY hh:mm:ss"
});
logger.info("log message");

> 16/10/2018 00:17:17 INFO  (test.js:34:20) log message
```

## Supports Stack Trace Dump
To print out stack trace, use "error()" or "trace()" to output log.

```
logger.error("log message");

> 16/10/2018 10:54:40 ERROR (test.js:32:20) log message
> at Context.<anonymous> (/Volumes/SDDrive/Google Drive/lib/jsclass-logger/test/test.js:32:20)
>     at callFn (/Volumes/SDDrive/Google Drive/lib/jsclass-logger/node_modules/mocha/lib/runnable.js:372:21)
>     at Test.Runnable.run (/Volumes/SDDrive/Google Drive/lib/jsclass-logger/node_modules/mocha/lib/runnable.js:364:7)
```

## Supports Multi Log Level
### Log Level for Debug Mode
"debug()" and "trace()" are log level for debug mode, so there will be no output
when you don't set "debug: true" option to get logger instance.

### Log Level For  Production Mode
"info()", "warn()", "error()" are log lebel for production mode, so they will
output log anytime.

## Usage
### API
## Modules

<dl>
<dt><a href="#module_jsclass-logger">jsclass-logger</a></dt>
<dd><p>Simple and easy to use logger.</p>
</dd>
</dl>

## Classes

<dl>
<dt><a href="#Logger">Logger</a></dt>
<dd><p>Logger class.</p>
</dd>
</dl>

## Functions

<dl>
<dt><a href="#getLogger">getLogger(desc, mod)</a> ⇒ <code><a href="#Logger">Logger</a></code></dt>
<dd><p>Get logger instance. supprted options are as below.<br>
-debug: true / false<br>
-toFile: true / false<br>
-outDir: directory to output log file, by default it is &quot;./log/&quot;<br>
-rotate: function to check when to rotate log file, should return true / false<br>
-generation: log files to keep on disk<br>
-ts: timestamp format in YYYYMMDDhhmmsszzz format<br></p>
</dd>
</dl>

<a name="module_jsclass-logger"></a>

## jsclass-logger
Simple and easy to use logger.

<a name="Logger"></a>

## Logger
Logger class.

**Kind**: global class  

* [Logger](#Logger)
    * [.debug(msg)](#Logger+debug)
    * [.info(msg)](#Logger+info)
    * [.warn(msg)](#Logger+warn)
    * [.trace(msg)](#Logger+trace)
    * [.error(msg)](#Logger+error)

<a name="Logger+debug"></a>

### logger.debug(msg)
Output log at DEBUG level, which is a debug level.

**Kind**: instance method of [<code>Logger</code>](#Logger)  

| Param | Type | Description |
| --- | --- | --- |
| msg | <code>any</code> | Log message. |

<a name="Logger+info"></a>

### logger.info(msg)
Output log at INFO level, which is a production level.

**Kind**: instance method of [<code>Logger</code>](#Logger)  

| Param | Type | Description |
| --- | --- | --- |
| msg | <code>any</code> | Log message. |

<a name="Logger+warn"></a>

### logger.warn(msg)
Output log at WARN level, which is a production level.

**Kind**: instance method of [<code>Logger</code>](#Logger)  

| Param | Type | Description |
| --- | --- | --- |
| msg | <code>any</code> | Log message. |

<a name="Logger+trace"></a>

### logger.trace(msg)
Output log at TRACE level, which is a debug level.

**Kind**: instance method of [<code>Logger</code>](#Logger)  

| Param | Type | Description |
| --- | --- | --- |
| msg | <code>any</code> | Log message. |

<a name="Logger+error"></a>

### logger.error(msg)
Output log at ERROR level, which is a production level.

**Kind**: instance method of [<code>Logger</code>](#Logger)  

| Param | Type | Description |
| --- | --- | --- |
| msg | <code>any</code> | Log message. |

<a name="getLogger"></a>

## getLogger(desc, mod) ⇒ [<code>Logger</code>](#Logger)
Get logger instance. supprted options are as below.<br>
-debug: true / false<br>
-toFile: true / false<br>
-outDir: directory to output log file, by default it is "./log/"<br>
-rotate: function to check when to rotate log file, should return true / false<br>
-generation: log files to keep on disk<br>
-ts: timestamp format in YYYYMMDDhhmmsszzz format<br>

**Kind**: global function  
**Returns**: [<code>Logger</code>](#Logger) - Logger instance  

| Param | Type | Description |
| --- | --- | --- |
| desc | <code>option</code> | Logger option |
| mod | <code>option</code> | modulename to output |
