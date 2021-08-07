# Unibeautify-CLI

[![Build Status](https://travis-ci.com/Unibeautify/unibeautify-cli.svg?branch=master)](https://travis-ci.com/Unibeautify/unibeautify-cli)

> CLI for [Unibeautify](https://github.com/Unibeautify/unibeautify)

## Installation

```bash
$ npm install --global unibeautify-cli
```

## Usage

The package will be globally accessible as a new command-line interface (CLI) application called [`unibeautify`](https://github.com/Unibeautify/unibeautify).

### Help

**Note**: Not all options are currently supported.

```bash
$ unibeautify --help

  Usage: unibeautify [options] [files...]

  Options:

    -h, --help                 output usage information
    -V, --version              output the version number
    -l, --language <language>  Language of file to beautify
    -o, --out-file <file>      Output file of beautified results
    -r, --replace              Replace file(s) with beautified results
    -c, --config-file <file>   Beautifier configuration file
    --config-json <JSON>       Beautifier configuration as stringified JSON

```

### Example

Install a Beautifier, such as [`beautifier-prettydiff`](https://github.com/Unibeautify/beautifier-prettydiff):

```
$ npm install --global beautifier-prettydiff
```

Then beautify using a language that Beautifier supports:

```bash
$ echo "function(n){return n+1;}" | unibeautify --language JavaScript --config-json '{"JavaScript":{"insize":2,"inchar":" "}}'
```

This returns the following formatted result:

```javascript
function (n) {
    return n + 1;
}                                                                                
```
