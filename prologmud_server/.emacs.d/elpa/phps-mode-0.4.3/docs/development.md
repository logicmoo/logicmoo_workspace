# Development

Make pull requests to `develop` branch or branches forked from develop branch. Tested changes that are considered improvements are merged to master. Make sure to sign the FSF-papers as well.

## Tests

If you have emacs at a customized location prefix the commands with your path, i.e.

`export emacs="~/Documents/emacs/src/emacs" && make tests`

Run all tests with `make tests`.

### Functions

Indentations, incremental processes, Imenu-support.

``` bash
make test-functions
```

### Integration

This should test all other parts in collaboration.

``` bash
make test-integration
```

### Lexer

Lexer token generation.

``` bash
make test-lexer
```

### Parser

Semantic grammar. Not ready yet.

``` bash
make test-parser
```

### Syntax-table

Basic point and region behaviour.

``` bash
make test-syntax-table
```

## Byte-compilation

Plug-in should support byte-compilation and it is recommended.

### Compile

``` bash
make compile
```

### Clean

``` bash
make clean
```



[Back to start](../../../)
