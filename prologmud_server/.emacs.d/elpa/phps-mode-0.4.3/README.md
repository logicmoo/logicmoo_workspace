# PHPs - Another Semantic Major-Mode for PHP in Emacs

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](https://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/cjohansson/emacs-phps-mode.svg?branch=master)](https://travis-ci.org/cjohansson/emacs-phps-mode)

**Goal**: An Emacs major mode for PHP scripting language which aims at making a full semantic integration. Currently at *usable* stage.

This mode does not require PHP installed on your computer because it has a built-in elisp based semantic lexer. It supports all PHP versions and Emacs >= 26.

## Features

* GPLv3 license
* Flycheck support with `(phps-mode-flycheck-setup)`
* Semantic lexer based on official PHP 8.0 re2c lexer
* Syntax coloring based on lexer tokens, makes it easier to spot invalid code
* PSR-1, PSR-2 and PSR-12 indentation based on lexer tokens
* PSR-1, PSR-2 and PSR-12 supported white-space
* Integration with `(electric-pair)`
* Incremental lexer and syntax coloring after buffer changes
* Incremental indentation and imenu generation after buffer changes
* Supports `(comment-region)` and `(uncomment-region)`
* Support indentation for inline-html areas
* Imenu support
* Minimal mode map
* Tested using unit tests and integration tests
* Continuous integration tests using Travis
* Included in GNU ELPA package archive
* A interactive function that can be used interactively to format buffers `(phps-mode-format-buffer)`
* Support for asynchronous lexer via processes (`async.el`) or threads
* Mode-line asynchronous status
* Bookkeeping in lexical-analysis, showing defined and undefined variables via syntax coloring

## Roadmap

* 1. LALR(1) Parser support
* 2. Improved token-blind indentation (alternative and inline control structures)
* 3. Other enhancements

## Keymap

* `C-c C-r` Rescan buffer
* `C-c C-f` Format buffer

## Installation

### Install manually

If you have downloaded manually i.e. to `~/.emacs.d/phps-mode/` you need to add this first to your init file:

``` emacs-lisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/phps-mode/"))
```

### Install via package manager

You can install via ELPA (`M-x package-install` + `RET` + `phps-mode` + `RET`), package will now be loaded automatically when Emacs starts.

## Configuration

### Enable flycheck support

For flycheck support run `(phps-mode-flycheck-setup)`.

### Asynchronous lexer

Enable with `(setq phps-mode-async-process t)`
Disable with `(setq phps-mode-async-process nil)`

### Asynchronous lexer via async.el processes

Enable with:

``` emacs-lisp
(setq phps-mode-async-process-using-async-el t)
```

### Asynchronous lexer via threads

Enable with:

``` emacs-lisp
(setq phps-mode-async-process-using-async-el nil)
```

## Installation and Configuration examples

### Install, load and configure via use-package with flycheck support, asynchronous support via async.el

``` emacs-lisp
(use-package phps-mode
    :after flycheck
    :ensure t
    :mode ("\\.php\\'" "\\.phtml\\'")
    :config
    (phps-mode-flycheck-setup)
    (setq phps-mode-async-process t)
    (setq phps-mode-async-process-using-async-el t))
```

### Load and configure using use-package with flycheck support, asynchronous support via threads

``` emacs-lisp
(use-package phps-mode
    :after flycheck
    :mode ("\\.php\\'" "\\.phtml\\'")
    :config
    (phps-mode-flycheck-setup)
    (setq phps-mode-async-process t)
    (setq phps-mode-async-process-using-async-el nil))
```

### Load and configure using regular emacs-lisp with flycheck support, no asynchronous support
``` emacs-lisp
(require 'phps-mode)
(add-to-list 'auto-mode-alist '("\\.\\(php\\|phtml\\)\\'" . phps-mode))
(phps-mode-flycheck-setup)
(setq phps-mode-async-process nil)
```

## Read more

* [Development](docs/development.md)
* [Heuristics](docs/heuristics.md)
* [Indentation algorithm](docs/indentation.md)
* [Imenu algorithm](docs/imenu.md)
* [TODO](docs/todo.md)
