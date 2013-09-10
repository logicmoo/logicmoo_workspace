# Synopsis

    :- use_module(library(re)).
    main :-
        '99 bottles of beer' =~ '[0-9]+ bottles',
        writeln('Take one down...').

# Description

Regular expression support for Prolog.

When Prologers want to match a string against a pattern, they typically write a DCG.  DCGs are powerful and flexible.  For medium to large patterns, they are also easier to read and maintain.  However, for small and local patterns the overhead of writing and naming auxiliary predicates can be too much.  In those circumastances, one might prefer a regular expression.  This pack makes it possible.

The `=~` operator matches a string (on the left side) against a regular expression (on the right side).  Either side can be an atom or a list of codes.  The `\~` operator succeeds if the string does _not_ match the pattern.

# Syntax Reference

This section lists the regular expression syntax accepted by library(re).  Syntax not listed here is not yet supported.  Patches welcome.

## Single characters

  * `.` - any character, including newline
  * `[xyz]` - character class
  * `[^xyz]` - negated character class

## Composites

  * `xy` - `x` followed by `y`
  * `x|y` - `x` or `y` (prefer x)

## Repetitions

  * `x*` - zero or more `x`, prefer more
  * `x+` - one or more `x`, prefer more
  * `x+` - zero or one `x`, prefer one
  * `x{n,m}` - `n` or `n+1` or ... or `m` `x`, prefer more
  * `x{n,}` - `n` or more `x`, prefer more
  * `x{n}` - exactly `n` `x`

## Grouping

  * `(re)` - numbered capturing group

## Empty strings

  * `^` - at beginning of text
  * `$` - at end of text

## Character class elements

  * `x` - single character
  * `A-Z` - character range (inclusive)

# Changes in this Version

  * First public release

# Acknowledgements

Rob Cameron for his [lecture notes](http://www.cs.sfu.ca/~cameron/Teaching/384/99-3/regexp-plg.html) on which the original implementation was based.

# Installation

Using SWI-Prolog 6.3 or later:

    ?- pack_install(re).

This module uses [semantic versioning](http://semver.org/).
Source code available and pull requests accepted at
http://github.com/mndrix/re

@author Michael Hendricks <michael@ndrix.org>
@license BSD
