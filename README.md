xtools
======

Extended tools for Prolog

Usage
=====

This library contains several development tools, although not all are listed here, the most stable and relevant ones follows:

Static Analysis
===============
A static analysis tool mostly based on looking the abstract syntax graph for inconsistencies, for example:

```
 ?- [library(checkers)].
true.
 ?- checkall([dir('.')]).
```

will execute all the checkers to analyze the modules in the directory '.'. Alternatively you can use checkallc instead, which is the concurrent version of the above.

If you want to use specific analysis, use the predicate showcheck(Analysis, Options), where Analsyis can be:
