:- module(assrt_example, [p/1]).

:- use_module(library(assertions)).
:- use_module(library(basicprops)).
:- use_module(library(nativeprops)).
:- use_module(library(compound_expand)). % compound expansions

:- check pred p(A) :: int(A) + not_fails.

p(1).
p(2).
