:- module(assrt_example, [p/1]).

:- use_module(library(compound_expand)). % compound expansions
:- use_module(library(assertions)).
:- use_module(library(nativeprops)).
:- use_module(library(basicprops)).

:- check pred p(A) :: int(A) + not_fails.

p(1).
p(2).
