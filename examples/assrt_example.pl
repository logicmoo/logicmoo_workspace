:- module(assrt_example, [p/1]).

:- use_module(assertions(assertions)).
:- use_module(assertions(basicprops)).
:- use_module(assertions(nativeprops)).
:- use_module(xlibrary(compound_expand)). % compound expansions

:- check pred p(A) :: int(A) + not_fails.

p(1).
p(2).
