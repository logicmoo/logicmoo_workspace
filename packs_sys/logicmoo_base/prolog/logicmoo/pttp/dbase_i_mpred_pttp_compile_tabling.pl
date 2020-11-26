:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
:- module(mpred_pttp_compile,[]).
:- endif.

/*
:-abolish(pttp_prove,6).
:- abolish(search_cost,3).
:- abolish(search,6).
:- abolish(make_wrapper,3).
:- abolish(add_features,2).
:- abolish(add_args,13).
:- abolish(query,0).
*/

:- use_module(library(dra/tabling3/swi_toplevel)).

% make_wrapper(_DefinedPreds,[query,0],true) :- !.
make_wrapper(_DefinedPreds,[P,N],(:-table(P/N))).

add_features(X,X).

pttp_prove(X):-query(X).


:- fixup_exports.

