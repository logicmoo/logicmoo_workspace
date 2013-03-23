:- module(assertions, []).

:- use_module(library(expansion_module)).
:- reexport(library(assertions_op)).
:- reexport(library(assertions/assrt_lib)).

assrt_lib:nodirective_error_hook(Assr) :-
    throw(error(context_error(nodirective, Assr), _)).

term_expansion((:- Decl), Records) :-
    assertion_records(Decl, Records).
