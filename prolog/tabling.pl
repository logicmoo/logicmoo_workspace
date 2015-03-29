:- module(tabling, [op(1150, fx, table)]).

:- use_module(library(compound_expand)).
:- use_module(library(goalstub)).
:- reexport(library(tabling_rt)).

:- register_stub(table).

term_expansion(Term0, P0, Term, P) :-
    context_module(CM),
    stub_term_expansion(Term0, P0, CM, Term, P).
