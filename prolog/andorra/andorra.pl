:- module(andorra, []).

:- reexport(library(compound_expand)).
:- reexport(library(andorra/andorra_op)).
:- reexport(library(andorra/andorra_rt)).
:- reexport(library(andorra/andorra_builtins)).
:- use_module(library(andorra/andorra_tr)).

term_expansion(Term1, Term) :-
    andorra_term_expansion(Term1, Term).
