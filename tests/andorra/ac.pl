:- module(ac, [atom_concat/4]).

:- use_module(library(andorra/andorra)).

atom_concat(A, B, C, D) :-
    atom_concat(A, B, AB),
    atom_concat(AB, C, D).
