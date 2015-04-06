:- module(tab2, [tab2/0]).

% :- use_module(library(tabling)).

%:- table(f/1).

f(X) :-
    display('X=?'),
    read(X),
    display(X),nl.
f(a) :- display(a),nl.
f(b) :- display(b),nl.
f(c) :- display(c),nl.

p(_).

tab2 :-
    p(_),
    f(A),
    f(B),
    display(A-B),nl,
    fail.
tab2.
