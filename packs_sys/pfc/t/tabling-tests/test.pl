
:- module(test,[p/2]).

:- use_module(library(dra)).

:- table((p/2, q/2)).


p(X,Y) :- p(X,Z), q(Z,Y).
p(X,Y) :- e(X,Y).
q(X,Y) :- p(X,Y).

e(1,2).
e(2,3).



:- listing([p/2,q/2,e/2,(tnot)/1]).

:- if(exists_source(library(logicmoo_utils))).
:- use_module(library(logicmoo_utils)).

% ?- rtrace(p(_X,_Y)).

:- endif.

?- p(_X,_Y).
