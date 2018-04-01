:- module(mutest, [mutest/0]).

:- use_module(library(andorra/andorra)).

:- determinate( test, true ).
:- determinate( rule1(_,_), true ).
:- determinate( rule2(_,_), true ).
%:- determinate( rule3(A,B), ( nonvar(A), A?\=[_|_] ; nonvar(B), B?\=[_|_] )).
%:- determinate( rule4(A,B), (nonvar(B), B?\=[_|_]; nonvar(A),nonvar(B), term(A,[1]) ?\= term(B,[1]))). 
%:- determinate( theorem(A,_B), (nonvar(A), \+atomic(A)) ).
%:- determinate( app(A,B,C), ( nonvar(C), nonvar(B) , B?\=C ;
%	                      nonvar(A),( A?=[] ;  A?=[_|_]) ; 
%	                       nonvar(C), C?\=[_|_] )).

mutest :-
        theorem(5,[m,u,i,i,u]).

rule1(A,B) :-
        app(C,[i],A),
        app(C,[i,u],B).

rule2([m|A],[m|B]) :-
        app(A,A,B).

rule3(A,B) :-
        app([i,i,i],C,A),
        app([u],C,B).
rule3([A|B],[A|C]) :-
        rule3(B,C).

rule4([A|B],C) :-
        app([u,u],C,[A|B]).
rule4([A|B],[A|C]) :-
        rule4(B,C).

:- determinate( rules(_,_), false ).

rules(A,B) :-
        rule3(A,B).
rules(A,B) :-
        rule4(A,B).
rules(A,B) :-
        rule1(A,B).
rules(A,B) :-
        rule2(A,B).

theorem(_,[m,i]).
theorem(A,[B|C]) :-
        A>0,
        D is A-1,
        theorem(D,E),
        rules(E,[B|C]).

app([],A,A).
app([A|B],C,[A|D]) :-
        app(B,C,D).
