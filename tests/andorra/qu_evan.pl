:- module(qu_evan, [queen/2]).

:- use_module(library(andorra/andorra)).

test:- queen(8,_), fail.

:- determinate(queen(_,_),true).
%% :- determinate(queen2(A,B,C), ( nonvar(A) ; B?\=C ) ).
%:- determinate(queen2(A,B,C), ( nonvar(A) ; nonvar(B),nonvar(C),\+(B=C) ) ).
:- determinate(queen2(A,B,C), ( nonvar(A) ; nonvar(B),nonvar(C),B?\=C ) ).
:- determinate(safe(A,_,_),nonvar(A)).
%:- determinate(del(A,B,C), ( nonvar(A),nonvar(B),B=[H|_],nonvar(H),\+(H=A) ;
%                             nonvar(C), \+(C=[_|_]) )).
:- determinate(del(A,B,C), ( nonvar(A),nonvar(B),A ?\= term(B,[1]) ; nonvar(C), C ?\= [_|_])).
:- determinate(gen(A,B), ( nonvar(A) ; nonvar(B) ) ).
:- determinate(ourdo,true).

queen(N,A) :- gen(N,L), queen2(L,[],A).

queen2([],R,R).
queen2([H|T],R,P) :-
    del(A,[H|T],L),        %
    safe(R,A,1),           % full parallelism here?  if so, what is
    queen2(L,[A|R],P).     % programmer's model?

safe([],_,_).
safe([H|T],U,D) :-
    G is H+D,
    G \== U,
    F is H-D,
    F \== U,
    D1 is D+1,
    safe(T,U,D1).

del(X,[X|Y],Y).
del(X,[Y|Z],[Y|W]) :- del(X,Z,W).

gen(0,[]) :- !.
gen(N,[N|X]) :- M is N-1, gen(M,X).

/* 
prolog

108580 millisecons
reductions: 109412
fail:       7657
total:      117069

93260 millisecons
reductions: 99545
fail:       5509
total:      105054

S 2
77170 millisecons
reductions: 102787
fail:       5588
total:      108375

no output

S 0
91110 millisecons
reductions: 98902
fail:       5509
total:      104411

S 1
79080 millisecons
reductions: 100655
fail:       5557
total:      106212
 */
%%%%%%%%%%%%%%%%%%%%%
