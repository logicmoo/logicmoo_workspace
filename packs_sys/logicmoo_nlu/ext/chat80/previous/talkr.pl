/*
 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/

/* Simplifying and executing the logical form of a NL query. */

:-op(500,xfy,--).
:-op(359,xf,ject).

write_tree(T):-
   numbervars80(T,1,_),
   wt(T,0),
   fail.
write_tree(_).

wt(T,D):- tab(D),fmt(T),!.

wt((P:-Q),L) :- !, L1 is L+3,
   write(P), tab(1), write((:-)), nl,
   tab(L1), wt(Q,L1).
wt((P,Q),L) :- !, L1 is L-2,
   wt(P,L), nl,
   tab(L1), put("&"), tab(1), wt(Q,L).
wt({P},L) :- complex(P), !, L1 is L+2,
   put("{"), tab(1), wt(P,L1), tab(1), put("}").
wt(E,L) :- decomp(E,H,P), !, L1 is L+2,
   header80(H), nl,
   tab(L1), wt(P,L1).
wt(E,_) :- write(E).

header80([]).
header80([X|H]) :- reply(X), tab(1), header80(H).

decomp(setof(X,P,S),[S,=,setof,X],P).  
decomp(\+(P),[\+],P) :- complex(P).
decomp(numberof(X,P,N),[N,=,numberof,X],P).
decomp(X^P,[exists,X|XX],P1) :- othervars(P,XX,P1).

othervars(X^P,[X|XX],P1) :- !, othervars(P,XX,P1).
othervars(P,[],P).

complex((_,_)).
complex({_}).
complex(setof(_,_,_)).
complex(numberof(_,_,_)).
complex(_^_).
complex(\+P) :- complex(P).

% Query execution.

respond([]) :- reply('Nothing satisfies your question.'), nl.
respond([A|L]) :- reply(A), replies(L).

answer80(S1):- answer802(S1,S),respond(S).

answer802((answer80([]):-E),[B]) :- !, holds_truthvalue(E,B).
answer802((answer80([X]):-E),S) :- !, seto(X,E,S).
answer802((answer80(X):-E),S) :- seto(X,E,S).

seto(X,E,S) :- setof(X,satisfy(E),S), !.
seto(_X,_E,[]).

holds_truthvalue(E,true) :- satisfy(E), !.
holds_truthvalue(_E,false).

yesno(true):-reply(' Yes. ').
yesno(false):-reply(' No.').

replies([]) :- reply('.').
replies([A]) :- reply(' and '), reply(A), reply('.').
replies([A|X]) :- reply(', '), reply(A), replies(X).

reply(N--U) :- !, write(N), write(' '), write(U).
reply(X) :- write(X).

satisfy((P,Q)) :- !, satisfy(P), satisfy(Q).
satisfy((P;Q)) :- !, satisfy(P) ; satisfy(Q).
%satisfy((P->Q;R)) :- !, satisfy(P) -> satisfy(Q) ; satisfy(R).
%satisfy((P->Q)) :- !, satisfy(P) -> satisfy(Q).
satisfy({P}) :- !, satisfy(P), !.
satisfy(_X^P) :- !, satisfy(P).
satisfy(\+P) :- satisfy(P), !, fail.
satisfy(\+_P) :- !.
satisfy(setof(X,P,S)) :- !, setof(X,satisfy(P),S).
satisfy(numberof(X,P,N)) :- !, numberof(X,P,N).
satisfy(X<Y) :- !, X<Y.
satisfy(X=<Y) :- !, X=<Y.
satisfy(X>=Y) :- !, X>=Y.
satisfy(X>Y) :- !, X>Y.
satisfy(P) :- call_u(P).


^(_X,P) :- !, satisfy(P).

+P :-!, satisfy_0(+P).

satisfy_0(+P) :- exceptionto(P), !, fail.
satisfy_0(+_P) :- !.

numberof(X,P,N) :- setof(X,satisfy(P),S), length(S,N).

exceptionto(P) :-
   functor(P,F,N), functor(P1,F,N),
   pickargs(N,P,P1),
   exception(P1).

exception(P) :- P, !, fail.
exception(_P).

pickargs(0,_,_) :- !.
pickargs(N,P,P1) :- N1 is N-1,
   arg(N,P,S),
   pick(S,X),
   arg(N,P1,X),
   pickargs(N1,P,P1).

pick([X|_S],X).
pick([_|S],X) :- !, pick(S,X).
pick([],_) :- !, fail.
pick(X,X).

