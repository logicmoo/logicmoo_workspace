/*
 _________________________________________________________________________
|       Copyright (C) 1982                                                |
|                                                                         |
|       David Warren,                                                     |
|               SRI International, 333 Ravenswood Ave., Menlo Park,       |
|               California 94025, USA;                                    |
|                                                                         |
|       Fernando Pereira,                                                 |
|               Dept. of Architecture, University of Edinburgh,           |
|               20 Chambers St., Edinburgh EH1 1JZ, Scotland              |
|                                                                         |
|       This program may be used, copied, altered or included in other    |
|       programs only for academic purposes and provided that the         |
|       authorship of the initial program is aknowledged.                 |
|       Use for commercial purposes without the previous written          |
|       agreement of the authors is forbidden.                            |
|_________________________________________________________________________|

*/

/* Simplifying and executing the logical form of a NL query. */

:-public write_tree/1, answer80/1.
:-op(500,xfy,--).

write_tree(T):-
   numbervars80(T,0,_),
   wt(T,0),
   fail.
write_tree(_).

wt(T,_L) :- as_is_old(T),fmt(T),!.
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

respond(true) :- reply('Yes.').
respond(false) :- reply('No.').
respond([]) :- reply('Nothing satisfies your question.'), nl.
respond([A|L]) :- reply(A), replies(L).

answer80(S1):- answer802(S1,S),respond(S).

answer802((answer80([]):-E),[B]) :- !, holds_truthvalue(E,B).
answer802((answer80([X]):-E),S) :- !, seto(X,E,S).
answer802((answer80(X):-E),S) :- seto(X,E,S).

/*seto(X,E,S) :- ground(X),
%	portray_clause(({X} :- E)),
	phrase(satisfy80(E,G),Vars),
	pprint_ecp_cmt(yellow,((X+Vars):-G)),!,
	seto2(X,Vars,G,S).

seto2(X,[],G,S):- !, (setof(X,G,S) -> ignore( S = [X] ) ;  S = []).
seto2(X,Vars,G,S):- setof(X,Vars^G,S) -> ignore( S = [X]) ;  S = [].
*/

seto(X,E,S) :-
%	portray_clause(({X} :- E)),
	phrase(satisfy80(E,G),Vars),
	pprint_ecp_cmt(yellow,((X+Vars):-G)),!,
	(   setof(X,Vars^G,S)
	*->  true
	;   S = []
	).



holds_truthvalue(E,True) :-
	phrase(satisfy80(E, G), _),
	(   pprint_ecp_cmt(yellow,G),
      call(G)
	->  True = true
	;   True = false
	).
	
replies([]) :- reply('.').
replies([A]) :- reply(' and '), reply(A), reply('.').
replies([A|X]) :- reply(', '), reply(A), replies(X).

reply(N--U) :- !, write(N), write(' '), write(U).
reply(X) :- write(X).

%%	satisfy80(+Term, -Goal)//
%
%	Originally, Term was meta-interpreted. If we   do not want every
%	^/2-term to act as an existential quantification, this no longer
%	works. Hence, we now compile the term   into  a goal and compute
%	the existentially quantified variables.
numberof(X,Vars^P,N):- setof(X,Vars^P,S),length(S,N).

%satisfy(X,Y):- satisfy80(X,Y).

satisfy80((P0,Q0), (P,Q)) --> !, satisfy80(P0, P), satisfy80(Q0, Q).
satisfy80({P0}, (P->true)) --> !, satisfy80(P0, P).
satisfy80(X^P0, P) --> !, satisfy80(P0, P), [X].
satisfy80(\+P0, \+P) --> !, satisfy80(P0, P).
satisfy80(numberof(X,P0,N), Out) --> !,
	{ phrase(satisfy80(P0,P),Vars) },
	 Vars,			% S is an internal variable!
  {Out = (numberof(X,Vars^P,N))}.
satisfy80(numberof(X,P0,N), Out) --> !,
	{ phrase(satisfy80(P0,P),Vars) },
	[S], Vars,			% S is an internal variable!
  {Out = (setof(X,Vars^P,S),length(S,N))}.
satisfy80(setof(X,P0,S), setof(X,Vars^P,S)) --> !,
	{ phrase(satisfy80(P0,P),Vars) },
	Vars.
satisfy80(+P0, \+ exceptionto(P)) --> !,
	satisfy80(P0, P).
satisfy80(X<Y, X<Y) --> !.
satisfy80(X=<Y, X=<Y) --> !.
satisfy80(X>=Y, X>=Y) --> !.
satisfy80(X>Y, X>Y) --> !.
satisfy80(P, database80(P)) --> [].

exceptionto(P) :-
   functor(P,F,N), functor(P1,F,N),
   pickargs(N,P,P1),
   exception80(P1).

exception80(P) :- database80(P), !, fail.
exception80(_P).

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

