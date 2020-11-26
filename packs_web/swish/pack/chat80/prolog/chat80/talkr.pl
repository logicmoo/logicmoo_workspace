/* @(#)talkr.pl	24.1 2/23/88 */

/*
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
/* Simplifying and executing the logical form of a NL query. */

:-public write_tree/1, answer/1, satisfy//2.

:-mode write_tree(+).
:-mode wt(+,+).
:-mode header(+).
:-mode decomp(+,-,-).
:-mode complex(+).
:-mode othervars(+,-,-).

write_tree(T):-
   numbervars(T,0,_),
   wt(T,0),
   fail.
write_tree(_).

wt((P:-Q),L) :- !, L1 is L+3,
   write(P), tab(1), write((:-)), nl,
   tab(L1), wt(Q,L1).
wt((P,Q),L) :- !, L1 is L-2,
   wt(P,L), nl,
   tab(L1), put("&"), tab(1), wt(Q,L).
wt({P},L) :- complex(P), !, L1 is L+2,
   put("{"), tab(1), wt(P,L1), tab(1), put("}").
wt(E,L) :- decomp(E,H,P), !, L1 is L+2,
   header(H), nl,
   tab(L1), wt(P,L1).
wt(E,_) :- write(E).

header([]).
header([X|H]) :- write(X), tab(1), header(H).

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

:-mode respond(?).
:-mode holds(+,?).
:-mode answer(+).
:-mode yesno(+).         :-mode replies(+).
:-mode reply(+).
:-mode seto(?,+,-).
:-mode satisfy(+).
:-mode pickargs(+,+,+).
:-mode pick(+,?).

respond([]) :- display('Nothing satisfies your question.'), nl.
respond([A|L]) :- reply(A), replies(L).

answer((answer([]):-E)) :- !, holds(E,B), yesno(B).
answer((answer([X]):-E)) :- !, seto(X,E,S), respond(S).
answer((answer(X):-E)) :- seto(X,E,S), respond(S).

seto(X,E,S) :-
%	portray_clause(({X} :- E)),
	phrase(satisfy(E,G),Vars),
%	portray_clause(({X} :- G)),
	(   setof(X,Vars^G,S)
	->  true
	;   S = []
	).

holds(E,True) :-
	phrase(satisfy(E, G), _),
	(   G
	->  True = true
	;   True = false
	).

yesno(true) :- display('Yes.').
yesno(false) :- display('No.').

replies([]) :- display('.').
replies([A]) :- display(' and '), reply(A), display('.').
replies([A|X]) :- display(', '), reply(A), replies(X).

reply(N--U) :- !, write(N), display(' '), write(U).
reply(X) :- write(X).

%%	satisfy(+Term, -Goal)//
%
%	Originally, Term was meta-interpreted. If we   do not want every
%	^/2-term to act as an existential quantification, this no longer
%	works. Hence, we now compile the term   into  a goal and compute
%	the existentially quantified variables.

satisfy((P0,Q0), (P,Q)) --> !, satisfy(P0, P), satisfy(Q0, Q).
satisfy({P0}, (P->true)) --> !, satisfy(P0, P).
satisfy(X^P0, P) --> !, satisfy(P0, P), [X].
satisfy(\+P0, \+P) --> !, satisfy(P0, P).
satisfy(numberof(X,P0,N), (setof(X,Vars^P,S),length(S,N))) --> !,
	{ phrase(satisfy(P0,P),Vars) },
	[S], Vars.			% S is an internal variable!
satisfy(setof(X,P0,S), setof(X,Vars^P,S)) --> !,
	{ phrase(satisfy(P0,P),Vars) },
	Vars.
satisfy(+P0, \+ exceptionto(P)) --> !,
	satisfy(P0, P).
satisfy(X<Y, X<Y) --> !.
satisfy(X=<Y, X=<Y) --> !.
satisfy(X>=Y, X>=Y) --> !.
satisfy(X>Y, X>Y) --> !.
satisfy(P, database(P)) --> [].

exceptionto(P) :-
   functor(P,F,N), functor(P1,F,N),
   pickargs(N,P,P1),
   exception(P1).

exception(P) :- database(P), !, fail.
exception(P).

pickargs(0,_,_) :- !.
pickargs(N,P,P1) :- N1 is N-1,
   arg(N,P,S),
   pick(S,X),
   arg(N,P1,X),
   pickargs(N1,P,P1).

pick([X|S],X).
pick([_|S],X) :- !, pick(S,X).
pick([],_) :- !, fail.
pick(X,X).

