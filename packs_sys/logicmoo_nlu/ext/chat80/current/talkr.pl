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

get_ex_set(ExV):- nb_current(ex_set,ExV)->true;ExV=[].

bago(X,E,S):- get_ex_set(ExV),bago2(ExV,X,E,S).
bago2(ExV,X,E,Set):- seto2(ExV,X,E,Set).

seto(X,E,S):- get_ex_set(ExV),seto2(ExV,X,E,S).


seto2(ExV,X,G^E,Set):- must(callable(E)), !,seto2(G^ExV,X,E,Set).
seto2(ExV,X,E,Set):- findall(S,setof(X,ExV^satisfy(E),S),L),ll_to_set(L,Set),!.
/*
seto2(ExV,X,E,S):- % nop(ExV\==[]),
   !,
  (setof(X,ExV^satisfy(E),S)*->true;S=[]).

seto2([],X,E,S):- 
  term_variables(E,EVars),
   term_singletons(X+X+EVars,EOnlyVars),
   term_singletons(E,ESingles),
   term_singletons(X+X+ESingles,ESinglesNoX),
   locally(b_setval(ex_set,ESinglesNoX),
     (setof(X,EOnlyVars^satisfy(E),S)*->true;S=[])).
*/

respond([]) :- reply('Nothing satisfies your question.'), nl.
respond([A|L]) :- reply(A), replies(L).

answer80(S1):- answer802(S1,S),respond(S).

answer802((answer80([]):-E),[B]) :- !, holds_truthvalue(E,B).
answer802((answer80([X]):-E),S) :- !, seto(X,E,S).
answer802((answer80(X):-E),S) :- seto(X,E,S).



holds_truthvalue(E,true) :- satisfy(E), !.
holds_truthvalue(_E,false).

yesno(true):-reply(' Yes. ').
yesno(false):-reply(' No.').

replies([]) :- reply('.').
replies([A]) :- reply(' and '), reply(A), reply('.').
replies([A|X]) :- reply(', '), reply(A), replies(X).

reply(N--U) :- !, write(N), write(' '), write(U).
reply(X) :- write(X).

must_be_callable(P):- notrace(( assertion(callable(P)),assertion(\+ is_list(P)))).

:- b_setval(ex_set,[]).

:- meta_predicate(satisfy(0)).
:- export(satisfy/1).
satisfy(P):- must_be_callable(P), fail.
satisfy(M:P):- !, M:satisfy(P).
satisfy((P,Q)) :- !, satisfy(P), satisfy(Q).
satisfy((P;Q)) :- !, satisfy(P) ; satisfy(Q).

satisfy((P*->Q;R)) :- !, satisfy(P) *-> satisfy(Q) ; satisfy(R).
satisfy((P->Q;R)) :- !, satisfy(P) -> satisfy(Q) ; satisfy(R).
satisfy((P->Q)) :- !, satisfy(P) -> satisfy(Q).

satisfy(once(P)) :- !, once(satisfy(P)).
satisfy({P}) :- !, satisfy(P), !.
satisfy(X^P) :- !, ^(X,P).
satisfy(\+P) :- !, \+ satisfy(P).
satisfy(setof(X,P,S)) :- !, setof80(X,P,S).
% satisfy(setof(X,P,S)) :- !, seto(X,P,S), S\==[].
satisfy(bagof(X,P,S)) :- !, bago(X,satisfy(P),S), S\==[].
satisfy(findall(X,P,S)) :- !, bago(X,satisfy(P),S).

satisfy(numberof(X,P,N)) :- !, numberof(X,P,N).
satisfy(X<Y) :- !, X<Y.
satisfy(X=<Y) :- !, X=<Y.
satisfy(X>=Y) :- !, X>=Y.
satisfy(X>Y) :- !, X>Y.
satisfy(satisfy(P)):- !, must_be_callable(P), !, satisfy(P).
satisfy(P) :- catch(call(P),E,(dmsg(call(P)-->E),break,fail)).
% satisfy(P) :- P.


:- module_transparent((^)/2).
^(X,P) :- get_ex_set(WazV), with_ex_v([X|WazV],satisfy(P)).

ex_satisfy(X,P) :- get_ex_set(WazV), with_ex_v([X|WazV],satisfy(P)).
% :- system:import((^)/2).

with_ex_v(WazV,G) :- locally(b_setval(ex_set,WazV),G).


+P :-!, satisfy_0(+P).

satisfy_0(+P) :- exceptionto(P), !, fail.
satisfy_0(+_P) :- !.

% setof(C,(D^(continent(C),in_ploc(D,C)),E^(country(E),in_ploc(E,C),F^(numberof(G,(city(G),in_ploc(G,E)),F),F>3))),B)
%WAS  numberof(X,P,N) :- !, seto(X,satisfy(P),S), length(S,N).
numberof(X,Ex^P,N) :- !, ex_satisfy((N,Ex),setof(X,P,S)), length(S,N).
% numberof(X,Ex^P,N) :- !, locally(b_setval(ex_set,[Ex]),seto(X,P,S)), S\==[], length(S,N).
numberof(X,P,N) :- setof(X,N^satisfy(P),S), S\==[], length(S,N).

% setof80(X,P,S):- var(X),!,findall(E,seto(X,P,E),L),ll_to_set(L,S).
setof80(X,P,S):- findall(E,seto(X,P,E),L),ll_to_set(L,S).
ll_to_set(S0,S):- append(S0,F),list_to_set(F,S).

into_set(S0,S):- flatten([S0],F),list_to_set(F,S).

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

