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

% Chat-80 : A small subset of English for database querying.

/* Control loop */

hi :-
   hi(user).

hi(File) :-
   repeat,
      ask(File,P),
      control(P), !,
      end(File).

ask(user,P) :- !,
   prompt(_,'Question: '),
   read_in(P).
ask(File,P) :-
   seeing(Old),
   see(File),
   read_in(P),
   nl,
   doing(P,0),
   nl,
   see(Old).

doing([],_) :- !.
doing([X|L],N0) :-
   out(X),
   advance(X,N0,N),
   doing(L,N).

out(nb(X)) :- !,
   write(X).
out(A) :-
   write(A).

advance(X,N0,N) :-
   uses(X,K),
   M is N0+K,
 ( M>72, !,
      nl,
      N is 0;
   N is M+1,
      put(" ")).

uses(nb(X),N) :- !,
   chars(X,N).
uses(X,N) :-
   chars(X,N).

chars(X,N) :- atomic(X), !,
   name(X,L),
   length(L,N).
chars(_,2).

end(user) :- !.
end(F) :-
   close(F).

control([bye,'.']) :- !,
   display('Cheerio.'),
   nl.
control([trace,'.']) :- !,
   assert(tracing),
   display('Tracing from now on!'), nl, fail.
control([do,not,trace,'.']) :-
   retract(tracing), !,
   display('No longer tracing.'), nl, fail.
control(U) :-
   process(U),
   fail.

process(U) :-
   runtime(StartParse),
   sentence(E,U,[],[],[]),
   runtime(StopParse),
   ParseTime is StopParse - StartParse,
   report(E,'Parse',ParseTime,tree),
   runtime(StartSem),
   logic(E,S), !,
   runtime(StopSem),
   SemTime is StopSem - StartSem,
   report(S,'Semantics',SemTime,expr),
   runtime(StartPlan),
   qplan(S,S1), !,
   runtime(StopPlan),
   TimePlan is StopPlan - StartPlan,
   report(S1,'Planning',TimePlan,expr),
   runtime(StartAns),
   answer(S1), !, nl,
   runtime(StopAns),
   TimeAns is StopAns - StartAns,
   report(_,'Reply',TimeAns,none).
process(_) :-
   failure.

failure :-
   display('I don''t understand!'), nl.

report(Item,Label,Time,Mode) :-
   tracing, !,
   nl, write(Label), write(': '), write(Time), write('sec.'), nl,
   report_item(Mode,Item).
report(_,_,_,_).

report_item(none,_).
report_item(expr,Item) :-
   write_tree(Item), nl.
report_item(tree,Item) :-
   print_tree(Item), nl.

runtime(Time) :-
   statistics(runtime,[MSec,_]),
   Time is MSec/1000.

quote(A&R) :-
   atom(A), !,
   quote_amp(R).
quote(_-_).
quote(_--_).
quote(_+_).
quote(verb(_,_,_,_,_)).
quote(wh(_)).
quote(name(_)).
quote(prep(_)).
quote(det(_)).
quote(quant(_,_)).
quote(int_det(_)).

quote_amp('$VAR'(_)) :- !.
quote_amp(R) :-
   quote(R).

logic(S0,S) :-
   i_sentence(S0,S1),
   clausify(S1,S2),
   simplify(S2,S).

simplify(C,(P:-R)) :- !,
   unequalise(C,(P:-Q)),
   simplify(Q,R,true).

simplify(setof(X,P0,S),R,R0) :- !,
   simplify(P0,P,true),
   revand(R0,setof(X,P,S),R).
simplify((P,Q),R,R0) :-
   simplify(Q,R1,R0),
   simplify(P,R,R1).
simplify(true,R,R) :- !.
simplify(X^P0,R,R0) :- !,
   simplify(P0,P,true),
   revand(R0,X^P,R).
simplify(numberof(X,P0,Y),R,R0) :- !,
   simplify(P0,P,true),
   revand(R0,numberof(X,P,Y),R).
simplify(\+P0,R,R0) :- !,
   simplify(P0,P1,true),
   simplify_not(P1,P),
   revand(R0,P,R).
simplify(P,R,R0) :-
   revand(R0,P,R).

simplify_not(\+P,P) :- !.
simplify_not(P,\+P).

revand(true,P,P) :- !.
revand(P,true,P) :- !.
revand(P,Q,(Q,P)).

unequalise(C0,C) :-
   numbervars(C0,1,N),
   functor(V,v,N),
   functor(M,v,N),
   inv_map(C0,V,M,C).

inv_map('$VAR'(I),V,_,X) :- !,
   arg(I,V,X).
inv_map(A=B,V,M,T) :- !,
   drop_eq(A,B,V,M,T).
inv_map(X^P0,V,M,P) :- !,
   inv_map(P0,V,M,P1),
   exquant(X,V,M,P1,P).
inv_map(A,_,_,A) :- atomic(A), !.
inv_map(T,V,M,R) :-
   functor(T,F,K),
   functor(R,F,K),
   inv_map_list(K,T,V,M,R).

inv_map_list(0,_,_,_,_) :- !.
inv_map_list(K0,T,V,M,R) :-
   arg(K0,T,A),
   arg(K0,R,B),
   inv_map(A,V,M,B),
   K is K0-1,
   inv_map_list(K,T,V,M,R).

drop_eq('$VAR'(I),'$VAR'(J),V,M,true) :- !,
 ( I=\=J, !,
      irev(I,J,K,L), 
      arg(K,M,L),
      arg(K,V,X),
      arg(L,V,X);
   true).
drop_eq('$VAR'(I),T,V,M,true) :- !,
   arg(I,V,T),
   arg(I,M,0).
drop_eq(T,'$VAR'(I),V,M,true) :- !,
   arg(I,V,T),
   arg(I,M,0).
drop_eq(X,Y,_,_,X=Y).

exquant('$VAR'(I),V,M,P0,P) :-
   arg(I,M,U),
 ( var(U), !,
      arg(I,V,X),
       P=(X^P0);
   P=P0).

irev(I,J,I,J) :- I>J, !.
irev(I,J,J,I).

