/*
 *	file:		apply.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains the definitions of the higher-order predicates
 *	like apply, map, iter, etc. 
 *
 *	history:
 *	891010	js	Added this comment
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

/* lambda(Vars,Body) or
 * lambda(Vars,Local^Body)
 * is an anonymous predicate for use in higher order functions.
 * Vars  is a list of patterns which correspond to the pattern
 *       in the head of a clause
 * Local contains local variables
 * Body  corresponds to the body of the clause
 */

apply(lambda(Vars,(Local^P)),Args):-
	varsOf((Vars,Local),LV),
	varsOf(P,VP),
	setof1(V,(member(V,VP),\+member_s(V,LV)),Global),
	renamePVars(closure(lambda(Vars,P),Global),closure(lambda(Args,Body),Global)),
	!,
	Body.
apply(lambda(Vars,P),Args):-
	varsOf(P,VP),
	varsOf(Vars,LV),
	setof1(V,(member(V,VP),\+member_s(V,LV)),Global),
	renamePVars(closure(lambda(Vars,P),Global),closure(lambda(Args,Body),Global)),
	!,
	Body.
apply((P,FreeVars),Args):-
	(atom(P) ->
		true
	;	systemError("illegal arguments to apply call: 
	Predicate = %,
	Free Variables = %,
	Arguments = %",[P,FreeVars,Args])),
	append(Args,FreeVars,A),
	Call=..[P|A],
	!,
	Call.	
apply(P,Args):-
	(atom(P) ->
		true
	;	systemError("illegal arguments to apply call: 
	Predicate = %,
	Arguments = %",[P,Args])),
	Call=..[P|Args],
	Call.



aplist([],lambda([X,[]],true)).
aplist([F|Fs],lambda([X,[FX|FsX]],
		(apply(F,[X,FX]),
		 aplist(Fs,X_FsX),
		 apply(X_FsX,[X,FsX])))).
	
:-dynamic mapF/3.
mapF(F,[],[]).
mapF(F,[X|L],[FX|FL]):-
	apply(F,[X,FX]),
	mapF(F,L,FL).

:-dynamic mapP/2.
mapP(F,[]).
mapP(F,[X|L]):-
	apply(F,[X]),
	mapP(F,L).

/* ---------- map-like functions on lists of subterms of a term ------ */

map(F,T,N,Res):-
	mapTo(F,T,N,[],Res),
	!.

mapTo(_F,_T,0,Rest,Rest):-!.
mapTo(F,T,J,Rest_JP1ToN,Res):-
	arg(J,T,TJ),
	apply(F,[TJ,RJ]),
	JM1 is J-1,
	mapTo(F,T,JM1,[RJ|Rest_JP1ToN],Res).

mapTo(_F,_T,_OtherArgs,0,Rest,Rest):-!.
mapTo(F,T,OtherArgs,J,Rest_JP1ToN,Res):-
	arg(J,T,TJ),
	apply(F,[TJ,RJ|OtherArgs]),
	JM1 is J-1,
	mapTo(F,T,OtherArgs,JM1,[RJ|Rest_JP1ToN],Res).


mapA(_F,_T,0):-!.
mapA(F,T,J):-
	arg(J,T,TJ),
	apply(F,[TJ]),
	JM1 is J-1,
	mapA(F,T,JM1).

:-dynamic map/3.	% identical to mapF
map(P,[],[]).
map(P,[X|R],[PX|PR]):-
	apply(P,[X,PX]),
	map(P,R,PR).

mapL(P,[],[],_).
mapL(P,[X|R],[PX|PR],OtherArgs):-
	apply((P,OtherArgs),[X,PX]),
	mapL(P,R,PR,OtherArgs).

mapl(P,[]).
mapl(P,[X|L]):-
	apply(P,X),
	mapl(P,L).









mapAL(P,[],_).
mapAL(P,[X|R],OtherArgs):-
	apply((P,OtherArgs),[X]),
	mapAL(P,R,OtherArgs).

:-dynamic mapA/2.	% identical to mapP
mapA(_P,[]) :- !.
mapA(P,[X|R]):-
	apply(P,[X]),
	mapA(P,R).


/*----------------------------------------------------------------------*/
/* iter(+Predicate,+ResultSoFar,+List,+OtherArgs,-Result)               */
/* iterates                                                             */
/* Predicate(head(List),NextResult,ResultSoFar,OtherArgs),              */
/* iter(Predicate,NextResult,tail(List),OtherArgs,Result)               */
/* until List is empty.                                                 */
/* Then ResultSofar is the result of the whole computation              */
/* 14.11.88 belongs to file "lists" tested 15.11.88  19:52 */

iter(_P,E,[],_,E).
iter(P,E,[X|R],OtherArgs,G) :-
   PCALL=..[P,X,F,E|OtherArgs],
   PCALL,
   iter(P,F,R,OtherArgs,G).



asFunction(FConstant/N,lambda(XN,FXN)):-
	atom(FConstant),
	number(N),
	genVarList(N,XN),
	FXN=..[FConstant,Result|FXN],
	!.
asFunction(FConstant,lambda([X,Result],FX)):-
	atom(FConstant),
	FX=..[FConstant,X,Result],
	!.
asFunction(F,_):-
	nl,
	write(' *** Error: asFunction called with '),
	write(F),
	!.


