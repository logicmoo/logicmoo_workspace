/*
 *	file:		imp.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates for imperative programming,
 *	assignment and some control structures.
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

:-op(950,xfx,:=).
:-op(950,xfx,':==').


(V:=E):-
	assign0(V,E).

(V:==E):-
	assign1(V,E).

assign0(Var,Val):-
	nonvar(Var),
	(\+ retract(cont(Var,_));true),   % to avoid bindings -- hg
	assert(cont(Var,Val)),!.
assign0(Var,_):-
	write('assign0: first parameter must be nonvar: '),
	write(Var),
	nl,
	abort.

assign1(Var,Val):-
	nonvar(Var),
	(\+ retract(cont1(Var,_));true), % to avoid bindings -- hg
	assert(cont1(Var,Val)),!.
assign1(Var,_):-
	write('assign1: first parameter must be nonvar: '),
	write(Var),
	nl,
	abort.


incr(Var):-
	cont(Var,_),
	repeat,                 % increments further on backtracking
	cont(Var,Val),
	number(Val),
	V1 is Val+1,
	assign0(Var,V1).
incr(Var):-
	assign0(Var,0).

inc(Var):-
	cont(Var,Val),
	number(Val),
	V1 is Val+1,
	assign0(Var,V1),
	!.
inc(Var):-
	assign0(Var,0).

callv(V):-
	cont(V,C),
	C.


callFun(Pred,Term,MaxCalls) :-
	callCounter:=0,
	Call=..[Pred,Term,X], 
	sPrint("Call: %",[Call]), nl,
	sPrint("Results for %: ",[X]), nl, 
	Call,
	print(X), nl,
	inc(callCounter),
	cont(callCounter,MaxCalls).
callFun(_,_,_) :-
	cont(callCounter,0),
	print('no solutions').




nmbClauses(P,_):-
	(nc:=0),
	P,
	inc(nc),
	fail.
nmbClauses(_,N):-
	cont(nc,N).




callNTimes(P,N):-
	assign0(calls,0),
	repeat,
	incr(calls),
	try(P),
	cont(calls,N),
	!.


repeat(P,Max):-
	calls:=0,
	(trace(step) ->
		nl,
		   sPrint("Trying to apply % .
	Type any term to continue.",[P]),
		   read1(_);
		true),
	P,
	inc(calls),
	((nonvar(Max),cont(calls,Max)) -> 
		true;
		(trace(step) ->
			nl,
			   sPrint("Trying another application of % .
		Type any term to continue.",[P]),
			   read1(_),
			   fail
		)
	),
	!.
repeat(_P,Max):-
	var(Max).


read1(T):-
	read(T),
	!.
	

% repeat(+Goal)
% calls and retries Goal until it fails, then succeeds.

repeat(P):-
	P,
	fail,
	!.
repeat(_).

repeat1(P):-
	rep1(P),
	cont(calls,N),
	N>=1,
	!.

rep1(P):-
	calls:=0,
	(trace(step) ->
		nl,
		   sPrint("Trying to apply % .
	Type any term to continue.",[P]),
		   read1(_);
		true),
	P,
	inc(calls),
	(trace(step) ->
		nl,
		   sPrint("Trying another application of % .
	Type any term to continue.",[P]),
		   read1(_),
		   fail
	),
	!.
rep1(_).


memoize(Fact,Pred):-
	Pred,
	assertz(Fact),
	fail.
memoize(_,_).


ignoreOutput(P):-
	tell('/dev/null'),
	call(P),
	told,
	!.
ignoreOutput(_):-
	told,
	fail.


for(From,To,P):-
	From =< To,
	!,
	try(apply(P,[From])),
	F is From+1,
	for(F,To,P).
for(_,_,_).


solutions(P,N):-
	foundSolutions:=0,
	P,
	inc(foundSolutions),
	cont(foundSolutions,N),
	!.
