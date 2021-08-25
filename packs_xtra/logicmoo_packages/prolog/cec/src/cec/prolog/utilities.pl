/*
 *	file:		utilities.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains some useful predicates, mostly timing.
 *
 *	history:
 *	891010	js	Added this comment
 *	891030  uh	Moved definition of
 *			try/1
 *			from completion/dbKit.pl into this file
 *	891108	uh	Moved definition of
 *			callOnce/1
 *			from worlds.pl into this file
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

time(Call) :-
	totalRuntime(A),
	executeCall(Call,Result),
	totalRuntime(E), 
	U is E-A,
	write('
Time used: '), write(U), write(' sec.
'),
	!,
	Result = exit.


initTime :-
	abolish(timeUsedFor/4),
	abolish(totalRTBeforeCall/2),
	abolish(totalRTAfterCall/2).


accTime(Call) :-
	benchmark(no),
	!,
	Call.
accTime(Call) :-
	functor(Call,P,N),
	!,
	accTime(P/N,Call).


accTime(_Name,Call) :-
	benchmark(no),
	!,
	Call.
accTime(Name,Call) :-
	totalRTBeforeCall(Name,_TimeBefore), 	% recursive Call
	!,
	executeCall(Call,Result),
	retract(timeUsedFor(Name,AccTime,Calls,RecCalls)),
	NewRecCalls is RecCalls+1,
	assertz(timeUsedFor(Name,AccTime,Calls,NewRecCalls)),
	Result = exit.
accTime(Name,Call) :-
	timeUsedFor(Name,_AccTime,_Calls,_RecCalls),
	!,
	addTime(Name,Call).
accTime(Name,Call) :-
	assertz(timeUsedFor(Name,0,0,0)),
	!,
	addTime(Name,Call).


addTime(Name,Call) :-
	totalRuntimeBeforeCall(Name),
	!,
	executeCall(Call,Result),
	totalRuntimeAfterCall(Name,Result),
	retract(totalRTBeforeCall(Name,TimeBefore)),
	retract(totalRTAfterCall(Name,TimeAfter)),
	once(retract(timeUsedFor(Name,AccTime,Calls,RecCalls1))),
	NewAccTime is AccTime+(TimeAfter-TimeBefore),
	NewCalls is Calls+1,
	assertz(timeUsedFor(Name,NewAccTime,NewCalls,RecCalls1)),
	Result = exit.

/* This is a useless comment */

retractNoRedo(X) :-
	retract(X), !.

totalRuntimeBeforeCall(Name) :-
	totalRuntime(TimeBefore),
	assertz(totalRTBeforeCall(Name,TimeBefore)),
	!.


totalRuntimeAfterCall(Name,_Result) :-
	totalRuntime(TimeAfter),
	assertz(totalRTAfterCall(Name,TimeAfter)).
totalRuntimeAfterCall(Name,exit) :-
	totalRuntime(RedoTimeBefore),
	assertz(totalRTBeforeCall(Name,RedoTimeBefore)),
	!,
	fail.


executeCall(C,Result) :-
	C,
	Result = exit.
executeCall(C,fail):-
	incrFailures(C).



incrFailures(C):-
	functor(C,F,N),
	incr(failures(F/N)),
	!.


executeCallAndIgnoreFail(C) :-
	C,
	!.
executeCallAndIgnoreFail(_).


once(P):-
	P,
	!.




date(Date) :-
	unix(system('date +date\(%d,\''%h\'',19%y,%H,%M\). > /tmp/dateOfCompilation')),
	see('/tmp/dateOfCompilation'),
	read(Date),
	seen,
	unix(system('rm /tmp/dateOfCompilation')),
	!.





%benchmark(P,File) :-
%	bench_start,
%	P,
%	tell(File),
%	bench_end,
%	show_user,
%	told.


appendAtoms(Atom1,Atom2,AppAtom) :-
	(atomic(Atom1) ->
		name(Atom1,NAtom1)
	;
		true
	),
	(atomic(Atom2) ->
		name(Atom2,NAtom2)
	;
		true
	),
	(atomic(AppAtom) ->
		name(AppAtom,NAppAtom)
	;
		true
	),
	!,
	append(NAtom1,NAtom2,NAppAtom),
	name(AppAtom,NAppAtom),
	name(Atom2,NAtom2),
	name(Atom1,NAtom1).

	
/*----------------------------------------------------------------------*
 *		 		try(P)					*
 *----------------------------------------------------------------------*/

try(P) :-
	P,
	!.
try(_).


listOf(_ElementList,[]) :- !.
listOf(ElementList, [A | Rest]) :-
	member(A,ElementList),
	listOf(ElementList,Rest),
	!.

callOnce(listing(reduceClause/3)) :-	% Hack to support C-Prolog
	nl,
	reduceClause(I,T,(C,_)),
	writeq(reduceClause(I,T,(C,[]))),
	write('.'),
	nl,
	fail.

callOnce(listing(reduceClause/3)):-
	!.

callOnce(C) :-
	C,
	!.
