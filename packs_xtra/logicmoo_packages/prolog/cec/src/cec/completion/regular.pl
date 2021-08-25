/*
 *	file:		regular.pl
 *	version:	1.5
 *	date:		November 6, 1989
 *	creation:	November 6, 1989
 *	author:		-
 *
 *	description:
 *	This file contains predicates for checking the preregularity
 *	and regularity of a the current signature, which is a necessary
 *	condition for the correctness of the equivalence of order-sorted
 *	equations and rewrite rules produced by CEC.
 *
 *	history:
 *	891106	uh 	Added this comment
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */


% Predicate: preregular
% Checks the following condition:
% A signature is preregular iff for every function symbol
% f and every string w of sorts the set
% { t | there is a declaration f : w' -> t such that w <= w'}
% is either empty or has a minimal element.

preregular :-
	error:==none,
	setof(O,[O1,T]^((O ofType (O1:T)),O\=='$inj'),Ops),
	member(Op,Ops),
	not preregular(Op),
	error("Signature not preregular for operator %.",[Op],regular),
	fail.
preregular :-
	cont1(error,none),
	write('The signature is preregular.'),
	nl.


preregular(O) :-
	setof1((XI,RT),
	[O1,ATs]^(	(O ofType (O1:[RT|ATs])),
			map(subsortRT,XI,ATs)
		),
	      RTypes),
	setof(Ts,R^member((Ts,R),RTypes),XIs),
	member(Xi,XIs),
	setof(R,member((Xi,R),RTypes),RTs),
	not hasMinimum(RTs),
	!,
	fail.
preregular(_).


hasMinimum(RTs) :-
	member(T,RTs),
	mapP(lambda([T1],subsortRT(T,T1)),RTs),
	!.


hasMinimumSeq(RTs) :-
	member((Xi,R),RTs),
	mapP(lambda([(Xi1,R1)],
		(	map(subsortRT,Xi,Xi1),
			subsortRT(R,R1)
		)
	           ),RTs),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicate: regular
% Checks the following condition:
% A signature is regular iff for every function symbol
% f and every string w of sorts the set
% {(w',t) | there is a declaration f : w' -> t such that w <= w'}
% is either empty or has a minimal element.

regular :-
	error:==none,
	setof(O,[O1,T]^((O ofType (O1:T)),O\=='$inj'),Ops),
	member(Op,Ops),
	\+ regular(Op),
	error("Signature not regular for operator %.",[Op],regular),
	fail.
regular :-
	cont1(error,none),
	write('The signature is regular.'),
	nl.


regular(O) :-
	setof1((XI,RT),
	[O1,ATs]^(	(O ofType (O1:[RT|ATs])),
			map(subsortRT,XI,ATs)
		),
	      RTypes),
	setof(Ts,R^member((Ts,R),RTypes),XIs),
	member(Xi,XIs),
	setof((Ts,R),
	[O1,Ts]^(	(O ofType(O1:[R|Ts])),
			map(subsortRT,Xi,Ts)
		),RTs),
	\+ hasMinimumSeq(RTs),
	!,
	fail.
regular(_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
