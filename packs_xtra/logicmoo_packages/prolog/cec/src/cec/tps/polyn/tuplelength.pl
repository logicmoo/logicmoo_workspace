/*
 *	file:		tuplelength.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates to change and access the tuplelength
 *	for polynomial interpretations.
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

% pol_change_tuplelength(Newlength) changes the tuplelength to Newlength
% and deletes all old interpretations, operator-declarations remain valid.
% If Newlength is not a positive integer, pol_change_tuplelength fails.

% new version : old interpretation remain valid. If the new tuplelength
% is less than the old one, the last component(s) of the tuples are
% deleted, if it is greater than the old one, the last component of
% the tuples are copied to get the right length.

pol_change_tuplelength(N) :-
	integer(N),
	N > 0,
	pol_get_current(pol_state(M,I,C,AC)),
	(M \== N ->
		pol_convert_tuplelength(M,N,I,IN),
		pol_make_current(pol_state(N,IN,C,AC))
	;
		true
	).

pol_convert_tuplelength(_,_,[],[]).
pol_convert_tuplelength(M,N,[pol_op_interpretation(Op,Arity,IM)|IMs],
			[pol_op_interpretation(Op,Arity,IN)|INs]) :-
	pol_ext(N,IM,IN),
	pol_convert_tuplelength(M,N,IMs,INs).

pol_get_tuplelength(N) :-
	pol_get_current(pol_state(N,_,_,_)).


