/*
 *	file:		operators.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates for checking the AC-property of
 *	operators and declaring operators to be AC.
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

% pol_op_is_C(Op) succeeds iff Op is declared as a C-Operator.

pol_op_is_C(Op) :-
	pol_get_current(pol_state(_, _, C, _)),
	member(Op, C).


% pol_op_is_AC(Op) succeeds iff Op is declared as an AC-Operator.

pol_op_is_AC(Op) :-
	pol_get_current(pol_state(_, _, _, AC)),
	member(Op, AC).


%  pol_declare_op(Operator,Type) gives the operator the property 'Type'
%  (Type is 'c' or 'ac' or 'none') if there is no interpretation
%  violating the constraints for operators of type 'Type'.
%  If 'Type' is 'none', old operator declarations are removed.
%  The first argument must be instantiated, if the second is not,
%  'none' is taken.

pol_declare_op(Op,none) :-
	atom(Op),
	pol_get_current(pol_state(N,IList,C,AC)),
	pol_remove(Op,C,NewC),
	pol_remove(Op,AC,NewAC),
	pol_make_current(pol_state(N,IList,NewC,NewAC)).

pol_declare_op(Op,c) :-
	atom(Op),
	pol_get_current(pol_state(N,IList,C,AC)),
	(	member(pol_op_interpretation(Op,2,I),IList),
		not(pol_has_C_property(I)),
		sPrint("*** C-property of Operator '%' is", [Op]),
		nl,
		write('incompatible with current interpretation :'),
		nl,
		pol_write_interpretation(I),
		!,
		fail
	;
		true
	),
	pol_insert(Op,C,NewC),
	pol_remove(Op,AC,NewAC),
	pol_make_current(pol_state(N,IList,NewC,NewAC)).

pol_declare_op(Op,ac) :-
	atom(Op),
	pol_get_current(pol_state(N,IList,C,AC)),
	(	member(pol_op_interpretation(Op,2,I),IList),
		not(pol_has_AC_property(I)),
		sPrint("*** AC-property of operator '%' is", [Op]),
		nl,
		write('incompatible with current interpretation :'),
		nl,
		pol_write_interpretation(I),
		!,
		fail
	;
		true
	),
	pol_insert(Op,AC,NewAC),
	pol_remove(Op,C,NewC),
	pol_make_current(pol_state(N,IList,NewC,NewAC)).


pol_has_AC_property([]).

pol_has_AC_property([Polynomial|Rest]) :-
	pol_make_ac_polynomial((A,B,C),Polynomial),
	0 is A*C+B-B*B,
	pol_has_AC_property(Rest).


pol_has_C_property([]).

pol_has_C_property([Polynomial|Rest]) :-
	pol_invert(Polynomial,Inverted),
	pol_quicksort(Inverted,Polynomial),
	pol_has_C_property(Rest).
