/*
 *	file:		current.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates for displaying and changing the
 *	polynomial interpretations of operators.
 *
 *	history:
 *	891128	uh	Added this comment
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

% pol_show_interpretations displays all known interpretations.

pol_show_interpretations :-
	pol_get_current(pol_state(_,[],_,_)),
	!,
	write('There are no interpretations !'),
	nl.
pol_show_interpretations :-
	pol_write_all_interpretations.


% pol_current_interpretations(IChanges) displays all known interpretations
% and asks the user, if he wants to change any. IChanges is a list of all
% pairs operator/arity whose interpretation was changed.

pol_current_interpretations([]) :-
	pol_get_current(pol_state(_,[],_,_)),
	!,
	write('There are no interpretations !'),
	nl,
	!.
pol_current_interpretations(IChanges) :-
	write('Current interpretations :'),
	nl,
	nl,
	pol_write_all_interpretations,
	pol_retractall(pol_comment(_,_,_)),
	nl,
	write('Do you want to change any ? (if so, type ''y'') '),
	pol_get_y_n(Char),
	pol_changing(Char,IChanges),
	!.


pol_write_all_interpretations :-
	pol_get_current(pol_state(_,IList,_,_)),
	member(pol_op_interpretation(Op,Arity,Interpretation),IList),
	sPrint("Interpretation of '%' with arity % ", [Op, Arity]),
	pol_write_ac_property(Op, Arity),
	nl,
	pol_write_interpretation(Interpretation),
	fail.
pol_write_all_interpretations.


pol_changing(121,IChanges) :-               % 121 = 'y'
	!,
	pol_get_current(pol_state(_,IList,_,_)),
	pol_change_list(IList,[],IChanges).
pol_changing(_,[]).


pol_change_list([],IChanges,IChanges).
pol_change_list([pol_op_interpretation(Op,Arity,Interpretation)|IList],
		 Changed,IChanges) :-
	nl,
	sPrint("Interpretation of '%' with arity % ", [Op, Arity]),
	pol_write_ac_property(Op, Arity),
	nl,
	pol_write_interpretation(Interpretation),
	repeat,
	write('Do you want to change it ? (''y'', <CR>, ''q'' for ''quit'') '),
	get0(Char),
	pol_skipinput(Char),
	pol_answer(Char,Op,Arity,Interpretation,IList,Changed,IChanges),
	!.


pol_answer(121,Op,Arity,Interpretation,IList,Changed,IChanges) :-   % 121 = 'y'
	pol_change_interpretation(Op,Arity,Interpretation,New),
	!,
	(New = Interpretation ->
		sPrint("!!! The interpretation has not changed !!! (Operator '%' with arity %)", [Op, Arity]),
		nl,
		pol_change_list(IList,Changed,IChanges) 
	;
		pol_change_list(IList,[Op/Arity|Changed],IChanges)
	).
pol_answer(113,_,_,_,_,IChanges,IChanges).        %  113 = 'q'
pol_answer(10,_,_,_,IList,Changed,IChanges) :-    %  10  = <CR>
	pol_change_list(IList,Changed,IChanges).
pol_answer(110,_,_,_,IList,Changed,IChanges) :-   %  110  = 'n'
	pol_change_list(IList,Changed,IChanges).


pol_write_ac_property(Op, 2) :-
	pol_op_is_AC(Op),
	!,
	write(' (AC-Operator) ').
pol_write_ac_property(Op, 2) :-
	pol_op_is_C(Op),
	!,
	write(' (C-Operator) ').
pol_write_ac_property(_, _).


% pol_reset_interpretations resets all known interpretations
% to the default value

pol_reset_interpretations :-
	 pol_get_current(pol_state(N,IList,C,AC)),
	 pol_reset_list(IList,DefaultList),
	 pol_make_current(pol_state(N,DefaultList,C,AC)).


pol_reset_list([],[]).
pol_reset_list([pol_op_interpretation(Op,Arity,_I)|IList],
	       [pol_op_interpretation(Op,Arity,Default)|DefaultList]) :-
	 pol_default_interpretation(Op,Arity,Default),
	 pol_reset_list(IList,DefaultList).


% pol_delete_interpretations deletes all known interpretations

pol_delete_interpretations :-
	 pol_get_current(pol_state(N,_,C,AC)),
	 pol_make_current(pol_state(N,[],C,AC)).
