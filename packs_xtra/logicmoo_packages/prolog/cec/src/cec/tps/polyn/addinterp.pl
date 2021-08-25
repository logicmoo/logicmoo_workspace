/*
 *	file:		addinterp.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates to add new interpratations
 *	for operators when the ordering is polyN.
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

% pol_add_interpretation(Op, VL, I) adds a new interpretation I
% for an operator Op/N to the current state, where N = length(VL).
% The interpretation I may be a term over the variables in
% the variable list VL (any term accepted by pol_transform is legal here,
% for further information see file 'transform').
% The variable names in the variable list VL and in the
% interpretation I must be of the form 'atom' or '@(atom)', these
% two are regarded as equal.
% The new interpretation must be compatible with all C- or AC-
% declarations in the current state.

/* Examples : addinterpretation is used for setInterpretation :
:- setInterpretation([g(x,y) : [2*x*y, x+y], i : [2,3],
		      c : 7,
		      n(x) : g(i,x),  		% or ip(g(ip(i),x))
		      f(x,y) : g(x,y)+2,	% or ip(g(x,y))+2
		      h(x,y) : g(x,y)*i,	% or ip(g(x,y))*ip(i)x
		      k(x,y) : g(x,y)^i,	% or ip(g(x,y))^ip(i)
		      l(x,y) : [ip(g(x,y), 2), g(x,y)] ]).
yields [7, 7] as interpretation for c (automatical conversion to tuple),
       [4*x, x+3] for n(x),
       [2*x*y+2, x+y+2] for f(x,y),
       [4*x*y, 3*x+3*y] for h(x,y),
       [4*(x^2)*(y^2), (x^3)+3*(x^2)*y+3*x*(y^2)+(y^3)] for k(x,y) and
       [x+y, x+y] for l(x,y) (automatic projection in the second component).
Notice, that t^exponent is only valid if exponent is a number or
a term, whose interpretation is a number (or a tuple of numbers).
Notice, that the order of declarations is significant, so if you
use an operator before it is declared, its
interpretation is unknown and the user is asked for one.
*/

pol_add_interpretation(Operator, VarList, InterprList) :-
	length(VarList, Arity),
	!,
	pol_add_interpr1(Operator, VarList, Arity, InterprList,notOnline, _).
pol_add_interpretation(Operator, VarList, _InterprList) :-
	sPrint("*** pol_add_interpretation : Illegal variable list for '%' : %",[Operator, VarList]),
	nl,
	fail.


pol_add_interpr1(Operator, VarList, Arity,Interpretation, Online, PolynomialList) :-
	pol_retractall(pol_comment(_,_,_)),
	((nonvar(Interpretation), Interpretation = [I|InterprList]) ->
		pol_get_tuplelength(N),
		(length(Interpretation, N) ->
			pol_transform_list([I|InterprList], VarList, PolynomialList, 1) 
		;
			sPrint("*** Interpretation of '%' with arity % has illegal tuplelength.", [Operator, Arity]),
			nl,
			fail
		)
	;
		pol_transform(Interpretation, VarList, PolynomialList)
	),
	!,
	pol_add_interpr2(Operator, Arity, VarList, PolynomialList,Online).
pol_add_interpr1(Operator, _VarList, Arity, Interpretation, _, _) :-
	sPrint("*** Cannot interpret expression (for '%' with arity %) :", [Operator, Arity]),
	nl,
	write(Interpretation),
	nl,
	fail.


pol_add_interpr2(Operator, Arity, VarList, PolynomialList, Online) :-
	pol_map_legal_interpretation(PolynomialList),
	!,
	pol_add_interpr3(Operator, Arity, VarList, PolynomialList, Online).
pol_add_interpr2(Operator, Arity, VarList, PolynomialList, _) :-
	sPrint("*** Illegal interpretation of '%' with arity % :", [Operator, Arity]),
	nl,
	pol_write_interpretation_with_vars(PolynomialList,VarList),
	fail.


pol_add_interpr3(Operator, 2, VarList, PolynomialList, Online) :-
	pol_op_is_C(Operator),
	!,
	pol_add_interpr3C(Operator, VarList, PolynomialList),
	!,
	(Online == online ->
		sPrint("Resulting interpretation for Operator '%' with arity 2 (C-Operator) :", [Operator]),
		nl,
		pol_write_interpretation(PolynomialList),
		pol_confirm
	;
		true
	),
	pol_assert(pol_op_interpretation(Operator, 2, PolynomialList)).
pol_add_interpr3(Operator, 2, VarList, PolynomialList, Online) :-
	pol_op_is_AC(Operator),
	!,
	pol_add_interpr3AC(Operator, VarList, PolynomialList),
	!,
	(Online == online ->
		sPrint("Resulting interpretation for Operator '%' with arity 2 (AC-Operator) :", [Operator]),
		nl,
		pol_write_interpretation(PolynomialList),
		pol_confirm 
	;
		true
	),
	pol_assert(pol_op_interpretation(Operator, 2, PolynomialList)).
pol_add_interpr3(Operator, Arity, _VarList, PolynomialList, Online) :-
	(Online == online ->
		sPrint("Resulting interpretation for Operator '%' with arity % :", [Operator, Arity]),
		nl,
		pol_write_interpretation(PolynomialList),
		pol_confirm 
	;
		true
	),
	pol_assert(pol_op_interpretation(Operator, Arity, PolynomialList)).



pol_add_interpr3C(_Operator, _, PolynomialList) :-
	pol_has_C_property(PolynomialList),
	!.
pol_add_interpr3C(Operator, VarList, PolynomialList) :-
	sPrint("*** Interpretation of C-Operator '%' does not have C-property :", [Operator]),
	nl,
	pol_write_interpretation_with_vars(PolynomialList, VarList),
	fail.


pol_add_interpr3AC(_Operator, _, PolynomialList) :-
	pol_has_AC_property(PolynomialList),
	!.
pol_add_interpr3AC(Operator, VarList, PolynomialList) :-
	sPrint("*** Interpretation of AC-Operator '%' does not have AC-property :", [Operator]),
	nl,
	pol_write_interpretation_with_vars(PolynomialList, VarList),
	fail.


pol_map_legal_interpretation([]) :- !.
pol_map_legal_interpretation([Pol|PolList]) :-
	pol_legal_interpretation(Pol),
	pol_map_legal_interpretation(PolList).

pol_transform_list([], _, [], _) :- !.
pol_transform_list([Interpr|InterprList], VarList, [Pol|PolList], I) :-
	pol_transform(Interpr, VarList, Pols),
	pol_select_component(Pols, I, Pol),
	I1 is I+1,
	pol_transform_list(InterprList, VarList, PolList, I1).
