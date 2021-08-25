/*
 *	file:		suggest.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates to suggest default interpretations.
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

% pol_default_interpretation(Operator,Arity,Default) creates a default
% interpretation for Operator/Arity.

pol_default_interpretation(Operator,Arity,Default) :-
	pol_default_polynomial(Operator,Arity,DefaultPol),
	pol_get_tuplelength(Length),
	pol_expand_to_list(DefaultPol,Length,Default).

pol_default_polynomial(_Op,0,[(q(2,1),[])]) :- !.
pol_default_polynomial(Op,2,[(q(1,1),[1,1]),(q(1,1),[1,0]),(q(1,1),[0,1])]) :-
	pol_op_is_AC(Op),
	!.
pol_default_polynomial(Op,2,[(q(1,1),[1,0]),(q(1,1),[0,1])]) :-
	pol_op_is_C(Op),
	!.
pol_default_polynomial(_Op,N,[(q(2,1),ExpList)]) :-
	pol_expand_to_list(1,N,ExpList).




%  pol_varlist(N,Varlist) creates a list of variable names
%  with length N
%  e.g. pol_varlist(2,[x,y])
%       pol_varlist(3,[x,y,z])
%       pol_varlist(4,[x1,x2,x3,x4])

pol_varlist(0,[]) :- !.
pol_varlist(1,[x]) :- !.
pol_varlist(2,[x,y]) :- !.
pol_varlist(3,[x,y,z]) :- !.
pol_varlist(N,[x1,x2,x3,x4|Listrest]) :-
	pol_varlistrest(N,4,Listrest).

pol_varlistrest(N,N,[]) :- !.
pol_varlistrest(N,M,[VarM|Listrest]) :-
	M1 is M+1,
	mkAtom('x%',[M1],VarM),
	pol_varlistrest(N,M1,Listrest).



%  pol_termlist(Varlist,Termlist) takes a list of atoms
%  and returns a list of terms with functor '@'
%  e.g. pol_termlist([x,y],[@ x,@ y])

pol_termlist([],[]).
pol_termlist([First|Rest],[@ First|Restterms]) :-
	pol_termlist(Rest,Restterms).



pol_int_to_string(0,L,[48|L]) :- !.            /* 48 = 0 */
pol_int_to_string(1,L,[49|L]) :- !.            /* 49 = 1 */
pol_int_to_string(2,L,[50|L]) :- !.            /* 50 = 2 */
pol_int_to_string(3,L,[51|L]) :- !.            /* 51 = 3 */
pol_int_to_string(4,L,[52|L]) :- !.            /* 52 = 4 */
pol_int_to_string(5,L,[53|L]) :- !.            /* 53 = 5 */
pol_int_to_string(6,L,[54|L]) :- !.            /* 54 = 6 */
pol_int_to_string(7,L,[55|L]) :- !.            /* 55 = 7 */
pol_int_to_string(8,L,[56|L]) :- !.            /* 56 = 8 */
pol_int_to_string(9,L,[57|L]) :- !.            /* 57 = 9 */
pol_int_to_string(I,L,String) :-
	I1 is I mod 10,
	pol_int_to_string(I1,L,Reststring),
	IRest is I//10,
	pol_int_to_string(IRest,Reststring,String).
