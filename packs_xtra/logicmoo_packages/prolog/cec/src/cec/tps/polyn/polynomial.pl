/*
 *	file:		polynomial.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates allowing the user to compute the
 *	polynomial interpretation of terms.
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

% polynomial(+Term, -Interpretation) yields the interpretation of Term
% only useful if you use the ordering poly
% Terms and not ITerms (cf. setInterpretation.hlp) are assumed

osPolInterpretation(T,Vars,Interp):-
	manyToOrderSorted(T,TV),
	pol_getvars(TV,[],Vars),
	pol_retractall(pol_comment(_,_,_)),
	pol_interpret_term(TV,Vars,Interp),
	!.

polInterpretation(T,Vars,Interp):-
	pol_getvars(T,[],Vars),
	pol_retractall(pol_comment(_,_,_)),
	pol_interpret_term(T,Vars,Interp),
	!.

'polynomial!'(T,I) :-
	polynomial(T,I,noSyntaxCheck).
polynomial(T,I) :-
	polynomial(T,I,syntaxCheck).
polynomial(T,I,Check) :-
	(tps_current_ordering(poly(_)) ->
		(Check == syntaxCheck ->
			internalTermRep(T,_) % for syntax check
		;
			true
		),
		pol_with_variables(T,TV),
		pol_getvars(TV,[],Vars),
		pol_retractall(pol_comment(_,_,_)),
		pol_interpret_term(TV,Vars,Interp),
		I = '$$interpretation_with_vars'(Interp, Vars)
%		write('polynomial interpretation for'),
%		nl,
%		sPrint("% is :", [T]),
%		nl,
%		pol_write_interpretation_with_vars(Interp,Vars)
	;
		write('*** polynomial : current ordering is not poly'),
		nl,
		fail
	).

pol_with_variables(X, @ X) :-
  variable(X),!.
pol_with_variables(X,X) :- atomic(X),!.
pol_with_variables(X,XV) :-
  X =.. [Op|Args],
  pol_w_v_list(Args,ArgsV),
  XV =.. [Op|ArgsV].

pol_w_v_list([],[]).
pol_w_v_list([X|Rest],[XV|RestV]) :-
   pol_with_variables(X,XV),
   pol_w_v_list(Rest,RestV).


% pol_write_interpretation_with_vars(Interpretation, Varlist) displays the
% interpretation with the variable names in varlist.
% If it is a tuple, each component is displayed on a separate line.

pol_write_interpretation_with_vars([Polynomial], VarList) :- 
	!,
	pol_write_polynomial_with_vars(Polynomial, VarList),
	nl.
pol_write_interpretation_with_vars([Pol1|Rest], VarList) :-
	write('   [ '),
	pol_write_polynomial_with_vars(Pol1, VarList),
	pol_write_pol_list_with_vars(Rest, VarList).

pol_write_pol_list_with_vars([], _) :-
	write(' ]'),
	nl.
pol_write_pol_list_with_vars([Pol1|Rest], VarList) :-
	write(' ,'),
	nl,
	tab(5),
	pol_write_polynomial_with_vars(Pol1, VarList),
	pol_write_pol_list_with_vars(Rest, VarList).


%  pol_write_polynomial_with_vars writes a polynomial without unnecessary 0's
%  and 1's.
%  e.g. not '1 * (@x^1)' , but '@x'

pol_write_polynomial_with_vars([], _) :- !,write(0).
pol_write_polynomial_with_vars([(q(1,1),M)|R], VarList) :-
	pol_zerolist(M),
	!,
	write(1),
	pol_write_p_with_vars(R, VarList).
pol_write_polynomial_with_vars([(q(-1,1),M)|R], VarList) :-
	pol_zerolist(M),
	!,
	write(-1),
	pol_write_p_with_vars(R, VarList).
pol_write_polynomial_with_vars([(q(0,_),_)|R], VarList) :-
	!,pol_write_polynomial_with_vars(R, VarList).
pol_write_polynomial_with_vars([(q(Z,N),M)|R], VarList) :-
	Z > 0,
	!,
	pol_write_rat(q(Z,N),Done),
	pol_write_m_with_vars(M,Done, VarList),
	pol_write_p_with_vars(R, VarList).
pol_write_polynomial_with_vars([(q(Z,N),M)|R], VarList) :-
	write('-'),
	pol_write_rat(q(-Z,N),Done),
	pol_write_m_with_vars(M,Done, VarList),
	pol_write_p_with_vars(R, VarList).


%  pol_write_m_with_vars(Monomial,Sign, VarList) writes a monomial with a leading ' * '
%  e.g. pol_write_m_with_vars([2,0,1],yes,[a,b,c]) -->  * (a^2) * c
%       pol_write_m_with_vars([1,2,0,1],no,[v1,v2,v3,v4]) -->  v1 * (v2^2) * v4

pol_write_m_with_vars([],_,_) :- !.
pol_write_m_with_vars([A|R],Sign,[Var1|VarRest]) :-
	(Var1 = @(V) ->
		pol_write_power(V,'',A,Sign,Sign1)
	;
		pol_write_power(Var1,'',A,Sign,Sign1)
	),
	pol_write_m_with_vars(R,Sign1,VarRest).


% pol_write_p_with_vars writes a polynomial with a leading ' - ' or ' + '

pol_write_p_with_vars([], _) :- !.
pol_write_p_with_vars([(q(1,1),M)|R], VarList) :-
	pol_zerolist(M),
	!,
	write(' + '),
	write(1),
	pol_write_p_with_vars(R, VarList).
pol_write_p_with_vars([(q(-1,1),M)|R], VarList) :-
	pol_zerolist(M),
	!,
	write(' - '),
	write(1),
	pol_write_p_with_vars(R, VarList).
pol_write_p_with_vars([(q(0,_),_)|R], VarList) :-
	!,
	pol_write_p_with_vars(R, VarList).
pol_write_p_with_vars([(q(Z,N),M)|R], VarList) :-
	Z > 0,
	!,
	write(' + '),
	pol_write_rat(q(Z,N),Done),
	pol_write_m_with_vars(M,Done, VarList),
	pol_write_p_with_vars(R, VarList).
pol_write_p_with_vars([(q(Z,N),M)|R], VarList) :-
	write(' - '),
	pol_write_rat(q(-Z,N),Done),
	pol_write_m_with_vars(M,Done, VarList),
	pol_write_p_with_vars(R, VarList).
