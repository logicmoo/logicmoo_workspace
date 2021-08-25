/*
 *	file:		interpret.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates for calculation of polynomial
 *	interpretation of a term under the current polynomial interpretation
 *	for the operators of the term, demanding user input of missing 
 *	operator interpretations.
 *	
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

% pol_interpret_term(Term, VarList, Result) calculates the
% interpretation of Term.
% The interpretation of a constant must be expanded to a
% polynomial with given arity (e.g. the constant 3 is transformed
% into the polynomial 3 * x1^0 * ... * xn^0).
% The interpretation of a variable @X is determined using Varlist.
% If @X occurs in VarList at the i-th position, its interpretation
% is 1 * x1^0 * ... * xi^1 * ... * xn^0.
% The interpretation of a compound term is computed from the
% interpretation of its functor and the interpretations of its
% arguments.


pol_interpret_term(@X, VarList, Result) :-
	!,
	pol_transformvar(@X, VarList, ExpList),
	pol_get_tuplelength(N),
	pol_expand_to_list([(q(1,1), ExpList)], N, Result).
pol_interpret_term(X, VarList, Result) :-
	functor(X, _X, 0),
	!,
	pol_get_op_interpretation(X, 0, Res1),
	pol_make_zero(VarList, ZeroList),
	pol_map_change_explists(Res1, ZeroList, Result).
pol_interpret_term(T, VarList, Result) :-
	functor(T, _, N),
	T =.. [Functor|Arguments],
	!,
	pol_map_interpret_term(Arguments, VarList, ArgInterp),
	pol_get_op_interpretation(Functor, N, FunctInterp),
	pol_map_papply(FunctInterp, ArgInterp, Result).


pol_map_change_explists([], _, []).
pol_map_change_explists([Polynomial|PList], L, [NewPolynomial|NewPList]) :-
	pol_change_explists(Polynomial, L, NewPolynomial),
	pol_map_change_explists(PList, L, NewPList).


pol_change_explists([], _, []).
pol_change_explists([(C,_)|PolynomialRest], L, [(C,L)|NewPolynomialRest]) :-
	pol_change_explists(PolynomialRest, L, NewPolynomialRest).


pol_map_interpret_term([], _, []).
pol_map_interpret_term([T1|TList], VarList, [R1|RList]) :-
	pol_interpret_term(T1, VarList, R1),
	pol_map_interpret_term(TList, VarList, RList).


pol_map_papply([], _, []).
pol_map_papply([F1|FList], ArgumentLists, [R1|RList]) :-
	pol_headsandtails(ArgumentLists, Heads, Tails),
	pol_papply(F1, Heads, R1),
	pol_map_papply(FList, Tails, RList).


pol_headsandtails([],[],[]).
pol_headsandtails([[H|T]|L],[H|HL],[T|TL]) :-
	pol_headsandtails(L,HL,TL).


pol_get_op_interpretation(Functor, Arity, Interpretation) :-
	pol_get_current(pol_state(_N,IList,_C,_AC)),
	member(pol_op_interpretation(Functor, Arity, Interpretation),IList),
	!.
pol_get_op_interpretation(Functor, Arity, Interpretation) :-
	pol_get_current(pol_state(_N,IList,_C,_AC)),
	TypeLength is Arity+1,
	(OF ofType (Functor:T)),
	length(T,TypeLength),
	member(pol_op_interpretation(OF, Arity, Interpretation),IList),
	!.
pol_get_op_interpretation(F,N,I):-
	nRedComplOp(F),
	listOfNumbers(N,Indexes),
%	Coeff is 2*(N+1),
	Coeff = 2,
	map(lambda([J,M],[M1]^(varAsMonomial(N,J,M1),M=(q(Coeff,1),M1))),
			Indexes,IR),
	pol_expand_to_tuple_length(IR,I),
	pol_assert(pol_op_interpretation(F,N,I)),
	!.
%pol_get_op_interpretation(F,K,I):-
%	nRedComplOp(F),
%	listOfNumbers(K,Indexes),
%	map(lambda([J,M],[M1]^(varAsMonomial(K,J,M1),M=(q(2,1),M1))),Indexes,IR),
%	pol_expand_to_tuple_length(IR,I),
%	pol_assert(pol_op_interpretation(F,K,I)),
%	!.
pol_get_op_interpretation(Functor, Arity, Interpretation) :-
	pol_enter_interpretation(Functor, Arity),
	pol_get_op_interpretation(Functor, Arity, Interpretation).



pol_expand_to_tuple_length(IR,I):-
	pol_get_tuplelength(N),
	pol_expand_to_list(IR, N, I).



varAsMonomial(N,N,[1|M]):-
	N1 is N-1,
	genList(N1,0,M),
	!.
varAsMonomial(N,V,[0|M]):-
	N1 is N-1,
	varAsMonomial(N1,V,M).

varPAsMonomial(N,N,[N|M]):-
	N1 is N-1,
	genList(N1,0,M),
	!.
varPAsMonomial(N,V,[0|M]):-
	N1 is N-1,
	varPAsMonomial(N1,V,M).
	


% pol_getvars(Term, OldVarList, NewVarList) collects all variables
% from Term and inserts them into OldVarList.


pol_getvars(@X, OldVarList, NewVarList) :-
	!,
	pol_insert(@X, OldVarList, NewVarList).
pol_getvars(X, OldVarList, OldVarList) :-
	functor(X, _X, 0),
	!.
pol_getvars(T, OldVarList, NewVarList) :-
	T =.. [_Functor|Arguments],
	pol_map_getvars(Arguments, OldVarList, NewVarList).


pol_map_getvars([], OldVarList, OldVarList) :- !.
pol_map_getvars([X|Rest], OldVarList, NewVarList) :-
	pol_getvars(X, OldVarList, VarList),
	pol_map_getvars(Rest, VarList, NewVarList).
