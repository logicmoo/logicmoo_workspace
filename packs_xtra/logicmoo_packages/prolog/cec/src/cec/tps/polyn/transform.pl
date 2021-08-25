/*
 *	file:		transform.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates for calculation of the interpretation
 *	of an arithmetic term.
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

% pol_transform(Term, VarList, Result) calculates the
% interpretation of an arithmetic Term (including ip(_) ).
% new version : in the expression describing the interpretation
% of an operator you can also use 'ip(Term)' and 'ip(Term, Component)'
% with an integer greater 0 as  Component. ip(Term) takes the interpretation
% of the headfunctor of Term and applies it to its arguments (which are
% also arithmetic terms, perhaps including ip's).
% ip(Term, Component) does the same, but selects then a special component
% of the resulting interpretation.
% If an operator is encountered which is not '.', +, *, ^ or a number,
% but an operator of the signature, an additional ip(_) is inserted
% (the only way to get the interpretation of +, if it
% is an operator of the signature, is ip(x+y) ).
% Single polynomials are automatically expanded to whole interpretations 
% (tuples) by copying the polynomial. 
% If a whole interpretation (a tuple of polynomials) is found when
% calculating the n-th component of an interpreting tuple, the interpretation
% is automatically projected to it's n-th component
% (e.g. in [x+y, x*y, ip(g(x,y))] the last component yields a triple of which
% only the third component is used, if you like to use the second component
% of the interpretation of g(x,y) in the third component of another
% interpretation you must type [x+y, x*y, ip(g(x,y),2)] with ip(_, 2).
% But using ip(g(x,y)) as a part of an interpretation which is not
% a single component or the whole interpretation is impossible, so 
% [x+y, ip(g(x,y))] never yields a triple by taking the last two components
% of g's interpretation ).
% For lists which have not the length N (tuplelength), the '.' losts its
% predefined meaning (N = 1 --> [1,2|3] is interpreted as ip([1, ip([2|3])) ).

pol_transform(V, _, _) :-
	var(V),
	!,
	write('*** detected prolog variable in interpretation'),
	nl,
	fail.
pol_transform(@X, VarList, Result) :-
	!,
	pol_transformvar(@X, VarList, ExpList),
	pol_get_tuplelength(N),
	pol_expand_to_list([(q(1,1), ExpList)], N, Result).
pol_transform(X, VarList, Result) :-
	atomic(X),
	!,
	(integer(X) ->
		(X > 0 ->
			pol_make_zero(VarList, ZeroList),
			pol_change_explists([(q(X,1),_)], ZeroList, Result1),
			pol_get_tuplelength(N),
			pol_expand_to_list(Result1, N, Result)
		;
			sPrint("*** detected a non positive integer in interpretation : %", [X]),
			nl,
			fail
		)
	;
		((member(X, VarList) ; member(@(X), VarList)) ->
			pol_transform(@(X), VarList, Result) 
		;
			(ofType(X,(_:[_])) ->
				pol_transform(ip(X), VarList, Result) 
			;
				sPrint("*** detected undeclared Operator : '%' with arity 0", [X]),
				nl,
				fail
			)
		)
	).
pol_transform([I|Is], VarList, Result) :-
	!,
	pol_get_tuplelength(N),
	(length([I|Is], N) ->
		pol_transform_list([I|Is], VarList, Result, 1)
	;
		(ofType('.',(_:[_,_,_])) ->
			pol_make_ip_list([I|Is],IPList),
			write('*** warning : list not of correct tuplelength, following conversion done :'),
			nl,
			print([I|Is]),
			write('  to  '),
			print(IPList),
			nl,
			write('*** if it is not what you wanted, use explicit ip''s.'),
			nl,
			pol_transform(IPList, VarList, Result)
		;
			write('*** expression with illegal tuplelength :'),
			nl,
			write([I|Is]),
			nl,
			fail
		)
	).
pol_transform(T1 + T2, VarList, Result) :-
	!,
	pol_map_transform([T1,T2], VarList, ArgInterp),
	pol_get_tuplelength(N),
	pol_expand_to_list([(q(1,1), [1,0]),(q(1,1),[0,1])], N, FunctInterp),
	pol_map_papply(FunctInterp, ArgInterp, Result).
pol_transform(T1 * T2, VarList, Result) :-
	!,
	pol_map_transform([T1,T2], VarList, ArgInterp),
	pol_get_tuplelength(N),
	pol_expand_to_list([(q(1,1), [1,1])], N, FunctInterp),
	pol_map_papply(FunctInterp, ArgInterp, Result).
pol_transform(T1 ^ T2, VarList, Result) :-
	!,
	pol_transform(T2, VarList, Arg2Interp),
	(pol_is_exponent(Arg2Interp, Exp) ->
		pol_transform(T1, VarList, Arg1Interp),
		pol_map_ppower(Arg1Interp, Exp, Result)
	;
		sPrint("*** illegal exponent : % in %", [T2, T1^T2]),
		nl,
		fail
	).
pol_transform(ip(T), VarList, Result) :-
	!,
	(var(T) ->
		pol_transform(T,VarList, Result)
		% yields error message
	;
		functor(T, _, N),
		(N = 0 ->
			pol_get_op_interpretation(T, N, Res1),
			pol_make_zero(VarList, ZeroList),
			pol_map_change_explists(Res1, ZeroList, Result) 
		;
			T =.. [Functor|Arguments],
			pol_map_transform(Arguments, VarList, ArgInterp),
			pol_get_op_interpretation(Functor, N, FunctInterp),
			pol_map_papply(FunctInterp, ArgInterp, Result)
		)
	).
pol_transform(ip(T, K), VarList, Pols) :-
	!,
	pol_transform(ip(T), VarList, Result),
	pol_select_component_with_message(Result, K, Polynomial),
	pol_get_tuplelength(M),
	pol_expand_to_list(Polynomial, M, Pols).
pol_transform(T, VarList, Result) :-
	functor(T, F, N),
	((ofType(F,(_:Type)), length(Type, N1),	N is N1-1) ->
		pol_transform(ip(T), VarList, Result)
	;
		sPrint("*** detected undeclared Operator : '%' with arity %", [F, N]),
		nl,
		fail
	).

pol_map_transform([], _, []).
pol_map_transform([T1|TList], VarList, [R1|RList]) :-
	pol_transform(T1, VarList, R1),
	pol_map_transform(TList, VarList, RList).


pol_is_exponent([[(q(Exp,1),ZList)]], [Exp]) :-
   pol_zerolist(ZList),
   !.
pol_is_exponent( [[(q(Exp,1),ZList)]|Pols], [Exp|ERest]) :-
   pol_zerolist(ZList),
   pol_is_exponent(Pols, ERest).

pol_map_ppower([], _, []).
pol_map_ppower([P|Rest], [Exp|ERest], [PP|PRest]) :-
   pol_ppower(P, Exp, PP),
   pol_map_ppower(Rest, ERest, PRest).


pol_select_component([P|_], 1, P) :- !.
pol_select_component([_|Ps], N, Result) :-
	integer(N),
	N > 1,
	N1 is N-1,
	pol_select_component(Ps, N1, Result).

pol_select_component_with_message(I,K,P) :-
	pol_select_component(I,K,P),
	!.
pol_select_component_with_message(_I,K,_P) :-
	sPrint("*** illegal component of interpretation : %", [K]),
	nl,
	fail.

pol_transformvar(Var, [Var|Rest], [1|Rest0]) :-
	!,
	pol_make_zero(Rest, Rest0).
pol_transformvar(@(Var), [Var|Rest], [1|Rest0]) :-
	!,
	pol_make_zero(Rest, Rest0).
pol_transformvar(Var, [_Var1|Rest], [0|Rest0]) :-
	pol_transformvar(Var, Rest, Rest0).


pol_make_ip_list([T|TRest], ip([T|IPRest])) :-
	!,
	pol_make_ip_list(TRest, IPRest).
pol_make_ip_list(T,T).
