/*
 *	file:		combine.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates to combine polynomial
 *	termination orderings.
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

%  pol_combine(System1,System2,System3) succeeds, if the interpretations
%  of System1 and System2 can be combined to System3.
%  If in both Systems (1 and 2) an operator declaration for the same
%  operator is encountered, the stronger one is taken ('c' < 'ac').
%  If in either System (1 or 2) an operator declaration for an
%  operator is encountered, it must be legal in both Systems,
%  otherwise an error message is printed and pol_combine/3 fails.
%  If in both Systems (1 and 2) there are different interpretations for
%  an Operator/Arity, an error message is printed and pol_combine/3 fails.
%  'different' means, that one interpretation is not a prefix of the other. 

pol_combine(pol_state(N,List1,C1,AC1),pol_state(M,List2,C2,AC2),
	    pol_state(N,List3,C3,AC3)) :-
	N>=M,
	!,
	pol_mix_op(C1,C2,C3,AC1,AC2,AC3),
	pol_op_compatible(List1,C3,AC3),
	pol_op_compatible(List2,C3,AC3),
	pol_mix(N,List2,List1,List3).
pol_combine(pol_state(_N,List1,C1,AC1),pol_state(M,List2,C2,AC2),
	    pol_state(M,List3,C3,AC3)) :-
	pol_mix_op(C1,C2,C3,AC1,AC2,AC3),
	pol_op_compatible(List1,C3,AC3),
	pol_op_compatible(List2,C3,AC3),
	pol_mix(M,List1,List2,List3).


pol_mix_op(C1,C2,C3,AC1,AC2,AC3) :-
	append(C1,C2,CI),
	pol_remove_dupl(CI,CII),
	append(AC1,AC2,ACI),
	pol_remove_dupl(ACI,AC3),
	pol_delete_dupl(CII,AC3,C3).


pol_delete_dupl([],_,[]).
pol_delete_dupl([Op|Rest],AC,NewRest) :-
	member(Op,AC),
	!,
	pol_delete_dupl(Rest,AC,NewRest).
pol_delete_dupl([Op|Rest],AC,[Op|NewRest]) :-
	pol_delete_dupl(Rest,AC,NewRest).


pol_op_compatible([],_,_).
pol_op_compatible([pol_op_interpretation(Op,2,I)|Rest],C,AC) :-
	member(Op,C),
	!,
	(pol_has_C_property(I) ->
		pol_op_compatible(Rest,C,AC)
	;
		write('*** combine: Operator declaration not compatible with one system'),
		nl,
		sPrint("operator '%' with arity 2 has the interpretation:", [Op]),
		nl,
	pol_write_interpretation(I),
		write('at least one polynomial is not symmetric !'),
		nl,
		pol_op_compatible(Rest,C,AC),
		fail
	).
pol_op_compatible([pol_op_interpretation(Op,2,I)|Rest],C,AC) :-
	member(Op,AC),
	!,
	(pol_has_AC_property(I) ->
		pol_op_compatible(Rest,C,AC)
	;
		write('*** combine: Operator declaration not compatible with one system'),
		nl,
		sPrint("operator '%' with arity 2 has the interpretation :", [Op]),
		nl,
		pol_write_interpretation(I),
		write('at least one polynomial has not AC-property !'),
		nl,
		pol_op_compatible(Rest,C,AC),
		fail
	).
pol_op_compatible([_|Rest],C,AC) :-
	pol_op_compatible(Rest,C,AC).



pol_mix(_N,[],List2,List2).
pol_mix(N,[Inter1|RestInter],List2,[Inter2|NewRest]) :-
	pol_exist_other(Inter1,List2,NewList2,Inter2),
	!,
	(pol_compatible(Inter1,Inter2) ->	
		pol_mix(N,RestInter,NewList2,NewRest)
	;
		pol_mix(N,RestInter,NewList2,NewRest),
		!,
		fail
	).
pol_mix(N,[Inter1|RestInter],List2,[NewInter|NewRest]) :-
	pol_extend(N,Inter1,NewInter),
	pol_mix(N,RestInter,List2,NewRest).


pol_exist_other(pol_op_interpretation(Op,Arity,_Inter1),
		[pol_op_interpretation(Op,Arity,Inter2)|Rest2],
		Rest2,
		pol_op_interpretation(Op,Arity,Inter2)) :-
	!.
pol_exist_other(Inter1,[First|Rest],[First|NewRest],Inter2) :-
	pol_exist_other(Inter1,Rest,NewRest,Inter2).


pol_compatible(pol_op_interpretation(Op,Arity,_Inter1),
	       pol_op_interpretation(Op,Arity,_Inter2)) :-
	(   nRedComplOp(Op)
	;   redComplOp(Op)),
	!.
pol_compatible(pol_op_interpretation(Op,Arity,Inter1),
	       pol_op_interpretation(Op,Arity,Inter2)) :-
	pol_prefix(Inter1,Inter2),
	!.
pol_compatible(pol_op_interpretation(Op,Arity,_Inter1),_) :-
	write('*** combine: different interpretations in both systems '),
	nl,
	sPrint("operator '%' with arity %", [Op, Arity]),
	nl,
	fail.


pol_prefix([],_L).
pol_prefix([First|Rest1],[First|Rest2]) :-
	pol_prefix(Rest1,Rest2).


pol_extend(N,pol_op_interpretation(Op,Arity,Inter1),
	     pol_op_interpretation(Op,Arity,Inter2)) :-
	pol_ext(N,Inter1,Inter2).


pol_ext(1,[Inter1|_],[Inter1]) :- !.  % modification 17.1.89
% pol_ext used for shrinking : pol_ext(2,[1,1,1,1],L)  --> L = [1,1]
pol_ext(N,[First,Second|Rest],[First|ExtRest]) :-
	!,
	N1 is N-1,
	pol_ext(N1,[Second|Rest],ExtRest).
pol_ext(N,[One],[One|Rest]) :-
	N1 is N-1,
	pol_ext(N1,[One],Rest).
