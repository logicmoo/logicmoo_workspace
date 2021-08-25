/*
 *	file:		
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates to rename operators, when the
 *	ordering is polyN.
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

%  pol_rename2(System,Substitution,RenamedSystem)
%  Substitution is a list of pairs (Oldnamew, Newname).
%  The user has to ensure, that Operator/Arity has no duplicates
%  in the Substituion (e.g. the result of the substitution
%  [(a/2, b), (a/2, c)] is not defined), and that each operator is
%  encountered in System with at most one arity.
%  All operators not encountered in Substitution are renamed to themselves.
%  If two Operators are renamed to the same Operator, their arity
%  Operator, their operator types ('none', 'c' or 'ac') and their
%  interpretations  must be identical (or one of them has no interpretation).
%  If one of the constraints above is violated, an error message is
%  printed and pol_rename fails, else operator names are renamed
%  resulting RenamedSystem.
%  Simultaneously renaming is possible (e.g. [(a, b),(b, a)] ).


pol_rename2(pol_state(N,IList,C,AC), Subst, pol_state(N,RList,RC,RAC)) :-
	pol_rename2_l(Subst, IList,C,AC, RList,RC,RAC).



pol_rename2_l([], IList,C,AC, IList,C,AC).
pol_rename2_l([(Op1,Op1) | RestSubst], IList,C,AC, RList,RC,RAC) :-
	!,
	%  renaming Op1 to itself
	pol_rename2_l(RestSubst, IList,C,AC, RList,RC,RAC).
pol_rename2_l([(Op1,Newname) | RestSubst], IList,C,AC, RList,RC,RAC) :-
	(	member(pol_op_interpretation(Newname,Arity2,_I),IList)
	;
		(	Arity2 = 2,
			(	member(Newname,C)
			;
				member(Newname,AC)
			)
		)
	),
	%
	%  Op1 is renamed to Newname, but Newname already exists
	%
	not((member((Newname, Newname2),RestSubst),
	     not(Newname = Newname2))),
	%
	%  and Newname is not simultaneously renamed
	%
	!,
	pol_get_arity(Op1,Arity1, IList,C,AC),
	pol_equal_arity(Op1,Arity1, Newname,Arity2, Newname),
	!,
	pol_equal_op(Op1,Newname,Arity1,C,AC,Newname),
	!,
	pol_equal_interpr(Op1,Newname,Arity1,IList,RemoveOp,Newname),
	!,
	pol_remove(RemoveOp,C,CI),
	pol_remove(RemoveOp,AC,ACI),
	pol_remove(pol_op_interpretation(RemoveOp,Arity1,_),IList,IListI),
	pol_remove((RemoveOp, Newname), [(Op1, Newname) | RestSubst],NewSubst),
	pol_rename2_l(NewSubst, IListI,CI,ACI, RList,RC,RAC).
pol_rename2_l([(Op1, Newname) | RestSubst], IList,C,AC, RList,RC,RAC) :-
	member((Op2, Newname), RestSubst),
	%
	%  Op1 and Op2 are both renamed to Newname
	%
	!,
	pol_get_arity(Op1,Arity1, IList,C,AC),
	pol_get_arity(Op2,Arity2, IList,C,AC),
	pol_equal_arity(Op1,Arity1, Op2,Arity2, Newname),
	!,
	pol_equal_op(Op1,Op2,Arity1,C,AC,Newname),
	!,
	pol_equal_interpr(Op1,Op2,Arity1,IList,RemoveOp,Newname),
	!,
	pol_remove(RemoveOp,C,CI),
	pol_remove(RemoveOp,AC,ACI),
	pol_remove(pol_op_interpretation(RemoveOp,Arity1,_),IList,IListI),
	pol_remove((RemoveOp, Newname), [(Op1, Newname) | RestSubst],NewSubst),
	pol_rename2_l(NewSubst, IListI,CI,ACI, RList,RC,RAC).
pol_rename2_l([(Op1, Newname) | RestSubst], IList,C,AC, 
	      [pol_op_interpretation(Newname,2,I)|RList],[Newname|RC],RAC) :-
	member(Op1,C),
	member(pol_op_interpretation(Op1,2,I),IList),
	!,
	pol_remove(Op1,C,CI),
	pol_remove(pol_op_interpretation(Op1,2,I),IList,IListI),
	pol_rename2_l(RestSubst, IListI,CI,AC, RList,RC,RAC).
pol_rename2_l([(Op1, Newname) | RestSubst], IList,C,AC,
	      RList,[Newname|RC],RAC) :-
	member(Op1,C),
	!,
	pol_remove(Op1,C,CI),
	pol_rename2_l(RestSubst, IList,CI,AC, RList,RC,RAC).
pol_rename2_l([(Op1, Newname) | RestSubst], IList,C,AC,
	      [pol_op_interpretation(Newname,2,I)|RList],RC,[Newname|RAC]) :-
	member(Op1,AC),
	member(pol_op_interpretation(Op1,2,I),IList),
	!,
	pol_remove(Op1,AC,ACI),
	pol_remove(pol_op_interpretation(Op1,2,I),IList,IListI),
	pol_rename2_l(RestSubst, IListI,C,ACI, RList,RC,RAC).
pol_rename2_l([(Op1, Newname) | RestSubst], IList,C,AC,
	      RList,RC,[Newname|RAC]) :-
	member(Op1,AC),
	!,
	pol_remove(Op1,AC,ACI),
	pol_rename2_l(RestSubst, IList,C,ACI, RList,RC,RAC).
pol_rename2_l([(Op1, Newname) | RestSubst], IList,C,AC,
	      [pol_op_interpretation(Newname,Arity,I) | RList],RC,RAC) :-
	member(pol_op_interpretation(Op1,Arity,I),IList),
	!,
	pol_remove(pol_op_interpretation(Op1,Arity,I),IList,IListI),
	pol_rename2_l(RestSubst, IListI,C,AC, RList,RC,RAC).
pol_rename2_l([(_Op1, _Newname) | RestSubst], IList,C,AC, RList,RC,RAC) :-
	pol_rename2_l(RestSubst, IList,C,AC, RList,RC,RAC).


pol_get_arity(Op, Arity, IList, _, _) :-
	member(pol_op_interpretation(Op,Arity,_),IList),
	!.
pol_get_arity(Op, 2, _, C, AC) :-
	(	member(Op,C)
	;
		member(Op,AC)
	),
	!.
pol_get_arity(_, _, _, _, _).


pol_equal_arity(_Op1, Arity, _Op2, Arity, _Newname) :- !.
pol_equal_arity(Op1, Arity1, Op2, Arity2, Newname) :-
	pol_rename_error(arities,[Op1,Arity1,Op2,Arity2,Newname,'?']),
	fail.


pol_equal_op(Op1,Op2,2,C,_AC,Newname) :-
	member(Op1,C),
	!,
	(	member(Op2,C)
	;
		pol_rename_error(operatortypes,[Op1,2,Op2,2,Newname,2]),
		fail
	).
pol_equal_op(Op1,Op2,2,_C,AC,Newname) :-
	member(Op1,AC),
	!,
	(	member(Op2,AC)
	;
		pol_rename_error(operatortypes,[Op1,2,Op2,2,Newname,2]),
		fail
	).
pol_equal_op(Op1,Op2,2,C,AC,Newname) :-
	(	member(Op2,C)
	;
		member(Op2,AC)
	),
	!,
	pol_rename_error(operatortypes,[Op1,2,Op2,2,Newname,2]),
	fail.
pol_equal_op(_,_,_,_,_,_).



pol_equal_interpr(Op1,Op2,Arity,IList,Op1,Newname) :-
	member(pol_op_interpretation(Op1,Arity,I1),IList),
	member(pol_op_interpretation(Op2,Arity,I2),IList),
	!,
	(	I1 = I2
	;
		pol_rename_error(interpretations,[Op1,Arity,Op2,Arity,Newname,Arity]),
		fail
	).
pol_equal_interpr(Op1,Op2,Arity,IList,Op2,_Newname) :-
	member(pol_op_interpretation(Op1,Arity,_),IList),
	!.
pol_equal_interpr(Op1,_Op2,_Arity,_IList,Op1,_Newname).


pol_rename_error(Comment,Args) :-
	nl,
	error("different % for %/% and %/%,
	          cannot rename them to %/%",
	       [Comment|Args],pol_rename).
