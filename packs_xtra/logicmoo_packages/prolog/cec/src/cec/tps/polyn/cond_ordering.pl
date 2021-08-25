/*
 *	file:		cond_ordering.pl
 *	version:	1.0
 *	date:		April 11, 1990
 *	creation:	April 11, 1990
 *	author:		Uwe Waldmann (uwe)
 *
 *	description:
 *	This file contains predicates for conditional ordering of equations.
 *	It replaces the old file "ordering.pl".
 *
 *	history:
 *	900430	uwe	Corrected the predicate description
 *
 *	Copyright (C) 1990
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

% pol_ordering(L, R, NL, NR):
% 
%   Succeeds, if the equation L == R can be ordered in either direction.
%   NL and NR are the left and right sides of the resulting rewriting rule.
% 
% 
% pol_ordering(L, R):
% 
%   Succeeds, if the equation L == R can be ordered as L --> R using the
%   given polynomial interpretation.
% 
% 
% pol_ordering(Comment, L, R, NL, NR),
% pol_ordering(Comment, L, R):
% 
%   Same as above, but the string "Comment" and the terms L and R are
%   printed before the user is asked to enter an interpretation for a new
%   operator.
% 
% 
% pol_cond_ordering(CL, CR, L, R, NL, NR),
% pol_cond_ordering(CL, CR, L, R),
% pol_cond_ordering(Comment, CL, CR, L, R, NL, NR), 
% pol_cond_ordering(Comment, CL, CR, L, R):
% 
%   same as above, but it is checked, whether sigma(L) == sigma(R) can be
%   ordered for all substitutions that satisfy sigma(CL) >= sigma(CR).


pol_ordering(L, R, NL, NR) :-
	pol_retractall(pol_comment(_,_,_)),
	pol_ordering0(L, R, NL, NR).

pol_ordering(Comment, L, R, NL, NR) :-
	pol_retractall(pol_comment(_,_,_)),
	asserta(pol_comment(Comment,L,R)),
	pol_ordering0(L, R, NL, NR).

pol_ordering(L, R) :-
	pol_retractall(pol_comment(_,_,_)),
	pol_ordering0(L, R).

pol_ordering(Comment, L, R) :-
	pol_retractall(pol_comment(_,_,_)),
	asserta(pol_comment(Comment,L,R)),
	pol_ordering0(L, R).



pol_ordering0(L, R, NL, NR) :-
	pol_getvars((L,R), [], VarList),
	pol_interpret_term(L, VarList, LI),
	pol_interpret_term(R, VarList, RI),
	pol_cond_tuple_gt_or_lt(L, R, true, LI, RI, NL, NR).

pol_ordering0(L, R) :-
	pol_getvars((L,R), [], VarList),
	pol_interpret_term(L, VarList, LI),
	pol_interpret_term(R, VarList, RI),
	pol_cond_tuplegreater(true, LI, RI).



pol_cond_ordering(CL, CR, L, R, NL, NR) :-
	pol_retractall(pol_comment(_,_,_)),
	pol_cond_ordering0(CL, CR, L, R, NL, NR).

pol_cond_ordering(Comment, CL, CR, L, R, NL, NR) :-
	pol_retractall(pol_comment(_,_,_)),
	asserta(pol_comment(Comment,L,R)),
	pol_cond_ordering0(CL, CR, L, R, NL, NR).

pol_cond_ordering(CL, CR, L, R) :-
	pol_retractall(pol_comment(_,_,_)),
	pol_cond_ordering0(CL, CR, L, R).

pol_cond_ordering(Comment, CL, CR, L, R) :-
	pol_retractall(pol_comment(_,_,_)),
	asserta(pol_comment(Comment,L,R)),
	pol_cond_ordering0(CL, CR, L, R).



pol_cond_ordering0(CL, CR, L, R, NL, NR) :-
	pol_getvars((CL,CR,L,R), [], VarList),
	pol_interpret_term(CL, VarList,	[CLI|_]),
	pol_interpret_term(CR, VarList, [CRI|_]),
	pol_interpret_term(L, VarList, LI),
	pol_interpret_term(R, VarList, RI),
	pol_cond_tuple_gt_or_lt(L, R, CLI >= CRI, LI, RI, NL, NR).


pol_cond_ordering0(CL, CR, L, R) :-
	pol_getvars((CL,CR,L,R), [], VarList),
	pol_interpret_term(CL, VarList, [CLI|_]),
	pol_interpret_term(CR, VarList, [CRI|_]),
	pol_interpret_term(L, VarList, LI),
	pol_interpret_term(R, VarList, RI),
	pol_cond_tuplegreater(CLI >= CRI, LI, RI).



pol_cond_tuple_gt_or_lt(L, R, Cond, LI, RI, L, R) :-
	pol_cond_tuplegreater(Cond, LI, RI),
	!.

pol_cond_tuple_gt_or_lt(L, R, Cond, LI, RI, R, L) :-
	pol_cond_tuplegreater(Cond, RI, LI).




% pol_cond_tuplegreater(Cond, PolList1, PolList2)
% checks, whether the polynomial list PolList1 is lexicographically
% greater than PolList2 whenever Cond is satisfied.

pol_cond_tuplegreater(Cond, [Pol|PList1], [Pol|PList2]) :-
	!,
	pol_cond_tuplegreater(Cond, PList1, PList2).

pol_cond_tuplegreater(Cond, [Pol1|_PList1], [Pol2|_PList2]) :-
	Minus1 is -1,
	pol_rpmult(q(Minus1,1), Pol2, NegPol),
	pol_pplus(Pol1, NegPol, ResPol),
	pol_cond_positive(ResPol, [], Cond).




% pol_cond_positive(Rest, PosList, Condition):
% tries to prove that the polynomial P = PosList + Rest is positive
% whenever Condition is satisfied.

%-------Tracing---------------

pol_cond_positive(Rest, PosList, Cond) :-
	pol_trace,
	print('Cond is '),
	print(Cond),
	nl,
	print('Rest is '),
	print(Rest),
	nl,
	print('PosList is '),
	print(PosList),
	nl,
	fail.

%-----------------------------

pol_cond_positive([], [], _) :- !, fail.

pol_cond_positive([], _, _).

pol_cond_positive([(q(Z,N),Expl)|Restp], L, Cond) :-
	Z > 0,
	!,
	pol_cond_positive(Restp, [(q(Z,N),Expl)|L], Cond).

pol_cond_positive([(q(Z,N),Expl)|Restp], L, Cond) :-
	pol_choose((q(Z,N),Expl), L, (X,X), (q(0,0),[]), (Y,Y), Restp, Cond).


% pol_choose(NegMonomial, PosList, AfterCandidate, Candidate, BeforeCandidate,
%            RestPolynomial, Condition)

pol_choose(Neg, [], (ReversedPosList,[]), (q(0,0),[]), _, Restp, Cond) :-
	!,
	pol_analyze_condition(Cond, SimplifiedCond),
	\+ (SimplifiedCond = true),
        ( SimplifiedCond = false ->
	    true
	  ; pol_revappend(ReversedPosList, [Neg|Restp], Pol),
	    pol_apply_condition(SimplifiedCond, Pol, NewPol, true),
	    pol_cond_positive(NewPol, [], SimplifiedCond)
	).

pol_choose(Neg, [], (LN,[]), K, VK, Restp, Cond) :-
	!,
	pol_change(LN, K, VK, Neg, Restp, Cond).

pol_choose((q(MinusZ,N),Expl1), [(q(Z,N),Expl2)|L], NK, K, VK, Restp, Cond) :-
	MinusZ is -Z,
	pol_expl_ge(Expl2, Expl1),
	!,
	pol_conc3(VK, K, NK, VNK),
	pol_change(L, (q(Z,N),Expl2), VNK, (q(MinusZ,N),Expl1), Restp, Cond).

pol_choose((Q1,Expl1), [(Q2,Expl2)|L], NK, (Q3,Expl3), VK, Restp, Cond) :-
	pol_expl_ge(Expl2, Expl1),
	pol_better_fit(Q2, Q1, Q3),
	!,
	pol_conc3(VK, (Q3,Expl3), NK, VNK),
	pol_choose((Q1,Expl1), L, (X,X), (Q2,Expl2), VNK, Restp, Cond).

pol_choose(Neg, [Lhead|L], (LN,[Lhead|RNN]), K, VK, Restp, Cond) :-
	!,
	pol_choose(Neg, L, (LN,RNN), K, VK, Restp, Cond).

pol_expl_ge([], []).

pol_expl_ge([E1|L1], [E2|L2]) :-
	E1 >= E2,
	!,
	pol_expl_ge(L1, L2).

pol_better_fit(_, _, q(0,0)).

pol_better_fit(q(Z2,N2), q(Z1,N1), q(Z3,N3)) :-
	M1 is -Z1*N2*N3,
	M2 is Z2*N1*N3 - M1,
	M3 is Z3*N1*N2 - M1,
	pol_abs(M2, A2),
	pol_abs(M3, A3),
	A2 < A3.

pol_abs(M, M) :-
	M >= 0,
	!.

pol_abs(M, A) :-
	A is -M.

pol_conc3((LN,LV), (q(0,0),[]), (LV,RV), (LN,RV)) :-
	!.

pol_conc3((LN,[K|LV]), K, (LV,RV), (LN,RV)).


pol_change(NK, (q(ZP,NP),ExplP), VK, (q(ZN,NN),ExplN), Restp, Cond) :-
	pol_expl_dif(ExplP, ExplN, 0, Expl_dif),
	pol_two_power(Expl_dif, 1, Exp),
	NNN is NN*Exp,
	Diff is ZP*NNN + ZN*NP,
	pol_change0(Diff, NK, (q(ZP,NP),ExplP), VK,
                    (q(ZN,NN),ExplN), Restp, NNN, Exp, Cond).


pol_change0(0, NK, _, (LV,NK), _, Restp, _, _, Cond) :-
	!,
	pol_cond_positive(Restp, LV, Cond).

pol_change0(Diff, NK, (q(ZP,NP),ExplP), (LV,[(q(ErgZ,ErgN),ExplP)|NK]),
	    (q(ZN,_NN),_ExplN), Restp, NNN, _, Cond) :-
	Diff > 0,
	!,
	pol_rcancel(ZN, NNN, CZN, CNN),
	pol_rplus(q(ZP,NP), q(CZN,CNN), q(ErgZ,ErgN)),
	!,
	pol_cond_positive(Restp, LV, Cond).

pol_change0(_Diff, NK, (q(ZP,NP),_ExplP), (LV,NK),
            (q(ZN,NN),ExplN), Restp, _, Exp, Cond) :-
	NZP is ZP*Exp,
	pol_rcancel(NZP, NP, CZP, CNP),
	pol_rplus(q(ZN,NN), q(CZP,CNP), q(ErgZ,ErgN)),
	!,
	pol_cond_positive([(q(ErgZ,ErgN),ExplN)|Restp], LV, Cond).


pol_expl_dif([], [], Expl_dif, Expl_dif).

pol_expl_dif([E1|L1], [E2|L2], Zwi_dif, Expl_dif) :-
	Dif is Zwi_dif + E1-E2,
	pol_expl_dif(L1, L2, Dif, Expl_dif).

pol_two_power(0, Exp, Exp) :-
	!.

pol_two_power(N, Zw, Exp) :-
	NZw is Zw*2,
	NN is N - 1,
	pol_two_power(NN, NZw, Exp).
