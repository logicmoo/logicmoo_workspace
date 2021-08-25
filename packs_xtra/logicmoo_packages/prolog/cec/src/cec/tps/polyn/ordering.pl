/*
 *	file:		ordering.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates for ordering of equations.
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

% pol_ordering(L,R,NL,NR) succeeds if the equation L == R can be
% ordered in either direction. NL and NR are the left and right
% sides of the resulting rewriting rule.
% pol_ordering(L,R) succeeds, if the equation L == R can be ordered
% as L --> R using the given polynomial interpretation.
%
% pol_ordering(Comment,L,R,NL,NR), pol_ordering(Comment,L,R):
% same as above, but the string "Comment" and the terms L and R
% are printed before the user is asked to enter an interpretation
% for a new operator.



pol_ordering(L,R,NL,NR) :-
	pol_retractall(pol_comment(_,_,_)),
	pol_ordering0(L,R,NL,NR).


pol_ordering(Comment,L,R,NL,NR) :-
	pol_retractall(pol_comment(_,_,_)),
	asserta(pol_comment(Comment,L,R)),
	pol_ordering0(L,R,NL,NR).

pol_ordering(L,R) :-
	pol_retractall(pol_comment(_,_,_)),
	pol_ordering0(L,R).


pol_ordering(Comment,L,R) :-
	pol_retractall(pol_comment(_,_,_)),
	asserta(pol_comment(Comment,L,R)),
	pol_ordering0(L,R).



pol_ordering0(L,R,NL,NR) :-
	pol_getvars((L,R),[],VarList),
	pol_interpret_term(L, VarList, LI),
	pol_interpret_term(R, VarList, RI),
	pol_ordering1(L,R,LI,RI,NL,NR).


pol_ordering0(L,R) :-
	pol_getvars((L,R),[],VarList),
	pol_interpret_term(L, VarList, LI),
	pol_interpret_term(R, VarList, RI),
	pol_tuplegreater(LI, RI).



pol_ordering1(L,R,LI,RI,L,R) :-
	pol_tuplegreater(LI, RI),
	!.

pol_ordering1(L,R,LI,RI,R,L) :-
	pol_tuplegreater(RI, LI).




% pol_tuplegreater(PolList1, PolList2) checks, whether the polynomial
% list PolList1 is lexicographically greater than PolList2.


pol_tuplegreater([Pol|PList1], [Pol|PList2]) :-
	!,
	pol_tuplegreater(PList1, PList2).

pol_tuplegreater([Pol1|_PList1], [Pol2|_PList2]) :-
	Minus1 is -1,
	pol_rpmult(q(Minus1,1),Pol2,NegPol),
	pol_pplus(Pol1, NegPol, ResPol),
	pol_positive(ResPol,[]).




% pol_positive(Rest,PosList):
% tries to prove the positivity of a polynomial P = PosList + Rest
% by compensating every negative monomial in P with one or more
% suitable positive ones.
%
% When pol_positive is called first, P is equal to Rest whereas
% PosList is set to [] (i.e. 0).
% If Rest begins with a positive monomial, it is pushed onto PosList
% and pol_positive is called recursively.
% If Rest begins with a negative monomial, pol_positive tries to
% eliminate it with positive monomials from PosList and fails
% if there are no candidates.
% If Rest is empty (i.e. 0), pol_positive succeeds provided that
% PosList contains at least one monomial (i.e. P is strictly
% greater than 0) and fails otherwise.
%
% Some auxiliary predicates of pol_positive use the so-called
% "difference list" technique (cf. Lloyd, Foundations of logic
% programming). This applies to pol_choose (3rd, 5th parameter),
% pol_change (3rd parameter) and pol_change0 (4th parameter).
% pol_conc3 is used to concatenate a difference list, an element,
% and a second difference list and returns a difference list.
% (If "element" is the dummy monomial (q(0,0),[]), it is eliminated.)



%-------Tracing---------------

pol_positive(Rest, PosList) :-
	pol_trace,
	write('Rest is '),
	pol_write_polynomial(Rest),
	nl,
	write('PosList is '),
	pol_write_polynomial(PosList),
	nl,
	fail.

%-----------------------------

pol_positive([], []) :- !, fail.

pol_positive([] , _).

pol_positive([(q(Z,N),Expl)|Restp],L) :-
	Z > 0,
	!,
	pol_positive(Restp,[(q(Z,N),Expl)|L]).

pol_positive([(q(Z,N),Expl)|Restp],L) :-
	pol_choose((q(Z,N),Expl),L,(X,X),(q(0,0),[]),(Y,Y),Restp).


pol_choose(_,[],_,(q(0,0),[]),_,_) :-
	!,
	fail.

pol_choose(Neg,[],(LN,[]),K,VK,Restp) :-
	!,
	pol_change(LN,K,VK,Neg,Restp).

pol_choose((q(MinusZ,N),Expl1),[(q(Z,N),Expl2)|L],NK,K,VK,Restp) :-
	MinusZ is -Z,
	pol_expl_ge(Expl2,Expl1),
	!,
	pol_conc3(VK,K,NK,VNK),
	pol_change(L,(q(Z,N),Expl2),VNK,(q(MinusZ,N),Expl1),Restp).

pol_choose((q(Z1,N1),Expl1),[(q(Z2,N2),Expl2)|L],NK,(q(Z3,N3),Expl3),VK,Restp) :-
	pol_expl_ge(Expl2,Expl1),
	pol_better_fit(q(Z2,N2),q(Z1,N1),q(Z3,N3)),
	!,
	pol_conc3(VK,(q(Z3,N3),Expl3),NK,VNK),
	pol_choose((q(Z1,N1),Expl1),L,(X,X),(q(Z2,N2),Expl2),VNK,Restp).

pol_choose(Neg,[Lhead|L],(LN,[Lhead|RNN]),K,VK,Restp) :-
	!,
	pol_choose(Neg,L,(LN,RNN),K,VK,Restp).

pol_expl_ge([],[]).

pol_expl_ge([E1|L1],[E2|L2]) :-
	E1 >= E2,
	!,
	pol_expl_ge(L1,L2).

pol_better_fit(_,_,q(0,0)).

pol_better_fit(q(Z2,N2),q(Z1,N1),q(Z3,N3)) :-
	M1 is -Z1*N2*N3,
	M2 is Z2*N1*N3 - M1,
	M3 is Z3*N1*N2 - M1,
	pol_abs(M2,A2),
	pol_abs(M3,A3),
	A2 < A3.

pol_abs(M,M) :-
	M >= 0,
	!.

pol_abs(M,A) :-
	A is -M.

pol_conc3((LN,LV),(q(0,0),[]),(LV,RV),(LN,RV)) :-
	!.

pol_conc3((LN,[K|LV]),K,(LV,RV),(LN,RV)).


pol_change(NK,(q(ZP,NP),ExplP),VK,(q(ZN,NN),ExplN),Restp) :-
	pol_expl_dif(ExplP,ExplN,0,Expl_dif),
	pol_two_power(Expl_dif,1,Exp),
	NNN is NN*Exp,
	Diff is ZP*NNN + ZN*NP,
	pol_change0(Diff,NK,(q(ZP,NP),ExplP),VK,(q(ZN,NN),ExplN),Restp,NNN,Exp).


pol_change0(0,NK,_,(LV,NK),_,Restp,_,_) :-
	!,
	pol_positive(Restp,LV).

pol_change0(Diff,NK,(q(ZP,NP),ExplP),(LV,[(q(ErgZ,ErgN),ExplP)|NK]),
	    (q(ZN,_NN),_ExplN),Restp,NNN,_) :-
	Diff > 0,
	!,
	pol_rcancel(ZN,NNN,CZN,CNN),
	pol_rplus(q(ZP,NP),q(CZN,CNN),q(ErgZ,ErgN)),
	!,
	pol_positive(Restp,LV).

pol_change0(_Diff,NK,(q(ZP,NP),_ExplP),(LV,NK),(q(ZN,NN),ExplN),Restp,_,Exp) :-
	NZP is ZP*Exp,
	pol_rcancel(NZP,NP,CZP,CNP),
	pol_rplus(q(ZN,NN),q(CZP,CNP),q(ErgZ,ErgN)),
	!,
	pol_positive([(q(ErgZ,ErgN),ExplN)|Restp],LV).


pol_expl_dif([],[],Expl_dif,Expl_dif).

pol_expl_dif([E1|L1],[E2|L2],Zwi_dif,Expl_dif) :-
	Dif is Zwi_dif + E1-E2,
	pol_expl_dif(L1,L2,Dif,Expl_dif).

pol_two_power(0,Exp,Exp) :-
	!.

pol_two_power(N,Zw,Exp) :-
	NZw is Zw*2,
	NN is N - 1,
	pol_two_power(NN,NZw,Exp).




