/*
 *	file:		arithmetic.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains arithmetic operations on polynomials.
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

% pol_papply(P, PList, Result):
% application of a polynomial P with arity N to a list PList
% of N polynomials where the I-th variable of P corresponds
% to the I-th element of PList.

pol_papply([], _, []) :- !.
pol_papply([Monomial|Rest], PolynomialList, ResPolynomial) :-
	pol_mapply(Monomial, PolynomialList, Res1),
	pol_papply(Rest, PolynomialList, Res2),
	pol_pplus(Res1, Res2, ResPolynomial).


% pol_mapply(M, PList, Result):
% application of a monomial M with arity N to a list PList
% of N polynomials.

pol_mapply((C,L), PolynomialList, ResPolynomial) :-
	pol_lapply(L, PolynomialList, Res1),
	pol_rpmult(C, Res1, ResPolynomial).


% pol_lapply(L, PList, Result):
% application of a list L of N exponents to a list PList
% of N polynomials.

pol_lapply([E], [Polynomial], ResPolynomial) :-
	!,
	pol_ppower(Polynomial, E, ResPolynomial).
pol_lapply([E|ERest], [Polynomial|PRest], ResPolynomial) :-
	pol_ppower(Polynomial, E, Res1),
	pol_lapply(ERest, PRest, Res2),
	pol_pmult(Res1, Res2, ResPolynomial).


% pol_rpmult(R, P, Result):
% multiplication of a rational number R with a polynomial P.

pol_rpmult(_, [], []).
pol_rpmult(q(0,_), _, []) :- !.
pol_rpmult(C, [(C0,L)|Rest], [(C1,L)|NewRest]) :-
	pol_rmult(C, C0, C1),
	pol_rpmult(C, Rest, NewRest).



% arithmetic operations on the set of monomials

% pol_mgreater(Monomial1,Monomial2) is provable if Monomial1 is greater
% than Monomial2

pol_mgreater((_,A),(_,B)) :-
	pol_lgreater(A,B).


pol_lgreater([A|_R],[B|_L]) :-
	A > B,
	!.
pol_lgreater([A|R],[B|L]) :-
	A = B,
	!,
	pol_lgreater(R,L).


% pol_pplus(Polynomial1,Polynomial2,ResultPolynomial)
% succeeds if ResultPolynomial is the sum of Polynomial1 and
% Polynomial2.

pol_pplus([],P,P) :- !.
pol_pplus(P,[],P) :- !.
pol_pplus([(K1,M)|R1],[(K2,M)|R2],R) :-
	pol_rplus(K1,K2,q(0,_)),
	!,
	pol_pplus(R1,R2,R).
pol_pplus([(K1,M)|R1],[(K2,M)|R2],[(K,M)|R]) :- 
	!,
	pol_rplus(K1,K2,K),
	pol_pplus(R1,R2,R).
pol_pplus([(K1,M1)|R1],[(K2,M2)|R2],[(K1,M1)|R]) :-
	pol_lgreater(M1,M2),
	!,
	pol_pplus(R1,[(K2,M2)|R2],R).
pol_pplus([(K1,M1)|R1],[(K2,M2)|R2],[(K2,M2)|R]) :-
	pol_pplus([(K1,M1)|R1],R2,R).


% pol_lplus(List1,List2,Listresult) realizes the componentwise
% addition of two lists

pol_lplus([],[],[]).
pol_lplus([A1|R1],[A2|R2],[A|R]) :-
	A is A1+A2,
	pol_lplus(R1,R2,R).


% pol_mmult(K,M,Polynomial,ResultPolynomial) multiplies the monomial
% (K,M) by Polynomial and delivers ResultPolynomial as result

pol_mmult(_K,_M,[],[]).
pol_mmult(K1,M1,[(K2,M2)|R2],[(K,M)|R]) :-
	pol_rmult(K1,K2,K),
	pol_lplus(M1,M2,M),
	pol_mmult(K1,M1,R2,R).


% pol_pmult(Poly1,Poly2,Polyresult) multiplies the polynomial Poly1
% by the polynomial Poly2 and delivers the polynomial Polyresult

pol_pmult([],_P,[]).
pol_pmult([(K,M)|R],P2,P) :-
	pol_mmult(K,M,P2,IP1),
	pol_pmult(R,P2,IP2),
	pol_pplus(IP1,IP2,P).


% pol_ppower(Poly,Exp,Polyresult)
% Polyresult is the result of the polynomial Poly raised to the
% Exp-th power

pol_ppower([(_K,M)|_R],0,[(q(1,1),Null)]) :-
	!,
	pol_make_zero(M,Null).
pol_ppower(Poly,1,Poly) :- !.
pol_ppower(Poly,2,Quadpoly) :-
	!, pol_pmult(Poly,Poly,Quadpoly).
pol_ppower(Poly,N,Polyerg) :-
	0 is N mod 2,
	!,
	M is N // 2,
	pol_ppower(Poly,M,Polyzwi),
	pol_pmult(Polyzwi,Polyzwi,Polyerg).
pol_ppower(Poly,N,Polyerg) :-
	M is N-1,
	pol_ppower(Poly,M,Polyzwi),
	pol_pmult(Polyzwi,Poly,Polyerg).


% pol_make_zero(List1,Zerolist)
% converts all elements of the list List1 into zero

pol_make_zero([],[]).
pol_make_zero([_A|R],[0|NR]) :-
	pol_make_zero(R,NR).


% pol_rplus(X1,X2,Result) succeeds, if Result is the sum of the
% two rational numbers X1 and X2

pol_rplus(q(X,Y),q(Z,Y),q(A,B)) :-
	!,
	IA is X+Z,
	pol_rcancel(IA,Y,A,B).
pol_rplus(q(X,Y),q(U,V),q(A,Y)) :-
	Y > V,!,
	D is Y//V,
	A is D*U+X.
pol_rplus(q(X,Y),q(U,V),q(A,V)) :-
	D is V//Y,
	A is D*X+U.


% pol_rcancel(N1,N2,N3,N4) simplifies the fraction N1/N2 into N3/N4

pol_rcancel(X,Y,A,B) :-
	Y > 1,
	0 is X mod 2,
	!,
	IA is X//2,
	IB is Y//2,
	pol_rcancel(IA,IB,A,B).
pol_rcancel(A,B,A,B).


% pol_rmult(C1,C2,C3) multiplies C1 by C2 and delivers C3 as result

pol_rmult(q(X,Y),q(U,V),q(A,B)) :-
	Z is X*U,
	N is Y*V,
	pol_rcancel(Z,N,A,B).


% pol_rpower(Rat,Exp,Res) succeeds if Res is the result of the
% rational number Rat raised to the Exp-th power

pol_rpower(q(_X,_Y),0,q(1,1)) :- !.
pol_rpower(q(X,Y),1,q(X,Y)) :- !.
pol_rpower(q(X,Y),2,q(A,B)) :-
	!, pol_rmult(q(X,Y),q(X,Y),q(A,B)).
pol_rpower(q(X,Y),N,q(A,B)) :-
	0 is N mod 2,
	!,
	M is N // 2,
	pol_rpower(q(X,Y),M,q(IA,IB)),
	pol_rmult(q(IA,IB),q(IA,IB),q(A,B)).
pol_rpower(q(X,Y),N,q(A,B)) :-
	M is N - 1,
	pol_rpower(q(X,Y),M,q(IA,IB)),
	pol_rmult(q(IA,IB),q(X,Y),q(A,B)).
