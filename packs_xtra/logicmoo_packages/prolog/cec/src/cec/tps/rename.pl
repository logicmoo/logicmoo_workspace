/*
 *	file:		rename.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains ..
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

%       tps_computeDirectAssociation(Pairs,DPairs)
%
%
% The following predicate has been removed, because the view of renaming
% as a simultaneous substitution seems to be more natural.
%
%       Indirect associations like (e.g. [(a,b),(b,c),...]) are evaluated.
%
%
%tps_computeDirectAssociation(Pairs,DPairs):-
%        (setof(DP,tps_isADirectPairIn(DP,Pairs),DPairs) ; DPairs = []),!.
%
%tps_isADirectPairIn((A,A),Pairs):-
%        member((A,A),Pairs).
%tps_isADirectPairIn((A,B),Pairs):-
%        member((A,B),Pairs),
%        not(member((B,_),Pairs)).
%tps_isADirectPairIn((A,B),Pairs):-
%        member((A,X),Pairs),
%        X \== A,
%        tps_isADirectPairIn((X,B),Pairs).


%       tps_facModAssoc(->AssocList,<-Fac)
%
%       computes the factorization of the set of all operators
%       modulo the association. Fac is of the form [[a,b,c],[g,h],...]
%

tps_facModAssoc(AssocList,Fac):-
	tps_computeFacAssoc(AssocList,FacAssocList),
	tps_isFacOf(FacAssocList,Fac), !.

tps_computeFacAssoc(AssocList,FacAssocList):-
	tps_FAL(AssocList,[],FacAssocList).

tps_FAL([],FAL,FAL):- !.
tps_FAL([Assoc|AL],FAL1,FAL2):-
	tps_FA(Assoc,FAL1,FALi),
	tps_FAL(AL,FALi,FAL2),
	!.

tps_FA((A,B),FAL1,FAL2):-
	chooseElem(FAL1,L,(F,B),R),
	chooseElem(FAL2,L,([A|F],B),R),
	!.
tps_FA((A,B),FAL,[([A],B)|FAL]).

tps_isFacOf([],[]):- !.
tps_isFacOf([(A,_B)|FacAssocList],[A|Fac]):-
	tps_isFacOf(FacAssocList,Fac).

tps_extractNotOrdOperators(_Ops,[],[]) :- !.
tps_extractNotOrdOperators(Ops,[(A,B)|Pairs],[(A,B)|Pairs2]):-
	member(A,Ops),
	tps_extractNotOrdOperators(Ops,Pairs,Pairs2),
	!.
tps_extractNotOrdOperators(Ops,[(_A,_B)|Pairs],Pairs2):-
	tps_extractNotOrdOperators(Ops,Pairs,Pairs2),
	!.

tps_CAlist([],[],[]):- !.

tps_CAlist([Op|Ops],Alist,[(Op,NewOp)|CAlist]):-
	chooseElem(Alist,L,(Op,NewOp),R),
	!,
	append(L,R,Alist2),
	tps_CAlist(Ops,Alist2,CAlist),!.

tps_CAlist([Op|Ops],Alist,[(Op,Op)|CAlist]):-
	tps_CAlist(Ops,Alist,CAlist),!.

