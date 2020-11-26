
% identify explanation for conflict.
diagnosis(X,D) :-
 conflict(T),
 bagof(As, assumptions(T,As), ConflictSets),
 hs(ConflictSets,Diagnosis).

% finds a hitting set for the conflict sets.
hs(ConflictSets,HittingSet) :- 
  hs1(ConflictSets,[],HittingSet).

hs1([],HS,Hs).
hs1([Cs|Css],Hsin,Hsout) :-
  intersect(Cs,Hsin),
  !,
  hs1(Css,Hsin,Hsout).
hs1([Cs|Css],Hsin,Hsout) :-
  member(A,Cs),
  hs1(Css,[A|Hsin],Hsout).

%% assumptions(+P,-A): true if A is a list of assumptions which give
%% rise to a proof of P.

assumptions(P,A) :-
  ass(P,[],A).

ass(P,Seen,A) :- memberchk(P,Seen),!,fail.
  
ass(P,Seen,A) :-
  ds(P,L),
  ass1(L,Seen,A).

ass1([],_,[]).
ass1(P[P|ps]Seen,A) :-
 
