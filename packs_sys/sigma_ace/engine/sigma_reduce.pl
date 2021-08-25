%:-include('sigma_header.pl').

% =========================================================
% Logicaly Equivalent
% =========================================================

inferLogicallyEquiv(P,PP):-P==PP,!.
inferLogicallyEquiv(P,PP):-
         getLogicalReduction(P,PR),
         getLogicalReduction(PP,PPR),!,
         PR==PPR.

% =========================================================
% Logicaly Reduce
% =========================================================

%Vars are not Reduced
getLogicalReduction(A,A):-isSlot(A),!.

% Not Not A
getLogicalReduction(not(not(A)),R):-!,getLogicalReduction(A,R).

% True => True
%getLogicalReduction(T,true):-isReducedToTrue(T),!.

% False => False
%getLogicalReduction(T,false):-isReducedToFalse(T),!.

% ~True => False
getLogicalReduction(not(T),false):-isReducedToTrue(T),!.

% ~False => True
getLogicalReduction(not(F),true):-isReducedToTrue(T),!.

% Reserved Fmls
getLogicalReduction(FML,FML):-member(FML,['AssignmentFn'(_,_),'zzskFn'(_,_),'zzskFn'(_),'equal'(_,_),'u'(_,_,_)]),!.


% Quantifiers using True  => True
getLogicalReduction(forall(V,T),true):-isReducedToTrue(T),!.
getLogicalReduction(some(V,T),true):-isReducedToTrue(T),!.
getLogicalReduction(exists(V,T),true):-isReducedToTrue(T),!.

% True -> Fml  => True
getLogicalReduction(=>(T,A),R):-isReducedToTrue(T),!,getLogicalReduction(A,R).

% Quantifiers Fml  => Fml
getLogicalReduction(forall(V,A),forall(V,R)):-!,getLogicalReduction(A,R).
getLogicalReduction(exists(V,A),exists(V,R)):-!,getLogicalReduction(A,R).
getLogicalReduction(some(V,A),some(V,R)):-!,getLogicalReduction(A,R).

% True ^ True => True
getLogicalReduction(and(T1,T2),true):-isReducedToTrue(T1),isReducedToTrue(T2),!.

% True ^ Fml => Fml
getLogicalReduction(and(T,Fml),Fml):-isReducedToTrue(T),!.

% Fml ^ True => Fml
getLogicalReduction(and(Fml,T),Fml):-isReducedToTrue(T),!.

% Fml v True => True
getLogicalReduction(or(Fml,T),true):-isReducedToTrue(T),!.

% True v Fml => True
getLogicalReduction(or(Fml,T),true):-isReducedToTrue(T),!.

/*
% Fml <=> True => True
getLogicalReduction(<=>(Fml,T),true):-isReducedToTrue(T),!.

% True <=> Fml => True
getLogicalReduction(<=>(Fml,T),true):-isReducedToTrue(T),!.
*/

% Fml v Fml => Fml
getLogicalReduction(or(Fml1,Fml2),Fml1):-Fml1==Fml2,!.

% Fml ^ ~Fml => False
getLogicalReduction(and(Fml1,not(Fml2)),Fml1):-Fml1==Fml2,!.

% ~Fml ^ Fml => False
getLogicalReduction(and(not(Fml1),Fml2),Fml1):-Fml1==Fml2,!.

% False ^ False => False
getLogicalReduction(and(F1,F2),false):-isReducedToFalse(F1),isReducedToFalse(F2),!,!.

% False ^ Fml => False
getLogicalReduction(and(F,Fml),false):-isReducedToFalse(F),!.

% Fml ^ False => False
getLogicalReduction(and(Fml,F),false):-isReducedToFalse(F),!.

% Fml v False => Fml
getLogicalReduction(or(Fml,F),Fml):-isReducedToFalse(F),!.

% False v Fml => Fml
getLogicalReduction(or(Fml,F),Fml):-isReducedToFalse(F),!.


% Higher Level
getLogicalReduction(known(T)):-isReducedToTrue(T).
getLogicalReduction(consistent(T)):-isReducedToTrue(T).
getLogicalReduction(after(T)):-isReducedToTrue(T).
getLogicalReduction(before(T)):-isReducedToTrue(T).

% Do not reduce Lists
getLogicalReduction(LIST,LIST) :-is_list(LIST),!. %,getLogicalReduction_l(LIST,LISTO).

% Nothing to Do
getLogicalReduction(A,B):-!,unify_with_occurs_check(A,B).


isReducedToTrue(V):-isSlot(V),!,fail.
isReducedToTrue(true).
isReducedToTrue(T):-getLogicalReduction(T,true).

isReducedToFalse(V):-isSlot(V),!,fail.
isReducedToFalse(false).
isReducedToFalse(F):-getLogicalReduction(F,false).



getLogicalReduction_l(V,V):-var(V),!.
getLogicalReduction_l([],[]).
getLogicalReduction_l([H|T],[HO|TO]):-!,
         getLogicalReduction(H,HO),
         getLogicalReduction_l(T,TO).




