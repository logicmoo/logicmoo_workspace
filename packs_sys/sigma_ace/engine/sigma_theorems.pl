:-include('sigma_header.pl').

% ============================================
% Theorem Checker
% ============================================

% Free Formulas are never 
isTheorem(T,V):-sigma_notrace(isTheoremTrue(T,V)).
isTheoremTrue(V,FreeV):-isSlot(V),!,fail.
isTheoremTrue(V,FreeV):-isReducedToTrue(V),!.

% Kalish and Monash (Declared Theorem 17)
isTheoremTrue(=>(PP,=>(not(P),Q)),FreeV):- ==(P,PP),!.

% Kalish and Monash (Declared Theorem 17)
isTheoremTrue(=>(PP,=>(not(P),Q)),FreeV):-inferLogicallyEquiv(P,PP),!.

% 'Kalish & Montague, Theorem 23'
isTheoremTrue(=>(=>(=>(D,_),D1),D2) ,FreeV):-inferLogicallyEquiv(D,D1),inferLogicallyEquiv(D1,D2),!.

% 'Kalish & Montague, Theorem 18'
isTheoremTrue(=>(not(D),=>(D1,D2)),FreeV):-inferLogicallyEquiv(D,D1),inferLogicallyEquiv(D1,D2),!.


 
% 'Kalish & Montague, Theorem 2'
isTheoremTrue(=>(D,=>(F,D1)),FreeV):-inferLogicallyEquiv(D,D1),!.


% 'Kalish & Montague, Theorem 21'
isTheoremTrue(=>(not(=>(D,F)),D1),FreeV):-inferLogicallyEquiv(D,D1),!.

% 'Kalish & Montague, Theorem 22' (=> (not (=> (isa ?X Dog) (isa ?X Fish))) (not (isa ?X Fish))) ) 
isTheoremTrue(=>(not(=>(D,F)),not(F1)),FreeV):-inferLogicallyEquiv(F,F1),!.

% 'Kalish & Montague, Theorem 241' 
% (forall ?X (=> (F ?X) (or(G ?X) (H ?X)) ) )  => isTheoremTrue(or(forall(X,=>(F,G)),exists(Y,and(F1,H))),FreeV):-inferLogicallyEquiv(F,F1),inferLogicallyEquiv(X,Y),!.        

% All X F exisits X F'Kalish & Montague, Theorem 22' (=> (not (=> (isa ?X Dog) (isa ?X Fish))) (not (isa ?X Fish))) ) 
isTheoremTrue(=>(F,exists(X,F1)),FreeV):-inferLogicallyEquiv(F,F1),!.

% All X F exisits X F'Kalish & Montague, Theorem 22' (=> (not (=> (isa ?X Dog) (isa ?X Fish))) (not (isa ?X Fish))) ) 
isTheoremTrue(=>(forall(Y,F),exists(X,F1)),FreeV):-inferLogicallyEquiv(F,F1),inferLogicallyEquiv(X,Y),!.



%(forall ?X (exists ?Y (<=> (F3 ?X) (not (F3 ?Y))))) 


% Le Logica De Morgan 
isTheoremTrue(<=>(not(and(P1,PP1)),or(not(P2),not(PP2))),FreeV):-
         inferLogicallyEquiv(P1,P2),
         inferLogicallyEquiv(PP1,PP2),!.

% Le Logica De Morgan Reversed
isTheoremTrue(<=>(or(not(P2),not(PP2)),not(and(P1,PP1))),FreeV):-
         inferLogicallyEquiv(P1,P2),
         inferLogicallyEquiv(PP1,PP2),!.

% Kalish and Monash (Declared Theorem)
isTheoremTrue(P,FreeV):-
         km_check(P,FreeV),!.

% Tautology #1
isTheoremTrue(=>(P1,P2),FreeV):-
         inferLogicallyEquiv(P1,P2),!.

isTheoremTrue(=>(P1,not(P2)),FreeV):-
         inferLogicallyEquiv(P1,P2),!.

% Tautology #2
isTheoremTrue(<=>(A,B),FreeV):-
         inferLogicallyEquiv(A,B),!.

% Reduces to True
isTheoremTrue(P,FreeV):-
         inferLogicallyEquiv(P,true),!.

% Kalish and Monash (Declared Theorem)
isTheoremTrue(P,FreeV):-
         getLogicalReduction(P,Q),
         km_check(Q,FreeV),!.


% Antecedent is a Theorem
isTheoremTrue(=>(P1,_),FreeV):-
         isTheoremTrue(P1,FreeV),!.

isTheoremTrue(forall(_,P),FreeV):-!,
         isTheoremTrue(P,FreeV),!.

isTheoremTrue(exists(_,P),FreeV):- !,
         isTheoremTrue(P,FreeV),!.
isTheoremTrue(query(P),FreeV):- !,
         isTheoremTrue(P,FreeV),!.
isTheoremTrue(queryyn(P),FreeV):- !,
         isTheoremTrue(P,FreeV),!.

 

% ============================================
% Specific Checks for theorms from Ned to Add Kalish and Monash
% ============================================

km_check(V,FreeV):-isSlot(V),!,fail.

% P => (~P => Q)  % KM 17
km_check(=>(PP,=>(not(P),Q)),FreeV):-!,inferLogicallyEquiv(P,PP).

% ~P v P
km_check(or(not(P),PP),FreeV):-!,inferLogicallyEquiv(P,PP).

% P v ~P
km_check(or(PP,not(P)),FreeV):-!,inferLogicallyEquiv(P,PP).

% ~P => P
km_check(=>(not(P),PP),FreeV):-inferLogicallyEquiv(P,PP),!.

% P => ~P
km_check(=>(PP,not(P)),FreeV):-!,inferLogicallyEquiv(P,PP).




% P v true
km_check(or(_,true),FreeV):-!.

% true v P
km_check(or(true,_),FreeV):-!.

% 'Kalish & Montague, Theorem 17'
km_check(=>(P,=>(not(PP),_)),FreeV):-trace,inferLogicallyEquiv(P,PP),!.
