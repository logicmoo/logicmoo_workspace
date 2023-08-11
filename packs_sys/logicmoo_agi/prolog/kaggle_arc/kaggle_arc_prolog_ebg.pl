%    /*<title->
%      Prolog Explanation-Based Reasoning: Cups Example #1
%    </title->*/
 % PROLOG EBG: THE CUP EXAMPLE


/* Domain theory */

cup(X) :- liftable(X), stable(X), open_vessel(X).
liftable(X) :- light(X), part(X, handle).
stable(X) :- part(X, bottom), flat(bottom).
open_vessel(X) :- part(X, concavity), points_up(concavity).
light(X) :- small(X).
light(X) :- made_of(X,feathers).

/* Training example */

small(obj1).
part(obj1,handle).
part(obj1, bottom).
flat(bottom).
part(obj1, concavity).
points_up(concavity).

/* operationality criteria */

operational(small(X)).
operational(part(X, Y)).
operational(flat(X)).   
operational(points_up(X)).  </pre>

   /*<title->
      Prolog Explanation-Based Reasoning: Cups Example #2
    </title->*/

cup(X) :- liftable(X), holds_liquid(X).

holds_liquid(Z) :- part(Z, W), concave(W), points_up(W).

liftable(Y) :- light(Y), part(Y, handle).

light(A):- small(A).light(A):- made_of(A, feathers). 

%cup(obj1).
small(obj1).
part(obj1,handle).
owns(bob, obj1).
part(obj1, bottom).
part(obj1, bowl).
points_up(bowl).
concave(bowl).
color(obj1, red).


operational(small(_)).
operational(part(_,_)).
operational(owns(_, _)).
operational(points_up(_)).
operational(concave(_)).
operational(color(_, _)).


    /*<title->
      Prolog Explanation-Based Reasoning: Goals
    </title->*/
% PROLOG EBG

ebg_1(Goal, Gen_goal, (Gen_goal :- Premise)) :- 
	prolog_ebg_1(Goal, Gen_goal, _, Gen_proof),
	extract_support_1(Gen_proof, Premise).

prolog_ebg_1(A, GenA, A, GenA) :- my_ebg_clause(A, true).

prolog_ebg_1((A, B), (GenA,GenB), (AProof, BProof), (GenAProof,GenBProof)) :-
   !,prolog_ebg_1(A, GenA, AProof, GenAProof),
   prolog_ebg_1(B, GenB, BProof,GenBProof).

prolog_ebg_1(A, GenA, (A :- Proof), (GenA :- GenProof)) :-
   my_ebg_clause(GenA, GenB), 
   duplicate_v1((GenA:-GenB), (A:-B)),
   prolog_ebg_1(B, GenB, Proof, GenProof).

my_ebg_clause(A, B) :- ebg_clause(A, [true]), B = true.
my_ebg_clause(A, B) :- var(B),ebg_clause(A, List_B), list2goals(List_B, B).

% The purpose of copy2 is to get a new copy of an expression 
% with all new variables.

duplicate_v1(Old, New) :- assert('$marker'(Old)), 
                      retract('$marker'(New)).

% Extract support walks down a proof tree and returns the sequence of the
% highest level operational nodes, as defined by the predicate "operational"

extract_support_1(Proof, Proof) :- operational(Proof).
extract_support_1((A :- _), A) :- operational(A).
extract_support_1((AProof, BProof), (A,B)) :-
   extract_support_1(AProof, A),
   extract_support_1(BProof, B).
extract_support_1((_ :- Proof), B) :- extract_support_1(Proof, B).

% A neat exercise would be to build a predicate, make_rule, that calls prolog_ebg to
% generate the generalized tree and extract_support_1 to get the operational nodes and
% constructs a rule of the form:
%     top_level_goal :- sequence of operational nodes

    /*<title->
      Prolog Explanation-Based Reasoning
    </title->*/

% PROLOG EBG

ebg_2(Goal, Gen_goal, (Gen_goal :- Premise)) :- 
	prolog_ebg_2(Goal, Gen_goal, _, Gen_proof),
	extract_support_v2(Gen_proof, Premise).

prolog_ebg_2(A, GenA, A, GenA) :- not(list(A)), ebg_clause(A, [true]).

prolog_ebg_2([],[],[],[]).

prolog_ebg_2([A|B], [GenA|GenB], [AProof| BProof], [GenAProof|GenBProof]) :-
   prolog_ebg_2(A, GenA, AProof, GenAProof),
   prolog_ebg_2(B, GenB, BProof,GenBProof).

prolog_ebg_2(A, GenA, [A :- Proof], [GenA :- GenProof]) :-
   ebg_clause(GenA, GenB), 
   duplicate_2((GenA:-GenB), (A:-B)),
   prolog_ebg_2(B, GenB, Proof, GenProof).

% The purpose of copy2 is to get a new copy of an expression 
% with all new variables.
duplicate_2(Old, New) :- assert('$marker'(Old)), retract('$marker'(New)).

% Extract support_2 walks down a proof tree and returns the sequence of the
% highest level operational nodes, as defined by the predicate "operational"

extract_support_v2(Proof, Proof) :- operational(Proof).
extract_support_v2([Proof],(R)):-extract_support_v2(Proof,R).
extract_support_v2([AProof| BProof], (A,B)) :-
   extract_support_v2(AProof, A),
   extract_support_v2(BProof, B).
extract_support_v2([A :- _], A) :- operational(A).
extract_support_v2([_ :- Proof], B) :- extract_support_v2(Proof, B).

% A neat exercise would be to build a predicate, make_rule, that calls prolog_ebg to
% generate the generalized tree and extract_support_2 to get the operational nodes and
% constructs a rule of the form:
%     top_level_goal :- sequence of operational nodes

 

    /*<title->
      Prolog Explanation-Based Reasoning: Sample Run
    </title->*/

% trace of various calls to prolog ebg using the cup example.
% a top level execution predicate would compine prolog ebg and extract rule 
% plus do some syntactic stuff to build a new rule

/*example

?- prolog_ebg(cup(obj1), cup(X), P, GenP).
    X = _0,
    P = [ (cup(obj1) :- 
               [[ (liftable(obj1) :- 
                       [[ (light(obj1) :- 
                               [small(obj1)])],part(obj1,handle)])],
                [ (stable(obj1) :- 
                       [part(obj1,bottom),flat(bottom)])],
                [ (open_vessel(obj1) :- 
                       [part(obj1,concavity),points_up(concavity)])]])],
    GenP = [ (cup(_0) :- 
                [[ (liftable(_0) :- 
                       [[ (light(_0) :- 
                                  [small(_0)])],
                           part(_0,handle)])],
                   [ (stable(_0) :- 
                          [part(_0,bottom),flat(bottom)])],
                   [ (open_vessel(_0) :- 
                          [part(_0,concavity),points_up(concavity)])]])] 


?- extract_support_1([ (cup(_0) :- 
                  [[ (liftable(_0) :- 
                          [[ (light(_0) :- 
                                  [small(_0)])],
                           part(_0,handle)])],
                   [ (stable(_0) :- 
                          [part(_0,bottom),flat(bottom)])],
                   [ (open_vessel(_0) :- 
                          [part(_0,concavity),points_up(concavity)])]])], Premise).
    _0 = _0,
    Premise =  (small(_0), part(_0,handle)), 
                (part(_0,bottom), flat(bottom)), 
                part(_0,concavity), points_up(concavity) 
		
		
%%%% This conjunctive query would be the heart (plus some syntactic sugar)
% of a rule constructor.
	    
    
?- prolog_ebg(cup(obj1), cup(X), P, GenP), extract_support_1(GenP, Premise).
    X = _0,
    P = [ (cup(obj1) :- 
               [[ (liftable(obj1) :- 
                       [[ (light(obj1) :- 
                               [small(obj1)])],part(obj1,handle)])],
                [ (stable(obj1) :- 
                       [part(obj1,bottom),flat(bottom)])],
                [ (open_vessel(obj1) :- 
                       [part(obj1,concavity),points_up(concavity)])]])],
    GenP = [ (cup(_0) :- 
                  [[ (liftable(_0) :- 
                          [[ (light(_0) :-
                                  [small(_0)])],
                           part(_0,handle)])],
                   [ (stable(_0) :- 
                          [part(_0,bottom),flat(bottom)])],
                   [ (open_vessel(_0) :- 
                          [part(_0,concavity),points_up(concavity)])]])],
    Premise =  (small(_0), part(_0,handle)), 
                (part(_0,bottom), flat(bottom)), 
                part(_0,concavity), points_up(concavity)   </pre>

*/


/*<title->
      Prolog Explanation-Based Reasoning: Suicide Example
    </title->*/

% The suicide example

/* Domain theory */

kill(A,B) :- hate(A,B), possess(A,C), weapon(C).
hate(W,W) :- depressed(W).
possess(U,V) :- buy(U,V).
weapon(Z) :- gun(Z).

/* Training example */

depressed(john).
buy(john, colt).
gun(colt).   
   
/* operationality criteria */

operational(depressed(X)).
operational(buy(X, Y)).
operational(gun(X)).


