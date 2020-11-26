%
%  fluent.pl:  predicates for manipulating fluent terms
%
%  Copyright 2008, Ryan Kelly
%
%  This file supplies the basic predicates for manipulating and reasoning
%  about fluent terms.  It shells out to a third-party prover for the
%  hard work.
%
%  Variables are expected to be actual prolog variables, as this vastly
%  increases the simplicity and efficiency of certain operations.  It also
%  means we need to take extra care in some other areas.  In particular,
%  we assume that the formula contains the *only* references to those
%  variables, so we are free to bind them in simplification and reasoning.
%
%  The following predicate is expected to be provided as a 'black box'
%  reasoning engine:
%
%    entails(Axioms,Conc):  Succeeds when Conc is logically entailed by Axioms
%
%  Axioms is a list of formulae, and Conc is a formula.  The predicate must
%  not bind any variables in Conc.
%


%
%  Our logical operators are:
% 
%     P & Q       -   logical and
%     P | Q       -   logical or
%     P => Q      -   implication
%     P <=> Q     -   equivalence
%     ~P          -   negation
%     !([X:T]:P)  -   universal quantification with typed variables
%     ?([X:T]:P)  -   existential quantification with types variables
%     A = B       -   object equality
%     A \= B      -   object inequality
%     knows(A,P)  -   individual-level knowledge modality
%     pknows(E,P) -   complex epistemic modality
%
%  Epistemic path operators are:
%
%     P ; Q       -   sequence
%     P | Q       -   choice
%     ?(P)        -   test
%     !(X:T)      -   nondet. rebind of a typed variable
%     -([X:V])    -   set vars to specific values
%     P*          -   iteration
%     
%  Most of these are native prolog operators so we dont have
%  to declare them ourselves.  Note that ! and ?
%  take a list variables as their first argument - this
%  allows more compact quantification over multiple variables.
%
%  Also note that variables are always paired with their type in the
%  quantifier - while we could probably deduce the type from its use
%  in the enclosed formula, having it explicit is oh so much easier.
%

:- op(200,fx,~).
:- op(500, xfy, <=>).
:- op(500, xfy, =>).
:- op(520, xfy, &).
:- op(1200, xfx, :).
:- op(550, fx, !).
:- op(550, fx, ?).
:- op(400, xf, *).


%
%  normalize(F,N) - perform some basic normalisations on a formula
%
%  This is designed to make formulae easier to reason about.  It
%  re-arranges orderless terms into a standard order, for example
%  the arguments to '=' and the list of variables in a quantification.
%  It also simplifies away some trivial tautologies.
% 
normalize((A=B),(A=B)) :-
    A @< B, !.
normalize((A=B),(B=A)) :-
    B @< A, !.
normalize((A=B),true) :-
    A == B, !.
normalize((A\=B),(A\=B)) :-
    A @< B, !.
normalize((A\=B),(B\=A)) :-
    B @< A, !.
normalize((A\=B),false) :-
    A == B, !.
normalize(?(X:P),?(Y:Q)) :-
    sort(X,Y),
    normalize(P,Q), !.
normalize(!(X:P),!(Y:Q)) :-
    sort(X,Y),
    normalize(P,Q), !.
normalize(~P,~Q) :-
    normalize(P,Q), !.
normalize((P1 & Q1),(P2 & Q2)) :-
    normalize(P1,P2),
    normalize(Q1,Q2), !.
normalize((P1 | Q1),(P2 | Q2)) :-
    normalize(P1,P2),
    normalize(Q1,Q2), !.
normalize((P1 => Q1),(P2 => Q2)) :-
    normalize(P1,P2),
    normalize(Q1,Q2), !.
normalize((P1 <=> Q1),(P2 <=> Q2)) :-
    normalize(P1,P2),
    normalize(Q1,Q2), !.
normalize(knows(A,P),knows(A,Q)) :-
    normalize(P,Q), !.
normalize(pknows(E,P),pknows(E,Q)) :-
    normalize(P,Q), !.
normalize(pknows0(E,P),pknows0(E,Q)) :-
    normalize(P,Q), !.
normalize(P,P). 


%
%  struct_equiv(P,Q)  -  two formulae are structurally equivalent,
%                        basically meaning they are identical up
%                        to renaming of variables.
%
%  struct_oppos(P,Q)  -  two formulae are structurally opposed,
%                        making their conjunction a trivial falsehood.
%

struct_equiv(P,Q) :-
    is_atom(P), is_atom(Q), P==Q.
struct_equiv(P1 & P2,Q1 & Q2) :-
    struct_equiv(P1,Q1),
    struct_equiv(P2,Q2).
struct_equiv(P1 | P2,Q1 | Q2) :-
    struct_equiv(P1,Q1),
    struct_equiv(P2,Q2).
struct_equiv(P1 => P2,Q1 => Q2) :-
    struct_equiv(P1,Q1),
    struct_equiv(P2,Q2).
struct_equiv(P1 <=> P2,Q1 <=> Q2) :-
    struct_equiv(P1,Q1),
    struct_equiv(P2,Q2).
struct_equiv(~P,~Q) :-
    struct_equiv(P,Q).
struct_equiv(?([] : P),?([] : Q)) :-
    struct_equiv(P,Q).
struct_equiv(?([V1:T|Vs1] : P),?([V2:T|Vs2] : Q)) :-
    subs(V1,V2,P,P1),
    struct_equiv(?(Vs1 : P1),?(Vs2 : Q)).
struct_equiv(!([] : P),!([] : Q)) :-
    struct_equiv(P,Q).
struct_equiv(!([V1:T|Vs1] : P),!([V2:T|Vs2] : Q)) :-
    subs(V1,V2,P,P1),
    struct_equiv(!(Vs1 : P1),!(Vs2 : Q)).
struct_equiv(knows(A,P),knows(A,Q)) :-
    struct_equiv(P,Q).
struct_equiv(pknows(E,P),pknows(E,Q)) :-
    struct_equiv(P,Q).
struct_equiv(pknows0(E,P),pknows0(E,Q)) :-
    struct_equiv(P,Q).

struct_oppos(P,Q) :-
    P = ~P1, struct_equiv(P1,Q) -> true
    ;
    Q = ~Q1, struct_equiv(P,Q1) -> true
    ;
    P=true, Q=false -> true
    ;
    P=false, Q=true.


%
%  contradictory(F1,F2)  -  F1 and F2 are trivially contradictory,
%                           meaning F1 -> ~F2 and F2 -> ~F1
%

contradictory(F1,F2) :-
    struct_oppos(F1,F2) -> true
    ;
    free_vars(F1,Vars1), member(V1,Vars1),
    free_vars(F2,Vars2), member(V2,Vars2),
    V1 == V2,
    really_var_given_value(V1,F1,B), 
    really_var_given_value(V2,F2,C),
    (\+ unifiable(B,C,_)) -> true
    ;
    fail.

really_var_given_value(A,B,C):- call(call,var_given_value(A,B,C)).
%
%  fml_compare  -  provide a standard order over formula terms
%
%  This allows them to be sorted, have duplicates removed, etc.
%  Formula should be normalised before this is applied.
%
fml_compare('=',F1,F2) :-
    struct_equiv(F1,F2), !.
fml_compare(Ord,F1,F2) :-
    ( F1 @< F2 ->
        Ord = '<'
    ;
        Ord = '>'
    ).


%
%  contains_var(A,V)  -  formula A contains variable V
%
%  ncontains_var(A,V)  -  formula A does not contain variable V
%
%
%  Since we know that V is a variable, we do this test by making a copy,
%  grounding the copied variable, then checking for structural equivalence
%  with the original term.
%
contains_var(A,V:_) :-
    copy_term(A:V,A2:V2),
    V2=groundme,
    A \=@= A2.

ncontains_var(A,V:_) :-
    copy_term(A:V,A2:V2),
    V2=groundme,
    A =@= A2.
    

%
%  flatten_op(Op,Terms,Result) - flatten repeated ops into a list
%
%  This precicate succeeds when Result is a list of terms from Terms,
%  which were joined by the operator Op.  Basically allows a tree of
%  similar operators such as ((A & B) & (C & (D & E))) to be flattened
%  into a list (A,B,C,D,E).
%

% :- index(flatten_op(0,1,0)).

flatten_op(_,[],[]).
flatten_op(O,[T|Ts],Res) :-
    %( var(T) ->
    %    Res = [T|Res2],
    %    flatten_op(O,Ts,Res2)
    ( T =.. [O|Args] ->
        append(Args,Ts,Ts2),
        flatten_op(O,Ts2,Res)
    ;
        Res = [T|Res2],
        flatten_op(O,Ts,Res2)
    ).

%
%  flatten_quant(Q,Ts,VarsIn,VarsOut,Body) - flatten nested quantifiers
%
flatten_quant(Q,T,Vars,Vars,T) :-
    \+ functor(T,Q,1), !.
flatten_quant(Q,T,Acc,Vars,Body) :-
    T =.. [Q,(V : T2)],
    append(V,Acc,Acc2),
    flatten_quant(Q,T2,Acc2,Vars,Body).

%
%  flatten_quant_and_simp(Q,BodyIn,VarsIn,VarsOut,BodyOut)
%       - flatten nested quantifiers, and apply simplification
%
%  Just like flatten_quant/5 above, but applies simplify/2 to the body
%  when it does not match the quantifier, in case its simplification
%  does match.
%
flatten_quant_and_simp(Q,T,VarsIn,VarsOut,Body) :-
    ( T =.. [Q,(V : T2)] ->
      append(V,VarsIn,Vars2),
      flatten_quant_and_simp(Q,T2,Vars2,VarsOut,Body)
    ;
      simplify1(T,Tsimp),
      ( Tsimp =.. [Q,(V : T2)] ->
          append(V,VarsIn,Vars2),
          flatten_quant_and_simp(Q,T2,Vars2,VarsOut,Body)
      ;
          Body = Tsimp, VarsIn = VarsOut
      )
    ).


%
%  ismember(Elem,List)  -  like member/2, but does not bind variables or
%                          allow backtracking.
%
ismember(_,[]) :- fail.
ismember(E,[H|T]) :-
    ( E == H ->
        true
    ;
        ismember(E,T)
    ).

%
%  vdelete(List,Elem,Result) - like delete/3 but using equivalence rather
%                              than unification, so it can be used on lists
%                              of variables
%
vdelete([],_,[]).
vdelete([H|T],E,Res) :-
    ( E == H ->
        vdelete(T,E,Res)
    ;
        Res = [H|T2],
        vdelete(T,E,T2)
    ).

% :- index(vdelete_list(0,1,0)).

vdelete_list(L,[],L).
vdelete_list(L,[H|T],L2) :-
    vdelete(L,H,L1),
    vdelete_list(L1,T,L2).


%
%  indep_of_vars(Vars,P)  -  P contains none of the vars in the list Vars,
%                            i.e. it is independent of the vars.
%
indep_of_vars(Vars,P) :-
    \+ ( member(X,Vars), contains_var(P,X) ).


%
%  var_in_list(Var,VarL)  -  variable V is in the list VarL
%
var_in_list([],_) :- fail.
var_in_list([H|T],V) :-
    ( V == H ->
        true
    ;
        var_in_list(T,V)
    ).

%
%  pairfrom(L,E1,E2,Rest)  -  E1 and E2 are a pair of (different) elements
%                             from L, wile Rest is the rest of the list
%
%  Like doing (member(E1,L), member(E2,L))  but more efficient, doesnt match
%  E1 and E2 to the same element, and doesnt generate equivalent permutations.
%
pairfrom(L,E1,E2,Rest) :-
    pairfrom_rec(L,[],E1,E2,Rest).

pairfrom_rec([H|T],Rest1,E1,E2,Rest) :-
    E1 = H, select(E2,T,Rest2), append(Rest1,Rest2,Rest)
    ;
    pairfrom_rec(T,[H|Rest1],E1,E2,Rest).


%
%  simplify(+F1,-F2) - simplify a formula
%  
%  This predicate applies some basic simplification rules to a formula
%  to eliminate redundancy and (hopefully) speed up future reasoning.
%  

simplify(P,S) :-
    normalize(P,Nml),
    simplify1(Nml,S1),
    fml2nnf(S1,Nnf),
    simplify1(Nnf,S).

simplify1(P,P) :-
    is_atom(P), P \= (_ = _), P \= (_ \= _).
simplify1(P & Q,S) :-
    flatten_op('&',[P,Q],TermsT),
    simplify1_conjunction(TermsT,TermsS),
    ( TermsS = [] ->
        S = true
    ;
        % This will remove duplicates, which includes struct_equiv formulae
        predsort(fml_compare,TermsS,Terms),
        joinlist('&',Terms,S)
    ).
simplify1(P | Q,S) :-
    flatten_op('|',[P,Q],TermsT),
    simplify1_disjunction(TermsT,TermsS),
    ( TermsS = [] ->
        S = false
    ;
        % This will remove duplicates, which includes struct_equiv formulae
        predsort(fml_compare,TermsS,Terms),
        joinlist('|',Terms,S)
    ).
simplify1(P => Q,S) :-
    simplify1(P,Sp),
    simplify1(Q,Sq),
    (
        Sp=false -> S=true
    ;
        Sp=true -> S=Sq
    ;
        Sq=true -> S=true
    ;
        Sq=false -> S=(~Sp)
    ;
        S = (Sp => Sq)
    ).
simplify1(P <=> Q,S) :-
    simplify1(P,Sp),
    simplify1(Q,Sq),
    (
        struct_equiv(P,Q) -> S=true
    ;
        struct_oppos(P,Q) -> S=false
    ;
        Sp=false -> S=(~Sq)
    ;
        Sq=true -> S=Sq
    ;
        Sq=false -> S=(~Sp)
    ;
        Sq=true -> S=Sp
    ;
        S = (Sp <=> Sq)
    ).
simplify1(~P,S) :-
    simplify1(P,SP),
    (
        SP=false -> S=true
    ;
        SP=true -> S=false
    ;
        SP = ~P2 -> S=P2
    ;
        SP = (A\=B) -> S=(A=B)
    ;
        S = ~SP
    ).
simplify1(!(Xs:P),S) :-
    ( Xs = [] -> simplify1(P,S)
    ;
    flatten_quant_and_simp('!',P,Xs,VarsT,Body),
    sort(VarsT,Vars),
    (
        Body=false -> S=false
    ;
        Body=true -> S=true
    ;
        % Remove any useless variables
        partition(ncontains_var(Body),Vars,_,Vars2),
        ( Vars2 = [] ->
            S2 = Body
        ;
          % Pull independent components outside the quantifier.
          % Both conjuncts and disjuncts can be handled in this manner.
          (flatten_op('|',[Body],BTerms), BTerms = [_,_|_] -> 
            partition(indep_of_vars(Vars),BTerms,Indep,BT2),
            % Because we have removed useless vars, BT2 cannot be empty
            joinlist('|',BT2,Body2),
            ( Indep = [] ->
              S2=!(Vars2:Body2)
            ;
              joinlist('|',Indep,IndepB),
              S2=(!(Vars2:Body2) | IndepB)
            )
          
          ; flatten_op('&',[Body],BTerms), BTerms = [_,_|_] ->
            partition(indep_of_vars(Vars),BTerms,Indep,BT2),
            joinlist('&',BT2,Body2),
            ( Indep = [] ->
              S2=!(Vars2:Body2)
            ;
              joinlist('&',Indep,IndepB),
              S2=(!(Vars2:Body2) & IndepB)
            )
          ;
            S2=!(Vars2:Body)
          )
        ),
        S = S2
    )).
simplify1(?(Xs:P),S) :-
   ( Xs = [] -> simplify1(P,S)
   ;
   flatten_quant_and_simp('?',P,Xs,VarsT,Body),
   sort(VarsT,Vars),
   (
       Body=false -> S=false
   ;
       Body=true -> S=true
   ;
       % Remove vars that are assigned a specific value, simply replacing
       % them with their value in the Body formula
       member(V1:T1,Vars), var_valuated(V1,Body,Body2) ->
           vdelete(Vars,V1:T1,Vars2), simplify1(?(Vars2:Body2),S)
   ;
       % Remove any useless variables
       partition(ncontains_var(Body),Vars,_,Vars2),
       ( Vars2 = [] ->
           S = Body
       ;
         % Pull independent components outside the quantifier,
         % Both conjuncts and disjuncts can be handled in this manner.
         (flatten_op('|',[Body],BTerms), BTerms = [_,_|_] -> 
           partition(indep_of_vars(Vars),BTerms,Indep,BT2),
           joinlist('|',BT2,Body2),
           ( Indep = [] ->
             S = ?(Vars2:Body2)
           ;
             joinlist('|',Indep,IndepB),
             S = (?(Vars2:Body2) | IndepB)
           )
         ; flatten_op('&',[Body],BTerms), BTerms = [_,_|_] ->
           partition(indep_of_vars(Vars),BTerms,Indep,BT2),
           joinlist('&',BT2,Body2),
           ( Indep = [] ->
             S = ?(Vars2:Body2)
           ;
             joinlist('&',Indep,IndepB),
             S = (?(Vars2:Body2) & IndepB)
           )
         ;
           S = ?(Vars2:Body)
         )
       )
   )).
simplify1((A=B),S) :-
   (
       A == B -> S=true
   ;
       % Utilising the unique names assumption, we can rewrite it to
       % a conjunction of simpler equalities by striping functors, or
       % to false if the terms will never unify
       ( unifiable(A,B,Eqs) ->
           joinlist('&',Eqs,S1),
           normalize(S1,S)
       ;
           S=false
       )
   ).
simplify1((A\=B),S) :-
   (
       A == B -> S=false
   ;
       % Utilising the unique names assumption, we can rewrite it to
       % a disjunction of simpler inequalities by striping functors, or
       % to true if the terms will never unify
       ( unifiable(A,B,Eqs) ->
           joinlist('&',Eqs,S1),
           normalize(S1,S2),
           S = ~S2
       ;
           S = true
       )
   ).
simplify1(knows(A,P),S) :-
   simplify1(P,Ps),
   ( Ps=true -> S=true
   ; Ps=false -> S=false
   ; S = knows(A,Ps)
   ).
simplify1(pknows(E,P),S) :-
   simplify_epath(E,Es),
   simplify1(P,Ps),
   ( Ps=true -> S=true
   ; Ps=false -> S=false
   ; Es=(?false) -> S=true
   ; Es=(?true) -> S=Ps
   ; S = pknows(Es,Ps)
   ).
simplify1(pknows0(E,P),S) :-
   simplify_epath(E,Es),
   simplify1(P,Ps),
   ( Ps=true -> S=true
   %; Ps=false -> S=false  % this may be OK if Es=(?false) but we can't
                           % always detect such a case
   ; Es=(?false) -> S=true
   ; Es=(?true) -> S=Ps
   ; S = pknows0(Es,Ps)
   ).


%
%  simplify1_conjunction(In,Out)  -  simplify the conjunction of list of fmls
%  simplify1_disjunction(In,Out)  -  simplify the disjunction of list of fmls
%

simplify1_conjunction(FmlsIn,FmlsOut) :-
    maplist(simplify1,FmlsIn,FmlsI1),
    simplify1_conjunction_rules(FmlsI1,FmlsI2),
    simplify1_conjunction_acc(FmlsI2,[],FmlsOut).

simplify1_conjunction_rules(ConjsI,ConjsO) :-
    (pairfrom(ConjsI,C1,C2,Rest), (simplify1_conjunction_rule(C1,C2,C1o,C2o) ; simplify1_conjunction_rule(C2,C1,C2o,C1o)) ->
      simplify1_conjunction_rules([C1o,C2o|Rest],ConjsO)
    ;
      ConjsO = ConjsI
    ).
 
simplify1_conjunction_rule(C1,C2,C1,C2o) :-
    C2 = (C2a | C2b),
    ( struct_oppos(C1,C2a), C2o=C2b
    ; struct_oppos(C1,C2b), C2o=C2a
    ).
simplify1_conjunction_rule(C1,C2,C1,true) :-
    C2 = (C2a | C2b),
    ( struct_equiv(C1,C2a)
    ; struct_equiv(C1,C2b)
    ).
    


simplify1_conjunction_acc([],FmlsAcc,FmlsAcc).
simplify1_conjunction_acc([F|FmlsIn],FmlsAcc,FmlsOut) :-
    ( member(Eq,FmlsAcc), struct_equiv(F,Eq) ->
        F2 = true
    ; member(Opp,FmlsAcc), struct_oppos(F,Opp) ->
        F2 = false
    ;
        F2 = F
    ),
    ( F2=true ->
        simplify1_conjunction_acc(FmlsIn,FmlsAcc,FmlsOut)
    ; F2=false ->
        FmlsOut=[false]
    ;
        simplify1_conjunction_acc(FmlsIn,[F2|FmlsAcc],FmlsOut)
    ).
    

simplify1_disjunction(FmlsIn,FmlsOut) :-
    maplist(simplify1,FmlsIn,FmlsI1),
    simplify1_disjunction_rules(FmlsI1,FmlsI2),
    simplify1_disjunction_acc(FmlsI2,[],FmlsOut).

simplify1_disjunction_rules(DisjI,DisjO) :-
    (pairfrom(DisjI,D1,D2,Rest), (simplify1_disjunction_rule(D1,D2,D1o,D2o) ; simplify1_disjunction_rule(D2,D1,D2o,D1o)) ->
      simplify1_disjunction_rules([D1o,D2o|Rest],DisjO)
    ;
      DisjO = DisjI
    ).
 
simplify1_disjunction_rule(D1,D2,D1,D2o) :-
    D2 = (D2a & D2b),
    ( struct_oppos(D1,D2a), D2o=D2b
    ; struct_oppos(D1,D2b), D2o=D2a
    ).
simplify1_disjunction_rule(D1,D2,D1,false) :-
    D2 = (D2a & D2b),
    ( struct_equiv(D1,D2a)
    ; struct_equiv(D1,D2b)
    ).

simplify1_disjunction_acc([],FmlsAcc,FmlsAcc).
simplify1_disjunction_acc([F|FmlsIn],FmlsAcc,FmlsOut) :-
    ( member(Eq,FmlsAcc), struct_equiv(F,Eq) ->
        F2 = false
    ; member(Opp,FmlsAcc), struct_oppos(F,Opp) ->
        F2 = true
    ;
        F2 = F
    ),
    ( F2=false ->
        simplify1_disjunction_acc(FmlsIn,FmlsAcc,FmlsOut)
    ; F2=true ->
        FmlsOut=[true]
    ;
        simplify1_disjunction_acc(FmlsIn,[F2|FmlsAcc],FmlsOut)
    ).


%
%  var_given_value(X,P,V,Q)  -  variable X is uniformly given the value V
%                               within the formula P.  Q is a formula equiv
%                               to P, with variable X set to V.
%
%  var_given_value_list(X,Ps,V,Qs)  -  variable X is uniformly given the value
%                                      V in all formulae in list Ps.
% 
%  Determining Q from P is not a simple substitution - if the value V
%  contains vars that are introduced by a quantifier in P, this quantifier
%  must be lifted to outermost scope.
%

/*
% :- index(var_given_value(0,1,0,0)).
% :- index(var_given_value_list(0,1,0,0)).
*/

var_given_value(X,A=B,V,true) :-
    ( X == A ->
        V = B
    ;
        X == B, V = A
    ).
var_given_value(X,P1 & P2,V,Q) :-
    flatten_op('&',[P1,P2],Cjs),
    select(Cj,Cjs,Rest), var_given_value(X,Cj,V,Qj),
    partition(vgv_partition(X),Rest,Deps,Indeps),
    (Indeps = [] -> IndepFml=true ; joinlist('&',Indeps,IndepFml)),
    (Deps = [] -> DepFml1=true ; joinlist('&',Deps,DepFml1)),
    subs(X,V,DepFml1,DepFml),
    % We may have lifted a variable outside the scope of its
    % quantifier.  We must push QRest back down through the quantifiers
    % to rectify this.  By invariants of the operation, we know all 
    % relevant quantifiers are at outermost scope in Qj.
    (DepFml \= true ->
      term_variables(V,Vars),
      vgv_push_into_quantifiers(Vars,Qj,DepFml,QFml)
    ;
      QFml = Qj
    ),
    Q = (IndepFml & QFml), !.
var_given_value(X,P1 | P2,V,Q) :-
    flatten_op('|',[P1,P2],Djs),
    var_given_value_list(X,Djs,V,Qs),
    joinlist('|',Qs,Q).
var_given_value(X,!(Vars:P),V,!(VarsQ:Q)) :-
    var_given_value(X,P,V,Q2),
    flatten_quant('!',Q2,Vars,VarsQ,Q).
var_given_value(X,?(Vars:P),V,?(VarsQ:Q)) :-
    var_given_value(X,P,V,Q2),
    flatten_quant('?',Q2,Vars,VarsQ,Q).
var_given_value(X,knows(A,P),V,knows(A,Q)) :-
    var_given_value(X,P,V,Q).
var_given_value(X,pknows(E,P),V,pknows(E,Q)) :-
    var_given_value(X,P,V,Q).
var_given_value(X,pknows0(E,P),V,pknows0(E,Q)) :-
    var_given_value(X,P,V,Q).
% There's no clause for ~P because that can never give X a specific value

var_given_value_list(_,[],_,[]).
var_given_value_list(X,[H|T],V,[Qh|Qt]) :-
    % Be careful not to do any unification on V.
    % This ruins tail-recursion, but meh...
    var_given_value(X,H,V,Qh),
    var_given_value_list(X,T,V2,Qt),
    V == V2.

vgv_partition(V,F) :-
    contains_var(F,V:_).

%  We can stop recursing either when Vars=[] or when we are
%  no longer at a quantifier, since we assume all relevant 
%  quantifiers have been brought to the front.
vgv_push_into_quantifiers(Vars,?(Qv:Qj),QDep,?(Qv:Q)) :-
    Vars \= [], !,
    vgv_subtract(Vars,Qv,Vars2),
    vgv_push_into_quantifiers(Vars2,Qj,QDep,Q).
vgv_push_into_quantifiers(Vars,!(Qv:Qj),QDep,!(Qv:Q)) :-
    Vars \= [], !,
    vgv_subtract(Vars,Qv,Vars2),
    vgv_push_into_quantifiers(Vars2,Qj,QDep,Q).
vgv_push_into_quantifiers(_,Qj,QDep,Qj & QDep).

vgv_subtract([],_,[]).
vgv_subtract(Vs,[],Vs).
vgv_subtract(Vs,[X:_|Xs],Vs2) :-
    vgv_subtract_helper(Vs,X,Vs1),
    vgv_subtract(Vs1,Xs,Vs2).

vgv_subtract_helper([],_,[]).
vgv_subtract_helper([V|Vs],X,Vs2) :-
    ( X == V ->
        vgv_subtract_helper(Vs,X,Vs2)
    ;
        Vs2 = [V|Vs22],
        vgv_subtract_helper(Vs,X,Vs22)
    ).

%
%  var_valuated(X,P,P2)  -  variable X takes specific values throughout P,
%                           and P2 is P with the appropriate valuations
%                           inserted.
%

% :- index(var_valuated(0,1,0)).
% :- index(var_valuated_list(0,1,0)).
% :- index(var_valuated_distribute(0,1,0,0)).

% In the base case, X is given a single value throughout the entire formula.
var_valuated(X,P,Q) :-
   var_given_value(X,P,_,Q), !.

% The base case for P & Q - when they both give X the same value - is
% already covered by the var_valuated/3 clause above.  But if one of the 
% conjuncts is a disjunction that valuates X, then we can distribute over
% it to achieve a valuation.
var_valuated(X,P & Q,V) :-
   flatten_op('&',[P,Q],Cjs),
   select(Cj,Cjs,RestCjs),
   flatten_op('|',[Cj],Djs),
   maplist(var_valuated_check(X),Djs), !,
   joinlist('&',RestCjs,RestFml),
   var_valuated_distribute(X,Djs,RestFml,VDjs),
   joinlist('|',VDjs,V), !.
var_valuated(X,P | Q,V) :-
   flatten_op('|',[P,Q],Cs),
   var_valuated_list(X,Cs,Vs),
   joinlist('|',Vs,V).
var_valuated(X,!(V:P),!(V:Q)) :-
   var_valuated(X,P,Q).
var_valuated(X,?(V:P),?(V:Q)) :-
   var_valuated(X,P,Q).

var_valuated_list(_,[],[]).
var_valuated_list(X,[H|T],[Hv|Tv]) :-
   var_valuated(X,H,Hv),
   var_valuated_list(X,T,Tv).

var_valuated_distribute(_,[],_,[]).
var_valuated_distribute(X,[P|Ps],Q,[Pv|T]) :-
   var_valuated(X,P & Q,Pv),
   var_valuated_distribute(X,Ps,Q,T).

var_valuated_check(X,F) :-
   %var_valuated(X,F,_).
   var_given_value(X,F,_,_).


%
%  joinlist(+Op,+In,-Out) - join items in a list using given operator
%

joinlist(_,[H],H) :- !.
joinlist(O,[H|T],J) :-
    T \= [],
    J =.. [O,H,TJ],
    joinlist(O,T,TJ).

%
%  subs(Name,Value,Old,New):  substitue values in a term
%
%  This predicate is true when New is equal to Old with all occurances
%  of Name replaced by Value - basically, a symbolic substitution
%  routine.  For example, it is usually used to produce a result such
%  as:
%
%      subs(now,S,fluent(now),fluent(S)).
%
subs(X,Y,T,Tr) :-
    T == X, Tr = Y, !.
subs(X,_,T,Tr) :-
    T \== X, var(T), T=Tr, !.
subs(X,Y,T,Tr) :-
    T \== X, nonvar(T), T =.. [F|Ts], subs_list(X,Y,Ts,Trs), Tr =.. [F|Trs], !.

%
%  subs_list(Name,Value,Old,New):  value substitution in a list
%
%  This predicate operates as subs/4, but Old and New are lists of terms
%  instead of single terms.  Basically, it calls subs/4 recursively on
%  each element of the list.
%
subs_list(_,_,[],[]).
subs_list(X,Y,[T|Ts],[Tr|Trs]) :-
    subs(X,Y,T,Tr), subs_list(X,Y,Ts,Trs).


%
%  fml2nnf:  convert a formula to negation normal form
%
fml2nnf(P <=> Q,N) :-
    fml2nnf((P => Q) & (Q => P),N), !.
fml2nnf(P => Q,N) :-
    fml2nnf((~P) | Q,N), !.
fml2nnf(~(P <=> Q),N) :-
    fml2nnf(~(P => Q) & ~(Q => P),N), !.
fml2nnf(~(P => Q),N) :-
    fml2nnf(P & ~Q,N), !.
fml2nnf(~(P & Q),N) :-
   fml2nnf((~P) | (~Q),N), !.
fml2nnf(~(P | Q),N) :-
   fml2nnf((~P) & (~Q),N), !.
fml2nnf(~(!(X:P)),N) :-
   fml2nnf( ?(X : ~P) ,N), !.
fml2nnf(~(?(X:P)),N) :-
   fml2nnf(!(X : ~P),N), !.
fml2nnf(~(~P),N) :-
    fml2nnf(P,N), !.
fml2nnf(P & Q,Np & Nq) :-
    fml2nnf(P,Np), fml2nnf(Q,Nq), !.
fml2nnf(P | Q,Np | Nq) :-
    fml2nnf(P,Np), fml2nnf(Q,Nq), !.
fml2nnf(!(X:P),!(X:N)) :-
    fml2nnf(P,N), !.
fml2nnf(?(X:P),?(X:N)) :-
    fml2nnf(P,N), !.
fml2nnf(knows(A,P),knows(A,N)) :-
    fml2nnf(P,N), !.
fml2nnf(pknows(E,P),pknows(E,N)) :-
    fml2nnf(P,N), !.
fml2nnf(pknows0(E,P),pknows0(E,N)) :-
    fml2nnf(P,N), !.
fml2nnf(~knows(A,P),~knows(A,N)) :-
    fml2nnf(P,N), !.
fml2nnf(~pknows(E,P),~pknows(E,N)) :-
    fml2nnf(P,N), !.
fml2nnf(~pknows0(E,P),~pknows0(E,N)) :-
    fml2nnf(P,N), !.
fml2nnf(P,P).

%
%  is_atom(P)  -  the formula P is a literal atom, not a compound expression
%
%  This is used to detect the base case of many predicates that structurally
%  decompose formulae.
%
is_atom(P) :-
    P \= (~_),
    P \= (_ => _),
    P \= (_ <=> _),
    P \= (_ & _),
    P \= (_ | _),
    P \= ?(_:_),
    P \= !(_:_),
    P \= knows(_,_),
    P \= pknows(_,_),
    P \= pknows0(_,_).


%
%  copy_fml(P,Q)  -  make a copy of a formula.  The copy will have all
%                    bound variables renamed to new vars.  Any free variables
%                    will retain their original names.
%

copy_fml(P,P) :-
    var(P), !.
copy_fml(P,P) :-
    is_atom(P).
copy_fml(P & Q,R & S) :-
    copy_fml(P,R),
    copy_fml(Q,S).
copy_fml(P | Q,R | S) :-
    copy_fml(P,R),
    copy_fml(Q,S).
copy_fml(P => Q,R => S) :-
    copy_fml(P,R),
    copy_fml(Q,S).
copy_fml(P <=> Q,R <=> S) :-
    copy_fml(P,R),
    copy_fml(Q,S).
copy_fml(~P,~Q) :-
    copy_fml(P,Q).
copy_fml(!(VarsP:P),!(VarsQ:Q)) :-
    rename_vars(VarsP,P,VarsQ,P2),
    copy_fml(P2,Q).
copy_fml(?(VarsP:P),?(VarsQ:Q)) :-
    rename_vars(VarsP,P,VarsQ,P2),
    copy_fml(P2,Q).
copy_fml(knows(A,P),knows(A,Q)) :-
    copy_fml(P,Q).
copy_fml(pknows(E,P),pknows(F,Q)) :-
    copy_epath(E,F),
    copy_fml(P,Q).
copy_fml(pknows0(E,P),pknows0(F,Q)) :-
    copy_epath(E,F),
    copy_fml(P,Q).

%
%  rename_vars(Vars,F,NewVars,NewF)  -  rename the given variables to new
%                                       ones, producing a modified formula.
%
rename_vars(Vs,P,Vs2,P2) :-
    rename_vars(Vs,P,[],Vs2,P2).

rename_vars([],P,Acc,Acc,P).
rename_vars([V:T|Vs],P,Acc,NewV,NewP) :-
    subs(V,V2,P,P2),
    append(Acc,[V2:T],Acc2),
    rename_vars(Vs,P2,Acc2,NewV,NewP).

%
%  free_vars(Fml,Vars)  -  list of all free variables in the formula
%
%  "Free" is in the FOL sense of "not bound by an enclosing quantifier"
%
free_vars(Fml,Vars) :-
    copy_fml(Fml,Fml2),
    term_variables(Fml,Vars1),
    term_variables(Fml2,Vars2),
    vars_in_both(Vars1,Vars2,[],Vars).

vars_in_both([],_,Vars,Vars).
vars_in_both([H|T],V2,Acc,Vars) :-
    ( ismember(H,V2) ->
        vars_in_both(T,V2,[H|Acc],Vars)
    ;
        vars_in_both(T,V2,Acc,Vars)
    ).
    

write_eqn(P) :-
    write('\\begin{multline}'),nl,write_latex(P),nl,write('\\end{multline}').

write_latex(P) :-
  copy_fml(P,Pc),
  number_vars(Pc),
  do_write_latex(Pc).

do_write_latex(P) :-
    var(P), write(P), !.
do_write_latex(P <=> Q) :-
    do_write_latex(P), write(' \\equiv '), do_write_latex(Q).
do_write_latex(P => Q) :-
    do_write_latex(P), write(' \\rightarrow '), do_write_latex(Q).
do_write_latex(~P) :-
    write(' \\neg '), do_write_latex(P).
do_write_latex(P & Q) :-
    flatten_op('&',[P & Q],[C|Cs]),
    write(' \\left( '), do_write_latex(C), reverse(Cs,CsR),
    do_write_latex_lst(CsR,' \\wedge '), write(' \\right) ').
do_write_latex(P | Q) :-
    flatten_op('|',['|'(P,Q)],[C|Cs]),
    do_write_latex(C), reverse(Cs,CsR),
    do_write_latex_lst(CsR,' \\vee ').
do_write_latex(!([X|Xs]:P)) :-
    write(' \\forall '),
    do_write_latex(X),
    do_write_latex_lst(Xs,','),
    write(' : \\left[ '),
    do_write_latex(P),
    write(' \\right] ').
do_write_latex(?([X|Xs]:P)) :-
    write(' \\exists '),
    do_write_latex(X),
    do_write_latex_lst(Xs,','),
    write(' : \\left[ '),
    do_write_latex(P),
    write(' \\right] ').
do_write_latex(knows(A,P)) :-
    write(' \\Knows( '),
    do_write_latex(A),
    write(','),
    do_write_latex(P),
    write(')').
do_write_latex(pknows(E,P)) :-
    write(' \\PKnows( '),
    do_write_latex(E),
    write(','),
    do_write_latex(P),
    write(')').
do_write_latex(P) :-
    is_atom(P), write(P).

do_write_latex_lst([],_).
do_write_latex_lst([T|Ts],Sep) :-
    write(Sep), nl, do_write_latex(T), do_write_latex_lst(Ts,Sep).


number_vars(X) :-
    term_variables(X,Vs),
    number_vars_rec(Vs,0).
number_vars_rec([],_).
number_vars_rec([V|Vs],N) :-
    name(x,N1), name(N,N2), append(N1,N2,Codes),
    atom_codes(V,Codes),
    Ns is N + 1,
    number_vars_rec(Vs,Ns).


%
%  pp_fml(P)  -  pretty-print the formula P
%

pp_fml(P) :-
    copy_fml(P,F), fml2nnf(F,N), number_vars(N),
    pp_fml(N,0,0), !, nl, nl.

pp_inset(0).
pp_inset(N) :-
    N > 0, N1 is N - 1,
    write('   '), pp_inset(N1).

pp_fml_list([P],_,_,O1,D1) :-
    pp_fml(P,O1,D1).
pp_fml_list([P1,P2|Ps],Op,D,O1,D1) :-
    pp_fml(P1,O1,D1), nl,
    pp_inset(D), write(Op), nl,
    pp_fml_list([P2|Ps],Op,D,D1,D1).

pp_fml(P1 & P2,O,D) :-
    flatten_op('&',[P1,P2],Ps),
    D1 is D + 1,
    O1 is O + 1,
    pp_fml_list(Ps,'&',D,O1,D1).
pp_fml(P1 | P2,O,D) :-
    flatten_op('|',[P1,P2],Ps),
    D1 is D + 1,
    O1 is O + 1,
    pp_fml_list(Ps,'|',D,O1,D1).
pp_fml(~P,O,D) :-
    D1 is D + 1,
    pp_inset(O), write('~  '),
    pp_fml(P,0,D1).
pp_fml(P1 => P2,O,D) :-
    D1 is D + 1,
    pp_inset(O), pp_fml(P1,1,D1), nl,
    pp_inset(D), write('=>'), nl,
    pp_fml(P2,D1,D1).
pp_fml(P1 <=> P2,O,D) :-
    D1 is D + 1,
    pp_inset(O), pp_fml(P1,1,D1), nl,
    pp_inset(D), write('<=>'), nl,
    pp_fml(P2,D1,D1).
pp_fml(!(V:P),O,D) :-
    D1 is D + 1,
    pp_inset(O), write('!('), write(V), write('):'), nl,
    pp_fml(P,D1,D1).
pp_fml(?(V:P),O,D) :-
    D1 is D + 1,
    pp_inset(O), write('?('), write(V), write('):'), nl,
    pp_fml(P,D1,D1).
pp_fml(knows(A,P),O,D) :-
    D1 is D + 1,
    pp_inset(O), write('knows('), write(A), write(','), nl,
    pp_fml(P,D1,D1), nl,
    pp_inset(D), write(')').
pp_fml(pknows(E,P),O,D) :-
    D1 is D + 1,
    pp_inset(O), write('pknows('), nl,
    pp_epath(E,D1,D1), nl,
    pp_inset(D), write('-----'), nl,
    pp_fml(P,D1,D1), nl,
    pp_inset(D), write(')').
pp_fml(pknows0(E,P),O,D) :-
    D1 is D + 1,
    pp_inset(O), write('pknows0('), nl,
    pp_epath(E,D1,D1), nl,
    pp_inset(D), write('-----'), nl,
    pp_fml(P,D1,D1), nl,
    pp_inset(D), write(')').
pp_fml(P,O,_) :-
    is_atom(P),
    pp_inset(O), write(P).


fml2cnf(P,P) :-
    is_atom(P).
fml2cnf(P1 & P2,C1 & C2) :-
    fml2cnf(P1,C1),
    fml2cnf(P2,C2).
fml2cnf(P1 | P2,C) :-
    fml2cnf(P1,C1),
    fml2cnf(P2,C2),
    flatten_op('&',[C1],C1s),
    flatten_op('&',[C2],C2s),
    setof(Cp,fml2cnf_helper(C1s,C2s,Cp),Cs),
    joinlist('&',Cs,C).

fml2cnf_helper(Cjs1,Cjs2,C) :-
    member(C1,Cjs1), member(C2,Cjs2), C = (C1 | C2).

:- begin_tests(fluent,[sto(rational_trees)]).

test(simp1) :-
    simplify(p & true,p).
test(simp2) :-
    simplify(p & false,false).
test(simp3) :-
    simplify(p | false,p).
test(simp4) :-
    simplify(p | true,true).
test(simp5) :-
    simplify(false | false,false).
test(simp6) :-
    simplify(false | (p & ~(a=a)),false).
test(simp7) :-
    simplify(true & true,true).
test(simp8) :-
    simplify(!([X:t]: p(X) & p(a)),!([X:t]:p(X)) & p(a)).
test(simp9) :-
    simplify(?([X:t]: p(X) & p(a)),?([X:t]:p(X)) & p(a)).
test(simp10) :-
    X1 = ?([X:t] : ((p & (X=nil)) | (q & (X=obs) & r ) | (?([Y:o]:(s(Y) & (X=pair(a,Y))))))),
    X2 = (p | ?([Y:o] : s(Y)) | (q & r)),
    simplify(X1,X2).

test(val1) :-
    var_given_value(X,X=a,a,true).
test(val2) :-
    var_given_value(X,(X=a) & (X=b),b,F), simplify(F,false).
test(val3) :-
    var_given_value(X,p(X) & q(a) & ?([Y:t]:X=Y),Val,_),
    Val == Y.

test(copy1) :-
    F1 = !([X:t,Y:q] : p(X) & r(Y)),
    copy_fml(F1,F2),
    F1 =@= F2,
    F1 \== F2.

:- end_tests(fluent).


