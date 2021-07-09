





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
subs(X,Y,T,Tr) :- T == X, Tr = Y, !.
subs(X,_,T,Tr) :- T \== X, var(T), T=Tr, !.
subs(X,Y,T,Tr) :- T \== X, nonvar(T), 
   T =.. [F|Ts], 
   subs_list(X,Y,Ts,Trs), 
   Tr =.. [F|Trs], !.

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































%  Our logical operators are:
%  Our logical terms and operators are:


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
:- module(pknows, [

	op(200,fx,~),
	op(500, xfy, <=>),
	op(500, xfy, =>),
	op(520, xfy, &),
	
	op(1200, xfx, :),
	op(550, fx, !),
	op(550, fx, ?),
	op(400, xf, *),
]).



%
%  is_atom(P)    -  the formula P is a literal atom, not a compound expression
%  is_literal(P) -  the formula P is a literal atom or the negation of one
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
%  sitcalc.pl:   domain-independent sitcalc predicates.
%
%  Copyright 2008, Ryan Kelly
%

:- multifile(adp_fluent/3).
:- index(adp_fluent(1,1,0)).
:- multifile(constraint/1).
:- multifile(initially/1).
:- multifile(holds0/1).
:- multifile(knows0/1).

:- discontiguous(causes_true/3).
:- discontiguous(causes_false/3).



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
%  copy_fml(P,Q)  -  make a copy of a formula.  The copy will have all
%                    bound variables renamed to new vars.  Any free variables
%                    will retain their original names.
%

copy_fml(P,P) :-
    var(P), !.
copy_fml(P,P) :-
    is_atom(P), !.
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

%
%  Specify domain-independent constraints, and some things we can safely
%  assume will hold in any initial situation.
%
%constraint(true).
%constraint(~false).
initially(~knows(Agt,false)) :-
    agent(Agt).

%
%  Perform reasoning relative to the domain axioms.
%
domain_axioms(Ax) :-
   findall(C,constraint(C),Ax1),
    maplist(make_cknows_fml,Ax1,Ax).
domain_falsehood(Fml) :-
    domain_axioms(Axs),
    entails(Axs,~Fml).
domain_tautology(Fml) :-
    domain_axioms(Axs),
    entails(Axs,Fml).

make_cknows_fml(F,CK) :-
    setof(A,agent(A),Agts),
    joinlist('|',Agts,E),
    CK = pknows0((E*),F).

%
%  adp_fluent(F,A,C)  -  C is the defn for ADP fluent F applied to action A
%
%  The specific ADP fluent definitions are given in domain.pl, but we give
%  a default implementation for the case when A is a variable, which simply
%  enumerates each possible action and conjoins the individual definitions.
%
adp_fluent(F,A,C) :-
    var(A), !,
    (bagof(Ft,adp_fluent_bagof(F,A,Ft),Fts) ->
        joinlist(('|'),Fts,Ftmp),
        simplify(Ftmp,C)
    ;
        C=F
    ).

adp_fluent_bagof(F,A,F1) :-
    action_with_vars(A1,V),
    adp_fluent(F,A1,F1t),
    F1 = ?(V : (F1t & (A=A1))).

%
%  Useful ADPs that can be defined in terms of other, simpler ADPs
%

adp_fluent(pbu(Agt),A,C) :-
    adp_fluent(poss,A,C1),
    adp_fluent(canObs(Agt),A,C2),
    C = C1 & ~C2.

adp_fluent(obs(Agt,O),A,C) :-
    ( adp_fluent(canObs(Agt),A,CO) -> true ; CO=false),
    ( adp_fluent(canSense(Agt),A,CS) -> true ; CS=false),
    ( adp_fluent(sr(R),A,CR) -> true ; CR=(R=nil)),
    C = ((~CO & (O=nil)) | (CO & ~CS & (O=A)) | (CO & CS & ?([R:result]: CR & (O=pair(A,R))))).


%
%  regression(+F,+A,-Fr) - Fr is the regression of F over action A
%
%  This predicate calculates the regression of a fluent F with respect to
%  an action A, yielding a new fluent Fr.  If A is free, will consider all
%  types of action that could affect the fluent.
%

%  If A is non-free, regression1 will succeed only once.
regression(F,A,Fr) :-
    nonvar(A), !,
    regression1(F,A,Frt),
    simplify(Frt,Fr).

%  If A is free, find all actions which could affect F.
regression(F,A,Fr) :-
    var(A),
    (bagof(Ft,B^regression_bagof(F,A,B,Ft),Fts) ->
        joinlist(('|'),Fts,Ftmp),
        simplify((Ftmp | ((A=nil) & F)),Fr)
    ;
        Fr=F
    ).

regression_bagof(F,A,B,Ft) :-
    action_with_vars(B,V),
    regression1(F,B,Ftr),
    Ft = ?(V : (Ftr & (A=B))).


% Regression base case, a primitive fluent.
% Build successor state axiom from causes_true/cases_false
regression1(F,A,Fr) :-
    is_atom(F), F \= (_ = _), F \= (_ \= _),
    (causes_true(F,A,Ep) -> true ; Ep = false),
    (causes_false(F,A,En) -> true ; En = false),
    simplify(Ep | (F & ~En),Fr).

% No functional fluents, so equality is rigid
regression1(T1=T2,_,T1=T2).
regression1(T1\=T2,_,T1\=T2).

% Regression is pushed inside the logical operators
regression1(!(X : P),A,!(X : R)) :-
    regression1(P,A,R).
regression1(?(X : P),A,?(X : R)) :-
    regression1(P,A,R).
regression1(~P,A,~R) :-
    regression1(P,A,R).
regression1((P & Q),A,(R & S)) :-
    regression1(P,A,R),
    regression1(Q,A,S).
regression1((P | Q),A,(R | S)) :-
    regression1(P,A,R),
    regression1(Q,A,S).
regression1((P => Q),A,(R => S)) :-
    regression1(P,A,R),
    regression1(Q,A,S).
regression1((P <=> Q),A,(R <=> S)) :-
    regression1(P,A,R),
    regression1(Q,A,S).

% Regression of a knowledge expression.
%
% Since we're defining obs() in terms of canObs() and canSense(), we
% can make the following simplifications:
%
%    * replace obs()=nil with CanObs
%    * avoid quantifying over actions inside knows(), since only A
%      can ever match the observations for A
%

regression1(knows(Agt,P),A,Fr) :-
    Fr = ?([O:observation]: (ObsDefn & (~CanObs => knows(Agt,P)) & (CanObs => KR))),
    KR = knows(Agt,((Poss & ObsDefn2) => Ppr)),
    pcond(P,pbu(Agt),Pp),
    regression(Pp,A,Ppr),
    adp_fluent(obs(Agt,O),A,ObsDefn),
    adp_fluent(obs(Agt,O),A,ObsDefn2),
    adp_fluent(canObs(Agt),A,CanObs),
    adp_fluent(poss,A,Poss).

%  Regression of pknows0 using epistemic path regressor
%
regression1(pknows0(E,P),A,Fr) :-
    regression(P,A2,Pr),
    regress_epath(E,A,A2,Er),
    Fr = !([A2:action] : pknows0(Er,Pr)).

%  Regression of pknows as a fixpoint of pknows0
%
regression1(pknows(E,P),A,Fr) :-
    regression1_pknows_fixpoint(pknows0(E,P),FP),
    regression1(FP,A,FPr),
    regression1_pknows_rename(FPr,Fr).
regression1_pknows_fixpoint(PKn,FP) :-
    regression1(PKn,nil,PKn1),
    ( domain_tautology(PKn => PKn1) ->
        FP = PKn
    ;
        regression1_pknows_fixpoint(PKn1,FP)
    ).
regression1_pknows_rename(!(V : PK),!(V : PKr)) :-
    regression1_pknows_rename(PK,PKr).
regression1_pknows_rename(pknows0(E,P),pknows(E,P)).


%  Knowledge fluents need an extra regression step once we reach the
%  initial situation.
%
regression_s0(F,F) :-
    is_atom(F).
regression_s0(!(X : P),!(X : R)) :-
    regression_s0(P,R).
regression_s0(?(X : P),?(X : R)) :-
    regression_s0(P,R).
regression_s0(~P,~R) :-
    regression_s0(P,R).
regression_s0((P & Q),(R & S)) :-
    regression_s0(P,R),
    regression_s0(Q,S).
regression_s0((P | Q),(R | S)) :-
    regression_s0(P,R),
    regression_s0(Q,S).
regression_s0((P => Q),(R => S)) :-
    regression_s0(P,R),
    regression_s0(Q,S).
regression_s0((P <=> Q),(R <=> S)) :-
    regression_s0(P,R),
    regression_s0(Q,S).
regression_s0(knows(Agt,P),knows(Agt,Pp)) :-
    pcond(P,pbu(Agt),Pp).
regression_s0(pknows0(E,P),pknows0(E,P)).
regression_s0(pknows(E,P),FPr) :-
    regression1_pknows_fixpoint(pknows0(E,P),FPr).


%  Definition of the epistemic path regressor.
%
regress_epath(P,C1,C2,POut) :-
  regress_epath_a(P,X,Pa),
  POut = (!(X:action) ; ?(X=C1) ; Pa ; ?(X=C2)).

regress_epath_a(A,X,P) :-
    atom(A),
    P = (!(Z:observation) ; ?(ObsDefn) ; A ; !(X:action) ; ?(PossDefn & ObsDefn)),
    adp_fluent(obs(A,Z),X,ObsDefn1),
    adp_fluent(poss,X,PossDefn1),
    simplify(ObsDefn1 | ((Z=nil) & (X=nil)),ObsDefn),
    simplify(PossDefn1 | (X=nil),PossDefn).
regress_epath_a(?(F),X,P) :-
    regression(F,X,Fr),
    P = ?(Fr).
regress_epath_a(!(Y:T),_,!(Y:T)).
regress_epath_a(P1 ; P2,X,P1a ; P2a) :-
    regress_epath_a(P1,X,P1a),
    regress_epath_a(P2,X,P2a).
regress_epath_a(P1 | P2,X,P1a | P2a) :-
    regress_epath_a(P1,X,P1a),
    regress_epath_a(P2,X,P2a).
regress_epath_a((P*),X,(Pa*)) :-
    regress_epath_a(P,X,Pa).


%
%  holds(+F,+S) - fluent F holds in situation S
%
%  This predicate is true whenever the fluent F holds in situation S.
%
holds(F,do(A,S)) :-
    regression(F,A,Fr),
    holds(Fr,S).
holds(F,s0) :-
    regression_s0(F,Fr),
    bagof(Ax,initially(Ax),Ax0s),
    joinlist('&',Ax0s,Ax0),
    domain_tautology(Ax0 => Fr).

%
%  pcond_d1(F,C,P1)  -  depth 1 persistence condition for fluent F
%
%    The basic meaning of this pedicate is: if fluent F holds in situation
%    s, then it will continue to hold in all C-successors of s as long
%    as P1 is true in s.
% 
pcond_d1(F,C,P1) :-
    ( bagof(Cn,pcond_d1_bagof(F,C,Cn),Cns) ->
        joinlist('&',Cns,P1t),
        simplify(P1t,P1)
    ;
        P1=true
    ).

pcond_d1_bagof(F,C,Cnt) :-
    action_with_vars(A,Vs),
    regression(F,A,R),
    adp_fluent(C,A,Ec),
    Cnt = !(Vs : (R | ~Ec)).

%
%  pcond(F,C,P)  -  persistence condition for F under C
%

pcond(F,C,P) :-
    (domain_falsehood(F) ->
        P = false
    ; domain_tautology(F) ->
        P = true
    ; 
        pcond_acc([F],C,P)
    ).

pcond_acc([F|Fs],C,P) :-
    pcond_d1(F,C,P1),
    (domain_falsehood(P1) ->
        P = false
    ; domain_tautology(P1) ->
        joinlist('&',[F|Fs],P)
    ; 
      joinlist('&',[F|Fs],Ff),
      (domain_tautology(Ff=>P1) ->
        P = Ff
      ;
        pcond_acc([P1,F|Fs],C,P)
      )
    ).




















%
%  enumerate_vars(Vs)  -  bind each variable V in the list to a member
%                         of its corresponding type T, backtracking to
%                         enumerate all possible values.
%
enumerate_vars([]).
enumerate_vars([V:T|Vs]) :-
    call(T,V), enumerate_vars(Vs).


%
%  guess_var_types(Vs,Fml,VTs)  -  guess the types of each variable in the
%                                  list Vs, by looking at how the var is
%                                  used in the formula Fml.  Returns a list
%                                  of (V:T) pairs.
%
guess_var_types([],_,[]).
guess_var_types([V|Vs],P,[V:T|Ts]) :-
    guess_var_type(V,P,T),
    guess_var_types(Vs,P,Ts).

guess_var_type(V,P,T) :-
    (guess_var_type_(V,P,T2) -> T=T2 ; T=object), !.

guess_var_type_(V,P,T) :-
    is_atom(P), P \= (_=_), P \= (_\=_),
    contains_var(P,V:T),
    P =.. [F|FArgs], length(FArgs,NumArgs),
    length(FTypes,NumArgs), P2 =.. [F|FTypes],
    prim_fluent(P2),
    guess_var_type_list(V,FArgs,FTypes,T).
guess_var_type_(V,P1 & P2,T) :-
    guess_var_type_(V,P1,T) ; guess_var_type_(V,P2,T).
guess_var_type_(V,P1 | P2,T) :-
    guess_var_type_(V,P1,T) ; guess_var_type_(V,P2,T).
guess_var_type_(V,P1 => P2,T) :-
    guess_var_type_(V,P1,T) ; guess_var_type_(V,P2,T).
guess_var_type_(V,P1 <=> P2,T) :-
    guess_var_type_(V,P1,T) ; guess_var_type_(V,P2,T).
guess_var_type_(V,~P,T) :-
    guess_var_type_(V,P,T).
guess_var_type_(V,?(_:P),T) :-
    guess_var_type_(V,P,T).
guess_var_type_(V,!(_:P),T) :-
    guess_var_type_(V,P,T).
guess_var_type_(V,knows(_,P),T) :-
    guess_var_type_(V,P,T).
guess_var_type_(V,pknows(_,P),T) :-
    guess_var_type_(V,P,T).

guess_var_type_list(_,[],[],_) :- fail.
guess_var_type_list(V,[Ah|At],[Th|Tt],T) :-
    ( V == Ah ->
        T = Th
    ;
        guess_var_type_list(V,At,Tt,T)
    ).















































%
%  epath.pl:  create and manipulate epistemic path terms.
%
%  Copyright 2008, Ryan Kelly
%
%  Syntax for epistemic paths is:
%
%    A           - primitive agent name
%    E1 ; E2     - sequence
%    E1 | E2     - choice
%    ?(C)        - test
%    E*          - iteration
%    !(X:T)      - nondet variable rebind with given type
%    -[X:V|..]   - variable assignment
%

%
%  epath_not_refuted_values(E,VVIn,VVOut)  -  determine possible var values
%
%  If path E is entered with variable bindings from the set VVIn, then
%  it will terminate with bindings from the set VVOut.
%  Each of these is a list mapping vars to a list of possible values, e.g.
%  [X:[a,b,c], Y:[f,g]].  Vars not in the mapping are allowed to take on
%  any value.
%
%  Since this is a propositional domain, we can handle the star operator
%  as a simple fixpoint calculation and be sure it will eventually terminate.
%

epath_not_refuted_values(A,VV,VV) :-
    agent(A).
epath_not_refuted_values(?(P),VVIn,VVOut) :-
    epath_not_refuted_values_test(VVIn,[],P,VVOut),
    vv_valid(VVOut).
epath_not_refuted_values(!(X:T),VVIn,VVOut) :-
    bagof(Val,call(T,Val),Vals),
    vv_update(VVIn,X,Vals,VVOut).
epath_not_refuted_values(-[],VVIn,VVIn).
epath_not_refuted_values(-[X:V|Xs],VVIn,VVOut) :-
    vv_update(VVIn,X,[V],VV2),
    epath_not_refuted_values(-Xs,VV2,VVOut).
epath_not_refuted_values(E1 ; E2,VVIn,VVOut) :-
    epath_not_refuted_values(E1,VVIn,VV2),
    epath_not_refuted_values(E2,VV2,VVOut).
epath_not_refuted_values(E1 | E2,VVIn,VVOut) :-
    ( epath_not_refuted_values(E1,VVIn,VV1) ->
        ( epath_not_refuted_values(E2,VVIn,VV2) ->
            vv_merge(VV1,VV2,VVOut)
        ;
            VVOut = VV1
        )
    ;
        epath_not_refuted_values(E2,VVIn,VVOut)
    ).
epath_not_refuted_values(E*,VVIn,VVOut) :-
    epath_not_refuted_values(E,VVIn,VV2),
    ( VV2 = VVIn ->
        VVOut = VV2
    ;
        epath_not_refuted_values(E*,VV2,VVOut)
    ).


epath_not_refuted_values_test([],_,_,[]).
epath_not_refuted_values_test([X:Vs|Xs],Sofar,P,VVOut) :-
    partition(epath_not_refuted_values_allowed(P,X,Xs,Sofar),Vs,Vs1,_),
    VVOut = [X:Vs1|VVOut2],
    Sofar2 = [X:Vs1|Sofar],
    epath_not_refuted_values_test(Xs,Sofar2,P,VVOut2).

epath_not_refuted_values_allowed(P,X,Others1,Others2,V) :-
    epath_not_refuted_values_allowed_sub1(P,Others1,P1),
    epath_not_refuted_values_allowed_sub1(P1,Others2,P2),
    subs(X,V,P2,P3),
    simplify(P3,P4),
    P4 \= false.

epath_not_refuted_values_allowed_sub1(P,[],P).
epath_not_refuted_values_allowed_sub1(P,[X:Vs|Xs],P2) :-
    member(Val,Vs),
    subs(X,Val,P,P1),
    epath_not_refuted_values_allowed_sub1(P1,Xs,P2).
    

vv_valid([]).
vv_valid([_:Vs|Xs]) :-
    Vs \= [],
    vv_valid(Xs).

vv_update([],X,Vs,[X:Vs]).
vv_update([X1:Vs1|Xs],X2,Vs2,Res) :-
    ( X1 == X2 ->
        Res = [X1:Vs2|Xs]
    ;
        Res = [X1:Vs1|Res2],
        vv_update(Xs,X2,Vs2,Res2)
    ).

vv_merge([],VV2,VV2).
vv_merge([X:Vs|Xs],VV1,Res) :-
    vv_merge1(VV1,X,Vs,VV2),
    vv_merge(Xs,VV2,Res).

vv_merge1([],X,Vs,[X:Vs]).
vv_merge1([X1:Vs1|Xs],X2,Vs2,Res) :-
    ( X1 == X2 ->
        union(Vs1,Vs2,VsU),
        Res = [X1:VsU|Xs]
    ;
        Res = [X1:Vs1|Res2],
        vv_merge1(Xs,X2,Vs2,Res2)
    ).



%
%  epath_enum_vars(E,En)  -  enumerate all possible values of each path 
%                            variable in the epath, reducing it from FODL
%                            to VPDL and hence making it decidable
%
%  We expand any unions produced during enumeration over sequence operators,
%  in the hope that each branch will simplify given the new variable bindings.
%  We try to push var assignments as far to the right as possible, doing subs
%  into tests and simplifying.
%

epath_enum_vars(E1 ; E2,En) :-
    epath_enum_vars(E1,En1),
    flatten_op('|',[En1],Ens1),
    epath_enum_vars(E2,En2),
    flatten_op('|',[En2],Ens2),
    epath_enum_vars_branches(Ens1,Ens2,Ens),
    epath_build('|',Ens,En).
epath_enum_vars(E1 | E2,En) :-
    flatten_op('|',[E1,E2],Es),
    maplist(epath_enum_vars,Es,Ens),
    epath_build('|',Ens,En).
epath_enum_vars(E*,En) :-
    epath_enum_vars(E,EnS),
    epath_build('*',EnS,En).
epath_enum_vars(?(P),?(P)).
epath_enum_vars(-VA,-VA).
epath_enum_vars(!(X:T),En) :-
    bagof(VA,V^(call(T,V),VA=(-[X:V])),VAs),
    epath_build('|',VAs,En).
epath_enum_vars(A,A) :-
    agent(A).
    

epath_enum_vars_branches([],_,[]).
epath_enum_vars_branches([B|Bs],Es,[R|Rs]) :-
    ( epath_ends_with_assign(B,VA,Head) ->
        epath_enum_vars_branches_assign(Es,VA,Head,[],R)
    ;
        epath_enum_vars_branches_noassign(Es,B,[],R)
    ),
    epath_enum_vars_branches(Bs,Es,Rs).


epath_enum_vars_branches_assign([],_,_,Acc,R) :-
    joinlist('|',Acc,R).
epath_enum_vars_branches_assign([E|Es],VA,B,Acc,R) :-
    epath_push_assign(E,VA,Ea),
    epath_build(';',[B,Ea],R1),
    epath_enum_vars_branches_assign(Es,VA,B,[R1|Acc],R).

epath_enum_vars_branches_noassign([],_,Acc,R) :-
    joinlist('|',Acc,R).
epath_enum_vars_branches_noassign([E|Es],B,Acc,R) :-
    epath_build(';',[B,E],R1),
    epath_enum_vars_branches_noassign(Es,B,[R1|Acc],R).
 
%
%  epath_ends_with_assign(E,VA,Head)  -  epath ends with a variable assignment
%
%  This predicate is true when E ends with a unique variable assignment
%  operator.  VA is bound to the assignment and Head is the remainder
%  of the path.
%
epath_ends_with_assign(E1 ; E2,VA,Head) :-
    epath_ends_with_assign(E2,VA2,Head2),
    ( Head2 = (?true) ->
        ( epath_ends_with_assign(E1,VA1,Head1) ->
            Head = Head1, vassign_merge(VA1,VA2,VA)
        ;
            Head = E1, VA=VA2
        )
    ;
        Head = (E1 ; Head2), VA=VA2
    ).
epath_ends_with_assign(E1 | E2,VA,Head) :-
    epath_ends_with_assign(E1,VA,Head1),
    epath_ends_with_assign(E2,VA2,Head2),
    VA == VA2,
    Head = (Head1 | Head2).
epath_ends_with_assign(-(VA),VA,?true).


%
%  epath_push_assign(E,VA,Ep)  -  push a variable assignment as far to the
%                                 right as possible.
%
%  This may involve, for example, pushing it over a test operator and
%  substituting the assigned values into the test formula.
%
epath_push_assign(E1 ; E2,VA,Ep) :-
    epath_push_assign(E1,VA,Ep1),
    ( epath_ends_with_assign(Ep1,VA2,Head) ->
        epath_push_assign(E2,VA2,Ep2),
        epath_build(';',[Head,Ep2],Ep)
    ;
        epath_build(';',[Ep1,E2],Ep)
    ).
epath_push_assign(E1 | E2,VA,Ep) :-
    epath_push_assign(E1,VA,Ep1),
    epath_push_assign(E2,VA,Ep2),
    epath_build('|',[Ep1,Ep2],Ep).
epath_push_assign(E*,VA,(-VA) ; (E*)).
epath_push_assign(!(X:T),VA,Ep) :-
    ( vassign_contains(VA,X) ->
        Ep = (-VA ; !(X:T))
    ;
        Ep = (!(X:T) ; -VA)
    ).
epath_push_assign(-VA2,VA,-VAm) :-
    vassign_merge(VA2,VA,VAm).
epath_push_assign(?(P),VA,?(Q) ; -VA) :-
    vassign_subs(VA,P,Q1),
    simplify(Q1,Q).
epath_push_assign(A,VA,A ; -VA) :-
    agent(A).


%
%  Predicates for manipulating a variable assignment list
%
vassign_merge([],VA,VA).
vassign_merge([(X:V)|Xs],VA2,VA) :-
    vassign_insert(X,V,VA2,VAt),
    vassign_merge(Xs,VAt,VA).

vassign_insert(X,V,[],[X:V]).
vassign_insert(X,V,[(X2:V2)|Xs],VA) :-
    ( X == X2 ->
        VA = [(X2:V2)|Xs]
    ;
        vassign_insert(X,V,Xs,VAs),
        VA = [(X2:V2)|VAs]
    ).

vassign_contains([(X:_)|Xs],Y) :-
    X == Y ; vassign_contains(Xs,Y).

vassign_subs([],P,P).
vassign_subs([(X:V)|Xs],P,Q) :-
    subs(X,V,P,P2),
    vassign_subs(Xs,P2,Q).


%
%  epath_vars(E,Vars)  -  find all path variables (as opposed to formula-level
%                         variables) in the given epistemic path.
%
epath_vars(E1 ; E2,Vars) :-
    epath_vars(E1,Vars1),
    epath_vars(E2,Vars2),
    epath_vars_union(Vars1,Vars2,Vars), !.
epath_vars(E1 | E2,Vars) :-
    epath_vars(E1,Vars1),
    epath_vars(E2,Vars2),
    epath_vars_union(Vars1,Vars2,Vars), !.
epath_vars(E*,Vars) :-
    epath_vars(E,Vars), !.
epath_vars(!(X:T),[X:T]) :- !.
epath_vars(?(_),[]) :- !.
epath_vars(-VA,VA) :- !.
epath_vars(A,[]) :-
    agent(A).

epath_vars_union([],Vars,Vars).
epath_vars_union([X:T|Vars1],Vars2,Vars) :-
    (ismember(X:T,Vars2) ->
        epath_vars_union(Vars1,Vars2,Vars)
    ;
        Vars = [X:T|VarsT],
        epath_vars_union(Vars1,Vars2,VarsT)
    ).

%
%  epath_build(Op,Args,EPath)  -  build an epath, with simplification
%
%  This predicate builds an epath, applying simplifications appropriate
%  to the top-level operator but assuming all argument paths are already
%  simplified.
%

epath_build('|',Es,E) :-
    flatten_op('|',Es,Es0),
    partition('='(?false),Es0,_,Es1),
    simplify_epath_choice_subsumes(Es1,Es2),
    simplify_epath_choice_union(Es2,Es3),
    ( Es3 = [] ->
        E = (?false)
    ;
        joinlist('|',Es3,E)
    ).

epath_build(';',Es,E) :-
    flatten_op(';',Es,Es0),
    ( member(?false,Es0) -> 
        E = (?false)
    ;
        partition('='(?true),Es0,_,Es1),
        ( Es1 = [] ->
            E = (?true)
        ;
            simplify_epath_seq_combine(Es1,Ss),
            ( member(?false,Ss) ->
                E = (?false)
            ;
                joinlist(';',Ss,E)
            )
        )
    ).

epath_build('*',E,Eb) :-
    simplify_star_contents(E,E1),
    ( E1 = (?(P)) ->
        Eb = (?(P))
    ;
        Eb = (E1*)
    ).


%
%  simplify_epath  -  simplify an epistemic path
%
%  we can do this by recursively stripping off the outermost operator,
%  simplifying the argument paths, then apply epath_build.
%
simplify_epath(X,_) :-
    var(X), !, throw(cant_simplify_a_free_epath).
simplify_epath(A,A) :-
    agent(A).
simplify_epath(E1 ; E2,Es) :-
    flatten_op(';',[E1,E2],Eseq),
    maplist(simplify_epath,Eseq,Esimp),
    epath_build(';',Esimp,Es).
simplify_epath(E1 | E2,Es) :-
    flatten_op('|',[E1,E2],Eseq),
    maplist(simplify_epath,Eseq,Esimp),
    epath_build('|',Esimp,Es).
simplify_epath(E*,Es) :-
    simplify_epath(E,E1s),
    epath_build('*',E1s,Es).
simplify_epath(?(P),?(S)) :-
    simplify(P,S).
simplify_epath(!(X:T),!(X:T)).



epath_elim_impossible_branches(A,_,A) :-
    agent(A).
epath_elim_impossible_branches(?(P),VVPoss,?(P1)) :-
    ( epath_not_refuted_values(?(P),VVPoss,_) ->
        P1 = P
    ;
        P1 = false
    ).
epath_elim_impossible_branches(!(X:T),_,!(X:T)).
epath_elim_impossible_branches(-VA,_,-VA).
epath_elim_impossible_branches(E1 ; E2,VVPoss,Er) :-
    ( epath_not_refuted_values(E1,VVPoss,VV2) ->
        epath_elim_impossible_branches(E1,VVPoss,Er1),
        epath_elim_impossible_branches(E2,VV2,Er2),
        (Er1 = ?false ->
            Er = ?false
        ; Er2 = ?false ->
            Er = ?false
        ;
            Er = (Er1 ; Er2)
        )
    ;
        Er = (?false)
    ).
epath_elim_impossible_branches(E1 | E2,VVPoss,Er) :-
    epath_elim_impossible_branches(E1,VVPoss,Er1),
    epath_elim_impossible_branches(E2,VVPoss,Er2),
    (Er1 = ?false ->
       Er = Er2
    ; Er2 = ?false ->
       Er = Er1
    ;
       Er = (Er1 | Er2)
    ).
epath_elim_impossible_branches(E*,VVPoss,Er) :-
    ( epath_not_refuted_values(E*,VVPoss,VV2) ->
        epath_elim_impossible_branches(E,VV2,E2),
        (E2 = ?false ->
            Er = ?false
        ;
            Er = (E2*)
        )
    ;
        Er = (?false)
    ).

%
%  Simplification for operations within a star.
%
simplify_star_contents(E1,E2) :-
    ( simplify_star_contents1(E1,Es) ->
        simplify_star_contents(Es,E2)
    ;
        E2 = E1
    ).

simplify_star_contents1(E*,E).

% Any choices within a star that are simply ?true can be removed,
% as we always have the option of staying in current state.
simplify_star_contents1(E1 | E2,Ep) :-
    flatten_op('|',[E1,E2],Es),
    partition('='(?true),Es,Ts,Es2),
    length(Ts,N), N > 0,
    joinlist('|',Es2,Ep).
%
%  Flatten stars in (B1* | (B2* | B3*)*)* 
simplify_star_contents1(E,Ep) :-
    ( E = ((B1*) | (((B2*) | (B3*))*)) ; E = ((((B1*) | (B2*))*) | (B3*)) ) ->
    Ep = ((B1*) | (B2*) | (B3*)).

%
%  Remove choices that are subsumed by repetition of another branch
simplify_star_contents1(E1 | E2,Ep) :-
    flatten_op('|',[E1,E2],Es),
    simplify_epath_star_subsumes(Es,Ss),
    joinlist('|',Ss,Ep).

simplify_epath_star_subsumes(Es,Ss) :-
    simplify_epath_star_subsumes(Es,[],0,Ss).
 
simplify_epath_star_subsumes([],Acc,1,Acc).
simplify_epath_star_subsumes([E|Es],Acc,HaveSimpd,Ss) :-
    ( member(E2,Acc), epath_subsumes(E2*,E) ->
        simplify_epath_star_subsumes(Es,Acc,1,Ss)
    ;
        partition(epath_subsumes(E*),Acc,Rem,Acc2),
        ( Rem = [] -> NewHaveSimpd = HaveSimpd ; NewHaveSimpd = 1 ),
        simplify_epath_star_subsumes(Es,[E|Acc2],NewHaveSimpd,Ss)
    ).



%
%  simplify branches in a choice by removing any subsumed by another branch
%
simplify_epath_choice_subsumes(Es,Ss) :-
    simplify_epath_choice_subsumes(Es,[],Ss).
 
simplify_epath_choice_subsumes([],Acc,Acc).
simplify_epath_choice_subsumes([E|Es],Acc,Ss) :-
    ( member(E2,Acc), epath_subsumes(E2,E) ->
        simplify_epath_choice_subsumes(Es,Acc,Ss)
    ;
        partition(epath_subsumes(E),Acc,_,Acc2),
        simplify_epath_choice_subsumes(Es,[E|Acc2],Ss)
    ).

%
%  simplify branches in a choice by combining two branches into a single,
%  simpler branch giving their union.
%
simplify_epath_choice_union(Es,Ss) :-
    simplify_epath_choice_union(Es,[],Ss).

simplify_epath_choice_union([],Acc,Acc).
simplify_epath_choice_union([E|Es],Acc,Ss) :-
    ( (select(E1,Acc,Rest), simplify_epath_union(E,E1,Eu)) ->
        simplify_epath_choice_union([Eu|Es],Rest,Ss)
    ;
        simplify_epath_choice_union(Es,[E|Acc],Ss)
    ).


%
%  simplify_epath_seq_combine(Es,Ss)  -  simplify sequence of paths by
%                                        combining adjacent ones.
%
simplify_epath_seq_combine(Es,Ss) :-
    simplify_epath_seq_combine(Es,[],Ss).

simplify_epath_seq_combine([E],Acc,Ss) :-
    reverse([E|Acc],Ss).
simplify_epath_seq_combine([E|Es],Acc,Ss) :-
    ( simplify_epath_combine([E|Es],Es2) ->
        simplify_epath_seq_combine_recheck(Es2,Acc,Ss)
    ;
      simplify_epath_seq_combine(Es,[E|Acc],Ss)
    ).

:- index(simplify_eapth_seq_combine_recheck(0,1,0)).

simplify_epath_seq_combine_recheck(Es,[],Ss) :-
    simplify_epath_seq_combine(Es,[],Ss).
simplify_epath_seq_combine_recheck(Es,[A|Acc],Ss) :-
    ( simplify_epath_combine([A|Es],Es2) ->
        simplify_epath_seq_combine_recheck(Es2,Acc,Ss)
    ;
      simplify_epath_seq_combine(Es,[A|Acc],Ss)
    ).

%
%  epath_subsumes(E1,E2)  -  detect common cases where one epath completely
%                            subsumes another.  That is, all worlds reachable
%                            by path E2 are also reachable by path E1.
%
%  epath_subsumes/2 is det, which we ensure using cuts
%
epath_subsumes(E,E) :- !.
epath_subsumes(E*,E1*) :-
    epath_subsumes(E*,E1), !.
epath_subsumes(E*,E1) :-
    epath_subsumes(E,E1), !.
epath_subsumes(E*,E1) :-
    epath_seq_split(E1,[P1,P2],[]),
    epath_subsumes(E*,P1),
    epath_subsumes(E*,P2), !.
epath_subsumes(E,E1 | E2) :-
    epath_subsumes(E,E1),
    epath_subsumes(E,E2), !.
epath_subsumes(E1 | E2,E) :-
    (epath_subsumes(E1,E) ; epath_subsumes(E2,E)), !.
epath_subsumes(E1 ; E2,E) :-
    epath_seq_split(E,[P1,P2],[]),
    epath_subsumes(E1,P1),
    epath_subsumes(E2,P2), !.

%
%  simplify_epath_union(E1,E2,Eu)  -  simplify E1 and E2 into their union
%                                     (E1 | E2) <=> Eu
%
%  simplify_epath_combine(Es,Esc)  -    simplify E1;E2;P into Ec;P
%
%  This basically allows us to special-case a number of common forms.
%
simplify_epath_union(E1,E2,Eu) :-
    simplify_epath_union1(E1,E2,Eu)
    ;
    simplify_epath_union1(E2,E1,Eu).

%  P1 | (P1 ; P2* ; P2)   =>   P1 ; P2*
simplify_epath_union1(E1,E2,Eu) :-
    P1 = E1,
    epath_seq_split(E2,[P1,P2*,P2],[]),
    epath_build('*',P2,P2S),
    epath_build(';',[P1,P2S],Eu).
%  P1 | (P2 ; P2* ; P1)   =>   P2* ; P1
simplify_epath_union1(E1,E2,Eu) :-
    P1 = E1,
    epath_seq_split(E2,[P2,P2*,P1],[]),
    epath_build('*',P2,P2S),
    epath_build(';',[P2S,P1],Eu).
%  P1 | (P2* ; P2 ; P1)   =>   P2* ; P1
simplify_epath_union1(E1,E2,Eu) :-
    P1 = E1,
    epath_seq_split(E2,[P2*,P2,P1],[]),
    epath_build('*',P2,P2S),
    epath_build(';',[P2S,P1],Eu).
% ?P1 | ?P2   =>   ?(P1 | P2)
simplify_epath_union1(?P1,?P2,?Pu) :-
    fml2cnf(P1 | P2,Pu1),
    simplify(Pu1,Pu).


% P1* ; (P2 ; (P1*))*   =>   (P1* | P2*)*
simplify_epath_combine(E,[Ec|Rest]) :-
    epath_seq_split(E,[P1*,Pr*],Rest),
    epath_seq_split(Pr,[P2,P1*],[]),
    epath_build('|',[P1*,P2*],Ec1),
    epath_build('*',Ec1,Ec).
% (P1* ; P2)* ; P1*   =>   (P1* | P2*)*
simplify_epath_combine(E,[Ec|Rest]) :-
    epath_seq_split(E,[Pr*,P1*],Rest),
    epath_seq_split(Pr,[P1*,P2],[]),
    epath_build('|',[P1,P2],Ec1),
    epath_build('*',Ec1,Ec).
% (P1* ; P2)* ; P1 ; P1*   =>   (P1* | P2*)*
simplify_epath_combine(E,[Ec|Rest]) :-
    epath_seq_split(E,[Pr*,P1,P1*],Rest),
    epath_seq_split(Pr,[P1*,P2],[]),
    epath_build('|',[P1,P2],Ec1),
    epath_build('*',Ec1,Ec).
% P1* ; P2*   =>   P1*   if P1 > P2
simplify_epath_combine(E,[Ec|Rest]) :-
    epath_seq_split(E,[P1*,P2*],Rest),
    ( epath_subsumes(P1,P2), Ec = P1
    ; epath_subsumes(P2,P1), Ec = P2
    ).
% ?P1 ; ?P2   =>   ?(P1 & P2)
simplify_epath_combine(E,[?(Pc)|Rest]) :-
    epath_seq_split(E,[?P1,?P2],Rest),
    fml2cnf(P1 & P2,Pc1),
    simplify(Pc1,Pc).

%
%  epath_seq_split(E,Seqs,Rest)  -  nondeterminstically split sequence of ops
%
%  This predicate nondeterministically splits a series of sequence operators
%  into patterns that match the elements of the list Seqs.  Each free var in
%  Seqs may be given one or more elements from the sequence, while each
%  any entries in Seq that are nonvar will be unified with precisely one
%  element.
%
:- index(epath_seq_split(1,1,0)).

epath_seq_split(E1 ; E2,Seqs,Rest) :-
    flatten_op(';',[E1,E2],Es),
    epath_seq_split_prep(Seqs,Preps),
    epath_seq_split(Es,Preps,Rest),
    epath_seq_split_unprep(Preps,Seqs).

epath_seq_split([E|Es],Seqs,Rest) :-
    epath_seq_split([E|Es],[],Seqs,Rest).

epath_seq_split(Rest,[],[],Rest).
epath_seq_split(Rest,[S-Vs|Todo],[],Rest) :-
    reverse(Vs,VsR),
    ( var(S) ->
        S = VsR
    ;
        joinlist(';',VsR,S)
    ),
    epath_seq_split(Rest,Todo,[],Rest).
epath_seq_split([E|Es],Todo,[S|Seqs],Rest) :-
    ( var(S) ->
        ((Todo = [S2-Vs|Todo2], S2 == S) ->
            (epath_seq_split([E|Es],Todo,Seqs,Rest)
            ;
            epath_seq_split(Es,[S-[E|Vs]|Todo2],[S|Seqs],Rest))
        ;
            epath_seq_split(Es,[S-[E]|Todo],[S|Seqs],Rest)
        )
    ;
      epath_seq_split_unify(S,[E|Es],Es2), epath_seq_split(Es2,Todo,Seqs,Rest)
    ).
epath_seq_split([],Todo,[S],Rest) :-
    var(S), Todo = [S2-_|_], S2 == S, 
    epath_seq_split([],Todo,[],Rest).


epath_seq_split_unify(P,[E|Es],Es) :-
    var(P), P=E, !.
epath_seq_split_unify(P1 ; P2,Es,Es2) :-
    epath_seq_split_unify(P1,Es,Es1),
    epath_seq_split_unify(P2,Es1,Es2), !.
epath_seq_split_unify(E,[E|Es],Es).

epath_seq_split_prep([],[]).
epath_seq_split_prep([S|Seqs],[P|Preps]) :-
    (var(S) -> true ; S=P ),
    epath_seq_split_prep(Seqs,Preps).
epath_seq_split_unprep([],[]).
epath_seq_split_unprep([P|Preps],[S|Seqs]) :-
    ( P = [_|_] -> joinlist(';',P,S) ; P=S ),
    epath_seq_split_unprep(Preps,Seqs).

    

%
%  copy_epath(EIn,EOut)  -  copy an epath, renaming path variables
%
%  This produces a copy of EIn with all path variables replaced by
%  fresh variables.  All free formula variables remain unchanged, while
%  all bound formula variables are also renamed.
%
copy_epath(E,Ec) :-
    epath_vars(E,EVarsT),
    maplist(arg(1),EVarsT,EVars),
    term_variables(E,TVars),
    vdelete_list(TVars,EVars,FVars),
    copy_term(E^FVars,E2^FVars2),
    FVars2=FVars,
    copy_epath_fmls(E2,Ec).

copy_epath_fmls(E1 ; E2,E1c ; E2c) :-
    copy_epath_fmls(E1,E1c),
    copy_epath_fmls(E2,E2c).
copy_epath_fmls(E1 | E2,E1c | E2c) :-
    copy_epath_fmls(E1,E1c),
    copy_epath_fmls(E2,E2c).
copy_epath_fmls(E*,Ec*) :-
    copy_epath_fmls(E,Ec).
copy_epath_fmls(?(P),?(Pc)) :-
    copy_fml(P,Pc).
copy_epath_fmls(!(V:T),!(V:T)).
copy_epath_fmls(-VA,-VA).
copy_epath_fmls(A,A) :-
    agent(A).


%
%  pp_epath(E)  -  pretty-print an epistemic path
%

pp_epath(E) :-
    pp_epath(E,0,0).

pp_epath_list([E],_,_,O1,D1) :-
    pp_epath(E,O1,D1).
pp_epath_list([E1,E2|Es],Op,D,O1,D1) :-
    pp_epath(E1,O1,D1), nl,
    pp_inset(D), write(Op), nl,
    pp_epath_list([E2|Es],Op,D,D1,D1).


pp_epath(E1 ; E2,O,D) :-
    flatten_op(';',[E1,E2],Es),
    D1 is D + 1,
    O1 is O + 1,
    pp_epath_list(Es,';',D,O1,D1).
pp_epath(E1 | E2,O,D) :-
    flatten_op('|',[E1,E2],Es),
    D1 is D + 1,
    O1 is O + 1,
    pp_epath_list(Es,'|',D,O1,D1).
pp_epath(?(P),O,D) :-
    D1 is D + 1,
    pp_inset(O), write('?  '),
    pp_fml(P,0,D1).
pp_epath(!(V:T),O,_) :-
    pp_inset(O), write('!  '), write(V:T).
pp_epath(-VA,O,_) :-
    pp_inset(O), write('-  '), pp_epath_assign(VA).
pp_epath(E*,O,D) :-
    D1 is D + 1,
    pp_inset(O), write('*'), nl,
    pp_epath(E,D1,D1).
pp_epath(A,O,_) :-
    agent(A),
    pp_inset(O), write(A).


pp_epath_assign([]).
pp_epath_assign([(X:V)|Xs]) :-
    write(X), write(' <= '), write(V), write(',  '),
    pp_epath_assign(Xs).


%
%  twb_vpdl.pl:  interface to our VPDL prover built using Tableaux Workbench
%
%  Copyright 2008, Ryan Kelly
%
%  The main predicate we export is entails/2, which shells out to a stand-alone
%  prover for "PDL-plus-variable-assignment".
%

twb_pdl_exe('vpdl/vpdl.twb').

entails(Axs,Conc) :-
    maplist(copy_term,Axs,Axs2),
    copy_term(Conc,Conc2),
    twb_pdl_prove(Axs2,Conc2,yes).

twb_pdl_prove(Axioms,Conc,Result) :-
    % Create input/output files
    tmp_file(twb_in,InFile),
    tmp_file(twb_out,OutFile),
    tell(InFile),
    % Universally quantify all free variables in conc
    free_vars(Conc,ConcVars),
    ( ConcVars = [] ->
          Conc2 = Conc
    ;
          guess_var_types(ConcVars,Conc,TypedVars),
          Conc2 = !(TypedVars : Conc)
    ),
    % Write out (Axioms) -> (Conjecture)
    write('('),
    twb_write_axioms(Axioms), !,
    write(') -> ('),
    twb_write(Conc2), write(').'), nl,
    told, !,
    % Call TWB and have it write its conclusions into output file
    twb_pdl_exe(CmdFile),
    sformat(PCmd,'~w < ~w > ~w 2>&1',[CmdFile,InFile,OutFile]),
    shell(PCmd,_),
    % Grep output file for "Result:Closed" indicating truthity
    sformat(TCmd,'grep "Result:Closed" ~w > /dev/null',[OutFile]),
    ( shell(TCmd,0) ->
        Result = yes
    ;
        % Check for any fatal errors, and report them
        sformat(ErrCmd,'grep "Fatal" ~w > /dev/null',[OutFile]),
        ( shell(ErrCmd,0) ->
            throw(badly_formed_twb_file(InFile))
        ;
            % Propositional prover, so we cannot get Result=unknown
            Result = no
        )
    ).


%
%  twb_write(P)  -  write formula in TWB PDL format
%

twb_write(P) :-
    twb_write_fml(P), !.

twb_write_fml(true) :-
    write(' (Verum) '), !.
twb_write_fml(false) :-
    write(' (Falsum) '), !.
twb_write_fml(A=B) :-
    write('( '),
    twb_write_term(A),
    write(' == '),
    twb_write_term(B),
    write(' )'), !.
twb_write_fml(A\=B) :-
    write('~ ( '),
    twb_write_term(A),
    write(' == '),
    twb_write_term(B),
    write(' )'), !.
twb_write_fml(P) :-
    is_atom(P),
    twb_write_pred(P).
twb_write_fml(P & Q) :-
    write('('),
    twb_write_fml(P),
    write(' & '),
    twb_write_fml(Q),
    write(')').
twb_write_fml(P | Q) :-
    write('('),
    twb_write_fml(P),
    write(' v '),
    twb_write_fml(Q),
    write(')').
twb_write_fml(P => Q) :-
    write('('),
    twb_write_fml(P),
    write(' -> '),
    twb_write_fml(Q),
    write(')').
twb_write_fml(P <=> Q) :-
    write('('),
    twb_write_fml(P),
    write(' <-> '),
    twb_write_fml(Q),
    write(')').
twb_write_fml(~P) :-
    write('~ ('),
    twb_write_fml(P),
    write(')').
twb_write_fml(!([]:P)) :-
    twb_write_fml(P).
twb_write_fml(!([V:T|Vs]:P)) :-
    write('( '),
    twb_write_fml_sols(twb_valuate_var(T,!(Vs:P)),V,'&',true),
    write(' )'), flush.
twb_write_fml(?([]:P)) :-
    twb_write_fml(P).
twb_write_fml(?([V:T|Vs]:P)) :-
    write('( '),
    twb_write_fml_sols(twb_valuate_var(T,?(Vs:P)),V,'v',false),
    write(' )'), flush.
twb_write_fml(knows(A,P)) :-
    write('( ['),
    write(A),
    write('] ('),
    twb_write_fml(P),
    write('))').
twb_write_fml(pknows0(E,P)) :-
    epath_vars(E,Vars),
    epath_enum_vars(E,En1),
    epath_elim_impossible_branches(En1,[],En),
    ( En = (?false) ->
        write(' (Verum) ')
    ; En = (?true) ->
        twb_write_fml(P)
    ;
        write('( ['),
        number_vars(Vars),
        twb_write_path(En),
        write('] ('),
        twb_write_fml(P),
        write('))')
    ).


twb_valuate_var(T,Fml,V,Res) :-
    call(T,Val), subs(V,Val,Fml,Res1),
    copy_fml(Res1,Res2),
    simplify(Res2,Res).

twb_write_fml_sols(Pred,Var,Sep,_) :-
    call(Pred,Var,Sol),
    twb_write_fml(Sol),
    write(' '), write(Sep), write(' '), fail.
twb_write_fml_sols(_,_,_,Final) :-
    twb_write_fml(Final).

twb_write_terms([],_).
twb_write_terms([T],_) :-
    twb_write_term(T), !.
twb_write_terms([T,T2|Ts],Sep) :-
    twb_write_term(T),
    write(Sep),
    twb_write_terms([T2|Ts],Sep).

twb_write_term(T) :-
    T =.. [F|Args],
    ( length(Args,0) ->
      write(F)
    ;
      write(F), write('_'),
      twb_write_terms(Args,'_')
    ).
    
twb_write_pred(P) :-
    P =.. [F|Terms],
    write(F),
    (Terms \= [] -> write('__'), twb_write_terms(Terms,'__') ; true).

twb_write_axioms([]) :-
    write('Verum').
twb_write_axioms([A]) :-
    twb_write_fml(A).
twb_write_axioms([A|Axs]) :-
    twb_write_fml(A), write(' & '),
    twb_write_axioms(Axs).


twb_write_path(E1 ; E2) :-
    write('('),
    twb_write_path(E1),
    write(' ; '),
    twb_write_path(E2),
    write(')').
twb_write_path(E1 | E2) :-
    write('('),
    twb_write_path(E1),
    write(' U '),
    twb_write_path(E2),
    write(')').
twb_write_path(?(P)) :-
    write('( ? '),
    twb_write_fml(P),
    write(' )').
twb_write_path(E*) :-
    write('( * '),
    twb_write_path(E),
    write(' )').
twb_write_path(-VA) :-
    ( VA = [] ->
      twb_write_path(?true)
    ;
      write('( '),
      twb_write_vassign(VA),
      write(' )')
    ).
twb_write_path(A) :-
    agent(A),
    write(A).

twb_write_vassign([]).
twb_write_vassign([(X:V)|Xs]) :-
    write('! '), write(X), write(' <= '), twb_write_term(V), write(' '),
    twb_write_vassign(Xs).
    
    

%
%  action_with_vars(A,Vs)  -  get an action term with variable arguments
%
%  This predicate binds A to an action term with all arguments set to
%  variables, and Vs to a matching variable list.
%
action_with_vars(A,Vs) :-
    prim_action(At),
    At =.. [F|ArgTypes],
    awv_collect(ArgTypes,Args,Vs),
    A =.. [F|Args].

awv_collect([],[],[]).
awv_collect([T|Ts],[Y|Ys],[Y:T|Vs]) :-
    awv_collect(Ts,Ys,Vs).


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

