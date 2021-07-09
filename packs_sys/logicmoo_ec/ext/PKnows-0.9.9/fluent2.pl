%
%  fluent.pl:  predicates for manipulating fluent terms
%
%  Copyright 2008-2014, Ryan Kelly
%
%  This file supplies some basic predicates for manipulating and reasoning
%  about reified fluent terms.  It relies on prover.pl for the core reasoning
%  engine.
%
%  Variables are expected to be actual prolog variables, as this vastly
%  increases the simplicity and efficiency of certain operations.  It also
%  means we need to take extra care in some other areas.  In particular,
%  we assume that the formula contains the *only* references to those
%  variables, so we are free to bind them in simplification and reasoning.
%

%
%  normalize(F,N) - perform some basic normalisations on a formula
%
%  This is designed to make formulae easier to reason about.  It
%  re-arranges orderless terms into a standard order, for example
%  the arguments to '=' and the list of variables in a quantification.
%  It also simplifies away some trivial tautologies.
% 

normalize((A=B), (A=B)) :-
  A @< B, !.
normalize((A=B), (B=A)) :-
  B @< A, !.
normalize((A=B), true) :-
  A == B, !.
normalize(ext(X, P), ext(Y, Q)) :-
  sort(X, Y),
  normalize(P, Q), !.
normalize(all(X, P), all(Y, Q)) :-
  sort(X, Y),
  normalize(P, Q), !.
normalize(~P, ~Q) :-
  normalize(P, Q), !.
normalize((P1 & Q1), (P2 & Q2)) :-
  normalize(P1, P2),
  normalize(Q1, Q2), !.
normalize((P1 | Q1), (P2 | Q2)) :-
  normalize(P1, P2),
  normalize(Q1, Q2), !.
normalize((P1 => Q1), (P2 => Q2)) :-
  normalize(P1, P2),
  normalize(Q1, Q2), !.
normalize((P1 <=> Q1), (P2 <=> Q2)) :-
  normalize(P1, P2),
  normalize(Q1, Q2), !.
normalize(knows(A, P), knows(A, Q)) :-
  normalize(P, Q), !.
normalize(P, P). 

%
%  struct_equiv(P, Q)  -  two formulae are structurally equivalent,
%                         basically meaning they are identical up
%                         to renaming of variables.
%
%  struct_oppos(P, Q)  -  two formulae are structurally opposed,
%                         making their conjunction a trivial falsehood.
%

struct_equiv(P, Q) :-
  is_atom(P), is_atom(Q), P == Q.
struct_equiv(P1 & P2, Q1 & Q2) :-
  struct_equiv(P1, Q1),
  struct_equiv(P2, Q2).
struct_equiv(P1 | P2, Q1 | Q2) :-
  struct_equiv(P1, Q1),
  struct_equiv(P2, Q2).
struct_equiv(P1 => P2, Q1 => Q2) :-
  struct_equiv(P1, Q1),
  struct_equiv(P2, Q2).
struct_equiv(P1 <=> P2, Q1 <=> Q2) :-
  struct_equiv(P1, Q1),
  struct_equiv(P2, Q2).
struct_equiv(~P, ~Q) :-
  struct_equiv(P, Q).
struct_equiv(ext([], P), ext([], Q)) :-
  struct_equiv(P,Q).
struct_equiv(ext([V1|Vs1], P), ext([V2|Vs2], Q)) :-
  subs(V1, V2, P, P1),
  struct_equiv(ext(Vs1, P1), ext(Vs2, Q)).
struct_equiv(all([], P), all([], Q)) :-
  struct_equiv(P, Q).
struct_equiv(all([V1|Vs1], P), all([V2|Vs2], Q)) :-
  subs(V1, V2, P, P1),
  struct_equiv(all(Vs1, P1), all(Vs2, Q)).
struct_equiv(knows(A, P), knows(A, Q)) :-
  struct_equiv(P, Q).

struct_oppos(P, Q) :-
  P = ~P1, struct_equiv(P1, Q) -> true
  ;
  Q = ~Q1, struct_equiv(P, Q1) -> true
  ;
  P=true, Q=false -> true
  ;
  P=false, Q=true.


%
%  fml_compare  -  provide a standard order over formula terms
%
%  This allows them to be sorted, have duplicates removed, etc.
%  Formula should be normalised before this is applied.
%
fml_compare('=', F1, F2) :-
  struct_equiv(F1, F2), !.
fml_compare(Ord, F1, F2) :-
  ( F1 @< F2 ->
      Ord = '<'
  ;
      Ord = '>'
  ).


%
%  contains_var(A, V)  -  formula A contains variable V
%
%  ncontains_var(A, V)  -  formula A does not contain variable V
%
%
%  Since we know that V is a variable, we do this test by making a copy,
%  grounding the copied variable, then checking for structural equivalence
%  with the original term.
%
contains_var(A,V) :-
  copy_term(A:V,A2:V2),
  V2=groundme,
  A \=@= A2.

ncontains_var(A,V) :-
  copy_term(A:V,A2:V2),
  V2=groundme,
  A =@= A2.
    

%
%  flatten_op(Op, Terms, Result) - flatten repeated ops into a list
%
%  This predicate succeeds when Result is a list of terms from Terms,
%  which were joined by the operator Op.  It basically allows a tree of
%  similar operators such as ((A & B) & (C & (D & E))) to be flattened
%  into a list (A,B,C,D,E).
%

flatten_op(_, [], []).
flatten_op(O, [T|Ts], Res) :-
  ( T =.. [O|Args] ->
    append(Args, Ts, Ts2),
    flatten_op(O, Ts2, Res)
  ;
    Res = [T|Res2],
    flatten_op(O, Ts, Res2)
  ).

%
%  flatten_quant(Q, Ts, VarsIn, VarsOut, Body) - flatten nested quantifiers
%

flatten_quant(Q, T, Acc, Vars, Body) :-
  ( T =.. [Q, Vs, T2] ->
    append(Vs, Acc, Acc2),
    flatten_quant(Q, T2, Acc2, Vars, Body)
  ;
    Vars = Acc,
    Body = T
  ).

%
%  flatten_quant_and_simp(Q, BodyIn, VarsIn, VarsOut, BodyOut)
%       - flatten nested quantifiers, and apply simplification
%
%  Just like flatten_quant/5 above, but applies simplify/2 to the body
%  when it does not match the quantifier, in case its simplification
%  does match.
%

flatten_quant_and_simp(Q, T, VarsIn, VarsOut, Body) :-
    ( T =.. [Q, V, T2] ->
      append(V, VarsIn, Vars2),
      flatten_quant_and_simp(Q, T2, Vars2, VarsOut, Body)
    ;
      simplify1(T, Tsimp),
      ( Tsimp =.. [Q, V, T2] ->
          append(V, VarsIn, Vars2),
          flatten_quant_and_simp(Q, T2, Vars2, VarsOut, Body)
      ;
          Body = Tsimp, VarsIn = VarsOut
      )
    ).

%
%  indep_of_vars(Vars, P)  -  P contains none of the vars in the list Vars,
%                             i.e. it is independent of the vars.
%

indep_of_vars(Vars, P) :-
    \+ ( member(X, Vars), contains_var(P, X) ).


%
%  simplify(+F1, -F2) - simplify a formula
%  
%  This predicate applies some basic simplification rules to a formula
%  to eliminate redundancy and (hopefully) speed up future reasoning.
%  This can be particularly important when applying regression, which
%  tends to result in a large increase in formula size.
%  

simplify(P, S) :-
    normalize(P, Nml),
    simplify1(Nml, S), !.

simplify1(P, P) :-
    is_atom(P), P \= (_ = _), P \= (_ \= _).

simplify1(P & Q, S) :-
    flatten_op('&', [P,Q], TermsT),
    simplify1_conjunction(TermsT, TermsS),
    ( TermsS = [] ->
        S = true
    ;
        % This will remove duplicates, which includes struct_equiv formulae
        predsort(fml_compare, TermsS, Terms),
        joinlist('&', Terms, S)
    ).

simplify1(P | Q, S) :-
    flatten_op('|', [P, Q], TermsT),
    simplify1_disjunction(TermsT, TermsS),
    ( TermsS = [] ->
        S = false
    ;
        % This will remove duplicates, which includes struct_equiv formulae
        predsort(fml_compare, TermsS, Terms),
        joinlist('|', Terms, S)
    ).

simplify1(P => Q, S) :-
    simplify1(P, Sp),
    simplify1(Q, Sq),
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

simplify1(P <=> Q, S) :-
    simplify1(P, Sp),
    simplify1(Q, Sq),
    (
        struct_equiv(P, Q) -> S=true
    ;
        struct_oppos(P, Q) -> S=false
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

simplify1(~P, S) :-
    simplify1(P, SP),
    (
        SP=false -> S=true
    ;
        SP=true -> S=false
    ;
        SP = ~P2 -> S=P2
    ;
        S = ~SP
    ).

simplify1(all(Xs, P), S) :-
    ( Xs = [] -> simplify1(P, S)
    ;
    flatten_quant_and_simp(all, P, Xs, VarsT, Body),
    sort(VarsT, Vars),
    (
        Body=false -> S=false
    ;
        Body=true -> S=true
    ;
        % Remove any useless variables
        partition(ncontains_var(Body), Vars, _, Vars2),
        ( Vars2 = [] ->
            S2 = Body
        ;
          % Pull independent components outside the quantifier.
          % Both conjuncts and disjuncts can be handled in this manner.
          (flatten_op('|', [Body], BTerms), BTerms = [_,_|_] -> 
            partition(indep_of_vars(Vars), BTerms, Indep, BT2),
            % Because we have removed useless vars, BT2 cannot be empty
            joinlist('|', BT2, Body2),
            ( Indep = [] ->
              S2=all(Vars2, Body2)
            ;
              joinlist('|', Indep, IndepB),
              S2=(all(Vars2, Body2) | IndepB)
            )
          
          ; flatten_op('&', [Body], BTerms), BTerms = [_,_|_] ->
            partition(indep_of_vars(Vars), BTerms, Indep, BT2),
            joinlist('&', BT2, Body2),
            ( Indep = [] ->
              S2=all(Vars2, Body2)
            ;
              joinlist('&', Indep, IndepB),
              S2=(all(Vars2, Body2) & IndepB)
            )
          ;
            S2=all(Vars2, Body)
          )
        ),
        S = S2
    )).

simplify1(ext(Xs, P), S) :-
   ( Xs = [] -> simplify1(P, S)
   ;
   flatten_quant_and_simp(ext, P, Xs, VarsT, Body),
   sort(VarsT,Vars),
   (
       Body=false -> S=false
   ;
       Body=true -> S=true
   ;
       % Remove vars that are assigned a specific value, simply replacing
       % them with their value in the Body formula
       member(V1, Vars), var_valuated(V1, Body, Body2) ->
           vdelete(Vars, V1, Vars2), simplify1(ext(Vars2, Body2), S)
   ;
       % Remove any useless variables
       partition(ncontains_var(Body), Vars, _, Vars2),
       ( Vars2 = [] ->
           S = Body
       ;
         % Pull independent components outside the quantifier,
         % Both conjuncts and disjuncts can be handled in this manner.
         (flatten_op('|', [Body], BTerms), BTerms = [_,_|_] ->
           partition(indep_of_vars(Vars), BTerms, Indep, BT2),
           joinlist('|', BT2, Body2),
           ( Indep = [] ->
             S = ext(Vars2, Body2)
           ;
             joinlist('|', Indep, IndepB),
             S = (ext(Vars2, Body2) | IndepB)
           )
         ; flatten_op('&', [Body], BTerms), BTerms = [_,_|_] ->
           partition(indep_of_vars(Vars), BTerms, Indep, BT2),
           joinlist('&', BT2, Body2),
           ( Indep = [] ->
             S = ext(Vars2, Body2)
           ;
             joinlist('&', Indep, IndepB),
             S = (ext(Vars2, Body2) & IndepB)
           )
         ;
           S = ext(Vars2, Body)
         )
       )
   )).

simplify1((A=B), S) :-
   (
       A == B -> S=true
   ;
       % Utilising the unique names assumption, we can rewrite it to
       % a conjunction of simpler equalities by striping functors, or
       % to false if the terms will never unify
       ( unifiable(A, B, Eqs) ->
           joinlist('&', Eqs, S1),
           normalize(S1, S)
       ;
           S=false
       )
   ).

simplify1(knows(A, P), S) :-
   simplify1(P, Ps),
   ( Ps=true -> S=true
   ; Ps=false -> S=false
   ; S = knows(A, Ps)
   ).


%
%  simplify1_conjunction(In,Out)  -  simplify the conjunction of list of fmls
%  simplify1_disjunction(In,Out)  -  simplify the disjunction of list of fmls
%

simplify1_conjunction(FmlsIn, FmlsOut) :-
    maplist(simplify1, FmlsIn, FmlsI1),
    simplify1_conjunction_rules(FmlsI1, FmlsI2),
    simplify1_conjunction_acc(FmlsI2, [], FmlsOut).

simplify1_conjunction_rules(ConjsI, ConjsO) :-
    % Pairwise try to apply rules for simplifying a conjunction.
    (pairfrom(ConjsI, C1, C2, Rest),
     (simplify1_conjunction_rule(C1,C2,C1o,C2o)
      ;
      simplify1_conjunction_rule(C2,C1,C2o,C1o)) ->
        simplify1_conjunction_rules([C1o,C2o|Rest],ConjsO)
      ;
        ConjsO = ConjsI
    ).
 
simplify1_conjunction_rule(C1,C2,C1,C2o) :-
    % (A & (B | ~A)) =>  (A & B)
    % XXX TODO: flatten the disjunction?
    C2 = (C2a | C2b),
    ( struct_oppos(C1,C2a), C2o=C2b
    ; struct_oppos(C1,C2b), C2o=C2a
    ).

simplify1_conjunction_rule(C1,C2,C1,true) :-
    % (A & (B | A)) =>  A
    % XXX TODO: flatten the disjunction?
    C2 = (C2a | C2b),
    ( struct_equiv(C1,C2a)
    ; struct_equiv(C1,C2b)
    ).
    

simplify1_conjunction_acc([],FmlsAcc,FmlsAcc).
simplify1_conjunction_acc([F|FmlsIn],FmlsAcc,FmlsOut) :-
    % Filter out obvious duplicates, or obvious negated-duplicates.
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
    % Pairwise try to apply some simplification rules.
    (pairfrom(DisjI,D1,D2,Rest),
     (simplify1_disjunction_rule(D1,D2,D1o,D2o)
      ; simplify1_disjunction_rule(D2,D1,D2o,D1o)) ->
          simplify1_disjunction_rules([D1o,D2o|Rest],DisjO)
      ;
          DisjO = DisjI
    ).
 
simplify1_disjunction_rule(D1,D2,D1,D2o) :-
    % A | (~A & B)  =>  A | B
    D2 = (D2a & D2b),
    ( struct_oppos(D1,D2a), D2o=D2b
    ; struct_oppos(D1,D2b), D2o=D2a
    ).
simplify1_disjunction_rule(D1,D2,D1,false) :-
    % A | (A & B)  =>  A
    D2 = (D2a & D2b),
    ( struct_equiv(D1,D2a)
    ; struct_equiv(D1,D2b)
    ).

simplify1_disjunction_acc([],FmlsAcc,FmlsAcc).
simplify1_disjunction_acc([F|FmlsIn],FmlsAcc,FmlsOut) :-
    % Filter out obvious duplicates, or obvious negated-duplicates.
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
var_given_value(X,all(Vars, P),V,all(VarsQ,Q)) :-
    var_given_value(X,P,V,Q2),
    flatten_quant(all,Q2,Vars,VarsQ,Q).
var_given_value(X,ext(Vars, P),V,ext(VarsQ,Q)) :-
    var_given_value(X,P,V,Q2),
    flatten_quant(ext,Q2,Vars,VarsQ,Q).
var_given_value(X,knows(A,P),V,knows(A,Q)) :-
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
    contains_var(F,V).

%  We can stop recursing either when Vars=[] or when we are
%  no longer at a quantifier, since we assume all relevant 
%  quantifiers have been brought to the front.
vgv_push_into_quantifiers(Vars,ext(Qv, Qj),QDep,ext(Qv,Q)) :-
    Vars \= [], !,
    vgv_subtract(Vars,Qv,Vars2),
    vgv_push_into_quantifiers(Vars2,Qj,QDep,Q).
vgv_push_into_quantifiers(Vars,all(Qv, Qj),QDep,all(Qv,Q)) :-
    Vars \= [], !,
    vgv_subtract(Vars,Qv,Vars2),
    vgv_push_into_quantifiers(Vars2,Qj,QDep,Q).
vgv_push_into_quantifiers(_,Qj,QDep,Qj & QDep).

vgv_subtract([],_,[]).
vgv_subtract(Vs,[],Vs).
vgv_subtract(Vs,[X|Xs],Vs2) :-
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
var_valuated(X,all(V,P),all(V,Q)) :-
   var_valuated(X,P,Q).
var_valuated(X,ext(V,P),ext(V,Q)) :-
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
%  copy_fml(P, Q)  -  make a copy of a formula.  The copy will have all
%                     bound variables renamed to new vars.  Any free variables
%                     will retain their original names.
%

copy_fml(P, P) :-
    var(P), !.
copy_fml(P, P) :-
    is_atom(P), !.
copy_fml(P & Q, R & S) :-
    copy_fml(P, R),
    copy_fml(Q, S).
copy_fml(P | Q, R | S) :-
    copy_fml(P, R),
    copy_fml(Q, S).
copy_fml(P => Q, R => S) :-
    copy_fml(P, R),
    copy_fml(Q ,S).
copy_fml(P <=> Q, R <=> S) :-
    copy_fml(P, R),
    copy_fml(Q, S).
copy_fml(~P, ~Q) :-
    copy_fml(P, Q).
copy_fml(all(VarsP,P), all(VarsQ,Q)) :-
    rename_vars(VarsP, P, VarsQ, P2),
    copy_fml(P2, Q).
copy_fml(ext(VarsP,P), ext(VarsQ,Q)) :-
    rename_vars(VarsP, P, VarsQ, P2),
    copy_fml(P2, Q).
copy_fml(knows(A, P), knows(A, Q)) :-
    copy_fml(P, Q).

%
%  rename_vars(Vars,F,NewVars,NewF)  -  rename the given variables to new
%                                       ones, producing a modified formula.
%

rename_vars(Vs, P, Vs2, P2) :-
    rename_vars(Vs, P, [], Vs2, P2).

rename_vars([], P, Acc, Acc, P).
rename_vars([V|Vs], P, Acc, NewV, NewP) :-
    subs(V, V2, P, P2),
    append(Acc, [V2], Acc2),
    rename_vars(Vs, P2, Acc2, NewV, NewP).


:- begin_tests(fluent,[sto(rational_trees)]).

test(simp1) :-
    simplify(p & true, p).

test(simp2) :-
    simplify(p & false, false).

test(simp3) :-
    simplify(p | false, p).

test(simp4) :-
    simplify(p | true, true).

test(simp5) :-
    simplify(false | false, false).

test(simp6) :-
    simplify(false | (p & ~(a=a)), false).

test(simp7) :-
    simplify(true & true, true).

test(simp8) :-
    simplify(all([X], p(X) & p(a)), all([X], p(X)) & p(a)).

test(simp9) :-
    simplify(ext([X], p(X) & p(a)), ext([X], p(X)) & p(a)).

test(simp10) :-
    X1 = ext([X], (
      (p & (X=nil)) |
      (q & (X=obs) & r ) |
      (ext([Y], (s(Y) & (X=pair(a,Y)))))
    )),
    X2 = (p | (q & r) | ext([Y], s(Y))),
    simplify(X1,X2).

test(val1) :-
    var_given_value(X, X=a, a, true).

test(val2) :-
    var_given_value(X, (X=a) & (X=b), b, F), simplify(F, false).

test(val3) :-
    var_given_value(X, p(X) & q(a) & ext([Y], X=Y), Val, _),
    Val == Y.

test(copy1) :-
    F1 = all([X,Y], p(X) & r(Y)),
    copy_fml(F1, F2),
    F1 =@= F2,
    F1 \== F2.

:- end_tests(fluent).
