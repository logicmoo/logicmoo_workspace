%
%  sitcalc.pl:   domain-independent sitcalc predicates.
%
%  Copyright 2008-2014, Ryan Kelly
%

:- multifile(adp_fluent/3).
:- multifile(constraint/1).


%
%  domain_prove/1       -  reason relative to the domain axioms
%  domain_prove_init/1  -  reason relative to the domain axioms and initial
%                          conditions
%

domain_prove(Axs, Fml) :-
  domain_preprocess_neg(Fml, PrepFml),
  maplist(domain_preprocess, Axs, PrepAxs),
  prove(PrepAxs, PrepFml).

domain_prove(Fml) :-
  domain_axioms(Axs),
  domain_prove(Axs, Fml).

domain_prove_init(Fml) :-
  % For the initial situation, condition the query on the initial facts.
  findall(I, initially(I), Inits),
  joinlist('&', Inits, Init),
  domain_axioms_init(Axs),
  domain_prove(Axs, Init => Fml).

domain_axioms(Axs) :-
  % The domain constraints must hold at every world,
  % i.e. they are common knowledge.
  findall(C, constraint(C), Axs).

domain_axioms_init(Axs) :-
  % For the initial situation, treat all initially-known facts as axioms,
  % i.e. they are common knowledge.
  domain_axioms(Axs1),
  findall(IK, initially_known(IK), Axs2),
  append(Axs1, Axs2, Axs).


%  Ensure that constraint/1 is defined even if the domain itself
%  doesn't specify any constraints.
constraint(true).


%
%  domain_preprocess/2  -  pre-process formula for consumption by prover.
%
%  The prover current doesn't accept existentially-quantified variables, so
%  we have to pre-process and simplify the formula to remove them.  It also
%  doesn't support skolem functions, so our only option is to enumerate the
%  possibilities and replace them with a disjunction.
%
%  This predicate also does some other useful pre-processing steps to help
%  the prover on its way:
%
%    * Expands all([X,Y] P) into all(X, all(Y, P)) as expected by the prover
%    * Replaces statically-known predicates with 'true' or 'false'
%    * Simplifies the resulting formula
%

domain_preprocess(P, Q) :-
  ( is_literal(P) ->
    % Don't pre-process literals, as this would replace statically-known
    % predicates with 'true' or 'false' and remove them from axioms list.
    Q = P
  ;
    domain_preprocess(P, pos, R),
    simplify(R, Q)
  ).

domain_preprocess_neg(P, Q) :-
  ( is_literal(P) ->
    % Don't pre-process literals, as this would replace statically-known
    % predicates with 'true' or 'false' and remove them from axioms list.
    Q = P
  ;
    domain_preprocess(P, neg, R),
    simplify(R, Q)
  ).

reverse_polarity(pos, neg).
reverse_polarity(neg, pos).
reverse_polarity(any, any).

domain_preprocess(~P, Polarity, ~Q) :-
  !,
  reverse_polarity(Polarity, RevPolarity),
  domain_preprocess(P, RevPolarity, Q).

domain_preprocess(P1 & Q1, Polarity, P2 & Q2) :-
  !,
  domain_preprocess(P1, Polarity, P2),
  domain_preprocess(Q1, Polarity, Q2).

domain_preprocess(P1 | Q1, Polarity, P2 | Q2) :-
  !,
  domain_preprocess(P1, Polarity, P2),
  domain_preprocess(Q1, Polarity, Q2).

domain_preprocess(P => Q, Polarity, R => S) :-
  !,
  reverse_polarity(Polarity, RevPolarity),
  domain_preprocess(P, RevPolarity, R),
  domain_preprocess(Q, Polarity, S).

domain_preprocess(P <=> Q, _, R <=> S) :-
  !,
  domain_preprocess(P, any, R),
  domain_preprocess(Q, any, S).

domain_preprocess(knows(A, P), Polarity, knows(A, Q)) :-
  !,  domain_preprocess(P, Polarity, Q).

% Positively-occurring existential quantifers become a disjunction.
domain_preprocess(ext([], P), Polarity, Q) :-
  !, domain_preprocess(P, Polarity, Q).
domain_preprocess(ext([X|Xs], P), neg, ext([X], Q)) :-
  !, domain_preprocess(ext(Xs, P), neg, Q).
domain_preprocess(ext([X|Xs], P), Polarity, Q) :-
  !,
  ( bagof(InstQ, domain_preprocess_enum(X, ext(Xs, P), Polarity, InstQ), Qs) ->
    joinlist('|', Qs, Q)
  ;
    Q = false
  ).

% Negatively-occurring universal quantifers become a conjunction.
domain_preprocess(all([], P), Polarity, Q) :-
  !, domain_preprocess(P, Polarity, Q).
domain_preprocess(all([X|Xs], P), pos, all([X], Q)) :-
  !, domain_preprocess(all(Xs, P), pos, Q).
domain_preprocess(all([X|Xs], P), Polarity, Q) :-
  !,
  ( bagof(InstQ, domain_preprocess_enum(X, all(Xs, P), Polarity, InstQ), Qs) ->
    joinlist('&', Qs, Q)
  ;
    Q = true
  ).
 
% Primitive fluents that are statically known can be simplified away.
domain_preprocess(P, _, Q) :-
  ( ground(P) ->
    ( constraint(P) ->
      Q = true
    ;
      ( constraint(~P) ->
        Q = false
      ;
        Q = P
      )
    )
  ;
    Q = P
  ).


% Backtrack over all instanciations of Var in the given Fml.

domain_preprocess_enum(Var, Fml, Polarity, Inst) :-
  ( infer_var_type(Var, Fml, Type) ->
    true
  ;
    write(failed_to_infer_var_type(Var, Fml)), nl, fail
  ),
  % this backtracks over the different values
  call(Type, Value),
  subs(Var, Value, Fml, Inst0),
  % this may replace statically-known facts  with 'true' or 'false'
  domain_preprocess(Inst0, Polarity, Inst1),
  % and this may simplify the resulting formulae based on the replacements
  simplify(Inst1, Inst).


% Infer the type of a variable, based on its use in the formula.

infer_var_type(V, ~P, T) :-
  !, infer_var_type(V, P, T).
infer_var_type(V, P & Q, T) :-
  !, ( infer_var_type(V, P, T) -> true ; infer_var_type(V, Q, T) ).
infer_var_type(V, P | Q, T) :-
  !, ( infer_var_type(V, P, T) -> true ; infer_var_type(V, Q, T) ).
infer_var_type(V, P => Q, T) :-
  !, ( infer_var_type(V, P, T) -> true ; infer_var_type(V, Q, T) ).
infer_var_type(V, P <=> Q, T) :-
  !, ( infer_var_type(V, P, T) -> true ; infer_var_type(V, Q, T) ).
infer_var_type(V, all(_, P), T) :-
  !, infer_var_type(V, P, T).
infer_var_type(V, ext(_, P), T) :-
  !, infer_var_type(V, P, T).
infer_var_type(V, knows(_, P), T) :-
  !, infer_var_type(V, P, T).
infer_var_type(V, P, T) :-
  P =.. [F|Args],
  findmember(V, Args, Idx),
  length(Args, N),
  length(Types, N),
  Tmplt =.. [F|Types],
  prim_fluent(Tmplt),
  nth0(Idx, Types, T).
% XXX TODO: infer by equality with another term of inferrable type?


%
%  adp_fluent(F, A, C)  -  C is the defn for ADP fluent F applied to action A
%
%  The specific ADP fluent definitions are given in domain.pl, but we give
%  a default implementation for the case when A is a variable, which simply
%  enumerates each possible action and conjoins the individual definitions.
%

adp_fluent(F, Act, Defn) :-
    var(Act), !,
    (bagof(Ft, adp_fluent_enum_actions(F, Act, Ft), Fts) ->
        joinlist(('|'), Fts, Ftmp),
        simplify(Ftmp, Defn)
    ;
        Defn = F
    ).

adp_fluent_enum_actions(F, Act, Defn) :-
    term_with_vars(prim_action, Act1, V),
    adp_fluent(F, Act1, Defn1),
    Defn = ext(V, (Defn1 & (Act=Act1))).

%
%  Useful ADPs that can be defined in terms of other, simpler ADPs
%

adp_fluent(pbu(Agt), Act, Defn) :-
    adp_fluent(poss, Act, Poss),
    adp_fluent(unobs(Agt), Act, Unobs),
    simplify(Poss & Unobs, Defn).

adp_fluent(obs(Agt, Obs), Act, Defn) :-
    %  We don't reify sets of observations as terms since that would greatly
    %  complicate the prover.  Rather we treat "obs(Agt, O, S)" as an ADP
    %  stating whether a specific observation O is observed by agent Agt in
    %  situation S.  The domain definition should enumerate all the concrete
    %  cases, and this rule closes over them to handle the general case
    %  where O is unbound.
    var(Obs), !,
    (bagof(DefnN, adp_fluent_enum_obs(obs(Agt, Obs), Act, DefnN), Defns) ->
        joinlist(('|'), Defns, Defn1),
        simplify(Defn1, Defn)
    ;
        Defn = false
    ).

adp_fluent_enum_obs(obs(Agt, Obs), Act, Defn) :-
    term_with_vars(prim_observation, Obs1, Vars),
    adp_fluent(obs(Agt, Obs1), Act, Defn1),
    Defn = ext(Vars, (Obs=Obs1) & Defn1).

adp_fluent(unobs(Agt), Act, Defn) :-
    (bagof(DefnN, adp_fluent_enum_unobs(unobs(Agt), Act, DefnN), Defns) ->
        joinlist(('|'), Defns, Defn1),
        simplify(~Defn1, Defn)
    ;
        Defn = true
    ).

adp_fluent_enum_unobs(unobs(Agt), Act, Defn) :-
    term_with_ground_args(prim_observation, Obs),
    adp_fluent(obs(Agt, Obs), Act, Defn).

%
%  regression(+F, +A, -Fr) - Fr is the regression of F over action A.
%
%  This predicate calculates the regression of a fluent F with respect to
%  an action A, yielding a new fluent Fr.  If A is free, it will consider all
%  types of action that could affect the fluent.
%

%  If A is non-free, regression1 will succeed exactly once.

regression(F, A, Fr) :-
    nonvar(A), !,
    regression1(F, A, Frt),
    simplify(Frt, Fr).

%  If A is free, find all actions which could affect F.

regression(F, A, Fr) :-
    var(A), !, 
    (bagof(Ft, A1^regression_enum_actions(F, A, A1, Ft), Fts) ->
        joinlist(('|'), Fts, Fr1),
        simplify(Fr1, Fr)
    ;
        Fr = F
    ).

regression_enum_actions(F, AVar, ATerm, Fr) :-
    term_with_vars(prim_action, ATerm, V),
    regression(F, ATerm, Fr1),
    Fr = ext(V, (Fr1 & (AVar=ATerm))).


% Regression base case, a primitive fluent.
% Build successor state axiom from causes_true/cases_false

regression1(F, A, Fr) :-
    is_atom(F), F \= (_ = _),
    (causes_true(F, A, Ep) -> true ; Ep = false),
    (causes_false(F, A, En) -> true ; En = false),
    simplify(Ep | (F & ~En), Fr).

% No functional fluents, so equality is rigid

regression1(T1=T2, _, T1=T2).

% Regression is pushed inside the logical operators

regression1(all(X, P), A, all(X, R)) :-
    regression1(P, A, R).
regression1(ext(X, P), A, ext(X, R)) :-
    regression1(P, A, R).
regression1(~P, A, ~R) :-
    regression1(P, A, R).
regression1((P & Q), A, (R & S)) :-
    regression1(P, A, R),
    regression1(Q, A, S).
regression1((P | Q), A, (R | S)) :-
    regression1(P, A, R),
    regression1(Q, A, S).
regression1((P => Q), A, (R => S)) :-
    regression1(P, A, R),
    regression1(Q, A, S).
regression1((P <=> Q), A, (R <=> S)) :-
    regression1(P, A, R),
    regression1(Q, A, S).

%  Regression of a knowledge expression.
%  For the simple finite domains we deal with here, it's better to
%  explicitly enumerate the potential observations and split by cases
%  rather than try to produce a generic knowledge expression.

regression1(knows(Agt, P), Act, Fr) :-
    % Calculate the required persistence condition.
    pcond(P, pbu(Agt), PCond),
    % Enumerate all the different sets of observations that might occur
    % as a result of this action, including the empty set.  This really
    % only works when there is a small number of potential observations...
    findall(OSet, regression1_knows_enum_obs_sets(Agt, Act, OSet), OSetL),
    % For each such set, regress assuming that's what was observed.
    maplist(regression1_knows_obs_set(Agt, P, PCond, Act), OSetL, RKnowsL),
    % The overall knowledge is the disjunction all all cases,
    % since we're expanding existential quantification over obs sets.
    joinlist('|', RKnowsL, RKnows),
    simplify(RKnows, Fr).

regression1_knows_enum_obs_sets(Agt, Act, Obs:NObs) :-
    findall(O, regression1_knows_enum_obs(Agt, Act, O), AllOs),
    regression1_knows_split_subsets(AllOs, Obs, NObs).

regression1_knows_enum_obs(Agt, Act, O) :-
    term_with_ground_args(prim_observation, O),
    adp_fluent(obs(Agt, O), Act, Defn),
    simplify(Defn, DefnS),
    DefnS \= false.

regression1_knows_split_subsets([], [], []).
regression1_knows_split_subsets([H|T], Incl, Excl) :-
  (
    Excl = [H|Rest],
    regression1_knows_split_subsets(T, Incl, Rest)
  ;
    Incl = [H|Rest],
    regression1_knows_split_subsets(T, Rest, Excl)
  ).

regression1_knows_obs_set(Agt, P, PC, Act, OSet, RKnows) :-
   % Regress knows(Agt, P) assuming that the observations set is exactly
   % as specified by Obs:NObs, i.e. the things in Obs are observed and the
   % things in NObs are not observed.
   Obs:NObs = OSet,
   % First, figure out what holds in the world to cause this observations.
   regression1_knows_obs_set_defn(Agt, Act, Obs:NObs, ObsCond),
   % Split on whether the set of observations was empty.
   ( Obs = [] ->
      % If so, then the state of knowledge must be unchanged.
      RKnows = ObsCond & knows(Agt, P)
   ;
      % If not, we need to split on each possible action that could
      % match those observations.
      (bagof(RK, regression1_knows_obs_set_enum_act(Agt, PC, OSet, RK), RKs) ->
        % The overall knowledge is the conjunction of these cases,
        % since we're expanding universal quantification over actions.
        joinlist('&', RKs, RK),
        simplify(ObsCond & RK, RKnows)
      ;
        % If there are no action that could match those observations,
        % something has gone terribly wrong with the agent's knowledge...
        RKnows = ObsCond & knows(Agt, false)
      )
   ).

regression1_knows_obs_set_defn(Agt, Act, Obs:NObs, Defn) :-
   maplist(regression1_knows_obs_defn(Agt, Act), Obs, ObsDefnL),
   maplist(regression1_knows_nobs_defn(Agt, Act), NObs, NObsDefnL),
   ( Obs = [] -> ObsDefn = true ; joinlist('&', ObsDefnL, ObsDefn) ),
   ( NObs = [] -> NObsDefn = true ; joinlist('&', NObsDefnL, NObsDefn) ),
   simplify(ObsDefn & NObsDefn, Defn).

regression1_knows_obs_defn(Agt, Act, O, Defn) :-
    ( adp_fluent(obs(Agt, O), Act, Defn) -> true ; Defn=false ).

regression1_knows_nobs_defn(Agt, Act, O, ~Defn) :-
    regression1_knows_obs_defn(Agt, Act, O, Defn).

regression1_knows_obs_set_enum_act(Agt, PCond, Obs:NObs, RKnows) :-
    % For every potential action Act', we must know that either:
    %     it is not possible, or
    %     it produces different observations to the set given, or
    %     R(PCond, Act') holds
    term_with_ground_args(prim_action, Act),
    adp_fluent(poss, Act, PossCond),
    regression1_knows_obs_set_defn(Agt, Act, Obs:NObs, ObsCond),
    regression(PCond, Act, RPCond),
    simplify(~PossCond | ~ObsCond | RPCond, RKnows).


%  Knowledge fluents need an extra regression step once we reach the
%  initial situation.
%
regression_s0(F, F) :-
    is_atom(F).
regression_s0(all(X, P), all(X, R)) :-
    regression_s0(P, R).
regression_s0(ext(X, P), ext(X, R)) :-
    regression_s0(P, R).
regression_s0(~P, ~R) :-
    regression_s0(P, R).
regression_s0((P & Q), (R & S)) :-
    regression_s0(P, R),
    regression_s0(Q ,S).
regression_s0((P | Q), (R | S)) :-
    regression_s0(P, R),
    regression_s0(Q, S).
regression_s0((P => Q), (R => S)) :-
    regression_s0(P, R),
    regression_s0(Q, S).
regression_s0((P <=> Q), (R <=> S)) :-
    regression_s0(P, R),
    regression_s0(Q, S).
regression_s0(knows(Agt, P), knows(Agt, PCond)) :-
    pcond(P, pbu(Agt), PCond).


%
%  holds(+F, +S) - fluent F holds in situation S
%
%  This predicate is true whenever the fluent F holds in situation S. It
%  is evaluated by regressing over the action terms in S until it reaches
%  the initial situation.  The regressed formula is then evaluated against
%  the defined properties of the initial situation.
%

holds(F, do(A, S)) :-
    !,
    regression(F, A, Fr),
    holds(Fr, S).

holds(F, s0) :-
    regression_s0(F, Fr),
    domain_prove_init(Fr).

%
%  pcond(F, ADP, P)  -  persistence condition for F under ADP.
%
%  The basic meaning of this pedicate is: if fluent P holds in situation
%  s, then fluent F will hold in s and in all successors of s reachable by
%  performing actions that satisfy ADP.
%
%  It is implemented as a simple fixpoint of pcond_d1/3.
%

pcond(F, ADP, P) :-
    (domain_prove(~F) ->
        P = false
    ; domain_prove(F) ->
        P = true
    ; 
        pcond_acc_fixpoint(F, ADP, true, P1),
        simplify(P1, P)
    ).

pcond_acc_fixpoint(P1Prev, ADP, PAccum, P) :-
    pcond_d1(P1Prev, ADP, P1Next),
    (domain_prove(~P1Next) ->
        P = false
    ; domain_prove(P1Next) ->
        P = P1Prev & PAccum
    ; 
      PAccum2 = P1Prev & PAccum,
      (domain_prove(PAccum2 => P1Next) ->
        P = PAccum2
      ;
        pcond_acc_fixpoint(P1Next, ADP, PAccum2, P)
      )
    ).

%
%  pcond_d1(F, ADP, P1)  -  depth 1 persistence condition for fluent F
%
%  The basic meaning of this pedicate is: if fluent F holds in situation
%  s, then it will continue to hold in all ADP-successors of s as long
%  as P1 is true in s.
% 

pcond_d1(F, ADP, P1) :-
    ( bagof(Defn, pcond_d1_enum_actions(F, ADP, Defn), Defns) ->
        joinlist('&', Defns, P1t),
        simplify(P1t, P1)
    ;
        P1=true
    ).

pcond_d1_enum_actions(F, ADP, PDefn) :-
    term_with_ground_args(prim_action, A),
    regression(F, A, Fr),
    ( struct_equiv(F, Fr) ->
      PDefn = true
    ;
      adp_fluent(ADP, A, Defn),
      ( domain_prove(F => ~Defn) ->
        PDefn = true
      ;
        PDefn = (Fr | ~Defn)
      )
    ).

%
%  term_with_vars(Type, Term, Vars)  -  instantiate a term with variable args.
%
%  Given a type predicate Type, this predicate binds Term to one of the
%  solutions of that predicate and Vars to a list of the free variables
%  in the term.  It is used for enumerating each of the finite set of e.g.
%  action types in the domain:
%
%    term_with_vars(prim_action, A, Vars)
%

term_with_vars(Type, Term, Vars) :-
    call(Type, TypedTerm),
    TypedTerm =.. [Functor|ArgTypes],
    term_with_vars_collect(ArgTypes, Vars),
    Term =.. [Functor|Vars].

term_with_vars_collect([], []).
term_with_vars_collect([_|Ts], [_|Vs]) :-
    term_with_vars_collect(Ts, Vs).

term_with_ground_args(Type, Term) :-
    call(Type, TypedTerm),
    TypedTerm =.. [Functor|ArgTypes],
    maplist(call, ArgTypes, Args),
    Term =.. [Functor|Args].
