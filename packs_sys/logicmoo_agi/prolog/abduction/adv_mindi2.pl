%  Most of these operators are native to prolog so we dont have to declare
%  them ourselves.  The all() and ext() terms takes a unique prolog-level
%  variable as its first argument; this variable will not be bound during the
%  proof search.
%

:- module(mindi2, [
    is_atom/1,
    is_literal/1,
    tbl_expand/3,
    prove/1,
    prove/2,
    op(200, fx, ~),
    op(500, xfy, <=>),
    op(500, xfy, =>),
    op(520, xfy, &)
]).

 :- discontiguous(agent/1).
 :- discontiguous(initially/1).
 :- discontiguous(initially_known/1).
 :- discontiguous(prim_action/1).
 :- discontiguous(prim_fluent/1).
 :- discontiguous(prim_observation/1).

:- set_prolog_flag(double_quotes, atom).
%:- set_prolog_stack(local, limit(2*10**9)).

:- use_module(library(plunit)).

%:- [utils].
%:- [prover].
%:- [fluent].
%:- [sitcalc].
%:- [domain_party].

%
%  utils.pl:  some low-level utility predicates.
%
%  Copyright 2014, Ryan Kelly
%
%  This file supplies some basic low-level utility predicates.
%

%
%  ismember(Elem,List)  -  like member/2, but does not bind variables or
%                          allow backtracking.
%

ismember(_, []) :- fail.
ismember(E, [H|T]) :-
  ( E == H ->
    true
  ;
    ismember(E, T)
  ).

%
%  findmember(Elem,List,Idx)  -  like nth0/2, but does not bind variables or
%                                allow backtracking.
%

findmember(E, L, N) :-
  findmember(E, L, 0, N).

findmember(_, [], _, _) :- fail.
findmember(E, [H|T], Acc, N) :-
  ( E == H ->
    N = Acc
  ;
    NextAcc is Acc + 1,
    findmember(E, T, NextAcc, N)
  ).

%
%  vdelete(List,Elem,Result) - like delete/3 but using equivalence rather
%                              than unification, so it can be used on lists
%                              of variables
%

vdelete([], _, []).
vdelete([H|T], E, Res) :-
  ( E == H ->
    vdelete(T, E, Res)
  ;
    Res = [H|T2],
    vdelete(T, E, T2)
  ).

vdelete_list(L, [], L).
vdelete_list(L, [H|T], L2) :-
  vdelete(L, H, L1),
  vdelete_list(L1, T, L2).

%
%  pairfrom(L,E1,E2,Rest)  -  E1 and E2 are a pair of (different) elements
%                             from L, wile Rest is the rest of the list
%
%  Like doing (member(E1,L), member(E2,L))  but more efficient, doesn't match
%  E1 and E2 to the same element, and doesnt generate equivalent permutations.
%

pairfrom(L, E1, E2, Rest) :-
  pairfrom_rec(L, [], E1, E2, Rest).

pairfrom_rec([H|T], Rest1, E1, E2, Rest) :-
  E1 = H, select(E2, T, Rest2), append(Rest1, Rest2, Rest)
  ;
  pairfrom_rec(T, [H|Rest1], E1, E2, Rest).

%
%  joinlist(+Op,+In,-Out) - join items in a list using given operator
%

joinlist(_, [H], H) :- !.
joinlist(O, [H|T], J) :-
  T \= [],
  J =.. [O, H, TJ],
  joinlist(O, T, TJ).

%
%  subs(Name,Value,Old,New) -  substitue values in a term
%
%  This predicate is true when New is equal to Old with all occurances
%  of Name replaced by Value - basically, a symbolic substitution
%  routine.  For example, it is usually used to produce a result such
%  as:
%
%      subs(now,S,fluent(now),fluent(S)).
%

subs(X, Y, T, Y) :- T == X, !.
subs(_, _, T, T) :- var(T), !.
subs(X, Y, T, Tr) :-
  T =.. [F|Ts],
  subs_list(X, Y, Ts, Trs),
  Tr =.. [F|Trs].

subs_list(_, _, [], []) :- !.
subs_list(X, Y, [T|Ts], [Tr|Trs]) :-
  subs(X, Y, T, Tr),
  subs_list(X, Y, Ts, Trs).


:- begin_tests(utils, [sto(rational_trees)]).


test(ismember) :-
  ismember(A, [A, B, c]),
  ismember(B, [A, B, c]),
  ismember(c, [A, B, c]),
  \+ ismember(A, [B, _, d]).

test(findmember) :-
  findmember(A, [A, B, c], 0),
  findmember(B, [A, B, c], 1),
  findmember(c, [A, B, c], 2),
  \+ findmember(_, [A, B, c], _),
  \+ findmember(e, [A, B, c], _).

:- end_tests(utils).
%
%  prover.pl:  a theorem prover for multi-agent epistemic first-order logic.
%
%  Copyright 2008-2014, Ryan Kelly
%
%  This is a theorem-prover for the variant of modal logic used in our
%  fluent domain, based on the ideas in leanTAP [1] and its expansion to
%  modal logic by Fitting [2].  It's a classic tableaux-style prover that
%  tries to refute a formula by closing all branches of its tableaux, and
%  it handles modalities by building an auxiliary tableaux for each new
%  modal world until it finds one that can be closed.
%
%  [1] Bernhard Beckert and Joachim Posegga.
%      leanTAP: Lean tableau-based deduction.
%      Journal of Automated Reasoning, 15(3):339-358, 1995.
%
%  [2] Melvin Fitting.
%      leanTAP Revisited
%      Journal of Logic and Computation, 8(1):33-47, 1998.
%
%  It is, sadly, a lot less "lean" than the source material due to several
%  additions that make it more suitable for use with situation calculus
%  domains:
%  
%    * Support for an distinct set of "axioms" which are known to hold
%      at all worlds in the model, to represent the background theory
%      of the domain.
%
%    * Special-case handling of term equality, assuming rigid terms and
%      unique-names axioms.
%
%    * Use of attributed variables to track each free variable back to
%      its source formula, and avoid making multiple instanciations of
%      the formula with the some variable bindings.
%
%    * More care taken to try to terminate when the tableaux cannot be
%      closed, since calculation of the persistence condition requires
%      a terminating decision procedure.
%
%  Our logical terms and operators are:
% 
%     true             -   truth literal (ie "top")
%     false            -   falsehood literal (ie "bottom")
%     p(...)           -   predicate, optionally with term arguments
%     A = B            -   term equality
%     ~P               -   negation
%     P & Q            -   logical and
%     P | Q            -   logical or
%     P => Q           -   implication
%     P <=> Q          -   equivalence
%     all([Xs], P)     -   universal quantification (positive scope only)
%     ~ext([Xs], P)    -   existential quantification (negative scope only)
%     knows(A,P)       -   agent knowledge modality
%
%  There is no support for existential quantification, this must be handled
%  handled in a pre-processing step.  Our treatment of equality-as-unification
%  means skolemization is not possible, so for now, existential quantifiers
%  will have to be expanded out into a finite disjunction of possibilities.
%


%
%  is_atom(P)    -  the formula P is a literal atom, not a compound expression
%  is_literal(P) -  the formula P is a literal atom or the negation of one
%
%  This can be used to detect the base case of predicates that structurally
%  decompose formulae.
%

is_atom(P) :-
  P \= (~_),
  P \= (_ => _),
  P \= (_ <=> _),
  P \= (_ & _),
  P \= (_ | _),
  P \= ext(_, _),
  P \= all(_, _),
  P \= knows(_, _).

is_literal(~P) :- !, is_atom(P).
is_literal(P) :- is_atom(P).


% 
%  prove/1 and prove/2:  top-level driver for the prover.
%
%  These predicates attempt to prove a formula by refuting its negation.
%  The optional set of axioms are formulae known to hold at all worlds
%  reachable in the model.  This list must not include any modalities,
%  or the prover may get stuck in a loop expanding copies of the same
%  modality over and over.
%

prove(Fml) :-
  prove([], Fml).

prove(Axioms, Fml) :-
  prove_iterdeep(Axioms, Fml, 500).

prove_iterdeep(Axioms, Fml, Limit) :-
  % Classic iterative-deepening for completeness, for which
  % swi-prolog conveniently provides builtin support via the
  % call_with_depth_limit/3 predicate.
  refute_with_depth_limit(Axioms, ~Fml, Limit, Result),
  ( Result = depth_limit_exceeded ->
      %write(not_proved_at_depth(Limit)), nl,
      NewLimit is Limit + 500,
      prove_iterdeep(Axioms, Fml, NewLimit)
  ;
      %write(proved_at_depth(Result)), nl
      true
  ).

refute_with_depth_limit(Axioms, Fml, Limit, Result) :-
  tbl_init(Axioms, Tbl),
  call_with_depth_limit(refute(Fml, Tbl), Limit, Result).

refute(Fml, Tbl) :-
  % Try to expand the given tableaux with the given formula so that
  % all branches are closed.  Since tbl_expand/3 generates all 'closed'
  % solutions before generating any 'open' solutions, we can safely
  % cut after the first solution and avoid lots of pointless backtracking.
  tbl_expand(Fml, Tbl, Res),
  !, Res=closed(_).


%
%  tbl_expand/3:  build a fully-expanded tableaux from the given state.
%
%  This is the core of the prover.  In the style of leanTaP, it takes the
%  current state of an under-construction tableaux and tries to expand it
%  into one where all branches are closed.  Unlike leanTaP, we use an explicit
%  "tableaux state" data structure to pass around the state, since our version
%  contains quite a few more auxiliary fields.
%
%  The state of the tableaux branch at each invocation represents a partially-
%  built model based at a particular world of the underlying kripke structure.
%
%  The expansion proceeds by expanding all formulae on the branch, trying
%  to produce a conflicting pair of literals at the current world that will
%  close the branch.  If this is not possible then one of the related worlds
%  implied by the model is selected and the search continues in a sub-tableaux
%  at that world.
%
%  If all branches are successfully closed, the predicate's third argument
%  will be bound to 'closed(NEqs)' where NEqs is a possibly-empty list of
%  pairs of terms that must not unify.  If some branch remains open then
%  its third argument will be bound to the atom 'open'.
%
%  All 'closed' solutions will be generated before any 'open' solutions.
%  

% Normalization rules.
% These decompose higher-level connectives into simpler ones by
% e.g. pushing negation inside other operators.  This effectively
% computes the required normal form at runtime.

tbl_expand(~(~X), Tbl, Res) :-
  !, tbl_expand(X, Tbl, Res).

tbl_expand(X => Y, Tbl, Res) :-
  !, tbl_expand((~X) | Y, Tbl, Res).

tbl_expand(X <=> Y, Tbl, Res) :- 
  !, tbl_expand((X & Y) | (~X & ~Y), Tbl, Res).

tbl_expand(~(X & Y), Tbl, Res) :-
  !, tbl_expand((~X) | (~Y), Tbl, Res).

tbl_expand(~(X | Y), Tbl, Res) :-
  !, tbl_expand(~X & ~Y, Tbl, Res).

tbl_expand(~(X => Y), Tbl, Res) :-
  !, tbl_expand(X & (~Y), Tbl, Res).
  
tbl_expand(~(X <=> Y), Tbl, Res) :-
  !, tbl_expand(((~X) | (~Y)) & (X | Y), Tbl, Res).

tbl_expand(~ext(Vs, P), Tbl, Res) :-
  !, tbl_expand(all(Vs, ~P), Tbl, Res).

%  Rule to handle universal quantification.
%  We create a new instance of the quantified formula using a fresh
%  variable, and stash the original formula in the tableaux state for
%  future re-use.  It will only be re-used if the previous instance of
%  that formula is bound during the search, to ensure that we don't keep
%  pointlessly re-using the same formula over and over.

tbl_expand(all([], P), Tbl, Res) :-
  !,
  tbl_expand(P, Tbl, Res).
tbl_expand(all([X|Xs], P), Tbl, Res) :-
  !,
  tbl_add_univ_fml(Tbl, all(X, all(Xs, P)), InstFml, Tbl2),
  tbl_expand(InstFml, Tbl2, Res).

%  Rule to guard against existential quantification.
%  Existential quantification is not supported, and must be handled as a
%  pre-processing step before passing formulae to the prover.  This rule
%  proides a simple sanity-check.

tbl_expand(ext(_, _), _, _) :-
  !, write(formula_cannot_contain_existential_quantifiers), nl, fail.

tbl_expand(~all(_, _), _, _) :-
  !, write(formula_cannot_contain_existential_quantifiers), nl, fail.

%  Rules for collecting modalities.
%  Modalities don't constrain the current world, but rather the worlds
%  that may be reachable from the current one.  They are simply collected
%  along the branch and used to imply/constraint other worlds that we might
%  move to if the current one cannot be closed.

tbl_expand(knows(A, F), Tbl, Res) :-
  !,
  tbl_add_necc(Tbl, k(A, F), Tbl2),
  tbl_expand(true, Tbl2, Res).

tbl_expand(~knows(A, F), Tbl, Res) :-
  !,
  tbl_add_poss(Tbl, k(A, ~F), Tbl2),
  tbl_expand(true, Tbl2, Res).

%  Rule for handling disjunction.
%  The tableaux bifurcates and we must try to close both branches, ensuring
%  that any declared non-unifiability of terms is respected across both
%  branches.
%
%  Note that our use of prolog variables when instantiating universally-
%  quantified formulae means that free-variable substitutions apply across
%  both branches, as required for a first-order tableaux.

tbl_expand(X | Y, Tbl, Res) :-
  !,
   % First try to close the LHS branch.
  tbl_expand(X, Tbl, ResX),
  ( ResX = closed(NEqsX) ->
    % Ensure that the RHS proof search doesn't unify things that the LHS
    % proof search declared must not unify.
    tbl_add_neqs(Tbl, NEqsX, Tbl2),
    tbl_expand(Y, Tbl2, ResY),
    ( ResY = closed(NEqsY) ->
      % Combine the things-that-must-not-unify lists from both branches.
      append(NEqsX, NEqsY, NEqs),
      Res = closed(NEqs)
    ;
      % The RHS could not be closed.
      % Backtrack to a different solution for the LHS.
      fail
    )
  ;
    % The LHS could not be closed, so the whole tableaux stays open.
    Res = open
  ).

%  Rule for handling conjunction.
%  Both sides simply get added to the existing branch.

tbl_expand(X & Y, Tbl, Res) :-
  !,
  tbl_push_fml(Tbl, Y, Tbl2),
  tbl_expand(X, Tbl2, Res).

%  Rule for closing the branch, or continuing the search.
%
%  The formula under consideration here must be a (possibly negated)
%  literal, since any other form would have matched a rule above.
%
%  We add the literal to the current tableaux state.  If that produces a
%  contradiction then we're done on this branch.  If not then the tableaux
%  remains open and we proceed as follows:
%
%    * If we have more unexpanded formulae on the branch, continue
%      expanding the current branch.
%
%    * If we have universal formulae where all instances have been used,
%      add fresh instances of them to the unexpanded list and continue
%      expanding the current branch.
%
%    * Enumerate the alternative worlds implied by the current tableaux
%      state, expanding a sub-tableaux for each:
%
%        * If any such sub-tableaux is closed, close the current branch.
%
%        * If all sub-tableaux remain open, check again for universal
%          formulae where all instances have been used.  If we have any
%          then add fresh instances of them to the unexpanded list and
%          resume expanding the current branch.
%
%    * Otherwise, there is no way to close the current branch, so report
%      it as being open.
%
%  This rather heavy-handled imperative control flow is designed to avoid
%  repeatedly instantiating the same universally-quantified formula over
%  and over, without stopping the search too early.  It has been found to
%  help termination in cases that obviously cannot be expanded to a closed
%  state, but for which a naive handling of universally-quantified formulae
%  would recurse forever.
%

tbl_expand(Lit, Tbl, Res) :-
  (
    tbl_add_literal(Tbl, Lit, Tbl2),
    ( Tbl2 = closed(NEqs) ->
      Res = closed(NEqs)
    ;
      ( tbl_pop_fml(Tbl2, Fml, Tbl3) ->
          tbl_expand(Fml, Tbl3, Res)
      ;
        tbl_copy_used_univ_fmls(Tbl2, Tbl3),
        ( tbl_pop_fml(Tbl3, Fml, _S_Tbl4a) ->
            tbl_expand(Fml, Tbl3, Res)
        ;
          tbl_expand_subtbls(Tbl2, SubRes),
          ( SubRes = closed(NEqs) ->
              Res = closed(NEqs)
          ;
            tbl_copy_used_univ_fmls(Tbl2, Tbl3),
            ( tbl_pop_fml(Tbl3, Fml, _S_Tbl4b) ->
              tbl_expand(Fml, Tbl3, Res)
            ;
              Res = open
            )
          )
        )
      )
    ) 
  ;
    % After all other possibilities have been exhaused, we want to make
    % sure to generate an 'open' solution so that failure-driven backtracking
    % will work correctly.
    Res = open
  ).


%  Helper predicate for expanding each sub-tableaux in turn.
%  This enumerates all the possible sub-tableaux and then walks the
%  list trying to close them.  It will unify its second argument with
%  'closed' if one of the sub-tableaux could be closed, and with 'open'
%  if they all remain open.
%
%  Like tbl_expand/3 this will backtrack over different possible ways
%  to close each sub-tableaux.

tbl_expand_subtbls(Tbl, Res) :-
  findall(SubTbl, tbl_pick_subtbl(Tbl, SubTbl), SubTbls),
  tbl_expand_subtbls(Tbl, SubTbls, Res).

tbl_expand_subtbls(_, [], open).
tbl_expand_subtbls(Tbl, [SubTbl | SubTbls], Res) :-
  tbl_expand(true, SubTbl, SubRes),
  ( SubRes = closed(NEqs) ->
      Res = closed(NEqs)
  ;
      tbl_expand_subtbls(Tbl, SubTbls, Res)
  ).


%
%  tbl_*  -  utility predicates for maintaining in-progess tableaux state.
%
%  The following are low-level state manipulation routines for the tableaux.
%  They're mostly just list-shuffling and could have been done inline in the
%  spirit of leanTaP, but factoring them out helps the code read better and
%  leaves the door open for e.g. better data structures.
%  
%  The full state of a tableaux branch is a term:
%
%      tbl(UnExp, TLits, FLits, NEqs, Axs, Necc, Poss, Univ, FVs)
%
%  Its arguments are:
%
%    * UnExp:   a list of formulae remaining to be expanded on the branch.
%    * TLits:   a list of literals true at the current world.
%    * FLits:   a list of literals false at the current world.
%    * NEqs:    a list of A=B pairs that are not allowed to unify.
%    * Axs:     a list of formulae true at every possible world.
%    * Necc:    a list of formulae that hold at all accessible worlds,
%               indexed by name of accessibility relation
%    * Poss:    a list of formulae that hold at some accessible world,
%               indexed by name of accessibility relation
%    * Univ:    a list of universally-quantified formulae that can be used
%               on the branch, along with previous instantiation variables.
%    * FVs:     a list of free variables instantiated by the prover, which
%               must be preserved when copying formulae.
%

%
%  tbl_init/2  -  initialize a new empty tableaux
%
%  This predicate takes a list of axioms that are true at every world,
%  and produces an initialized tableux state.
%

tbl_init(Axs, TblOut) :-
  TblOut = tbl(Axs, [], [], [], Axs, [], [], [], []).

tbl_write(Tbl) :-
  Tbl = tbl(UE, TLits, FLits, NEqs, _, Necc, Poss, Univ, _),
  write(tbl), nl,
  write('    UE = '), write(UE), nl,
  write('    TLits = '), write(TLits), nl,
  write('    FLits = '), write(FLits), nl,
  write('    NEqs = '), write(NEqs), nl,
  write('    Necc = '), write(Necc), nl,
  write('    Poss = '), write(Poss), nl,
  tbl_write_univ(Univ).

tbl_write_univ([]).
tbl_write_univ([U|Univ]) :-
  write('        Univ: '), write(U), nl,
  tbl_write_univ(Univ).

%
%  tbl_push_fml/3  -  add a formula to be expanded on this tableaux branch
%  tbl_pop_fml/3   -  pop a formula off the unexpanded list for this branch
%
%  These predicates simply push/push from a list of yet-to-be-expanded
%  formulae, basically maintaining a worklist for the current branch.
%

tbl_push_fml(TblIn, Fml, TblOut) :-
  TblIn = tbl(UE, TLits, FLits, NEqs, Axs, Necc, Poss, Univ, FVs),
  TblOut = tbl([Fml|UE], TLits, FLits, NEqs, Axs, Necc, Poss, Univ, FVs).

tbl_pop_fml(TblIn, Fml, TblOut) :-
  TblIn = tbl([Fml|UE], TLits, FLits, NEqs, Axs, Necc, Poss, Univ, FVs),
  TblOut = tbl(UE, TLits, FLits, NEqs, Axs, Necc, Poss, Univ, FVs).

%
%  tbl_add_neqs/3  - add non-unification constraints to the tableaux
%
%  This predicate declares that the list of [A=B] pairs in its second arugment
%  must not be made to unify, recording them in an internal list in the 
%  tableaux state.  Subsequent attempts to bind free variables will be
%  checked for compatability with this list.
%

tbl_add_neqs(TblIn, NEqs, TblOut) :-
  TblIn = tbl(UnExp, TLits, FLits, NEqsIn, Axs, Necc, Poss, Univ, FVs),
  append(NEqs, NEqsIn, NEqsOut),
  TblOut = tbl(UnExp, TLits, FLits, NEqsOut, Axs, Necc, Poss, Univ, FVs).

%
%  tbl_add_necc/4  - add a formula that holds at all possible worlds
%  tbl_add_poss/4  - add a formula that holds at some possible world
%
%  These predicates are used to accumulate information about worlds reachable
%  from the current one.  This information is then used to initialize a
%  sub-tableuax if the proof moves to a new world.
%

tbl_add_necc(TblIn, N, TblOut) :-
  TblIn = tbl(UnExp, TLits, FLits, NEqs, Axs, Necc, Poss, Univ, FVs),
  TblOut = tbl(UnExp, TLits, FLits, NEqs, Axs, [N|Necc], Poss, Univ, FVs).

tbl_add_poss(TblIn, P, TblOut) :-
  TblIn = tbl(UnExp, TLits, FLits, NEqs, Axs, Necc, Poss, Univ, FVs),
  TblOut = tbl(UnExp, TLits, FLits, NEqs, Axs, Necc, [P|Poss], Univ, FVs).

%
%  tbl_add_univ_fml/4  -  add a universally-quantified formula to the tableaux
%
%  This predicate is applied to universally-quantified formula found during
%  the expansion.  It produces an instance of the formula with a fresh variable
%  and notes the formula for potential re-use in the future.
% 

tbl_add_univ_fml(TblIn, all(X, P), InstFml, TblOut) :-
  TblIn = tbl(UnExp, TLits, FLits, NEqs, Axs, Necc, Poss, Univ, FVs),
  tbl_copy_term(TblIn, [X, P], [V, InstFml]),
  % Remember where this var come from, so we can avoid duplicate bindings.
  put_attr(V, mindi2, []),
  U = u(X, P, [V]),
  TblOut = tbl(UnExp, TLits, FLits, NEqs, Axs, Necc, Poss, [U|Univ], [V|FVs]).

%
%  tbl_copy_term/2  -  copy term while preserving unbound vars.
%
%  This predicate can be used like copy_term/2 expect that it will not
%  rename unbound free variables in the formula.  Basically it renames
%  variables defined outside of the proof search, while maintaining any
%  variables created by the search itself.
%

tbl_copy_term(Tbl, TermIn, TermOut) :-
  Tbl = tbl(_, _, _, _, _, _, _, _, FVs),
  copy_term([TermIn, FVs], [TermOut, FVs]).

%
%  tbl_copy_used_univ_fmls/2  -  make fresh copes of used universal formulae
%
%  This predicate finds any universally-quantified formula for which all
%  existing instances have been "used" - that is, have had their instance
%  variable bound.  If it finds any then it copies fresh instances of them
%  into the list of unexpanded formulae.
%
%  This is a simple way to avoid making lots of useless duplicate expansions
%  of universally-quantified formulae.
%

tbl_copy_used_univ_fmls(TblIn, TblOut) :-
  TblIn = tbl([], TLits, FLits, NEqs, Axs, Necc, Poss, UnivIn, FVs),
  TblOut = tbl(UnExp, TLits, FLits, NEqs, Axs, Necc, Poss, UnivOut, FVs),
  tbl_copy_used_univ_fmls_rec(TblIn, UnivIn, UnivOut, UnExp).

tbl_copy_used_univ_fmls_rec(_, [], [], []).
tbl_copy_used_univ_fmls_rec(Tbl, [U|UnivIn], UnivOut, UnExp) :-
  U = u(X, P, Vars),
  Vars = [PrevV|_],
  ( ( \+ var(PrevV) ) ->
      tbl_copy_term(Tbl, [X, P], [NewV, NewP]),
      % Remember where this var come from, so we can avoid duplicate bindings.
      put_attr(NewV, mindi2, Vars),
      UnivOut = [u(X, P, [NewV|Vars])|UnivOutRest],
      UnExp = [NewP|UnExpRest]
  ;
      UnivOut = [U|UnivOutRest],
      UnExp = UnExpRest
  ),
  tbl_copy_used_univ_fmls_rec(Tbl, UnivIn, UnivOutRest, UnExpRest).

%
%  tbl_pick_subtbl/2  -  generate a sub-tableaux for a related world
%
%  The tableaux state contains a collection of "possible" and "necessary"
%  formulae that imply the existence of other worlds related to the one
%  currently under consideration.  This predicate picks one such world
%  and returns a new tableaux based on the formulae relevant to that world8,
%  backtracking over each choice in turn.
%

tbl_pick_subtbl(Tbl, SubTbl) :-
  Tbl = tbl(_, _, _, NEqs, Axioms, Necc, Poss, _, FVs),
  % Pick a possible world.
  member(k(A, F), Poss),
  % Pair it with all the things that necessarily hold there.
  ( bagof(FN, member(k(A, FN), Necc), FNs) ->
    copy_term(Axioms, AXs),
    append(FNs, AXs, FNecc)
  ;
    copy_term(Axioms, FNecc)
  ),
  SubTbl = tbl([F|FNecc], [], [], NEqs, Axioms, [], [], [], FVs).


tbl_pick_subtbl(Tbl, SubTbl) :-
  Tbl = tbl(_, _, _, NEqs, Axioms, Necc, Poss, _, FVs),
  % Pick an agent for whom no possible world was implied.
  % We assume that they consider at least one world possible,
  % so we can check the consistency of their knowledge.
  setof(A, F^(member(k(A, F), Necc), \+ member(k(A, _), Poss)), AllA),
  member(A, AllA),
  % Pair it with all the things that necessarily hold there.
  ( bagof(FN, member(k(A, FN), Necc), FNs) ->
    copy_term(Axioms, AXs),
    append(FNs, AXs, FNecc)
  ;
    copy_term(Axioms, FNecc)
  ),
  SubTbl = tbl(FNecc, [], [], NEqs, Axioms, [], [], [], FVs).

%
%  tbl_add_literal/3  - expand the branch with a new literal
%
%  This predicate attempts to add a new literal to the branch.
%
%  It first tries to add it in such a way as to form a contradiction, in
%  which case the third argument is bound to closed(NEqs) with NEqs a list
%  of A=B pairs that must *not* be made to unify on some other branch.
%
%  If closing the branch fails, it adds the literal to the appropriate field
%  in the tableaux state and binds the third argument to the new tableaux.
%
%  This is really the heart of the prover, where the contrdictions are made.
%  It has special-case handling of equality based on rigid terms and unique
%  names axioms, meaning we can treat it using unification.
%

tbl_add_literal(Tbl, true, Tbl) :- !.
tbl_add_literal(Tbl, ~false, Tbl) :- !.

tbl_add_literal(_, false, closed([])) :- !.
tbl_add_literal(_, ~true, closed([])) :- !.

tbl_add_literal(TblIn, A=B, TblOut) :-
  !,
  ( unifiable(A, B, Bindings) ->
    ( Bindings = [] ->
      % The terms are identical, so this literal is a tautology.
      TblOut = TblIn
    ;
      (
        % This can be made into a contradiction by insisting that any one of
        % the free-variable bindings be in fact un-unifiable.
        member(Binding, Bindings),
        TblOut = closed([Binding])
      ;
        % Or the branch can be left open, and we just do the bindings.
        tbl_apply_var_bindings(TblIn, Bindings, TblOut)
      )
    )
  ;
    % The terms are not unifiable, so this literal is a contradiction.
    TblOut = closed([])
  ).

tbl_add_literal(TblIn, ~(A=B), TblOut) :-
  !,
  ( unifiable(A, B, Bindings) ->
    ( Bindings = [] ->
      % The terms are identical, so this literal is a contradiction.
      TblOut = closed([])
    ;
      (
        % This literal can be made into a contradiction via unification.
        tbl_apply_var_bindings(TblIn, Bindings),
        TblOut = closed([])
      ;
        % Or the branch can be left open by not doing the bindings.
        tbl_add_neqs(TblIn, [A=B], TblOut)
      )
    )
  ;
    % The terms are not unifiable, so this literal is a tautology.
    TblOut = TblIn
  ).

tbl_add_literal(TblIn, ~Lit, TblOut) :-
  !,
  TblIn = tbl(UnExp, TLits, FLitsIn, NEqs, Axs, Necc, Poss, Univ, FVs),
  % Try to produce a contradiction with one of the true literals.
  tbl_add_literal_find_contradiction(TblIn, Lit, TLits, Res),
  ( Res = closed ->
    TblOut = closed([])
  ;
    % Add it to the list of things known to be false, unless we already know.
    ( ismember(Lit, FLitsIn) ->
        FLitsOut = FLitsIn
    ;
        FLitsOut = [Lit|FLitsIn]
    ),
    TblOut = tbl(UnExp, TLits, FLitsOut, NEqs, Axs, Necc, Poss, Univ, FVs)
  ).

tbl_add_literal(TblIn, Lit, TblOut) :-
  !,
  TblIn = tbl(UnExp, TLitsIn, FLits, NEqs, Axs, Necc, Poss, Univ, FVs),
  % Try to produce a contradiction with one of the false literals.
  tbl_add_literal_find_contradiction(TblIn, Lit, FLits, Res),
  ( Res = closed ->
    TblOut = closed([])
  ;
    % Add it to the list of things known to be true, unless we already know.
    ( ismember(Lit, TLitsIn) ->
        TLitsOut = TLitsIn
    ;
        TLitsOut = [Lit|TLitsIn]
    ),
    TblOut = tbl(UnExp, TLitsOut, FLits, NEqs, Axs, Necc, Poss, Univ, FVs)
  ).


%
%  Helper predicate to search for contradictions in a set of complimentary
%  literals.  It first tries to find a member identical to the target,
%  in which case the branch is always closed and no backtracking is required.
%  Otherwise, it tries to find one unifiable with the taret, but allows
%  backtracking over these choices.  If all such choices are backtracked over
%  (or none are found) then the branch remains open.
%

tbl_add_literal_find_contradiction(Tbl, Lit, NLits, Res) :-
  tbl_add_literal_find_contradiction_bindings(Lit, NLits, Bindings),
  ( Bindings = [] ->
    % No possible bindings, so we fail to find any contraditions.
    Res = open
  ;
    sort(Bindings, OrderedBindings), 
    ( OrderedBindings = [[]|_] ->
      % Found a contradiction with no bindings, so no need to backtrack.
      Res = closed
    ;
      % Found a contradiction if we do some backtrackable unification.
      (
        member(B, OrderedBindings),
        tbl_apply_var_bindings(Tbl, B),
        Res = closed
      ;
        Res = open
      )
    )
  ).

tbl_add_literal_find_contradiction_bindings(_, [], []).
tbl_add_literal_find_contradiction_bindings(Lit, [NLit|NLits], Bindings) :-
  ( unifiable(Lit, NLit, B) ->
    Bindings = [B|Bs],
    tbl_add_literal_find_contradiction_bindings(Lit, NLits, Bs)
  ;
    tbl_add_literal_find_contradiction_bindings(Lit, NLits, Bindings)
  ).

%
%  tbl_apply_var_bindings/2
%  tbl_apply_var_bindings/3  -  apply variable bindings and check constraints
%
%  This predicate takes a list of [A=B] pairs makes them unify with occurs
%  check.  It also checks that various proof-search constraints are not
%  violated by the bindings:
%
%    * two free variables generated from the same source formula are never
%      bound to an identical term, as this would be redundant.
%
%    * any declared non-unifiability constrains are not violated. 
%
%  If called with three arguments, this predicate will also remove any
%  non-unifiability constraints that have become tautological and return
%  the updated tableaux state in its third argument.
%

tbl_apply_var_bindings(TblIn, Bindings) :-
  tbl_apply_var_bindings(TblIn, Bindings, _).

tbl_apply_var_bindings(TblIn, [], TblOut) :-
  tbl_check_neqs(TblIn, TblOut).

tbl_apply_var_bindings(TblIn, [A=B|Bindings], TblOut) :-
  unify_with_occurs_check(A, B),
  tbl_apply_var_bindings(TblIn, Bindings, TblOut).

attr_unify_hook(OtherValues, Value) :-
  % This hook gets called when one of our free variables is bound.
  % Veto the binding if it is bound to a previously-used value, since
  % that's redundant for the proof search.
  \+ ismember(Value, OtherValues).



%
%  Helper predicate to check that non-unifiability constraints have
%  not been violated.  It also helps to trim down the list of constraints
%  if some have become tautological.
%

tbl_check_neqs(TblIn, TblOut) :-
  TblIn = tbl(UnExp, TLits, FLits, NEqsIn, Axs, Necc, Poss, Univ, FVs),
  tbl_check_neqs_list(NEqsIn, NEqsOut),
  TblOut = tbl(UnExp, TLits, FLits, NEqsOut, Axs, Necc, Poss, Univ, FVs).

tbl_check_neqs_list([], []).
tbl_check_neqs_list([A=B|NEqsIn], NEqsOut) :-
  ( unifiable(A, B, Bindings) ->
    Bindings \= [],
    NEqsOut = [A=B|NEqsOutRest],
    tbl_check_neqs_list(NEqsIn, NEqsOutRest)
  ;
    tbl_check_neqs_list(NEqsIn, NEqsOut)
  ).

%
%  Test cases for basic prover functionality.
%

:- begin_tests(prover, [sto(rational_trees)]).

test(prop1) :-
  prove(true),
  prove(~false),
  prove(red = red),
  prove(~(red = blue)),
  prove(p | ~p),
  \+ prove(p | q).

test(prop2) :-
  prove([q], p | q).

test(knows1) :-
  prove(knows(ann, p | ~p)),
  \+ prove(knows(ann, p | q)),
  \+ prove(knows(ann, p & ~p)).

test(knows2) :-
  prove([q], knows(ann, p | q)).

test(knows4) :-
  prove(knows(ann, knows(bob, p)) => knows(ann, knows(bob, p | q))),
  \+ prove(knows(ann, knows(bob, p | q)) => knows(ann, knows(bob, p))).

test(knows5) :-
  prove([(p | q)], knows(ann, knows(bob, p | q))).

test(knows6) :-
  prove(knows(ann, knows(bob, p(c) | ~p(c)))).

test(knows7) :-
  prove(knows(ann, p(c)) => knows(ann, ext([X], p(X)))),
  prove(knows(ann, all([X], p(X))) => knows(ann, p(c))).

test(knows12) :-
  prove(knows(ann, knows(bob, p(x))) => ext([X], knows(ann, knows(bob, p(X))))),
  prove(knows(ann, knows(bob, p(x))) => knows(ann, ext([X], knows(bob, p(X))))),
  prove(knows(ann, knows(bob, p(x))) => knows(ann, knows(bob, ext([X], p(X))))).

test(eq1) :-
  prove(all([X], (X=red) => hot(X)) => hot(red)).

:- end_tests(prover).
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
regression_s0(~P, ~R) :- !,
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

% :- [(domain_wumpus)].
%
%  Domain-specific definitions.
%
%  Copyright 2008-2014, Ryan Kelly
%
%  This axiomatisation is for the "multi-agent hunt the wumpus"" domain from
%  the paper "Asynchronous Knowledge with Hidden Actions in the Situation
%  Calculus" by Ryan F Kelly and Adrian R Pearce.
%

:- discontiguous(causes_true/3).
:- discontiguous(causes_false/3).

:- multifile(adp_fluent/3).

% Enumerate the values of the various object types in the domain

agent(ann).
agent(bob).

room(room1).
room(room2).
room(room3).
room(room4).
room(room5).
room(room6).
room(room7).
room(room8).
room(room9).

adjacent(room1, room2).
adjacent(room1, room4).
adjacent(room2, room1).
adjacent(room2, room3).
adjacent(room2, room5).
adjacent(room3, room2).
adjacent(room3, room6).
adjacent(room4, room1).
adjacent(room4, room5).
adjacent(room4, room7).
adjacent(room5, room2).
adjacent(room5, room4).
adjacent(room5, room6).
adjacent(room5, room8).
adjacent(room6, room3).
adjacent(room6, room5).
adjacent(room6, room9).
adjacent(room7, room4).
adjacent(room7, room8).
adjacent(room8, room5).
adjacent(room8, room7).
adjacent(room8, room9).
adjacent(room9, room6).
adjacent(room9, room8).

% Enumerates primitive actions, and the types of their arguments.

prim_action(move(agent, room)).
prim_action(shoot(agent, room)).
prim_action(alert(agent)).

% Enumerates primitive observation terms, and the types of their arguments.

prim_observation(move(agent, room)).
prim_observation(steps).
prim_observation(shoot(agent, room)).
prim_observation(alert).
prim_observation(stench).
prim_observation(scream).

% Enumerates primitive fluents, and types of arguments

prim_fluent(in(agent, room)).
prim_fluent(wumpus(room)).
prim_fluent(stench(room)).
prim_fluent(adjacent(room, room)).
prim_fluent(killed).

% Definitions for action description predicate fluents

% Possibility.

adp_fluent(poss, move(Agt, Room),
  ext([R], in(Agt, R) & adjacent(R, Room))
).

adp_fluent(poss, shoot(Agt, Room),
  ext([R], in(Agt, R) & adjacent(R, Room))
).

adp_fluent(poss, alert(Agt),
  ext([R], in(Agt, R) & stench(R))
).

% Observations.

adp_fluent(obs(Agt, move(Agt1, Room1)), move(Agt2, Room2),
  % Full move action is observable if...
  (Room1=Room2) & (Agt1=Agt2) & (
      % the observer is in the destination room
      in(Agt, Room1)
    |
      % or the observer is in the source room.
      ext([Room3], in(Agt, Room3) & in(Agt1, Room3))
  )
).

adp_fluent(obs(Agt, steps), move(Agt1, Room1),
  % Footsteps can be heard when someone moves if...
  ext([Room2], (in(Agt, Room2) & (
      % the observer is adjacent to the destination room
      adjacent(Room2, Room1)
    |
      % or the observer is adjacent to the source room.
      ext([Room3], in(Agt1, Room3) & adjacent(Room2, Room3))
  )))
).

adp_fluent(obs(Agt, shoot(Agt1, Room1)), shoot(Agt2, Room2),
  % Full shoot action is observable if...
  (Room1=Room2) & (Agt1=Agt2) & (
      % the observer is in the target room
      in(Agt, Room1)
    |
      % or the observer is in the source room.
      ext([Room3], in(Agt, Room3) & in(Agt1, Room3))
  )
).

adp_fluent(obs(Agt, alert), alert(Agt1),
  % An alert can be heard if...
  ext([Room, Room1], (
    in(Agt, Room) & in(Agt1, Room1) & (
        % the observer is in the same room as the announcer
        (Room = Room1)
      |
        % or the observer is in a room adjacent to the announcer
        adjacent(Room, Room1)
    )
  ))
).

adp_fluent(obs(Agt, stench), move(Agt1, Room1),
  % A stench is observed by anyone entering a stench-filled room.
  (Agt=Agt1) & stench(Room1)
).

adp_fluent(obs(_, scream), shoot(_, Room1),
  % Everyone hears the scream if the wumpus is shot.
  wumpus(Room1)
).


% Causal rules for each fluent/action combo

causes_true(in(Agt, Room), move(Agt2, Room2),
  (Agt=Agt2) & (Room=Room2)
).

causes_false(in(Agt, Room), move(Agt2, Room2),
  (Agt=Agt2) & ~(Room=Room2)
).

causes_true(killed, shoot(_, Room),
  wumpus(Room)
).


%  Specify what holds in the initial situation.

% The wumpus is in room five, but this is not known to the agents.
initially(wumpus(room5)).

% The agents are known to start in the entry room.
initially_known(in(Agt, room1)) :-
  agent(Agt).

% The wumpus is known to start out alive.
initially_known(~killed).

%  Specify the constraints.
%  These are true in all situations and hence are common knowledge.
%  They form the background theory of the domain.

% There's nothing suspicious about the entry room.
constraint(~wumpus(room1)).
constraint(~stench(room1)).

% The cave layout is fixed, according to adjacency definitions.
constraint(adjacent(R1, R2)) :-
  room(R1), room(R2), adjacent(R1, R2).
constraint(~adjacent(R1, R2)) :-
  room(R1), room(R2), \+ adjacent(R1, R2).

% Exactly one room contains the wumpus.
constraint(all([R1,R2], (wumpus(R1) & wumpus(R2)) => (R1=R2))).
constraint(ext([R], wumpus(R))).

% Each agent is in exactly one room.
% XXX TODO: including these tips the scales into too much exponential
% branching during proof search.
%constraint(all([R1,R2], (in(Agt, R1) & in(Agt, R2)) => (R1=R2))) :-
%  agent(Agt).
%constraint(ext([R], in(Agt, R))) :-
%  agent(Agt).

% A room has a stench iff the wumpus is in that room or in an adjacent one.
constraint(all([R1], (
  stench(R1) => (wumpus(R1) | ext([R2], (wumpus(R2) & adjacent(R1, R2))))
))).
constraint(all([R1], (
  (wumpus(R1) | ext([R2], (wumpus(R2) & adjacent(R1, R2)))) => stench(R1)
))).


%
%  And now for the unit tests...
%

:- begin_tests(domain_wumpus,[sto(rational_trees)]).

test(sanity1) :-
  domain_prove(~wumpus(room1)),
  domain_prove(all([R], adjacent(room1, R) => ~wumpus(R))),
  %domain_prove(in(bob, room1) => ~in(bob, room2)),
  true.

test(example0) :-
  holds(in(ann, room1), s0),
  holds(in(bob, room1), s0),
  %holds(knows(ann, in(bob, room1)), s0),
  %holds(~ext([X], knows(ann, wumpus(X))), s0),
  %holds(knows(ann, ~wumpus(room1)), s0),
  %holds(knows(ann, ~wumpus(room2)), s0),
  %holds(knows(ann, ~wumpus(room4)), s0),
  true. %\+ holds(knows(ann, ~wumpus(room5)), s0).

:- end_tests(domain_wumpus).



%:- [(domain_party)].
%
%  Domain-specific definitions.
%
%  Copyright 2008-2014, Ryan Kelly
%
%  This axiomatisation is for the "party invitation"" domain from the
%  PhD thesis "Asynchronous Multi-Agent Reasoning in the Situation Calculus"
%  by Ryan F Kelly.
%

:- discontiguous(causes_true/3).
:- discontiguous(causes_false/3).

:- multifile(adp_fluent/3).

% Enumerate the values of the various object types in the domain

agent(ann).
agent(bob).

location(cathys_house).
location(dannys_house).

% Enumerates primitive actions, and the types of their arguments.

prim_action(read(agent)).
prim_action(leave(agent)).
prim_action(enter(agent)).

% Enumerates primitive observation terms, and the types of their arguments.

prim_observation(read(agent)).
prim_observation(leave(agent)).
prim_observation(enter(agent)).
prim_observation(party_at(location)).

% Enumerates primitive fluents, and types of arguments

prim_fluent(inroom(agent)).
prim_fluent(party_at(location)).

% Definitions for action description predicate fluents

% Possibility.

adp_fluent(poss, read(Agt), inroom(Agt)).
adp_fluent(poss, enter(Agt), ~inroom(Agt)).
adp_fluent(poss, leave(Agt), inroom(Agt)).

% Observations.

adp_fluent(obs(Agt, read(Agt1)), read(Agt2), (Agt1=Agt2) & (Agt1=Agt | inroom(Agt))).
adp_fluent(obs(_, enter(Agt1)), enter(Agt2), Agt1=Agt2).
adp_fluent(obs(_, leave(Agt1)), leave(Agt2), Agt1=Agt2).
adp_fluent(obs(Agt, party_at(Loc)), read(Agt1), (Agt=Agt1) & party_at(Loc)).

% Causal rules for each fluent/action combos

causes_true(inroom(Agt1), enter(Agt2), Agt1=Agt2).
causes_false(inroom(Agt1), leave(Agt2), Agt1=Agt2).

%  Specify what holds in the initial situation.

initially(party_at(cathys_house)).
initially(~party_at(dannys_house)).
initially(~knows(Agt, party_at(Loc))) :-
    agent(Agt), location(Loc).
initially(knows(Agt1, ~knows(Agt2, party_at(Loc)))) :-
    agent(Agt1), agent(Agt2), location(Loc).

initially_known(inroom(ann)).
initially_known(inroom(bob)).
initially_known(party_at(cathys_house) | party_at(davids_house)).

%
%  And now for the unit tests...
%

:- begin_tests(domain_party, [sto(rational_trees)]).

test(reg1) :-
    regression(inroom(ann), read(bob), inroom(ann)).
test(reg2) :-
    regression(inroom(ann), enter(bob), inroom(ann)).
test(reg3) :-
    regression(inroom(ann), leave(bob), inroom(ann)).
test(reg4) :-
    regression(inroom(ann), enter(ann), true).
test(reg5) :-
    regression(inroom(ann), leave(ann), false).

test(adp1) :-
    adp_fluent(obs(ann, read(ann)), O, O=read(ann)).
test(adp2) :-
    adp_fluent(obs(ann, read(bob)), O, inroom(ann) & (O=read(bob))).

%@TODO
test(adp3) :-
    adp_fluent(pbu(ann), read(bob), inroom(bob)& ~inroom(ann)).

test(holds1) :-
    holds(inroom(ann), s0).

%@TODO
test(holds2, fail) :-
    holds(~inroom(ann), s0).

test(holds3) :-
    holds(~inroom(ann), do(leave(ann), s0)), !.
test(holds4) :-
    holds(inroom(ann), do(leave(bob), s0)), !.
test(holds5) :-
    holds(ext([X], inroom(X)), do(leave(bob), s0)), !.
test(holds6) :-
    holds(all([X], inroom(X)), s0).

    %@TODO
test(holds7) :-
    \+ holds(all([X], inroom(X)), do(leave(bob), s0)).

test(holds8) :-
    holds(party_at(cathys_house), s0).

test(knows1) :-
    holds(knows(ann, inroom(ann)), s0), !.
test(knows2_p) :-
    holds(knows(ann, inroom(bob)), s0), !.
test(knows3) :-
    holds(knows(bob, ~inroom(ann)), do(leave(ann), s0)), !.
test(knows4) :-
    holds(~knows(bob, inroom(ann)), do(leave(ann), s0)), !.
%@TODO
test(knows6_p) :-
    \+ holds(knows(bob, party_at(cathys_house)), s0).

test(example1) :-
    % Initially, ann does not know where the party is.
    holds(~ ext([L], knows(ann, party_at(L))), s0), !.
test(example2) :-
    % Bob knows the true location of the party after reading the invite.
    holds(knows(bob, party_at(cathys_house)), do(read(bob), s0)), !.
test(example3) :-
    % Bob knows that ann does not know where the party is.
    holds(knows(bob, ~knows(ann, party_at(cathys_house))), s0).
test(example4) :-
    % After leaving the room, bob no longer knows that
    % ann does not know where the party is.
    holds(~knows(bob, ~knows(ann, party_at(cathys_house))),
          do(leave(bob), s0)), !.

:- end_tests(domain_party).

% :- run_tests.
