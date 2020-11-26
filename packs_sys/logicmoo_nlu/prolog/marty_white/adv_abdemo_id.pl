/*

   ABDUCTIVE EVENT CALCULUS

   MURRAY SHANAHAN

   Version 1.15

   July 1998

   Written for LPA MacProlog 32

*/


/*
   This is an abductive meta-interpreter for abduction with negation-as-
   failure, with built-in features for handling event calculus queries.
   In effect this implements a partial-order planner. not(clipped) facts
   correspond to protected links. The planner will also perform
   hierarchical decomposition, given definitions of compound events
   (happens if happens clauses).

   The form of queries is as follows.

        abdemo(Goals, Residue)

   Goals is a list of atoms. Residue is a pair of [RH, RB], where RH is a
   list of happens facts and RB is a list of temporal ordering facts.
   Negations is a list of lists of atoms.

   Roughly speaking, the above formulae should hold if,

        EC and CIRC[Domain] and CIRC[Residue] |= Goals

   where EC is the event calculus axioms, CIRC[Domain] is the completion
   of initiates, terminates and  releases, and CIRC[Residue] is the
   completion of happens. (Note that completion isn't applied to temporal
   ordering facts.)

   F is expected to be fully bound when we call abdemo(holds_at(F, T)).
   It's assumed that all primitive actions (two argument happens) are
   in the residue (ie: none are in the program).

   Although the interpreter will work with any logic program, the axioms of
   the event calculus are compiled in to the meta-level.

   The function symbol "neg" is introduced to represent classically
   negated fluents. holds_at(neg(F)) corresponds to not holds_at(F)
   (with classical "not"). terminates formulae play a role in clipping
   positive fluents and initiating negative fluents. Conversely,
   initiates formulae play a role in clipping negative fluents and
   initiating positive ones.

   Computation interleaves search (abdemo) and refinement phases. After each
   step of the computation, where a step is an iteration of abdemo or a
   refinement, the residue so far plus the remaining goal list entail the goal
   state. In other words, these computational steps are also proof steps.

   An iterative deepening search strategy is employed, where depth
   corresponds to length of plan.
*/




/* TOP LEVEL */


/*
   Top level calls to abdemo have to be transformed to the following form.

        abdemo_top(Goals, ResidueIn, ResidueOut, NegationsIn, NegationsOut, DepthBound)

   The residue has the form [[HA, HC], [BA, BC]], where HA is a list of happens
   atoms, HC is the transistive closure of the before-or-equal relation on times
   that follows from HA, BA is a list of before atoms, and BC is the transitive
   closure of BA.
*/

abdemo(Gs, [HA, BA]) :-
     init_gensym(t), ticks(Z1),
     abdemo_top(Gs, [[[], []], [[], []]], [[HA, HC], [BA, BC]], [], N, 0),
     ticks(Z2), Z is (Z2-Z1)/60, write('Total time taken '), writenl(Z), nl.


abdemo_top(Gs, R1, R3, N1, N3, D) :-
     abdemo_id(Gs, R1, R2, N1, N2, D), !, abdemo_cont(R2, R3, N2, N3).


/*
   abdemo_cont carries out one step of refinement, if necessary. It then
   resumes abdemo, but with an initial depth bound equal to the length of
   the plan so far. Obviously to start from 1 again would be pointless.
*/

abdemo_cont([[HA, TC], RB], [[HA, TC], RB], N, N) :- all_executable(HA), !.

abdemo_cont([[HA, HC], [BA, BC]], R2, N1, N3) :-
     write('Abstract plan: '), write(HA), writenl(BA), nl,
     refine([[HA, HC], [BA, BC]], N1, Gs, R1, N2), action_count([[HA, HC], [BA, BC]], D),
     abdemo_top(Gs, R1, R2, N2, N3, D).


/* abdemo_id is an iterative deepening version of abdemo. */

abdemo_id(Gs, R1, R2, N1, N2, D) :-
     write('Depth: '), writenl(D), nl, abdemo(Gs, R1, R2, N1, N2, D).

abdemo_id(Gs, R1, R2, N1, N2, D1) :- D2 is D1+1, abdemo_id(Gs, R1, R2, N1, N2, D2).


all_executable([]).

all_executable([happens(A, T1, T2)|R]) :- executable(A), all_executable(R).




/* ABDEMO */


/*
   abdemo/6 is a depth bounded abdemo. It fails if it can't generate a plan
   with DepthBound or fewer steps. Calls to abdemo/6 have the following form.

        abdemo(Goals, ResidueIn, ResidueOut,
             NegationsIn, NegationsOut, DepthBound)

*/

abdemo(Gs, [[HA, TC1], [BA, TC2]], R, N1, N2, D) :-
     trace(on), write('Goals: '), writenl(Gs),
     write('Happens: '), writenl(HA),
     write('Befores: '), writenl(BA),
     write('Nafs: '), writenl(N1), nl, nl, fail.

abdemo([], R, R, N, N, D).

/*
   Now we have two clauses which are meta-level representations of the two
   event calculus axioms for holds_at.

        holds_at(F, T) <- initiallyp(F) and not clipped(0, F, T)

        holds_at(F, T3) <-
             happens(A, T1, T2) and T2 < T3 and
             initiates(A, F, T1) and not clipped(T1, F, T2)

   Note the way the object-level axioms have been compiled into the
   meta-level. Note also that happens goals are not resolved against
   clauses in the object-level program. Instead, they're put straight into
   the residue. Resolving happens goals is the job of the refinement phase.
*/

abdemo([holds_at(F1, T)|Gs1], R1, R3, N1, N4, D) :-
     F1 \= neg(F2), abresolve(initially(F1), R1, Gs2, R1, B),
     append(Gs2, Gs1, Gs3), add_neg([clipped(0, F1, T)], N1, N2),
     abdemo_naf([clipped(0, F1, T)], R1, R2, N2, N3, D),
     abdemo(Gs3, R2, R3, N3, N4, D).

/*
   The order in which resolution steps are carried out in the next
   clause is crucial. We resolve initiates first in order to instantiate
   A, but we don't want to proceed with sub-goals of initiates until
   we've added the corresponding happens and before facts to the residue.
*/

abdemo([holds_at(F1, T3)|Gs1], R1, R6, N1, N5, D) :-
     F1 \= neg(F2), abresolve(initiates(A, F1, T1), R1, Gs2, R1, B1),
     abresolve(happens(A, T1, T2), R1, [], R2, B2),
     check_depth(R2, D),
     abresolve(before(T2, T3), R2, [], R3, B3),
     append(Gs2, Gs1, Gs3), check_nafs(B2, N1, R3, R4, N1, N2, D),
     add_neg([clipped(T1, F1, T3)], N2, N3),
     abdemo_naf([clipped(T1, F1, T3)], R4, R5, N3, N4, D),
     abdemo(Gs3, R5, R6, N4, N5, D).

/*
   The next two clauses are a meta-level representation of the two
   event calculus axioms for not holds_at.

        not holds_at(F, T) <- initiallyn(F) and not declipped(0, F, T)

        not holds_at(F, T3) <-
             happens(A, T1, T2) and T2 < T3 and
             terminates(A, F, T1) and not declipped(T1, F, T2)
*/

abdemo([holds_at(neg(F), T)|Gs1], R1, R3, N1, N4, D) :-
     abresolve(initially(neg(F)), R1, Gs2, R1, B),
     append(Gs2, Gs1, Gs3), add_neg([declipped(0, F, T)], N1, N2),
     abdemo_naf([declipped(0, F, T)], R1, R2, N2, N3, D),
     abdemo(Gs3, R2, R3, N3, N4, D).

abdemo([holds_at(neg(F), T3)|Gs1], R1, R6, N1, N5, D) :-
     abresolve(terminates(A, F, T1), R1, Gs2, R1, B1),
     abresolve(happens(A, T1, T2), R1, [], R2, B2),
     check_depth(R2, D),
     abresolve(before(T2, T3), R2, [], R3, B3),
     append(Gs2, Gs1, Gs3), check_nafs(B2, N1, R3, R4, N1, N2, D),
     add_neg([declipped(T1, F, T3)], N2, N3),
     abdemo_naf([declipped(T1, F, T3)], R4, R5, N3, N4, D),
     abdemo(Gs3, R5, R6, N4, N5, D).

/*
   The next two clauses cater for happens goals.

   Note that happens goals only appear either in the initial goal list or,
   in an "expand" goal, as a result of refinement. Note also that these
   clauses, because of the way abresolve(happens) works, will ensure sharing
   of sub-goals between compound events wherever possible.
*/

abdemo([happens(A, T1, T2)|Gs], R1, R4, N1, N3, D) :-
     !, abresolve(happens(A, T1, T2), R1, [], R2, B), check_depth(R2, D),
     check_nafs(B, N1, R2, R3, N1, N2, D), abdemo(Gs, R3, R4, N2, N3, D).

abdemo([happens(A, T)|Gs], R1, R4, N1, N3, D) :-
     !, abresolve(happens(A, T), R1, [], R2, B), check_depth(R2, D),
     check_nafs(B, N1, R2, R3, N1, N2, D), abdemo(Gs, R3, R4, N2, N3, D).

/*
   The next clause deals with the expansion of compound actions. These appear
   as goals of the form expand([happens(A, T1, T2)|Bs]), where happens(A, T1, T2)
   is the compound action to be expanded and Bs is a list of "before" goals.
   These goals are placed in the goal list by calls to "refine". The various
   sub-goals generated by the expansion are treated in a careful order. First,
   the compound action's sub-actions are added to the residue. Then the "before"
   goals that "refine" took out of the residue are put back (with their newly
   skolemised time constants). The holds_at goals are solved next, on the
   assumption that they are either guards or supply context for the expansion.
   Then other, non event calculus goals are dealt with. Only then, with all the
   temporal ordering constraints sorted out, is it safe to tackle not(clipped)
   and not(declipped) goals, first those that are part of the compound action
   definition, then those recorded in the negations list.
*/

abdemo([expand([happens(A, T1, T2)|Bs])|Gs1], R1, R8, N1, N8, D) :-
     !, axiom(happens(A, T1, T2), Gs2),
     add_sub_actions(Gs2, R1, R2, N1, N2, D, Hs),
     solve_befores(Bs, R2, R3, N2, N3, D),
     abdemo_holds_ats(Gs2, R3, R4, N3, N4, D),
     solve_other_goals(Gs2, R4, R5, N4, N5, D),
     check_clipping(Gs2, R5, R6, N5, N6, D),
     check_sub_action_nafs(Hs, N1, R6, R7, N6, N7, D),
     abdemo(Gs1, R7, R8, N7, N8, D).

/*
   The last two clauses cater for the general case (ie: goals other
   than holds_at and happens). They're also used to tackle domain
   constraints (ie: holds_at if holds_at clauses).
*/

abdemo([not(G)|Gs], R1, R3, N1, N4, D) :-
     !, abdemo_naf([G], R1, R2, N1, N2, D), add_neg([G], N2, N3),
     abdemo(Gs, R2, R3, N3, N4, D).

abdemo([G|Gs1], R1, R3, N1, N2, D) :-
     abresolve(G, R1, Gs2, R2, B), append(Gs2, Gs1, Gs3),
     abdemo(Gs3, R2, R3, N1, N2, D).


check_depth(R, D) :- action_count(R, L), L =< D.


action_count([[HA, TC], RB], L) :- length(HA, L).




/* EXPANSION OF COMPOUND ACTIONS */


/* The following collection of predicates are called by abdemo(expand). */


abdemo_holds_ats([], R, R, N, N, D).

abdemo_holds_ats([holds_at(F, T)|Gs], R1, R3, N1, N3, D) :-
     !, abdemo([holds_at(F, T)], R1, R2, N1, N2, D),
     abdemo_holds_ats(Gs, R2, R3, N2, N3, D).

abdemo_holds_ats([G|Gs], R1, R2, N1, N2, D) :-
     abdemo_holds_ats(Gs, R1, R2, N1, N2, D).


solve_other_goals([], R, R, N, N, D).

solve_other_goals([G|Gs], R1, R3, N1, N3, D) :-
     G \= holds_at(F, T), G \= happens(A, T1, T2),
     G \= happens(A, T), G \= before(T1, T2),
     G \= not(clipped(T1, F, T2)), G \= not(declipped(T1, F, T2)), !,
     abdemo([G], R1, R2, N1, N2, D),
     solve_other_goals(Gs, R2, R3, N2, N3, D).

solve_other_goals([G|Gs], R1, R2, N1, N2, D) :-
     solve_other_goals(Gs, R1, R2, N1, N2, D).

/*
   In its last argument, add_sub_actions accumulates a list of new actions
   added to the residue, so that subsequent re-checking of not(clipped) and
   not(declipped) goals can be done via check_nafs rather than the less
   efficient abdemo_nafs.
*/

add_sub_actions([], R, R, N, N, D, []).

add_sub_actions([happens(A, T1, T2)|Gs], R1, R3, N1, N3, D, Hs2) :-
     !, abresolve(happens(A, T1, T2), R1, [], R2, B), check_depth(R2, D),
     add_sub_actions(Gs, R2, R3, N2, N3, D, Hs1), cond_add(B, happens(A, T1, T2), Hs1, Hs2).

add_sub_actions([happens(A, T)|Gs], R1, R3, N1, N3, D, Hs2) :-
     !, abresolve(happens(A, T), R1, [], R2, B), check_depth(R2, D),
     add_sub_actions(Gs, R2, R3, N2, N3, D, Hs1), cond_add(B, happens(A, T, T), Hs1, Hs2).

add_sub_actions([before(T1, T2)|Gs], R1, R3, N1, N3, D, Hs) :-
     !, abresolve(before(T1, T2), R1, [], R2, B),
     add_sub_actions(Gs, R2, R3, N2, N3, D, Hs).

add_sub_actions([G|Gs], R1, R2, N1, N2, D, Hs) :-
     add_sub_actions(Gs, R1, R2, N1, N2, D, Hs).


cond_add(false, H, Hs, Hs) :- !.

cond_add(true, H, Hs, [H|Hs]).


solve_befores([], R, R, N, N, D).

solve_befores([before(T1, T2)|Gs], R1, R3, N1, N3, D) :-
     !, abdemo([before(T1, T2)], R1, R2, N1, N2, D),
     solve_befores(Gs, R2, R3, N2, N3, D).

solve_befores([G|Gs], R1, R2, N1, N2, D) :-
     solve_befores(Gs, R1, R2, N1, N2, D).


check_sub_action_nafs([], N1, R, R, N2, N2, D) :- !.

check_sub_action_nafs([happens(A, T1, T2)|Hs], N1, R1, R3, N2, N4, D) :-
     check_nafs(A, T1, T2, N1, R1, R2, N2, N3, D),
     check_sub_action_nafs(Hs, N1, R2, R3, N3, N4, D).


check_clipping([], R, R, N, N, D) :- !.

check_clipping([not(clipped(T1, F, T2))|Gs], R1, R3, N1, N3, D) :-
     !, abdemo_naf([clipped(T1, F, T2)], R1, R2, N1, N2, D),
     check_clipping(Gs, R2, R3, N2, N3, D).

check_clipping([not(declipped(T1, F, T2))|Gs], R1, R3, N1, N3, D) :-
     !, abdemo_naf([declipped(T1, F, T2)], R1, R2, N1, N2, D),
     check_clipping(Gs, R2, R3, N2, N3, D).

check_clipping([G|Gs], R1, R2, N1, N2, D) :-
     check_clipping(Gs, R1, R2, N1, N2, D).




/* ABRESOLVE */


/*
   The form of a call to abresolve is as follows.

        abresolve(Goal, ResidueIn, Goals, ResidueOut, Flag)

   where Goals is the body of clause resolved with, and Flag is set to true
   if a happens fact has been added to the residue.
*/

abresolve(terms_or_rels(A, F, T), R, Gs, R, false) :- axiom(releases(A, F, T), Gs).

abresolve(terms_or_rels(A, F, T), R, Gs, R, false) :- !, axiom(terminates(A, F, T), Gs).

abresolve(inits_or_rels(A, F, T), R, Gs, R, false) :- axiom(releases(A, F, T), Gs).

abresolve(inits_or_rels(A, F, T), R, Gs, R, false) :- !, axiom(initiates(A, F, T), Gs).

/*
   happens goals get checked to see if they are already in the residue.
   This permits the re-use of actions already in the residue. However,
   this decision may lead to later failure, in which case we try adding
   a new action to the residue.

   happens goals aren't resolved against object-level program clauses.
   Only goals that are marked as expand(goal) are resolved against
   program clauses, and this is done by abdemo.

   Time variables get skolemised, but not action variables because
   they end up getting instantiated anyway.
*/

abresolve(happens(A, T), R1, Gs, R2, B) :- !, abresolve(happens(A, T, T), R1, Gs, R2, B).

abresolve(happens(A, T1, T2), [[HA, TC], RB], [], [[HA, TC], RB], false) :-
     member(happens(A, T1, T2), HA).

abresolve(happens(A, T, T), [[HA, TC], RB], [], [[[happens(A, T, T)|HA], TC], RB], B) :-
     executable(A), !, B = true, skolemise(T).

abresolve(happens(A, T1, T2), R1, [], R2, B) :-
     !, B = true, skolemise(T1), skolemise(T2), add_happens(A, T1, T2, R1, R2).

/*
   If either X or Y is not bound in a call to abresolve(before(X, Y)) then
   they get skolemised.
*/

abresolve(before(X, Y), R, [], R, false) :-
     skolemise(X), skolemise(Y), demo_before(X, Y, R), !.

abresolve(before(X, Y), R1, [], R2, B) :-
     !, B = false, skolemise(X), skolemise(Y), \+ demo_beq(Y, X, R1),
     add_before(X, Y, R1, R2).

/*
   The predicates "diff" (meaning not equal) and "is" (for evaluating
   arithmetic expressions) are built in.
*/

abresolve(diff(X, Y), R, [], R, false) :- !, X \= Y.

abresolve(is(X, Y), R, [], R, false) :- !, X is Y.

abresolve(G, R, [], [G|R], false) :- abducible(G).

abresolve(G, R, Gs, R, false) :- axiom(G, Gs).




/* ABDEMO_NAFS and CHECK_NAFS */


/*
   abdemo_nafs([G1...Gn], R1, R2) demos not(G1) and ... and not(Gn).

   Calls to abdemo_naf have the following form.

        abdemo_nafs(Negations, ResidueIn, ResidueOut,
             NegationsIn, NegationsOut, DepthIn, DepthOut)

   where Negations is the list of negations to be established, ResidueIn
   is the old residue, ResidueOut is the new residue (abdemo_nafs can add
   both before and happens facts, as well as other abducibles, to the
   residue), NegationsIn is the old list of negations (same as Negations
   for top-level call), and NegationsOut is the new list of negations
   (abdemo_nafs can add new clipped goals to NegationsIn).

   DepthIn and DepthOut keep track of the number of sub-goals generated,
   for iterative deepening purposes.
*/


abdemo_nafs([], R, R, N, N, D).

abdemo_nafs([N|Ns], R1, R3, N1, N3, D) :-
     abdemo_naf(N, R1, R2, N1, N2, D), abdemo_nafs(Ns, R2, R3, N2, N3, D).


/*
   abdemo_naf([G1...Gn], R1, R2) demos not((G1) and ... and (Gn)).

   As for abdemo, the main event calculus axioms are compiled into the
   meta-level in abdemo_naf. In addition to the two holds_at axioms, we
   have,

        clipped(T1, F, T3) <-
             happens(A, T2) and T1 < T2 < T3 and
             [terminates(A, F, T2) or releases(A, F, T2)]

        declipped(T1, F, T3) <-
             happens(A, T2) and T1 < T2 < T3 and
             [initiates(A, F, T2) or releases(A, F, T2)]

   We have to use findall here, because all ways of achieving a goal
   have to fail for the goal itself to fail.

   Note that only the two-argument version of happens is used, because
   the residue only contains instantaneous actions, and the effects of
   compound actions are assumed to match the effects of their
   component actions.
*/

abdemo_naf([clipped(T1, F, T4)|Gs1], R1, R2, N1, N2, D) :-
     !, findall(Gs3,
          (abresolve(terms_or_rels(A, F, T2), R1, Gs2, R1, false),
          abresolve(happens(A, T2, T3), R1, [], R1, false),
          append([before(T1, T3), before(T2, T4)|Gs2], Gs1, Gs3)), Gss),
     abdemo_nafs(Gss, R1, R2, N1, N2, D).

abdemo_naf([declipped(T1, F, T4)|Gs1], R1, R2, N1, N2, D) :-
     !, findall(Gs3,
          (abresolve(inits_or_rels(A, F, T2), R1, Gs2, R1, false),
          abresolve(happens(A, T2, T3), R1, [], R1, false),
          append([before(T1, T3), before(T2, T4)|Gs2], Gs1, Gs3)), Gss),
     abdemo_nafs(Gss, R1, R2, N1, N2, D).

/*
   To show the classical negation of holds_at(F) (which is what we want), we
   have to show that holds_at(neg(F)). Conversely, to show the classical
   negation of holds_at(neg(F)) we have to show holds_at(F). Within a call
   to abdemo_naf, we can add both happens and before (and other abducibles)
   to the residue. This removes a potential source of incompleteness.

   Note that F must_mw be a ground term to preserve soundness and completeness.
   To guarantee this, all variables that occur in the body of an
   initiates, terminates or releases clause must_mw occur in the fluent
   argument in its head.
*/

/* DANGER: Cut in next clause rules out other ways to solve holds_at(F2, T). */

abdemo_naf([holds_at(F1, T)|Gs1], R1, R3, N1, N3, D) :-
     opposite(F1, F2), copy_term(Gs1, Gs2),
     abdemo([holds_at(F2, T)], R1, R2, N1, N2, D), !,
     abdemo_naf_cont(R1, Gs2, R2, R3, N1, N3, D).

abdemo_naf([holds_at(F, T)|Gs], R1, R2, N1, N2, D) :-
     !, abdemo_naf(Gs, R1, R2, N1, N2, D).

/*
   Special facilities for handling temporal ordering facts are built in.
   We can add a before fact to the residue to preserve the failure of
   a clipped goal. The failure of a before goal does NOT mean that the
   negation of that goal is assumed to be true. (The Clark completion is
   not applicable to temporal ordering facts.) Rather, to make before(X, Y)
   fail, before(Y, X) has to follow. One way to achieve this is to add
   before(Y, X) to the residue.
*/

abdemo_naf([before(X, Y)|Gs], R, R, N, N, D) :- X == Y, !.

abdemo_naf([before(X, Y)|Gs], R, R, N, N, D) :- demo_before(Y, X, R), !.

abdemo_naf([before(X, Y)|Gs1], R1, R2, N1, N2, D) :-
     !, append(Gs1, [postponed(before(X, Y))], Gs2),
     abdemo_naf(Gs2, R1, R2, N1, N2, D).

/*
   A before fact is only added to the residue as a last resort. Accordingly,
   if we encounter a before(X, Y) goal and cannot show before(Y, X), we
   postpone that goal until we've tried other possibilities. A postponed
   before goal appears in the goal list as postponed(before(X, Y)).
*/

abdemo_naf([postponed(before(X, Y))|Gs], R1, R2, N, N, D) :-
     \+ demo_beq(X, Y, R1), add_before(Y, X, R1, R2).

abdemo_naf([postponed(before(X, Y))|Gs], R1, R2, N1, N2, D) :-
     !, abdemo_naf(Gs, R1, R2, N1, N2, D).

/*
   We drop through to the general case for goals other than special event
   calculus goals.
*/

/* DANGER: Cut in next clause rules out other ways to solve G. */

abdemo_naf([not(G)|Gs1], R1, R3, N1, N3, D) :-
     copy_term(Gs1, Gs2), abdemo([G], R1, R2, N1, N2, D), !,
     abdemo_naf_cont(R1, Gs2, R2, R3, N1, N3, D).

abdemo_naf([not(G)|Gs], R1, R2, N1, N2, D) :- !, abdemo_naf(Gs, R1, R2, N1, N2, D).

abdemo_naf([G|Gs1], R, R, N, N, D) :- \+ abresolve(G, R, Gs2, R, false), !.

abdemo_naf([G1|Gs1], R1, R2, N1, N2, D) :-
     findall(Gs2, (abresolve(G1, R1, Gs3, R1, false), append(Gs3, Gs1, Gs2)), Gss),
     abdemo_nafs(Gss, R1, R2, N1, N2, D).


/*
   abdemo_naf_cont gets an extra opportunity to succeed if the residue
   has been altered. This is determined by comparing R1 with R2. If
   a sub-goal has failed and the residue hasn't been altered, there's
   no need to look for other ways to prove the negation of the overall goal.
*/

abdemo_naf_cont(R1, Gs, R2, R2, N, N, D).
abdemo_naf_cont(R1, Gs, R2, R3, N1, N2, D) :- R1 \= R2, abdemo_naf(Gs, R1, R3, N1, N2, D).


/*
   check_nafs is just like abdemo_nafs, except that it only checks
   negated clipped and declipped facts against the most recent event
   added to the residue. To check one of these negations, not only can
   we confine our attention to the most recent event, but we can ignore
   that event if it doesn't fall inside the interval covered by the
   clipped/declipped in question. Of course, the negated clipped/declipped
   fact might depend on other holds_at facts. But their preservation is
   ensured because the clipped/declipped negation they themselves depend
   on will also be checked.
*/


check_nafs(false, N1, R, R, N2, N2, D) :- !.

check_nafs(true, N, [[[happens(A, T1, T2)|HA], TC], RB], R, N1, N2, D) :-
     check_nafs(A, T1, T2, N, [[[happens(A, T1, T2)|HA], TC], RB], R, N1, N2, D).

check_nafs(A, T1, T2, [], R, R, N, N, D).

check_nafs(A, T1, T2, [N|Ns], R1, R3, N1, N3, D) :-
     check_naf(A, T1, T2, N, R1, R2, N1, N2, D),
     check_nafs(A, T1, T2, Ns, R2, R3, N2, N3, D).


check_naf(A, T2, T3, [clipped(T1, F, T4)], R1, R2, N1, N2, D) :-
     !, findall([before(T1, T3), before(T2, T4)|Gs],
          (abresolve(terms_or_rels(A, F, T2), R1, Gs, R1, false)), Gss),
     abdemo_nafs(Gss, R1, R2, N1, N2, D).

check_naf(A, T2, T3, [declipped(T1, F, T4)], R1, R2, N1, N2, D) :-
     !, findall([before(T1, T3), before(T2, T4)|Gs],
          (abresolve(inits_or_rels(A, F, T2), R1, Gs, R1, false)), Gss),
     abdemo_nafs(Gss, R1, R2, N1, N2, D).

check_naf(A, T1, T2, N, R1, R2, N1, N2, D) :- abdemo_naf(N, R1, R2, N1, N2, D).




/* DEMO_BEFORE, ADD_BEFORE and ADD_HAPPENS */


/*
   demo_before simply checks membership of the transitive closure half of
   the temporal ordering part of the residue. Likewise demo_beq checks for
   demo_before or for membership of the transitive closure half of the
   happens part of the residue.
*/

demo_before(0, Y, R) :- !, Y \= 0.

demo_before(X, Y, [RH, [BA, TC]]) :- member(before(X, Y), TC).

/* demo_beq is demo before or equal. */

demo_beq(X, Y, R) :- X == Y, !.

demo_beq(X, Y, R) :- demo_before(X, Y, R), !.

demo_beq(X, Y, [[HA, TC], RB]) :- member(beq(X, Y), TC).


/*
   add_before(X, Y, R1, R2) adds before(X, Y) to the residue R1 giving R2.
   It does this by maintaining the transitive closure of the before and beq
   relations in R2, and assumes that R1 is already transitively closed.
   R1 and R2 are just the temporal ordering parts of the residue.
*/

add_before(X, Y, [RH, [BA, TC]], [RH, [BA, TC]]) :- member(before(X, Y), TC), !.

add_before(X, Y, [[HA, HC], [BA, BC1]], [[HA, HC], [[before(X, Y)|BA], BC2]]) :-
     \+ demo_beq(Y, X, [[HA, HC], [BA, BC1]]), find_bef_connections(X, Y, BC1, C1, C2),
     find_beq_connections(X, Y, HC, C3, C4), delete(X, C3, C5), delete(Y, C4, C6),
     append(C5, C1, C7), append(C6, C2, C8),
     cross_prod_bef(C7, C8, C9, BC1), append(C9, BC1, BC2).

/*
   add_happens(A, T1, T2, R1, R2) adds happens(A, T1, T2) to the residue R1
   giving R2. Because happens(A, T1, T2) -> T1 <= T2, this necessitates
   updating the transitive closures of the before and beq facts in the residue.
   Duplicates aren't checked for, as it's assumed this is done by the
   calling predicate.
*/

add_happens(A, T1, T2, [[HA, HC1], [BA, BC1]], [[[happens(A, T1, T2)|HA], HC2], [BA, BC2]]) :-
     \+ demo_before(T2, T1, [[HA, HC1], [BA, BC1]]),
     find_beq_connections(T1, T2, HC1, C1, C2), cross_prod_beq(C1, C2, C3, HC1),
     append(C3, HC1, HC2), find_bef_connections(T1, T2, BC1, C4, C5),
     cross_prod_bef(C4, C5, C6, BC1), delete(before(T1, T2), C6, C7),
     append(C7, BC1, BC2).

/*
   find_bef_connections(X, Y, TC, C1, C2) creates two lists, C1 and C2,
   containing respectively all the time points before X and after
   Y according to TC, which is assumed to be transitively closed.
*/

find_bef_connections(X, Y, [], [X], [Y]).

find_bef_connections(X, Y, [before(Z, X)|R], [Z|C1], C2) :-
     !, find_bef_connections(X, Y, R, C1, C2).

find_bef_connections(X, Y, [before(Y, Z)|R], C1, [Z|C2]) :-
     !, find_bef_connections(X, Y, R, C1, C2).

find_bef_connections(X, Y, [before(Z1, Z2)|R], C1, C2) :-
     find_bef_connections(X, Y, R, C1, C2).

/*
   find_beq_connections is like find_bef_connections, except that it works
   on the transtive closure of happens part of the residue.
*/

find_beq_connections(X, Y, [], [X], [Y]).

find_beq_connections(X, Y, [beq(Z, X)|R], [Z|C1], C2) :-
     !, find_beq_connections(X, Y, R, C1, C2).

find_beq_connections(X, Y, [beq(Y, Z)|R], C1, [Z|C2]) :-
     !, find_beq_connections(X, Y, R, C1, C2).

find_beq_connections(X, Y, [beq(Z1, Z2)|R], C1, C2) :-
     find_beq_connections(X, Y, R, C1, C2).


cross_prod_bef([], C, [], R).

cross_prod_bef([X|C1], C2, R3, R) :-
     cross_one_bef(X, C2, R1, R), cross_prod_bef(C1, C2, R2, R), append(R1, R2, R3).

cross_one_bef(X, [], [], R).

cross_one_bef(X, [Y|C], [before(X, Y)|R1], R) :-
     \+ member(before(X, Y), R), !, cross_one_bef(X, C, R1, R).

cross_one_bef(X, [Y|C], R1, R) :- cross_one_bef(X, C, R1, R).


cross_prod_beq([], C, [], R).

cross_prod_beq([X|C1], C2, R3, R) :-
     cross_one_beq(X, C2, R1, R), cross_prod_beq(C1, C2, R2, R), append(R1, R2, R3).

cross_one_beq(X, [], [], R).

cross_one_beq(X, [Y|C], [beq(X, Y)|R1], R) :-
     \+ member(beq(X, Y), R), !, cross_one_beq(X, C, R1, R).

cross_one_beq(X, [Y|C], R1, R) :- cross_one_beq(X, C, R1, R).




/* REFINE */


/*
   refine(R1, N1, Gs, R2, N2) takes the earliest compound action out of the residue
   R1 and puts it in the goal list Gs, along with all "before", not(clipped) and
   not(declipped) facts that refer to the same time points. These time points
   are unskolemised, so that they can be bound to existing time points in the
   residue, thus permitting sub-actions to be shared between abstract actions.
   This almost restores the state of computation to the state S before the chosen
   action was added to the residue in the first place. But not quite, because
   some derived before facts will be retained in the transitive closure part
   of the before part of the residue that weren't there in state S. This doesn't
   matter, however, because these derived facts will need to be there eventually
   anyway, when those before facts that have been transferred from residue to goal
   list get put back in the residue.

   The compound action extracted from the residue is marked for expansion by
   inserting it in the goal list in the form expand([happens(A, T1, T2)|Bs]),
   where Bs is the list of "before" facts taken out of the residue, as
   described above. When abdemo comes across a goal of this form, it will
   resolve the "happens" part against program clauses. The reason for
   not doing this resolution here is that we want to backtrack to alternative
   definitions of a compound goal inside the iterative deepening search, not
   outside it. This ensures the desired behaviour for compound actions which
   expand to one series of sub-actions given certain conditions but to another
   series of sub-actions given other conditions. Otherwise the check for these
   conditions, which will be a holds_at goal in the compound action definition,
   is in danger of being treated as a goal to be established instead of just
   checked.
*/

refine([[HA1, HC1], [BA1, BC1]], N1, Gs, [[HA2, HC2], [BA2, BC2]], N3) :-
     split_happens(HA1, [BA1, BC1], happens(A, T1, T2), HA2),
     split_beqs(HC1, T1, T2, T3, T4, HC3, HC2),
     split_befores(BA1, T1, T2, T3, T4, BA3, BA2),
     split_befores(BC1, T1, T2, T3, T4, BC3, BC2),
     split_nafs(N1, T1, T2, T3, T4, N2, N3),
     append([expand([happens(A, T3, T4)|BA3])], N2, Gs).


/*
   split_happens(RH1, RB, H, RH2) holds if H is the earliest non-executable
   (abstract) action in RH1 according to RB. The remainder of RH1 ends up in
   RH2. If there are no non-executable actions, H is the earliest action.
*/

split_happens([happens(A, T1, T2)], RB, happens(A, T1, T2), []) :- !.

split_happens([happens(A1, T1, T2)|RH1], RB, happens(A3, T5, T6),
          [happens(A4, T7, T8)|RH2]) :-
     split_happens(RH1, RB, happens(A2, T3, T4), RH2),
     order(happens(A1, T1, T2), happens(A2, T3, T4), RB,
          happens(A3, T5, T6), happens(A4, T7, T8)).


order(happens(A1, T1, T2), happens(A2, T3, T4), RB,
          happens(A1, T1, T2), happens(A2, T3, T4)) :-
     \+ executable(A1), executable(A2), !.

order(happens(A1, T1, T2), happens(A2, T3, T4), RB,
          happens(A2, T3, T4), happens(A1, T1, T2)) :-
     \+ executable(A2), executable(A1), !.

order(happens(A1, T1, T2), happens(A2, T3, T4), RB,
          happens(A1, T1, T2), happens(A2, T3, T4)) :-
     demo_before(T1, T3, [[], RB]), !.

order(happens(A1, T1, T2), happens(A2, T3, T4), RB, happens(A2, T3, T4), happens(A1, T1, T2)).


split_befores([], T1, T2, T3, T4, [], []).

split_befores([before(T1, T2)|Bs1], T3, T4, T5, T6, Bs2, [before(T1, T2)|Bs3]) :-
     no_match(T1, T2, T3, T4), !, split_befores(Bs1, T3, T4, T5, T6, Bs2, Bs3).

split_befores([before(T1, T2)|Bs1], T3, T4, T5, T6, [before(T7, T8)|Bs2], Bs3) :-
     substitute_time(T1, T3, T4, T5, T6, T7), substitute_time(T2, T3, T4, T5, T6, T8),
     split_befores(Bs1, T3, T4, T5, T6, Bs2, Bs3).


split_beqs([], T1, T2, T3, T4, [], []).

split_beqs([beq(T1, T2)|Bs1], T3, T4, T5, T6, Bs2, [beq(T1, T2)|Bs3]) :-
     no_match(T1, T2, T3, T4), !, split_beqs(Bs1, T3, T4, T5, T6, Bs2, Bs3).

split_beqs([beq(T1, T2)|Bs1], T3, T4, T5, T6, [beq(T7, T8)|Bs2], Bs3) :-
     substitute_time(T1, T3, T4, T5, T6, T7), substitute_time(T2, T3, T4, T5, T6, T8),
     split_beqs(Bs1, T3, T4, T5, T6, Bs2, Bs3).


split_nafs([], T1, T2, T3, T4, [], []).

split_nafs([[clipped(T1, F, T2)]|Bs1], T3, T4, T5, T6, Bs2, [[clipped(T1, F, T2)]|Bs3]) :-
     no_match(T1, T2, T3, T4), !, split_nafs(Bs1, T3, T4, T5, T6, Bs2, Bs3).

split_nafs([[clipped(T1, F, T2)]|Bs1], T3, T4, T5, T6,
          [not(clipped(T7, F, T8))|Bs2], Bs3) :-
     !, substitute_time(T1, T3, T4, T5, T6, T7), substitute_time(T2, T3, T4, T5, T6, T8),
     split_nafs(Bs1, T3, T4, T5, T6, Bs2, Bs3).

split_nafs([[declipped(T1, F, T2)]|Bs1], T3, T4, T5, T6, Bs2,
          [[declipped(T1, F, T2)]|Bs3]) :-
     no_match(T1, T2, T3, T4), !, split_nafs(Bs1, T3, T4, T5, T6, Bs2, Bs3).

split_nafs([[declipped(T1, F, T2)]|Bs1], T3, T4, T5, T6,
          [not(declipped(T7, F, T8))|Bs2], Bs3) :-
     !, substitute_time(T1, T3, T4, T5, T6, T7), substitute_time(T2, T3, T4, T5, T6, T8),
     split_nafs(Bs1, T3, T4, T5, T6, Bs2, Bs3).

split_nafs([N|Bs1], T3, T4, T5, T6, Bs2, [N|Bs3]) :-
     substitute_time(T1, T3, T4, T5, T6, T7), substitute_time(T2, T3, T4, T5, T6, T8),
     split_nafs(Bs1, T3, T4, T5, T6, Bs2, Bs3).


substitute_time(T1, T1, T2, T3, T4, T3) :- !.

substitute_time(T2, T1, T2, T3, T4, T4) :- !.

substitute_time(T1, T2, T3, T4, T5, T1).


no_match(T1, T2, T3, T4) :- T1 \= T3, T2 \= T3, T1 \= T4, T2 \= T4.




/* OTHER BITS AND PIECES */


/*
   add_neg(N, Ns1, Ns2) adds goal N to the list of (lists of) negations Ns1,
   giving Ns2. Duplicates are ignored, but N must_mw be fully bound.
*/

add_neg(N, Ns, Ns) :- member(N, Ns), !.

add_neg(N, Ns, [N|Ns]).


/* append_negs is just append, but ignoring duplicates. */

append_negs([], [], []).

append_negs([N|Ns1], Ns2, Ns4) :- add_neg(N, Ns2, Ns3), append(Ns1, Ns3, Ns4).


delete(X, [], []).

delete(X, [X|L], L) :- !.

delete(X, [Y|L1], [Y|L2]) :- delete(X, L1, L2).


/* Skolemisation */

skolemise(T) :- var(T), gensym(t, T), !.

skolemise(T).


opposite(neg(F), F) :- !.

opposite(F, neg(F)).


trace(off).


