/*

   ABDUCTIVE EVENT CALCULUS

   MURRAY SHANAHAN

   Version 1.9a

   September 1997

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

   Goals is a list of atoms (goals). Residue is a pair of lists of atoms
   [RH, RB], where RH contains happens facts and RB contains temporal
   ordering facts. Negations is a list of lists of atoms.

   Roughly speaking, the above formulae should hold if,

        EC and CIRC[Domain] and CIRC[ResidueOut] |= Goals

   where EC is the event calculus axioms, CIRC[Domain] is the completion
   of initiates, terminates and  releases, and CIRC[ResidueOut] is the
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

*/

ticks(Z1):- statistics(runtime, [Z1, _]).
writenl(X):- write(X), nl.

:- style_check(- singleton).

/*
   Top level calls to abdemo have to be transformed to the following form.

        abdemo(Goals, ResidueIn, ResidueOut, NegationsIn, NegationsOut)
*/

abdemo(Gs, R) :-
     ticks(Z1), abdemo(Gs, [[], []], R, [], N), ticks(Z2),
     Z is (Z2-Z1)/60, write('Total time taken '), writenl(Z), nl.

abdemo([], R, R, N, N).


/*
   The next few clauses are the result of compiling the axioms of
   the event calculus into the meta-level. The first of these clauses
   checks to see whether a holds_at goal is already provable from the
   residue. (Note that, in such cases, we still need to record and
   preserve the not(clipped) facts the goal depends on.)
   DANGER: The cut in the following clause is a source of incompleteness,
   but without it we get duplicate solutions.
*/

abdemo([holds_at(F, T)|Gs], R1, R2, N1, N3) :-
     demo([holds_at(F, T)], R1, N1, N2), !, abdemo(Gs, R1, R2, N2, N3).

/*
   Now we have two clauses which are meta-level representation of the two
   event calculus axioms for holds_at.

        holds_at(F, T) <- initiallyp(F) and not clipped(0, F, T)

        holds_at(F, T3) <-
             happens(A, T1, T2) and T2 < T3 and
             initiates(A, F, T1) and not clipped(T1, F, T2)
*/

abdemo([holds_at(F1, T)|Gs1], R1, R3, N1, N4) :-
     F1 \= neg(F2), abresolve(initially(F1), R1, Gs2, R1, B),
     append(Gs2, Gs1, Gs3), add_neg([clipped(0, F1, T)], N1, N2),
     abdemo_naf([clipped(0, F1, T)], R1, R2, N2, N3),
     abdemo(Gs3, R2, R3, N3, N4).

/*
   The order in which resolution steps are carried out in the next
   clause is crucial. We resolve initiates first in order to instantiate
   A, but we don't want to proceed with sub-goals of initiates until
   we've added the corresponding happens and before facts to the residue.
*/

abdemo([holds_at(F1, T3)|Gs1], R1, R7, N1, N6) :-
     F1 \= neg(F2), abresolve(initiates(A, F1, T1), R1, Gs2, R1, B1),
     abresolve(happens(A, T1, T2), R1, Gs3, R2, B2),
     abdemo(Gs3, R2, R3, N1, N2),
     abresolve(before(T2, T3), R3, [], R4, B3),
     append(Gs2, Gs1, Gs4), check_nafs(B2, N2, R4, R5, N2, N3),
     add_neg([clipped(T1, F1, T3)], N3, N4),
     abdemo_naf([clipped(T1, F1, T3)], R5, R6, N4, N5),
     abdemo(Gs4, R6, R7, N5, N6).

/*
   The next two clauses are a meta-level representation of the two
   event calculus axioms for not holds_at.

        not holds_at(F, T) <- initiallyn(F) and not declipped(0, F, T)

        not holds_at(F, T3) <-
             happens(A, T1, T2) and T2 < T3 and
             terminates(A, F, T1) and not declipped(T1, F, T2)
*/

abdemo([holds_at(neg(F), T)|Gs1], R1, R3, N1, N4) :-
     abresolve(initially(neg(F)), R1, Gs2, R1, B),
     append(Gs2, Gs1, Gs3), add_neg([declipped(0, F, T)], N1, N2),
     abdemo_naf([declipped(0, F, T)], R1, R2, N2, N3),
     abdemo(Gs3, R2, R3, N3, N4).

abdemo([holds_at(neg(F), T3)|Gs1], R1, R7, N1, N6) :-
     abresolve(terminates(A, F, T1), R1, Gs2, R1, B1),
     abresolve(happens(A, T1, T2), R1, Gs3, R2, B2),
     abdemo(Gs3, R2, R3, N1, N2),
     abresolve(before(T2, T3), R3, [], R4, B3),
     append(Gs2, Gs1, Gs4), check_nafs(B2, N2, R4, R5, N2, N3),
     add_neg([declipped(T1, F, T3)], N3, N4),
     abdemo_naf([declipped(T1, F, T3)], R5, R6, N4, N5),
     abdemo(Gs4, R6, R7, N5, N6).

/*
   The last two clauses cater for the general case (ie: goals other
   than holds_at).
*/

abdemo([not(G)|Gs], R1, R3, N1, N4) :-
     !, abdemo_naf([G], R1, R2, N1, N2), add_neg([G], N2, N3),
     abdemo(Gs, R2, R3, N3, N4).

abdemo([G|Gs1], R1, R3, N1, N2) :-
     abresolve(G, R1, Gs2, R2, B), append(Gs2, Gs1, Gs3),
     abdemo(Gs3, R2, R3, N1, N2).




/*
   The form of a call to abresolve is as follows.

        abresolve(Goal, ResidueIn, Goals, ResidueOut, Flag)

   where Goals is the body of clause resolved with, and Flag is set to true
   if a happens fact has been added to the residue.
*/

abresolve(happens(A, T, T), R1, Gs, R2, B) :- abresolve(happens(A, T), R1, Gs, R2, B).

abresolve(happens(A, T1, T2), R, Gs, R, false) :- !, axiom(happens(A, T1, T2), Gs).

/*
   happens goals get checked to see if they are already in the residue.
   This permits the re-use of actions already in the residue. However,
   this decision may lead to later failure, in which case we try adding
   a new action to the residue.
*/

abresolve(happens(A, T), [RH, RB], [], [RH, RB], false) :- member(happens(A, T), RH).

/*
   Time variables get skolemised, but not action variables because
   they end up getting instantiated anyway.
*/

abresolve(happens(A, T), [RH, RB], [], [[happens(A, T)|RH], RB], true) :-
     !, skolemise(T), executable(A).

/*
   It's assumed that X and Y are bound when we call abresolve(before(X, Y)).
   If either X or Y is not bound, we may miss solutions due to the cut in
   the following clause.
*/

abresolve(before(X, Y), [RH, RB], [], [RH, RB], false) :- demo_before(X, Y, RB), !.

abresolve(before(X, Y), [RH, RB1], [], [RH, RB2], false) :-
     !, skolemise(X), skolemise(Y), \+ demo_beq(Y, X, RB1),
     add_before(X, Y, RB1, RB2).

/*
   The predicates "diff" (meaning not equal) and "is" (for evaluating
   arithmetic expressions) are built in.
*/

abresolve(diff(X, Y), R, [], R, false) :- !, X \= Y.

abresolve(is(X, Y), R, [], R, false) :- !, X is Y.

abresolve(G, R, [], [G|R], false) :- abducible(G).

abresolve(G, R, Gs, R, false) :- axiom(G, Gs).




/*
   add_neg(N, Ns1, Ns2) adds goal N to the list of (lists of) negations Ns1,
   giving Ns2. Duplicates are ignored, but N must_mw be fully bound.
*/

add_neg(N, Ns, Ns) :- member(N, Ns), !.
add_neg(N, Ns, [N|Ns]).


/* append_negs is just append, but ignoring duplicates. */

append_negs([], [], []).
append_negs([N|Ns1], Ns2, Ns4) :- add_neg(N, Ns2, Ns3), append(Ns1, Ns3, Ns4).




/*
   abdemo_nafs([G1...Gn], R1, R2) demos not(G1) and ... and not(Gn).

   Calls to abdemo_naf have the following form.

        abdemo_nafs(Negations, ResidueIn, ResidueOut,
             NegationsIn, NegationsOut)

   where Negations is the list of negations to be established, ResidueIn
   is the old residue, ResidueOut is the new residue (abdemo_nafs can add
   both before and happens facts, as well as other abducibles, to the
   residue), NegationsIn is the old list of negations (same as Negations
   for top-level call), and NegationsOut is the new list of negations
   (abdemo_nafs can add new clipped goals to NegationsIn).
*/


abdemo_nafs([], R, R, N, N).
abdemo_nafs([N|Ns], R1, R3, N1, N3) :-
     abdemo_naf(N, R1, R2, N1, N2), abdemo_nafs(Ns, R2, R3, N2, N3).




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
*/

abdemo_naf([clipped(T1, F, T3)|Gs1], R1, R2, N1, N2) :-
     !, findall(Gs3,
          (resolve(terms_or_rels(A, F, T2), R1, Gs2),
          resolve(happens(A, T2), R1, []),
          append([before(T1, T2), before(T2, T3)|Gs2], Gs1, Gs3)), Gss),
     abdemo_nafs(Gss, R1, R2, N1, N2).

abdemo_naf([declipped(T1, F, T3)|Gs1], R1, R2, N1, N2) :-
     !, findall(Gs3,
          (resolve(inits_or_rels(A, F, T2), R1, Gs2),
          resolve(happens(A, T2), R1, []),
          append([before(T1, T2), before(T2, T3)|Gs2], Gs1, Gs3)), Gss),
     abdemo_nafs(Gss, R1, R2, N1, N2).

/*
   To show the classical negation of holds_at(F) (which is what we want), we
   have to show that holds_at(neg(F)). Conversely, to show the classical
   negation of holds_at(neg(F)) we have to show holds_at(F). Within a call
   to abdemo_naf, we can add both happens and before (and other abducibles)
   to the residue. This removes a potential source of incompleteness.
   However, we only want to add to the residue as a last resort. Accordingly,
   holds_at goals are postponed if they can't be proved without adding to
   the residue.  A postponed holds_at goal appears in the goal list as
   postponed(holds_at(F, T)).
*/

abdemo_naf([holds_at(F1, T)|Gs], R, R, N1, N2) :-
     opposite(F1, F2), demo([holds_at(F2, T)], R, N1, N2), !.

abdemo_naf([holds_at(F, T)|Gs1], R1, R2, N1, N2) :-
     !, append(Gs1, [postponed(holds_at(F, T))], Gs2),
     abdemo_naf(Gs2, R1, R2, N1, N2).

abdemo_naf([postponed(holds_at(F1, T))|Gs], R1, R3, N1, N3) :-
     opposite(F1, F2), abdemo([holds_at(F2, T)], R1, R2, N1, N2), !,
     abdemo_naf_cont(R1, Gs, R2, R3, N2, N3).

abdemo_naf([postponed(holds_at(F, T))|Gs], R1, R2, N1, N2) :-
     !, abdemo_naf(Gs, R1, R2, N1, N2).

/*
   Special facilities for handling temporal ordering facts are built in.
   We can add a before fact to the residue to preserve the failure of
   a clipped goal. The failure of a before goal does NOT mean that the
   negation of that goal is assumed to be true. (The Clark completion is
   not applicable to temporal ordering facts.) Rather, to make before(X, Y)
   fail, before(Y, X) has to follow. One way to achieve this is to add
   before(Y, X) to the residue.
*/

abdemo_naf([before(X, Y)|Gs], R, R, N, N) :- X == Y, !.

abdemo_naf([before(X, Y)|Gs], [RH, RB], [RH, RB], N, N) :- demo_before(Y, X, RB), !.

abdemo_naf([before(X, Y)|Gs1], R1, R2, N1, N2) :-
     !, append(Gs1, [postponed(before(X, Y))], Gs2),
     abdemo_naf(Gs2, R1, R2, N1, N2).

/*
   A before fact is only added to the residue as a last resort. Accordingly,
   if we encounter a before(X, Y) goal and cannot show before(Y, X), we
   postpone that goal until we've tried other possibilities. A postponed
   before goal appears in the goal list as postponed(before(X, Y)).
*/

abdemo_naf([postponed(before(X, Y))|Gs], [RH, RB1], [RH, RB2], N, N) :-
     \+ demo_beq(X, Y, RB1), add_before(Y, X, RB1, RB2).

abdemo_naf([postponed(before(X, Y))|Gs], R1, R2, N1, N2) :-
     !, abdemo_naf(Gs, R1, R2, N1, N2).

/*
   We drop through to the general case for goals other than special event
   calculus goals.
*/

abdemo_naf([not(G)|Gs], R1, R3, N1, N3) :-
     abdemo([G], R1, R2, N1, N2), !, abdemo_naf_cont(R1, Gs, R2, R3, N2, N3).

abdemo_naf([not(G)|Gs], R1, R2, N1, N2) :- !, abdemo_naf(Gs, R1, R2, N1, N2).

abdemo_naf([G|Gs1], R, R, N, N) :- \+ resolve(G, R, Gs2), !.

abdemo_naf([G1|Gs1], R1, R2, N1, N2) :-
     findall(Gs2, (resolve(G1, R1, Gs3), append(Gs3, Gs1, Gs2)), Gss),
     abdemo_nafs(Gss, R1, R2, N1, N2).


/*
   abdemo_naf_cont gets an extra opportunity to succeed if the residue
   has been altered. This is determined by comparing R1 with R2. If
   a sub-goal has failed and the residue hasn't been altered, there's
   no need to look for other ways to prove the negation of the overall goal.
*/

abdemo_naf_cont(R1, Gs, R2, R2, N, N).
abdemo_naf_cont(R1, Gs, R2, R3, N1, N2) :- R1 \= R2, abdemo_naf(Gs, R1, R3, N1, N2).


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


check_nafs(false, N1, R, R, N2, N2) :- !.

check_nafs(true, N, [[happens(A, T)|RH], RB], R, N1, N2) :-
     check_nafs(A, T, N, [[happens(A, T)|RH], RB], R, N1, N2).

check_nafs(A, T, [], R, R, N, N).

check_nafs(A, T, [N|Ns], R1, R3, N1, N3) :-
     check_naf(A, T, N, R1, R2, N1, N2), check_nafs(A, T, Ns, R2, R3, N2, N3).


check_naf(A, T2, [clipped(T1, F, T3)], R1, R2, N1, N2) :-
     !, findall([before(T1, T2), before(T2, T3)|Gs],
          (resolve(terms_or_rels(A, F, T2), R1, Gs)), Gss),
     abdemo_nafs(Gss, R1, R2, N1, N2).

check_naf(A, T2, [declipped(T1, F, T3)], R1, R2, N1, N2) :-
     !, findall([before(T1, T2), before(T2, T3)|Gs],
          (resolve(inits_or_rels(A, F, T2), R1, Gs)), Gss),
     abdemo_nafs(Gss, R1, R2, N1, N2).

check_naf(A, T2, N, R1, R2, N1, N2) :- abdemo_naf(N, R1, R2, N1, N2).




/*
   demo is just like abdemo, except that it doesn't add to the residue.
   It does, however add to the list of negations.
*/

demo([], R, N, N).

demo([holds_at(F1, T)|Gs1], R, N1, N3) :-
     F1 \= neg(F2), resolve(initially(F1), R, Gs2),
     demo_naf([clipped(0, F1, T)], R),
     append(Gs2, Gs1, Gs3), add_neg([clipped(0, F1, T)], N1, N2),
     demo(Gs3, R, N2, N3).

demo([holds_at(F1, T2)|Gs1], R, N1, N4) :-
     F1 \= neg(F2), resolve(initiates(A, F1, T1), R, Gs2),
     resolve(happens(A, T1), R, Gs3),
     resolve(before(T1, T2), R, []),
     demo(Gs2, R, N1, N2), demo_naf([clipped(T1, F1, T2)], R),
     append(Gs3, Gs1, Gs4), add_neg([clipped(T1, F1, T2)], N2, N3),
     demo(Gs4, R, N3, N4).

demo([holds_at(neg(F), T)|Gs1], R, N1, N3) :-
     resolve(initially(neg(F)), R, Gs2),
     demo_naf([declipped(0, F, T)], R),
     append(Gs2, Gs1, Gs3), add_neg([declipped(0, F, T)], N1, N2),
     demo(Gs3, R, N2, N3).

demo([holds_at(neg(F), T2)|Gs1], R, N1, N4) :-
     resolve(terminates(A, F, T1), R, Gs2),
     resolve(happens(A, T1), R, Gs3),
     resolve(before(T1, T2), R, []),
     demo(Gs2, R, N1, N2), demo_naf([declipped(T1, F, T2)], R),
     append(Gs3, Gs1, Gs4), add_neg([declipped(T1, F, T2)], N2, N3),
     demo(Gs4, R, N3, N4).

demo([before(X, Y)|Gs], R, N1, N2) :- !, demo_before(X, Y, R), demo(Gs, R, N1, N2).

demo([not(G)|Gs], R, N1, N2) :-
     !, demo_naf([G], R), add_neg([G], N1, N2), demo(Gs, R, N2, N3).

demo([G|Gs1], R, N1, N3) :-
     resolve(G, R, Gs2), demo(Gs2, R, N1, N2), demo(Gs1, R, N2, N3).




/*
   demo_before simply checks membership of the temporal ordering part
   of the residue.
*/

demo_before(0, Y, R) :- !.

demo_before(X, Y, R) :- member(before(X, Y), R).

/* demo_beq is demo before or equal. */

demo_beq(X, Y, R) :- X == Y, !.

demo_beq(X, Y, R) :- demo_before(X, Y, R).


/*
   add_before(X, Y, R1, R2) adds before(X, Y) to the residue R1 giving R2.
   It does this by maintaining the transitive closure of the before relation
   in R2, and assumes that R1 is already transitively closed.
   R1 and R2 are just the temporal ordering parts of the residue.
*/

add_before(X, Y, R1, R2) :-
     find_connections(X, Y, R1, C1, C2),
     cross_prod(C1, C2, C3, R1), append(C3, R1, R2).

/*
   find_connections(X, Y, R, C1, C2) creates two lists, C1 and C2,
   containing respectively all the time points before X and after
   Y according to R, which is assumed to be transitively closed.
*/

find_connections(X, Y, [], [X], [Y]).

find_connections(X, Y, [before(Z, X)|R], [Z|C1], C2) :-
     !, find_connections(X, Y, R, C1, C2).

find_connections(X, Y, [before(Y, Z)|R], C1, [Z|C2]) :-
     !, find_connections(X, Y, R, C1, C2).

find_connections(X, Y, [before(Z1, Z2)|R], C1, C2) :-
     find_connections(X, Y, R, C1, C2).

cross_prod([], C, [], R).

cross_prod([X|C1], C2, R3, R) :-
     cross_one(X, C2, R1, R), cross_prod(C1, C2, R2, R), append(R1, R2, R3).

cross_one(X, [], [], R).

cross_one(X, [Y|C], [before(X, Y)|R1], R) :-
     \+ member(before(X, Y), R), !, cross_one(X, C, R1, R).

cross_one(X, [Y|C], R1, R) :- cross_one(X, C, R1, R).




/*
   Note that resolve doesn't check for happens axioms (defining
   compound events). This would precipitate looping. This omission is
   justified by the assumption that a compound event only occurs if its
   sub-events occur, which in turn follows from the completion of
   happens.

   terms_or_rels means terminates or releases. Recall that terminates(F)
   is really shorthand for initiates(neg(F)).
*/

resolve(terms_or_rels(A, F, T), R, Gs) :- axiom(releases(A, F, T), Gs).

resolve(terms_or_rels(A, F, T), R, Gs) :- !, axiom(terminates(A, F, T), Gs).

resolve(inits_or_rels(A, F, T), R, Gs) :- axiom(releases(A, F, T), Gs).

resolve(inits_or_rels(A, F, T), R, Gs) :- !, axiom(initiates(A, F, T), Gs).

resolve(happens(A, T, T), R, Gs) :- resolve(happens(A, T), R, Gs).

resolve(happens(A, T), [RH, RB], []) :- !, member(happens(A, T), RH).

resolve(before(X, Y), [RH, RB], []) :- !, demo_before(X, Y, RB).

resolve(diff(X, Y), R, []) :- !, X \= Y.

resolve(is(X, Y), R, []) :- !, X is Y.

resolve(G, R, Gs) :- axiom(G, Gs).




/*
   demo_nafs([G1...Gn], R) demos not(G1) and ... and not(Gn).

   demo_nafs is just like abdemo_nafs, except that it doesn't add
   to the residue.
*/

demo_nafs([], R).

demo_nafs([N|Ns], R) :-
     demo_naf(N, R), demo_nafs(Ns, R).


/*
   demo_naf([G1...Gn], R1, R2) demos not((G1) and ... and (Gn)).

   Note the use of \+ demo_beq(T2, T1) rather than \+ demo_before(T1, T2)
   in demo_naf(clipped). This ensures that "not clipped" fails if there
   exists a linearisation of the actions in the residue which would clip
   the fluent in question.

   In effect, demo_naf(clipped) proves the classical negation of clipped,
   given the completions of clipped, initiates, terminates, releases and
   happens. Likewise, demo_naf(holds_at) proves the classical negation
   of holds_at. Assuming that F is a ground term, to show the classical
   negation of holds_at(F), we simply have to show holds_at(neg(F)), and
   to show the classical negation of holds_at(neg(F)), we show holds_at(F).
   This is justified by the implicit adoption of the axiom,

        not holds_at(F, T) <-> holds_at(neg(F), T)

   where "not" is interpreted classically.

   If F is not a ground term, we can still show demo_naf([holds_at(F)|Gs])
   by showing demo_naf(Gs) for all values of F for which demo(holds_at(F))
   succeeds. (We need to be able to do this to show not(clipped(F)) in the
   presence of terminates clauses with holds_at conditions which supply
   context rather than being preconditions. These will have unbound fluent
   arguments when they are called.) However, this doesn't work in all cases
   and is a potential source of incompleteness.
*/

demo_naf([clipped(T1, F, T3)|Gs1], [RH, RB]) :-
     !, findall(Gs3,
          (resolve(terms_or_rels(A, F, T2), [RH, RB], Gs2),
          resolve(happens(A, T2), [RH, RB], []),
          \+ demo_beq(T2, T1, RB), \+ demo_beq(T3, T2, RB),
          append(Gs2, Gs1, Gs3)), Gss),
     demo_nafs(Gss, [RH, RB]).

demo_naf([declipped(T1, F, T3)|Gs1], [RH, RB]) :-
     !, findall(Gs3,
          (resolve(inits_or_rels(A, F, T2), [RH, RB], Gs2),
          resolve(happens(A, T2), [RH, RB], []),
          \+ demo_beq(T2, T1, RB), \+ demo_beq(T3, T2, RB),
          append(Gs2, Gs1, Gs3)), Gss),
     demo_nafs(Gss, [RH, RB]).

demo_naf([holds_at(F1, T)|Gs], R) :-
     ground(F1), opposite(F1, F2), demo([holds_at(F2, T)], R, [], N), !.

demo_naf([holds_at(F, T)|Gs], R) :- ground(F), !, demo_naf(Gs, R).

demo_naf([holds_at(F, T)|Gs], R) :-
     !, forall(demo([holds_at(F, T)], R, [], N), (ground(F), demo_naf(Gs, R))).

demo_naf([before(X, Y)|Gs], R) :- X == Y, !.

demo_naf([before(X, Y)|Gs], [RH, RB]) :- demo_before(Y, X, RB), !.

demo_naf([before(X, Y)|Gs], R) :- !, demo_naf(Gs, R).

demo_naf([not(G)|Gs], R) :- demo([G], R, [], N), !.

demo_naf([not(G)|Gs], R) :- !, demo_naf(Gs, R).

demo_naf([G|Gs1], R) :- \+ resolve(G, R, Gs2), !.

demo_naf([G1|Gs1], R) :-
     findall(Gs2, (resolve(G1, R1, Gs3), append(Gs3, Gs1, Gs2)), Gss),
     demo_nafs(Gss, R).




/* Skolemisation */

skolemise(T) :- var(T), gensym(t, T), !.

skolemise(T).




/* Odds and sods */

opposite(neg(F), F) :- !.
opposite(F, neg(F)).


abdemo:or(true, B, true) :- !.
abdemo:or(false, false, false).
abdemo:or(false, true, true).


