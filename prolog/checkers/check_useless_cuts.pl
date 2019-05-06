/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2019, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(check_useless_cuts, []).

:- use_module(library(assrt_lib)).
:- use_module(library(neck)).
:- use_module(library(abstract_interpreter), []).
:- use_module(library(gcu)).
:- use_module(library(location_utils)).
:- use_module(library(option_utils)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(safe_prolog_cut_to)).
:- use_module(library(checkers/checker)).
:- use_module(library(check), []).

:- multifile
    prolog:message//1,
    walk_body_hook/9,
    has_body_hook/2.

:- dynamic
    cut_info/3,
    inferred_det_db/3,
    det_checking/1,
    det_checking/2,
    det_clause_db/2.

prolog:message(acheck(useless_cuts)) -->
    ['Check useless cuts',nl,
     '------------------',nl,
     'The predicates contain cuts that are actually not needed.', nl,
     'That could happen when the analysis determined that all', nl,
     'the calls before the cut and the clause itself where', nl,
     'deterministic or they have one of the next properties:', nl,
     'det, semidet, is_det or fails.  Note that in recursive', nl,
     'predicates the cut would look as needed, but it is not', nl,
     'since it can be removed or avoided via refactoring.', nl, nl].
prolog:message(acheck(useless_cuts, Issue)) -->
    issue_type_message(Issue).

issue_type_message(useless_cut(Loc, CI)-CutPosL) -->
    Loc,
    {length(CutPosL, N)},
    ['~q has ~w useless cut(s) (~q)'-[CI, N, CutPosL], nl].

checker:check(useless_cuts, Result, Options) :-
    check_useless_cuts(Options, Result).

check_useless_cuts(Options1, Pairs) :-
    foldl(select_option_default,
          [module(M)-M],
          Options1, Options2),
    merge_options(Options2,
                  [module(M)
                  ], Options),
    option_fromchk(Options, _, FromChk),
    cuts_check(M, FromChk, Pairs),
    cleanup_useless_cuts.

cleanup_useless_cuts :-
    retractall(det_checking(_)),
    retractall(det_checking(_, _)),
    retractall(det_clause_db(_, _)),
    retractall(inferred_det_db(_, _, _)).

cuts_check(M, FromChk, Pairs) :-
    forall(current_det_check(M, FromChk),
           true),
    retractall(cut_info(_, _, needed)),
    findall(warning-Issue,
            collect_issues(Issue, FromChk), Pairs).

collect_issues(useless_cut(Loc, M:F/A-I)-CutPos, FromChk) :-
    retract(cut_info(Ref, RCutPos, unused)),
    call(FromChk, clause(Ref)), % Avoid warnings from out there
    nth_clause(M:H, I, Ref),
    functor(H, F, A),
    reverse(RCutPos, CutPos), % reverse is more human-readable
    from_location(clause(Ref), Loc).

% 1. A cut is useless, if is located at the last clause, and the literals above
% are semidet

current_det_check(M, FromChk) :-
    order_by([asc(M:F/A)],
             ( current_predicate(M:F/A),
               functor(H, F, A),
               MH = M:H,
               % Start analyzing exported predicates, ancillary predicates will
               % be analyzed by det_check/3 if the argument instantiation can
               % not be determined, or by walk_lit/5 for specific
               % instantiations.
               predicate_property(MH, exported),
               \+ predicate_property(MH, imported_from(_)),
               \+ \+ ( catch(clause(MH, _, Ref), _, fail),
                       call(FromChk, clause(Ref))
                     )
             )),
    check_det(H, M, _).

clauses_accessible(MH) :-
    \+ predicate_property(MH, built_in),
    \+ predicate_property(MH, foreign),
    % Although for dynamic and multifile clauses are readable, its provided
    % information could change and therefore are useless for this analysis
    \+ predicate_property(MH, dynamic),
    \+ predicate_property(MH, multifile).

check_det(H, M, Det) :-
    ( inferred_det_db(H, M, Det)
    ->true
    ; ( predef_det(H, M, Det)
      ->true
      ; % format(user_error, "? ~q~n", [M:H]),
        with_det_checking_pr(H, M, forall(walk_call(H, M, info), true)),
        infer_det(H, M, Det)
        % format(user_error, "! ~q: ~w~n", [M:H, Det])
      )
    ->assertz(inferred_det_db(H, M, Det))
    ; print_message(error, format("unexpected failure of infer_det/3 or predef_det/3 ~q", [M:H]))
    ).

inferred_det(C, M, Det) :-
    functor(C, F, A),
    functor(H, F, A),
    predicate_property(M:H, implementation_module(I)),
    check_det(H, I, Det1),
    Det1 = Det.

%!  predef_det(-DetInfo, +Head, +Module) is semidet.
%
%   Get Determinism information for a given predicate, that comes from
%   user-defined assertions (which has priority) or predicate properties. If the
%   predicate is being analyzed, assumes nondet, which is the value used when
%   there is no determinism information (yet).

predef_det(H, M, Det) :-
    once(det_predef(Det, H, M)).

valid_prop_asr(Lit, M, Asr) :-
    prop_asr(Lit, M, Status, Type, _, _, Asr),
    valid_status(Status),
    valid_type(Type).

valid_status(true).
valid_status(check).

valid_type(pred).
valid_type(prop).

valid_glob_asr(GL, A) :-
    member(G, GL),
    prop_asr(glob, globprops:G, _, A).

:- public
    det_props/2.

det_props(fails, [fails(_), failure(_)]).
det_props(isdet, [det(_), semidet(_), is_det(_), no_choicepoints(_)]).
det_props(nodet, [multi(_), non_det(_), nondet(_)]).

det_predef_asr(Det, H, M) :-
    det_props(Det, GL),
    neck,
    collect_valid_glob_asr(Det, GL, H, M).

collect_valid_glob_asr(Det, GL, H, M) :-
    member(Det, [fails, isdet]),
    neck,
    \+ \+ valid_prop_asr(H, M, _),
    forall(valid_prop_asr(H, M, A),
           valid_glob_asr(GL, A)).
collect_valid_glob_asr(Det, GL, H, M) :-
    member(Det, [nodet]),
    neck,
    valid_prop_asr(H, M, A),
    valid_glob_asr(GL, A).

det_predef(Det,   H, M) :- inferred_det_db(H, M, Det).
det_predef(Det,   H, M) :- det_predef_asr(Det, H, M).
det_predef(nodet, H, M) :- det_checking(H, M).
det_predef(nodet, H, M) :- \+ clauses_accessible(M:H).
det_predef(fails, H, M) :- predicate_property(M:H, number_of_clauses(0 )).

infer_det(H, M, Det) :-
    member(Det1, [multi, isdet, fails]),
    \+ \+ ( clause(M:H, _, Ref),
            det_clause(Ref, Det1)
          ),
    !,
    Det = Det1.

add_cp.
add_cp :- fail.

:- meta_predicate
    with_det_checking_pr(+, +, 0 ),
    with_det_checking_cl(+, +, 0 ).


%!  with_det_checking_pr(+Head, +Module, :Call).
%!  with_det_checking_cl(+Head, +Module, :Call).

%   with_det_checking ensures decidability by not analyzing if there is a
%   recursion that requires to analyze the same call again (abstraction step)

with_det_checking_pr(H, M, Call) :-
    ( det_checking(H, M)
    ->true
    ; functor(H, F, A),
      functor(P, F, A),
      setup_call_cleanup(
          assertz(det_checking(P, M), DCRef),
          ( Call,
            erase(DCRef)
          ),
          erase_nf(DCRef))
    ).

with_det_checking_cl(info, _,      Call) :- call(Call).
with_det_checking_cl(noop, Clause, Call) :-
    ( det_checking(Clause)
    ->add_cp
    ; setup_call_cleanup(
          assert_det_checking(Clause, Ref),
          ( call(Call),
            erase(Ref)
          ),
          erase_nf(Ref))
    ).

erase_nf(Ref) :- ignore(erase(Ref)).

assert_det_checking(Clause, Ref) :-
    assertz(det_checking(Clause), Ref).

walk_call(H, M, CA) :-
    prolog_current_choice(CP1),
    clause(M:H, Body, Ref),
    prolog_current_choice(CP2),
    ( Body = true
    ->true
    ; add_neg_clause(CA, Ref),
      clause_property(Ref, module(CM)),
      with_det_checking_cl(
          CA, Ref,
          do_walk_body(Body, CM, Ref, CA, CP1, CP2))
    ),
    remove_new_cp(CA, CP2).

cut_to(CP) :- catch(safe_prolog_cut_to(CP), _, true).

remove_new_cp(noop, _).
remove_new_cp(info, CP) :- cut_to(CP).

do_walk_body(Body, CM, Ref, CA, CP1, CP2) :-
    % We can not cut CP1, since we are analyzing clause by clause, so if there
    % is a choice point at clause level, we insert a choicepoint with add_cp to
    % detect any cut in the clause
    prolog_current_choice(CP3),
    ( CP1 == CP2
    ->true
    ; add_cp
    ),
    prolog_current_choice(CP4),
    walk_body(Body, CM, [], Ref, CA, CP3, CP4, _),
    prolog_current_choice(CP5),
    add_det_clause(CA, Ref, CP3, CP5).

add_cut_info(unused, LitPos, Ref) :-
    ( cut_info(Ref, LitPos, _)
    ->true
    ; assertz(cut_info(Ref, LitPos, unused))
    ).
add_cut_info(needed, LitPos, Ref) :-
    retractall(cut_info(Ref, LitPos, _)),
    assertz(cut_info(Ref, LitPos, needed)).

det_clause(Ref, Det) :-
    nth_clause(Pred, Idx, Ref),
    clause(_, Body, Ref),
    Body = true,
    !,
    ( predicate_property(Pred, number_of_clauses(Idx))
    ->Det = isdet
    ; Det = multi
    ).
det_clause(Ref, Det) :-
    det_clause_db(Ref, Det1),
    !,
    Det = Det1.
det_clause(_, multi).

add_det_clause(info, Ref, CP2, CP3) :-
    ( CP2 == CP3
    ->DetInfo = isdet
    ; DetInfo = multi
    ),
    retractall(det_clause_db(Ref, _)),
    assertz(det_clause_db(Ref, DetInfo)).
add_det_clause(noop, _, _, _).

add_neg_clause(info, Ref) :-
    assertz(det_clause_db(Ref, fails)).
add_neg_clause(noop, _).

add_pos([], Pos, [Pos]).
add_pos([Pos1|LitPos1], Pos, LitPos) :-
    ( Pos1 = Pos
    ->LitPos = [Pos-2|LitPos1]
    ; Pos1 = Pos-N1
    ->succ(N1, N),
      LitPos = [Pos-N|LitPos1]
    ; LitPos = [Pos, Pos1|LitPos1]
    ).

:- discontiguous walk_body/8.

walk_body(V, M, _, _, _, _, CP, CP) :-
    ( var(V)
    ; var(M)
    ),
    !,
    add_cp.
walk_body(!, _, LitPos, Ref, _, CP1, _, CP1) :-
    !,
    prolog_current_choice(CP),
    ( CP1 \= CP
    ->Info = needed,
      cut_to(CP1)
    ; Info = unused
    ),
    add_cut_info(Info, LitPos, Ref).
walk_body(M:A, _, LitPos, Ref, CA, CP1, CP2, CP) :-
    !,
    add_pos(LitPos, 2, LitPosA),
    walk_body(A, M, LitPosA, Ref, CA, CP1, CP2, CP).
walk_body((A, B), M, LitPos, Ref, CA, CP1, CP2, CP) :-
    !,
    add_pos(LitPos, 1, LitPosA),
    add_pos(LitPos, 2, LitPosB),
    walk_body(A, M, LitPosA, Ref, CA, CP1, CP2, CP3),
    walk_body(B, M, LitPosB, Ref, CA, CP1, CP3, CP).
walk_body((A; B), M, LitPos, Ref, CA, CP1, CP2, CP) :-
    !,
    ( member(A, [(_->_), (_*->_)])
    ->prolog_current_choice(CP3),
      SFlag = s([]),
      findall((A; B),
              ( member(Branch-Pos, [A-1, B-2]),
                walk_body_if_branch(Branch, M, [Pos|LitPos], Ref, CA, SFlag, CP1, CP3)
              ), [Term|TermL]),
      foldl(greatest_common_unifier, TermL, Term, (A; B)),
      SFlag = s(FlagL),
      ( member(leave_cp1, FlagL)
      ->CP = CP2
      ; cut_to(CP1),
        CP = CP1
      ),
      ( member(insert_cp, FlagL)
      ->add_cp
      ; true
      )
    ; ( prolog_current_choice(CP3),
        % add_cp will mark a cut as needed, since it can cut the internal
        % choicepoint of :/2
        add_cp,
        prolog_current_choice(CP4),
        add_pos(LitPos, 1, LitPosA),
        walk_body(A, M, LitPosA, Ref, CA, CP3, CP4, CP)
      ; add_pos(LitPos, 2, LitPosB),
        walk_body(B, M, LitPosB, Ref, CA, CP1, CP2, CP)
      )
    ).

walk_body_if_branch(C, M, LitPos, Ref, CA, SFlag, CP1, CP2) :-
    ( CP1 == CP2
    ->prolog_current_choice(CP3)
    ; prolog_current_choice(CP3),
      ( true
      ; SFlag = s(FlagL),
        nb_setarg(1, SFlag, [leave_cp1|FlagL]),
        fail
      )
    ),
    prolog_current_choice(CP4),
    walk_body(C, M, LitPos, Ref, CA, CP3, CP4, _),
    prolog_current_choice(CP5),
    ( CP3 == CP5
    ->true
    ; CP4 == CP5
    ->true
    ; cut_to(CP4),
      SFlag = s(FlagL),
      nb_setarg(1, SFlag, [insert_cp|FlagL])
    ).

walk_body((A->B), M, LitPos, Ref, CA, CP1, CP2, CP) :-
    !,
    ( prolog_current_choice(CP3),
      add_pos(LitPos, 1, LitPosA),
      walk_body(A, M, LitPosA, Ref, CA, CP3, CP2, CP4)
    ->add_pos(LitPos, 2, LitPosB),
      walk_body(B, M, LitPosB, Ref, CA, CP1, CP4, CP)
    ).
walk_body((A*->B), M, LitPos, Ref, CA, CP1, CP2, CP) :-
    !,
    ( prolog_current_choice(CP3),
      add_pos(LitPos, 1, LitPosA),
      walk_body(A, M, LitPosA, Ref, CA, CP3, CP2, CP4),
      add_pos(LitPos, 2, LitPosB),
      walk_body(B, M, LitPosB, Ref, CA, CP1, CP4, CP)
    ).
walk_body(call(A), M, LitPos, Ref, CA, _, CP2, CP) :-
    !,
    prolog_current_choice(CP3),
    add_pos(LitPos, 1, LitPosA),
    walk_body(A, M, LitPosA, Ref, CA, CP3, CP2, CP).
walk_body(\+ (A), M, LitPos, Ref, CA, _, CP2, CP2) :-
    !,
    \+ ( prolog_current_choice(CP),
         add_pos(LitPos, 1, LitPosA),
         walk_body(A, M, LitPosA, Ref, CA, CP, CP, _),
         fail
       ).
walk_body(true, _, _, _, _, _, CP, CP) :- !.
walk_body(fail, _, _, _, _, _, CP, _) :-
    !,
    catch(safe_prolog_cut_to(CP), _, true),
    fail.
walk_body(false, _, _, _, _, _, CP, _) :-
    !,
    catch(safe_prolog_cut_to(CP), _, true),
    fail.
walk_body(A=B, _, _, _, _, _, CP, CP) :-
    !,
    ( A = B
    ->true
    ; cut_to(CP),
      fail
    ).
walk_body(A\=B, _, _, _, _, _, CP, CP) :-
    !,
    ( A \= B
    ->true
    ; A \== B
    ->true
    ; A == B
    ->cut_to(CP),
      fail
    ).
walk_body(C, M, LitPos, Ref, CA, CP1, CP2, CP3) :-
    predicate_property(M:C, implementation_module(I)),
    has_body_hook(C, I),
    !,
    walk_body_hook(C, I, M, LitPos, Ref, CA, CP1, CP2, CP3).
walk_body(A, M, _, Ref, _, _, CP, CP) :-
    abstract_interpreter:evaluable_body_hook(A, M, Condition),
    call(Condition),
    !,
    ( \+ \+ catched_call(M:A, Ref)
    ->catched_call(M:A, Ref)
    ; cut_to(CP),
      fail
    ).

catched_call(Call, Ref) :-
    catch(Call,
          Error,
          ( print_message(
                error,
                at_location(
                    clause(Ref),
                    Error)),
            fail
          )).

walk_body(atom_concat(_, B, _), _, _, _, _, _, CP, CP) :-
    atomic(B),
    !.
walk_body(atom_concat(A, B, C), _, _, _, _, _, CP, CP) :-
    atomic(C),
    !,
    atom_concat(A, B, C).
walk_body(nb_getval(A, B), _, _, _, _, _, CP, CP) :-
    ignore((nonvar(A), nb_current(A, B))).
walk_body(@(M:H, C), _, _, Ref, _, _, CP, CP) :-
    walk_lit(H, M, C, Ref, CP).
walk_body(H, M, _, Ref, _, _, CP, CP) :-
    walk_lit(H, M, M, Ref, CP).

walk_lit(H, M, CM, Ref, CP) :-
    ( predicate_property(M:H, meta_predicate(Meta))
    ->qualify_meta_goal(CM:H, Meta, C)
    ; C = H
    ),
    ( \+ predicate_property(M:C, defined)
    ->print_message(
          error,
          at_location(clause(Ref),
          format("Undefined predicate found: ~q", [M:C]))),
      fail
    ; true
    ),
    ( inferred_det(C, M, nodet)
      % We have to check for nodet, instead of multi, since all predicates that
      % don't have det properties inferred yet are marked as multi.
    ->add_cp
    ; % We can check multy only if all arguments of C are independent
      % variables, meaning that we can reuse the result of the determinism
      % analysis:
      functor(C, F, A),
      functor(P, F, A),
      C =@= P,
      inferred_det(C, M, multi)
    ->add_cp
    ; inferred_det(C, M, isdet)
    ->true
    ; inferred_det(C, M, fails)
    ->cut_to(CP),
      fail
    ; catch(findall(R, clause(M:C, _, R), ClauseL),
            _,
            AddCP = true),
      ( ClauseL = []
      ->cut_to(CP),
        fail
      ; AddCP == true
      ->add_cp
      ; walk_call(C, M, noop)
      )
    ).
