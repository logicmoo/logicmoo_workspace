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
:- use_module(library(countsols)).
:- use_module(library(gcu)).
:- use_module(library(location_utils)).
:- use_module(library(option_utils)).
:- use_module(library(checkers/checker)).
:- use_module(library(check), []).

:- multifile
    prolog:message//1.

:- dynamic
    cut_info/2,
    inferred_det_db/3,
    det_checking/1,
    det_clause/2.

prolog:message(acheck(useless_cuts)) -->
    ['Check useless cuts',nl,
     '------------------',nl,
     'The predicates contain cuts that are actually', nl,
     'not needed.', nl, nl].
prolog:message(acheck(useless_cuts, useless_cut(Loc, CI))) -->
    Loc, ['Useless cut in ~q'-[CI], nl].

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
    cuts_check(M, FromChk, Pairs).

cuts_check(M, FromChk, Pairs) :-
    forall(current_det_check(M, FromChk),
           true),
    retractall(cut_info(_, needed)),
    findall(warning-Issue,
            collect_useless_cut(Issue, FromChk), Pairs).

collect_useless_cut(useless_cut(Loc, M:F/A-I), FromChk) :-
    retract(cut_info(Ref, unused)),
    call(FromChk, clause(Ref)), % Avoid warnings from out there
    nth_clause(M:H, I, Ref),
    functor(H, F, A),
    from_location(clause(Ref), Loc).

% 1. A cut is useless, if is located at the last clause, and the literals above
% are semidet

current_det_check(M, FromChk) :-
    % PI=M:F/A,
    current_predicate(M:F/A),
    functor(H, F, A),
    MH = M:H,
    \+ inferred_det_db(H, M, _),
    \+ predicate_property(MH, imported_from(_)),
    \+ \+ ( catch(clause(MH, _, Ref), _, fail),
            call(FromChk, clause(Ref))
          ),
    one_det_check(H, M).

has_clauses(MH) :-
    \+ predicate_property(MH, built_in),
    \+ predicate_property(MH, foreign),
    \+ predicate_property(MH, dynamic),
    \+ predicate_property(MH, multifile).

one_det_check(H, M) :-
    ( ( predef_det(H, M, Det)
      ->true
      ; forall(walk_call(H, M, info), true),
        infer_det(H, M, Det)
      )
    ->assertz(inferred_det_db(H, M, Det))
    ; print_message(error, format("unexpected failure of infer_def/3 or predef_det/3 ~q", [M:H]))
    ).

inferred_det(Lit, M, Det) :-
    functor(Lit, F, A),
    functor(H, F, A),
    predicate_property(M:H, implementation_module(IM)),
    ( inferred_det_db(H, IM, Det1)
    ->Det = Det1
    ; one_det_check(H, IM),
      inferred_det_db(H, IM, Det)
    ).

predef_det(H, M, nodet) :-
    valid_prop_asr(H, M, Asr),
    member(Prop,
           [globprops:multi(_),
            globprops:non_det(_),
            globprops:nondet(_)
           ]),
    prop_asr(glob, Prop, _, Asr),
    !.
predef_det(H, M, isdet) :-
    once(valid_prop_asr(H, M, _)),
    forall(valid_prop_asr(H, M, Asr),
           ( member(Prop,
                    [globprops:det(_),
                     globprops:semidet(_),
                     globprops:is_det(_),
                     globprops:no_choicepoints(_)
                    ]),
             prop_asr(glob, Prop, _, Asr)
           )),
    !.
predef_det(H, M, fails) :-
    once(valid_prop_asr(H, M, _)),
    forall(valid_prop_asr(H, M, Asr),
           ( member(Prop,
                    [globprops:fails(_),
                     globprops:failure(_)
                    ]),
             prop_asr(glob, Prop, _, Asr)
           )).
predef_det(H, M, nodet) :-
    \+ has_clauses(M:H),
    !.
predef_det(H, M, fails) :-
    predicate_property(M:H, number_of_clauses(0 )),
    !.

infer_det(H, M, Det) :-
    member(Det, [multi, isdet, fails]),
    clause(M:H, _, Ref),
    det_clause(Ref, Det),
    !.

walk_call(H, M, CA) :-
    ( \+ ( clause(M:H, _, Ref),
           det_checking(Ref)
         )
    ->del_det_clause(CA, H, M),
      prolog_current_choice(CP1),
      clause(M:H, Body, Ref),
      prolog_current_choice(CP2),
      add_neg_clause(CA, Ref),
      clause_property(Ref, module(CM)),
      setup_call_cleanup(
          assertz(det_checking(Ref)),
          do_walk_body(Body, CM, Ref, CA, CP1, CP2),
          retractall(det_checking(Ref))),
      prolog_cut_to(CP2)
    ; ( true
      ; fail
      )
    ).

do_walk_body(Body, CM, Ref, CA, CP1, CP2) :-
    prolog_current_choice(CP3),
    ( CP1 == CP2
    ->true
    ; ( true
      ; fail
      )
    ),
    walk_body(Body, CM, Ref, CA, CP3),
    add_det_clause(CA, Ref, CP3).

% TBD: distinguish between different cuts in a given clause
add_cut_info(noop, _, _).
add_cut_info(info, Ref, Info) :-
    add_cut_info(Info, Ref).

add_cut_info(unused, Ref) :-
    ( cut_info(Ref, _)
    ->true
    ; assertz(cut_info(Ref, unused))
    ).
add_cut_info(needed, Ref) :-
    retractall(cut_info(Ref, _)),
    assertz(cut_info(Ref, needed)).

del_det_clause(info, H, M) :-
    forall(clause(M:H, _, Ref),
           retractall(det_clause(Ref, _))).
del_det_clause(noop, _, _).

add_det_clause(info, Ref, CP2) :-
    prolog_current_choice(CP3),
    ( CP2 == CP3
    ->DetInfo = isdet
    ; DetInfo = multi
    ),
    retractall(det_clause(Ref, _)),
    assertz(det_clause(Ref, DetInfo)).
add_det_clause(noop, _, _).

add_neg_clause(info, Ref) :-
    assertz(det_clause(Ref, fails)).
add_neg_clause(noop, _).

valid_prop_asr(Lit, M, Asr) :-
    prop_asr(Lit, M, Status, Type, _, _, Asr),
    valid_status(Status),
    valid_type(Type).

valid_status(true).
valid_status(check).

valid_type(pred).
valid_type(prop).

walk_body_if_branch(C, M, Ref, CA, SFlag, CP1, CP2) :-
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
    walk_body(C, M, Ref, CA, CP3),
    prolog_current_choice(CP5),
    ( CP3 == CP5
    ->true
    ; CP4 == CP5
    ->true
    ; catch(prolog_cut_to(CP4), _, true), % catch because CP4 could be gone
      SFlag = s(FlagL),
      nb_setarg(1, SFlag, [insert_cp|FlagL])
    ),
    fail.

walk_body(V, M, _, _, _) :-
    ( var(V)
    ; var(M)
    ),
    !,
    ( true
    ; fail
    ).
walk_body(!, _, Ref, CA, CP1) :-
    !,
    prolog_current_choice(CP),
    ( CP1 \= CP
    ->Info = needed,
      prolog_cut_to(CP1)
    ; Info = unused
    ),
    add_cut_info(CA, Ref, Info).
walk_body(M:Lit, _, Ref, CA, CP) :-
    !,
    walk_body(Lit, M, Ref, CA, CP).
walk_body((A, B), M, Ref, CA, CP) :-
    !,
    walk_body(A, M, Ref, CA, CP),
    walk_body(B, M, Ref, CA, CP).
walk_body((A; B), M, Ref, CA, CP1) :-
    !,
    ( member(A, [(_->_), (_*->_)])
    ->prolog_current_choice(CP2),
      SFlag = s([]),
      findall(Lit,
              ( member(Branch, [A, B]),
                walk_body_if_branch(Branch, M, Ref, CA, SFlag, CP1, CP2)
              ), [Term|TermL]),
      foldl(greatest_common_unifier, TermL, Term, Lit),
      ( member(leave_cp1, FlagL)
      ->true
      ; prolog_cut_to(CP1)
      ),
      ( member(insert_cp, FlagL)
      ->( true
        ; fail
        )
      ; true
      )
    ; ( prolog_current_choice(CP2),
        walk_body(A, M, Ref, CA, CP2)
      ; walk_body(B, M, Ref, CA, CP1)
      )
    ).
walk_body((A->B), M, Ref, CA, CP1) :-
    !,
    ( prolog_current_choice(CP2),
      walk_body(A, M, Ref, CA, CP2)
    ->walk_body(B, M, Ref, CA, CP1)
    ).
walk_body((A*->B), M, Ref, CA, CP1) :-
    !,
    ( prolog_current_choice(CP2),
      walk_body(A, M, Ref, CA, CP2),
      walk_body(B, M, Ref, CA, CP1)
    ).
walk_body(call(A), M, Ref, CA, _) :-
    !,
    prolog_current_choice(CP),
    walk_body(A, M, Ref, CA, CP).
walk_body(\+ (A), M, Ref, CA, _) :-
    !,
    \+ ( prolog_current_choice(CP),
         walk_body(A, M, Ref, CA, CP),
         fail
       ).
walk_body(true, _, _, _, _) :- !.
walk_body(fail, _, _, _, _) :- !, fail.
walk_body(false, _, _, _, _) :- !, fail.
walk_body(Lit, M, Ref, _, _) :-
    ( \+ predicate_property(M:Lit, defined)
    ->print_message(
          error,
          at_location(clause(Ref),
          format("Undefined predicate found: ~q", [Lit]))),
      fail
    ; true
    ),
    ( inferred_det(Lit, M, nodet) % don`t check inferred multi, but the nodet flag
    ->( true
      ; fail
      )
    ; inferred_det(Lit, M, isdet)
    ->true
    ; inferred_det(Lit, M, fails)
    ->fail
    ; predicate_property(M:Lit, built_in)
    ->( true
      ; fail
      )
    ; catch(findall(clause(B, R), countsols(2, clause(M:Lit, B, R)), ClauseL),
            _,
            ClauseL = [_, _]),
      ( ClauseL = []
      ->fail
      ; ClauseL = [_]
      ->walk_call(Lit, M, noop)
      ; % abstraction step: making this decidable by not analyzing if there is a
        % choice point introduced by the clause
        ( true % leave a choicepoint
        ; fail
        )
      )
    ).
