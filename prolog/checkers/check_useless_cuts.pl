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

:- use_module(library(location_utils)).
:- use_module(library(intercept)).
:- use_module(library(option_utils)).
:- use_module(library(checkers/checker)).
:- use_module(library(check), []).

:- multifile
    prolog:message//1.

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
    findall(warning-Issue, current_det_check(M, FromChk, Issue), Pairs).

% 1. A cut is useless, if is located at the last clause, and the literals above
% are semidet

current_det_check(M, FromChk, useless_cut(Loc, M:F/A-I)) :-
    % PI=M:F/A,
    current_predicate(M:F/A),
    functor(H, F, A),
    MH = M:H,
    \+ predicate_property(MH, imported_from(_)),
    \+ predicate_property(MH, built_in),
    \+ predicate_property(MH, dynamic),
    \+ predicate_property(MH, foreign),
    \+ predicate_property(MH, multifile),
    \+ \+ ( catch(clause(MH, _, Ref), _, fail),
            call(FromChk, clause(Ref))
          ),
    do_current_det_check(H, M, I, Loc).

:- dynamic
    useless_cut_db/1.

do_current_det_check(H, M, I, Loc) :-
    findall(clause(M:H, B, R), clause(M:H, B, R), ClauseL),
    intercept(walk_call(ClauseL, H, M, report_useless_cut),
              useless_cut(Ref),
              assertz(useless_cut_db(Ref))),
    retract(useless_cut_db(C)),
    nth_clause(_, I, C),
    from_location(clause(C), Loc).

walk_call(ClauseL, H, M, CA) :-
    prolog_current_choice(CP1),
    member(clause(M:H, Body, Ref), ClauseL),
    prolog_current_choice(CP2),
    ( CP1 == CP2
    ->true
    ; ( true
      ; fail
      )
    ),
    walk_body(Body, M, Ref, CA, CP2).

report_useless_cut(Ref) :- send_signal(useless_cut(Ref)).

noop(_).

valid_prop_asr(Lit, M, Asr) :-
    prop_asr(Lit, M, Status, Type, _, _, Asr),
    valid_status(Status),
    valid_type(Type).

valid_status(true).
valid_status(check).

valid_type(pred).
valid_type(prop).

walk_body(!, _, Ref, CA, CP1) :-
    !,
    prolog_current_choice(CP),
    ( CP1==CP
    ->call(CA, Ref)
    ; prolog_cut_to(CP1)
    ).
walk_body(M:Lit, _, Ref, CA, CP) :-
    !,
    walk_body(Lit, M, Ref, CA, CP).
walk_body((A, B), M, Ref, CA, CP) :-
    !,
    walk_body(A, M, Ref, CA, CP),
    walk_body(B, M, Ref, CA, CP).
walk_body((A; B), M, Ref, CA, CP1) :-
    !,
    ( prolog_current_choice(CP2),
      walk_body(A, M, Ref, CA, CP2)
    ; walk_body(B, M, Ref, CA, CP1)
    ).
walk_body(Lit, M, _, _, _) :-
    ( valid_prop_asr(Lit, M, _)
    ->( forall(valid_prop_asr(Lit, M, Asr),
               ( member(DetProp,
                        [globprops:det(_),
                         globprops:semidet(_),
                         globprops:is_det(_),
                         globprops:no_choicepoints(_)
                        ]),
                 prop_asr(glob, DetProp, _, Asr)
               ))
      ->true
      ; forall(valid_prop_asr(Lit, M, Asr),
               ( member(NegProp,
                        [globprops:fails(_),
                         globprops:failure(_)
                        ]),
                 prop_asr(glob, NegProp, _, Asr)
               ))
      ->fail
      )
    ; predicate_property(M:Lit, built_in)
    ->( true
      ; fail
      )
    ; catch(findall(clause(B, R), clause(M:Lit, B, R), ClauseL), _, ClauseL = [_, _]),
      ( ClauseL = []
      ->fail
      ; ClauseL = [_]
      ->walk_call(ClauseL, Lit, M, noop)
      ; % abstraction step: making this decidable by not analyzing if there is a
        % choice point introduced by the clause
        ( true % leave a choicepoint
        ; fail
        )
      )
    ).
