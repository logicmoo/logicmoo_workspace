/*  Part of Run-Time Checker for Assertions

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
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

:- module(rtchecks,
          [(rtcheck)/1,
           unrtcheck/1,
           rtcheck_wrap/1,
           with_rtchecks/1,
           op(1150, fx, rtcheck)
          ]).

:- discontiguous '$exported_op'/3.
:- reexport(library(compound_expand)).
:- use_module(library(assertions)).
:- use_module(library(prolog_wrap)).
:- use_module(library(rtcprops), []).
:- use_module(library(ctrtchecks)).
:- use_module(system:library(rtchecks_rt)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(group_pairs_or_sort)).

:- multifile
    prolog:rename_predicate/2.

:- meta_predicate rtcheck_wrap(0 ).

rtcheck_wrap(M:G) :-
    functor(G, F, A),
    functor(P, F, A),
    collect_asrs(F/A, M, [P-AsrL], []),
    qualify_meta_goal(G, M, CM, P),
    maplist(wrap_asr_rtcheck, AsrL, RAsrL),
    rtcheck_wrap(M:G, CM, RAsrL).

rtcheck_wrap(M:G, CM, RAsrL) :-
    wrap_predicate(M:G, rtchecks, W, rtcheck_goal(W, M, CM, RAsrL)).

wrappers(Var) -->
    { var(Var),
      !,
      instantiation_error(Var)
    }.
wrappers((A,B)) -->
    wrappers(A),
    wrappers(B).
wrappers(Name//Arity) -->
    { atom(Name), integer(Arity), Arity >= 0,
      Arity1 is Arity+2
    },
    wrappers(Name/Arity1).
wrappers(Name/Arity) -->
    { atom(Name), integer(Arity), Arity >= 0,
      functor(Head, Name, Arity),
      prolog_load_context(module, Module)
    },
    ['$rtchecked'(Head)],
    [(:- initialization(rtcheck_wrap(Module:Head)))].

collect_asrs(Var, _) -->
    { var(Var),
      !,
      instantiation_error(Var)
    }.
collect_asrs((A,B), M) -->
    !,
    collect_asrs(A, M),
    collect_asrs(B, M).
collect_asrs(Name//Arity, M) -->
    { atom(Name), integer(Arity), Arity >= 0,
      !,
      Arity1 is Arity+2
    },
    collect_asrs(Name/Arity1, M).
collect_asrs(Name/Arity, M) -->
    { atom(Name), integer(Arity), Arity >= 0,
      functor(H, Name, Arity)
    },
    ( { findall(H-Asr, current_assertion(rt, H, M, Asr), PIAsrLL),
        group_pairs_or_sort(PIAsrLL, [H-AsrL])
      }
    ->[H-AsrL]
    ; []
    ).

:- meta_predicate
    rtcheck(:),
    unrtcheck(:).

rtcheck(M:PIList) :-
    collect_asrs(PIList, M, PIL, []),
    rtcheck2(M-PIL).

unrtcheck(M:PIList) :-
    collect_asrs(PIList, M, PIL, []),
    unrtcheck2(M-PIL).

generate_rtchecks(Preds, Clauses) :-
    phrase(( ( { '$current_source_module'(CM),
                 '$defined_predicate'(CM:'$rtchecked'(_))
               }
             ->[]
             ; [(:- discontiguous('$rtchecked'/1)),
                (:- public '$rtchecked'/1)]
             ),
             wrappers(Preds)
           ), Clauses).

term_expansion((:- rtcheck(Preds)), Clauses) :-
    generate_rtchecks(Preds, Clauses).

term_expansion(assertions:asr_head_prop(Asr, M, Pred, Status, Type, D, F),
               [assertions:asr_head_prop(Asr, M, Pred, Status, Type, D, F)|Clauses]) :-
    current_prolog_flag(rtchecks_static, StaticL),
    memberchk(Status, StaticL),
    Type \= (prop),
    \+ prop_asr(Pred, M, _, (prop), _, _, _),
    is_valid_status_type(Status, Type),
    \+ ( '$current_source_module'(CM),
         '$defined_predicate'(CM:'$rtchecked'(_)),
         CM:'$rtchecked'(Pred)
       ),
    functor(Pred, F, A),
    generate_rtchecks(F/A, Clauses).

:- multifile
    sandbox:safe_directive/1.

%!  sandbox:safe_directive(+Directive) is semidet.
%
%   Allow rtchecks directives that affect locally defined predicates.

sandbox:safe_directive(Dir) :-
    ground(Dir),
    local_rtchecks_dir(Dir).

local_rtchecks_dir(rtcheck(Preds)) :-
    local_preds(Preds).

local_preds((A,B)) :-
    !,
    local_preds(A),
    local_preds(B).

local_preds(Name/Arity) :-
    atom(Name), integer(Arity).
local_preds(Name//Arity) :-
    atom(Name), integer(Arity).

:- meta_predicate with_rtchecks(0 ).
with_rtchecks(Goal) :-
    collect_rtcheckable_preds(GLLL),
    setup_call_cleanup(
        maplist(rtcheck2, GLLL),
        Goal,
        maplist(unrtcheck2, GLLL)).

rtcheck2(M-GLL) :-
    discontiguous(M:'$rtchecked'/1),
    dynamic(M:'$rtchecked'/1),
    public(M:'$rtchecked'/1),
    maplist(rtcheck2(M), GLL).

rtcheck2(M, (CM:G)-AsrL) :-
    maplist(wrap_asr_rtcheck, AsrL, RAsrL),
    dyn_rtcheck_record(G, M),
    rtcheck_wrap(M:G, CM, RAsrL).

dyn_rtcheck_record(Head, M) :-
    (   M:'$rtchecked'(Head)
    ->  true
    ;   assertz(M:'$rtchecked'(Head))
    ).

unrtcheck2(M-GLL) :-
    dynamic(M:'$rtchecked'/1),
    maplist(unrtcheck2(M), GLL).

unrtcheck2(M, H-_) :-
    functor(H, F, A),
    (   M:'$rtchecked'(H)
    ->  retractall(M:'$rtchecked'(H)),
        unwrap_predicate(M:F/A, rtchecks)
    ;   true
    ).

collect_rtcheckable_preds(Groups) :-
    findall(M-((CM:G)-Asr),
            ( current_assertion(rt, H, M, Asr),
              functor(H, F, A),
              functor(G, F, A),
              qualify_meta_goal(G, M, CM, H)
            ), PIAsrLL),
    group_pairs_or_sort(PIAsrLL, Groups).
