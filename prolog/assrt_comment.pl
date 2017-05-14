/*  Part of Assertion Reader for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/assertions
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

:- module(assrt_comment, []).

:- use_module(library(assertions)).
:- use_module(library(assertions_op)).
:- use_module(library(predicate_from)).
:- use_module(user:library(plprops)).
:- use_module(pldoc(doc_modes)).

:- create_prolog_flag(assrt_comment, yes, [type(atom)]).

% Don't reinvent the wheel, use pldoc and the mode/2 information.

with_acp(Goal, OldFlag, NewFlag) :-
    setup_call_cleanup(
        set_prolog_flag(assrt_comment, NewFlag),
        Goal,
        set_prolog_flag(assrt_comment, OldFlag)).

ac_head_prop_idx(Head, M, Mode, Det, From) :-
    current_prolog_flag(assrt_comment, Flag),
    Flag \= none,
    % copy_term_nat(Head, Term),
    with_acp(do_ac_head_prop_idx(Head, M, Mode, Det, From), Flag, none).

do_ac_head_prop_idx(Head, M, Mode, Det, From) :-
    var(Mode), !,
    ( var(Head)
    ->module_property(M, class(user)),
      current_predicate(M:F/A),
      functor(Head, F, A)
    ; functor(Head, F, A),
      module_property(M, class(user)),
      current_predicate(M:F/A)
    ),
    functor(Mode, F, A),
    mode(M:Mode, Det),
    predicate_from(M:Head, From).
do_ac_head_prop_idx(_, _, _, _, _).

assrt_lib:asr_head_prop(ac_asr(M, H, Mode, D, F), M, H, check, pred, [], F) :-
    ac_head_prop_idx(H, M, Mode, D, F).
assrt_lib:asr_comp(ac_asr(M, H, Mode, D, F), PM, P, F) :- asrc_prop(comp, M, H, Mode, D, F, PM, P).
assrt_lib:asr_call(ac_asr(M, H, Mode, D, F), PM, P, F) :- asrc_prop(call, M, H, Mode, D, F, PM, P).
assrt_lib:asr_succ(ac_asr(M, H, Mode, D, F), PM, P, F) :- asrc_prop(succ, M, H, Mode, D, F, PM, P).
assrt_lib:asr_glob(ac_asr(M, H, Mode, D, F), PM, P, F) :- asrc_prop(glob, M, H, Mode, D, F, PM, P).

asrc_prop(Type, M, H, Mode, D, F, PM, P) :-
    ac_head_prop_idx(H, M, Mode, D, F),
    assrt_lib:current_normalized_assertion(pred Mode is D, M, _, M:H, _, _, CpL, CaL, SuL, GlL, _, _, _),
    ( member(Type-PrL, [comp-CpL, call-CaL, succ-SuL]),
      member(MPr-_, PrL),
      strip_module(MPr, PM, P)
    ; Type = glob,
      member(MGl-_, GlL),
      strip_module(MGl, PM, Gl),
      assrt_lib:add_arg(_, Gl, P, _, _)
    ).

/* TBD: create a unit test for this:

:- module(rr, [r/1]).

%! r(-Value) is det.

r(a).


7 ?- r(a).
ERROR: /home/edison/apps/plsteroids/pp.pl:8: Assertion failure for r(a).
ERROR:     In *calls*, unsatisfied properties: 
ERROR:         /home/edison/apps/plsteroids/pp.pl:8: instance(pp:var(a)).

*/
