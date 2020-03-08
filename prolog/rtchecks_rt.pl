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

:- module(rtchecks_rt,
	  [rtcheck_goal/2,
           rtcheck_goal/3,
           rtcheck_goal/4,
           start_rtcheck/1,
	   start_rtcheck/2,
           rtcheck_lit/3,
	   rtc_call/3]).

:- use_module(library(apply)).
:- use_module(library(assertions)).
:- use_module(library(metaprops)).
:- use_module(library(rtchecks_flags)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(resolve_calln)).
:- use_module(library(send_check)).
:- use_module(library(clambda)).
:- use_module(library(ctrtchecks)).
:- reexport(library(ctrtchecks),
            ['$with_asr'/2,
             '$with_gloc'/2,
             '$with_ploc'/2,
             check_call/3]).

/** <module> Predicates that are required to implement run-time checks

Algorithm:
----------

pred :- body.

is executed as if where transformed to:

* *Step one*

```
pred :-
     "check entry...",
     "check exit...",
     'pred$rtc1'.
```

* *Step two*
```
'pred$rtc1' :-
        "check compat pre..."
        "check calls...",
        "check success pre",
        "check comp..."(
        'pred$rtc2',
        "check success pos",
        "check compat pos..."

'pred$rtc2' :-
        body.
```

However the current implementation is an interpreter, rather than a compiler,
since SWI-Prolog is fully dynamic and the status of a module could change at
run-time. A future improvement could be to apply a partial evaluator to the
interpreter.

*/

check_cond(Cond, Check, PredName) :-
    ( Cond
    ->last_prop_failure(L),
      send_check([[]/Check-L], pp_check, PredName, [])
    ; true
    ).

ppassertion_type_goal(check(Goal), check, Goal).
ppassertion_type_goal(trust(Goal), trust, Goal).
ppassertion_type_goal(true( Goal), true,  Goal).
ppassertion_type_goal(false(Goal), false, Goal).

:- meta_predicate rtcheck_goal(0, 1).
rtcheck_goal(CM:Goal1, Call) :-
    resolve_calln(Goal1, Goal),
    ( ppassertion_type_goal(Goal, Type, Pred)
    ->rtc_call(Type, call(Call, CM:Pred), CM:Pred)
    ; predicate_property(CM:Goal, implementation_module(M)),
      collect_rtasr(Goal, CM, Pred, M, RAsrL),
      check_goal(rt, call(Call, CM:Pred), M, CM, RAsrL)
    ).

:- meta_predicate rtcheck_goal(0, 1, -).
rtcheck_goal(CM:Goal1, Call, RTCheck) :-
    resolve_calln(Goal1, Goal),
    ( ppassertion_type_goal(Goal, Type, Pred)
    ->RTCheck = rtc_call(Type, call(Call, CM:Pred), CM:Pred)
    ; predicate_property(CM:Goal, implementation_module(M)),
      collect_rtasr(Goal, CM, Pred, M, RAsrL),
      RAsrL \= [],
      RTCheck = rtcheck_goal(call(Call, CM:Pred), M, CM, RAsrL)
    ).

:- meta_predicate rtcheck_goal(0, +, +, +).
rtcheck_goal(Goal, M, CM, RAsrL) :-
    check_goal(rt, Goal, M, CM, RAsrL).

:- meta_predicate start_rtcheck(0 ).
start_rtcheck(Goal) :-
    do_start_rtcheck(Goal, Goal).

:- meta_predicate start_rtcheck(+, 0 ).
start_rtcheck(Goal, WrappedHead) :-
    do_start_rtcheck(Goal, WrappedHead).

do_start_rtcheck(M:Goal1, CM:WrappedHead) :-
    resolve_calln(Goal1, Goal),
    collect_rtasr(Goal, CM, _, M, RAsrL),
    check_goal(rt, CM:WrappedHead, M, CM, RAsrL).

prolog:called_by(rtcheck_lit(_, C, _), rtchecks_rt, M, [M:C]) :- nonvar(C).

% Note: it should be 0, but we use : to avoid loop in goal_expansion
:- meta_predicate rtcheck_lit(+, :, +).
rtcheck_lit(body, Goal, From) :-
    '$with_ploc'(start_rtcheck(Goal), From).
rtcheck_lit(head, Goal, From) :-
    '$with_ploc'(Goal, From).

collect_rtasr(Goal, CM, Pred, M, RAsrL) :-
    qualify_meta_goal(Goal, M, CM, Pred),
    collect_assertions(rt, Pred, M, AsrL),
    maplist(wrap_asr_rtcheck, AsrL, RAsrL).

wrap_asr_rtcheck(Asr, rtcheck(Asr)).

assertions:asr_aprop(rtcheck(Asr), Key, Prop, From) :-
    curr_prop_asr(Key, Prop, From, Asr).

% ----------------------------------------------------------------------------

:- meta_predicate rtc_call(+, 0, ?).

rtc_call(Type, Check, Pred) :-
    \+ do_rtcheck(Type, Check, Pred).

do_rtcheck(Status, Check, Pred) :-
    rtcheck_assr_status(Status),
    ( Status = false
    ->Call = Check
    ; Call = (\+ Check)
    ),
    check_cond(Call, Pred, Status/1),
    fail.

sandbox:safe_meta_predicate(rtchecks_rt:start_rtcheck/2).
sandbox:safe_meta_predicate(rtchecks_rt:rtcheck_lit/3).
