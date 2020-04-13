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
	  [rtchecks_disable/0,
           rtchecks_enable/0,
           rtcheck_pred/4,
           rtcheck_call/3]).

:- use_module(library(assertions)).
:- use_module(library(metaprops)).
:- use_module(library(neck)).
:- use_module(library(rtchecks_flags)).
:- use_module(library(send_check)).
:- use_module(library(clambda)).
:- use_module(library(ctrtchecks)).
:- use_module(library(call_inoutex)).
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

:- thread_local rtchecks_disabled/0.

rtchecks_disable :- assertz(rtchecks_disabled).

rtchecks_enable :- retractall(rtchecks_disabled).

:- meta_predicate rtcheck_pred(0, +, +, +).

rtcheck_pred(CM:Goal, M, CM, RAsrL) :-
    ( rtchecks_disabled
    ->CM:Goal
    ; call_inoutex(
          check_goal(rt,
                     call_inoutex(CM:Goal,
                                  rtchecks_enable,
                                  rtchecks_disable),
                     M, CM, RAsrL),
          rtchecks_disable,
          rtchecks_enable)
    ).

assertions:asr_aprop(rtcheck(Asr), Key, Prop, From) :-
    curr_prop_asr(Key, Prop, From, Asr).

% ----------------------------------------------------------------------------

:- meta_predicate do_rtcheck(+, 0, +).

status_cond(false,  Call) :- Call.
status_cond(Status, Call) :-
    rtcheck_assr_status(Status),
    Status \= false,
    neck,
    \+ Call.

do_rtcheck(Status, Call, PLoc) :-
    '$with_ploc'(status_cond(Status, Call), PLoc),
    last_prop_failure(L),
    send_check([[]/Call-L], pp_check, Status/1, PLoc, []),
    fail.

:- meta_predicate rtcheck_call(+,0,+).

rtcheck_call(Status, Call, PLoc) :-
    rtchecks_disable,
    \+ do_rtcheck(Status, Call, PLoc),
    rtchecks_enable.
