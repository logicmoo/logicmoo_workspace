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
	  [check_goal/6,
	   check_call/3,
           rtcheck_goal/2,
	   check_asrs/4,
	   start_rtcheck/2,
	   rtc_call/2,
	   '$with_gloc'/2,
	   '$with_asr_head'/2]).

:- use_module(library(apply)).
:- use_module(library(assertions)).
:- use_module(library(metaprops)).
:- use_module(library(rtchecks_flags)).
:- use_module(library(context_values)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(implementation_module)).
:- use_module(library(resolve_calln)).
:- use_module(library(send_check)).
:- use_module(library(clambda)).
:- use_module(library(check_ctrt)).

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

:- meta_predicate '$with_asr_head'(0, ?).
'$with_asr_head'(Comp, AsrHead) :-
    with_value(Comp, '$with_asr_head', AsrHead).

:- meta_predicate '$with_gloc'(0, ?).
'$with_gloc'(Comp, GLoc) :-
    with_value(Comp, '$with_gloc', GLoc).

rtcheck_cond(Cond, Check, PredName) :-
    ( Cond
    ->send_rtcheck([[]/Check-[]], pp_check, PredName, [])
    ; true
    ).

check_asr_props(Asr, Cond, PType, PropValues) :-
    copy_term_nat(Asr, NAsr),
    findall(NAsr-PropValue,
            ( ( type_cond_part_check_mult(Cond, PType, Part, Check, Mult),
                asr_aprop(NAsr, Part, Prop, From)
              *->
                \+ check_prop(Check, Prop),
                (Mult = once -> ! ; true),
                CheckProp =.. [Check, Prop],
                PropValue = (From/CheckProp-[])
              ; PropValue = [] % Skip property
              )
            ),
            AsrPropValues),
    maplist([Asr] +\ (Asr-PV)^PV^true, AsrPropValues, PropValues).

check_prop(compat,   Prop) :- compat(  Prop).
check_prop(instance, Prop) :- instance(Prop).

type_cond_part_check_mult(inco, Cond, Part, Check, Mult) :-
    type_inco_part_check_mult(Cond, Part, Check, Mult).
type_cond_part_check_mult(cond, Cond, Part, Check, Mult) :-
    type_cond_part_check_mult(Cond, Part, Check, Mult).

type_inco_part_check_mult(calls,   call, instance, call).
type_inco_part_check_mult(calls,   comp, compat,   call).
type_inco_part_check_mult(success, call, instance, once).
type_inco_part_check_mult(success, comp, compat,   once).
type_inco_part_check_mult(comp,    call, instance, once).

type_cond_part_check_mult(success, comp, compat,   call).
type_cond_part_check_mult(success, succ, instance, call).

check_asrs_props_calls(AsrPVL) :-
    check_asrs_props_all(calls, AsrPVL).

check_asrs_props_all(PType, AsrPVL) :-
    check_asrs_props(PType, AsrPVL),
    ( \+ memberchk(_-[], AsrPVL)
    ->maplist(send_rtcheck_asr(PType), AsrPVL)
    ; true
    ).

check_asrs_props(PType, AsrPVL) :-
    maplist(check_asr_props(PType), AsrPVL).

checkif_asrs_props(PType, AsrPVL) :-
    maplist(checkif_asr_props(PType), AsrPVL).

checkif_asr_props(PType, Asr-CondValues) :-
    checkif_asr_props(CondValues, Asr, PType).

check_asr_props(PType, Asr-PropValues) :-
    check_asr_props(Asr, inco, PType, PropValues).

send_rtcheck_asr(PType, Asr-PropValues) :-
    ( PropValues = [[]] % Skip property
    ->true
    ; once(asr_aprop(Asr, head, _:Pred, ALoc)),
      send_rtcheck(PropValues, PType, Pred, ALoc)
    ).

checkif_asr_props(CondValues, Asr, PType) :-
    ( member(CondValues, [[], [[]]])
    ->check_asr_props(Asr, cond, PType, PropValues),
      send_rtcheck_asr(PType, Asr-PropValues)
    ; true
    ).

:- meta_predicate checkif_asrs_comp(+, 0).
checkif_asrs_comp([], _, Goal) :-
    call(Goal).
checkif_asrs_comp([Asr-PVL|AsrL], Head, Goal1) :-
    checkif_asr_comp(PVL, Asr, Head, Goal1, Goal),
    checkif_asrs_comp(AsrL, Head, Goal).

checkif_asr_comp(PropValues, Asr, Head, Goal1, Goal) :-
    ( member(PropValues, [[], [[]]]),
      copy_term_nat(Asr, NAsr),
      findall(g(NAsr, M, Glob, Loc),
              asr_aprop(NAsr, glob, M:Glob, Loc), GlobL),
      GlobL \= []
    ->comps_to_goal(GlobL, comp_pos_to_goal(Asr), Goal2, Goal1),
      Goal = '$with_asr_head'(Goal2, Asr-Head)
    ; Goal = Goal1
    ).

comp_pos_to_goal(Asr, g(Asr, M, Glob, Loc), '$with_gloc'(M:Glob, Loc), Goal) :-
    functor(Glob, _, N),
    arg(N, Glob, Goal).

%!  comps_to_goal(+Check:list, :Goal)//
%
%   This predicate allows to compound a list of global properties in to
%   successive meta-calls, but in the third argument you can use your own
%   selector:
%   ```
%   ?- comps_to_goal([not_fails(p(A)), is_det(p(A)), exception(p(A), exc)],G,p(A)).
%   G = not_fails(is_det(exception(p(A),exc)))
%   ```

:- meta_predicate comps_to_goal(?, 3, ?, ?).
comps_to_goal([],             _) --> [].
comps_to_goal([Check|Checks], Goal) -->
    comps_to_goal2(Checks, Check, Goal).

:- meta_predicate comps_to_goal2(?, ?, 3, ?, ?).
comps_to_goal2([], Check, Goal) -->
    call(Goal, Check).
comps_to_goal2([Check|Checks], Check0, Goal) -->
    call(Goal, Check0),
    comps_to_goal2(Checks, Check, Goal).

check_goal(T, Goal, Call, M, CM, AsrL) :-
    current_prolog_flag(rtchecks_level, Level),
    checkif_modl(M, CM,
                 check_asrs(is_prop_check(step1, Level, T), AsrL, Goal, G2), G2,
                 check_asrs(is_prop_check(step2, Level, T), AsrL, Goal, Call)).

:- meta_predicate check_call(+, +, 0).
check_call(T, AsrL, CM:Goal) :-
    implementation_module(CM:Goal, M),
    ctrt_call(T, CM:Goal, Call),
    check_goal(T, Goal, Call, M, CM, AsrL).

ctrt_call(rt, Call, Call).
ctrt_call(ct, _, true). % TBD: use a partial evaluator instead of true

ppassertion_type_goal(check(Goal), check, Goal).
ppassertion_type_goal(trust(Goal), trust, Goal).
ppassertion_type_goal(true( Goal), true,  Goal).
ppassertion_type_goal(false(Goal), false, Goal).

:- meta_predicate rtcheck_goal(0, 1).
rtcheck_goal(CM:Goal0, Call) :-
    resolve_calln(Goal0, Goal),
    ( ppassertion_type_goal(Goal, Type, Pred)
    ->rtc_call(Type, CM:Pred)
    ; implementation_module(CM:Goal, M),
      collect_rtasr(Goal, CM, Pred, M, RAsrL),
      check_goal(rt, Pred, call(Call, CM:Pred), M, CM, RAsrL)
    ).

:- meta_predicate start_rtcheck(+, 0).
start_rtcheck(M:Goal0, CM:WrappedHead) :-
    resolve_calln(Goal0, Goal),
    collect_rtasr(Goal, CM, Pred, M, RAsrL),
    check_goal(rt, Pred, M:WrappedHead, M, CM, RAsrL).

collect_rtasr(Goal, CM, Pred, M, RAsrL) :-
    qualify_meta_goal(Goal, M, CM, Pred),
    collect_assertions(rt, Pred, M, AsrL),
    maplist(wrap_asr_rtcheck, AsrL, RAsrL).

wrap_asr_rtcheck(Asr, rtcheck(Asr)).

% ----------------------------------------------------------------------------

:- meta_predicate check_asrs(2, +, +, +).

check_asrs(IsPropCheck, AsrL, Head, Goal) :-
    check_asrs_pre(IsPropCheck, AsrL, AsrGlobL, AsrSuccL),
    checkif_asrs_comp(AsrGlobL, Head, Goal),
    checkif_asrs_props(success, AsrSuccL).

check_asrs_pre(IsPropCheck, AsrL, AsrGlobL, AsrSuccL) :-
    prop_rtchecks(AsrL, IsPropCheck, call, AsrCallL),
    prop_rtchecks(AsrL, IsPropCheck, succ, AsrSuccL),
    subtract(AsrSuccL, AsrCallL, DAsrSuccL),
    prop_rtchecks(AsrL, IsPropCheck, glob, AsrGlobL),
    subtract(AsrGlobL, AsrCallL, DAsrGlobL),
    check_asrs_props_calls(   AsrCallL),
    check_asrs_props(success, DAsrSuccL),
    check_asrs_props(comp,    DAsrGlobL).

prop_rtchecks(AsrL1, IsPropCheck, Part, AsrPVL) :-
    include(call(IsPropCheck, Part), AsrL1, AsrL),
    pairs_keys_values(AsrPVL, AsrL, _PValuesL).

:- meta_predicate rtc_call(+, 0).

rtc_call(Type, Check) :-
    ignore(do_rtcheck(Type, Check)).

rtcheck_ifnot(Check, PredName) :-
    rtcheck_cond(\+ Check, Check, PredName).

do_rtcheck(check, Check) :-
    rtcheck_ifnot(Check, check/1).
do_rtcheck(trust, Check) :-
    current_prolog_flag(rtchecks_trust, yes),
    rtcheck_ifnot(Check, trust/1).
do_rtcheck(true, Check) :-
    current_prolog_flag(rtchecks_true, yes),
    rtcheck_ifnot(Check, true/1).
do_rtcheck(false, Check) :-
    current_prolog_flag(rtchecks_false, yes),
    rtcheck_cond(Check, Check, false/1),
    fail.

sandbox:safe_meta_predicate(rtchecks_rt:start_rtcheck/2).
