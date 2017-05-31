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

:- module(ctrtchecks, 
	  [check_call/3,
           check_goal/6,
	   is_prop_check/5,
           collect_assertions/4,
	   current_assertion/4]).

:- use_module(library(apply)).
:- use_module(library(assrt_lib)).
:- use_module(library(send_check)).
:- use_module(library(clambda)).
:- use_module(library(rtcprops)).
:- use_module(library(implementation_module)).
:- use_module(library(metaprops)).

:- meta_predicate checkif_modl(?, ?, 0, ?, 0).
checkif_modl(M, M,    _,    _, Goal) :- !, call(Goal).
checkif_modl(_, _, GMod, Goal, Goal) :- call(GMod).

is_prop_check(Step, Level, T, Part, Asr) :-
    part_time(Part, T),
    asr_aprop(Asr, type, Type,   _),
    asr_aprop(Asr, stat, Status, _),
    assrt_op(Part, Step, Level, Type),
    is_valid_status_type(Status, Type), !.

part_time(call, _).
part_time(succ, _).
part_time(glob, rt).

%!  assrt_op(+Part, +Step, +Level, +Type).

assrt_op(call, step1, _,       entry).
assrt_op(call, step1, exports, calls).
assrt_op(call, step1, exports, pred).
assrt_op(call, step1, exports, prop).
assrt_op(call, step2, inner,   calls).
assrt_op(call, step2, inner,   pred).
assrt_op(call, step2, inner,   prop).
assrt_op(succ, step1, _,       exit).
assrt_op(succ, step1, exports, success).
assrt_op(succ, step1, exports, pred).
assrt_op(succ, step1, exports, prop).
assrt_op(succ, step2, inner,   success).
assrt_op(succ, step2, inner,   pred).
assrt_op(succ, step2, inner,   prop).
assrt_op(glob, step1, exports, comp).
assrt_op(glob, step1, exports, pred).
assrt_op(glob, step1, exports, prop).
assrt_op(glob, step2, inner,   comp).
assrt_op(glob, step2, inner,   pred).
assrt_op(glob, step2, inner,   prop).

is_valid_status_type(true, entry) :- current_prolog_flag(rtchecks_entry, yes).
is_valid_status_type(Status, Type) :-
        rtcheck_assr_type(Type),
        rtcheck_assr_status(Status).

rtcheck_assr_status(true)  :- current_prolog_flag(rtchecks_true,  yes).
rtcheck_assr_status(trust) :- current_prolog_flag(rtchecks_trust, yes).
rtcheck_assr_status(debug) :- current_prolog_flag(rtchecks_debug, yes).
rtcheck_assr_status(trace) :- current_prolog_flag(rtchecks_trace, yes).
rtcheck_assr_status(check) :- current_prolog_flag(rtchecks_check, yes).

rtcheck_assr_type(calls).
rtcheck_assr_type(pred).
rtcheck_assr_type(prop).
rtcheck_assr_type(comp).
rtcheck_assr_type(exit) :- current_prolog_flag(rtchecks_exit, yes).
rtcheck_assr_type(success).

/*
  Combination of status and rtcheck indicators, to control the compile
  and run-time checking:

  ===========+============++===========+==========
  Status     | + Command  || ctchecked | rtchecked
  ===========+============++===========+==========
  true/trust | -          || no        | no
  true/trust | acheck     || no        | yes
  check      | no_acheck  || yes       | no
  check      | -          || yes       | yes
  ===========+============++===========+==========
  Note: This is weird to preserve ciao-compatibility
*/

black_list_pred(_=_).

assertion_is_valid(T, Status, Type, Asr) :-
    ( \+ prop_asr(glob, acheck(_), _, Asr),
      \+ prop_asr(glob, acheck(T, _), _, Asr)
    ->is_valid_status_type(Status, Type),
      \+ prop_asr(glob, no_acheck(_), _, Asr),
      \+ prop_asr(glob, no_acheck(T, _), _, Asr)
    ; true % Force run-time checking
    ).

current_assertion(T, Pred, M, Asr) :-
    \+ ( T = rt,
         prop_asr(Pred, M, _, prop, _, _, _)
       ),
    prop_asr(Pred, M, Status, Type, _, _, Asr),
    assertion_is_valid(T, Status, Type, Asr),
    ( current_prolog_flag(rtchecks_level, inner)
    ->true
    ; current_prolog_flag(rtchecks_level, exports),
      predicate_property(M:Pred, export)
    ->true
    ),
    \+ black_list_pred(Pred).

collect_assertions(T, Pred, M, AsrL) :-
    copy_term_nat(Pred, Head), % copy to avoid duplication of atributes
    findall(Head-Asr, current_assertion(T, Head, M, Asr), Pairs),
    maplist([Pred] +\ (Pred-T)^T^true, Pairs, AsrL).

check_goal(T, Goal, Call, M, CM, AsrL) :-
    current_prolog_flag(rtchecks_level, Level),
    checkif_modl(M, CM,
                 check_asrs(is_prop_check(step1, Level, T), AsrL, Goal, G2), G2,
                 check_asrs(is_prop_check(step2, Level, T), AsrL, Goal, Call)).

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

:- meta_predicate check_call(+, +, 0).
check_call(T, AsrL, CM:Goal) :-
    implementation_module(CM:Goal, M),
    ctrt_call(T, CM:Goal, Call),
    check_goal(T, Goal, Call, M, CM, AsrL).

ctrt_call(rt, Call, Call).
ctrt_call(ct, _, true). % TBD: use a partial evaluator instead of true

check_asrs_props_calls(AsrPVL) :-
    check_asrs_props_all(calls, AsrPVL).

check_asrs_props_all(PType, AsrPVL) :-
    check_asrs_props(PType, AsrPVL),
    ( \+ memberchk(_-[], AsrPVL)
    ->maplist(send_check_asr(PType), AsrPVL)
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

send_check_asr(PType, Asr-PropValues) :-
    ( PropValues = [[]] % Skip property
    ->true
    ; once(asr_aprop(Asr, head, _:Pred, ALoc)),
      send_check(PropValues, PType, Pred, ALoc)
    ).

checkif_asr_props(CondValues, Asr, PType) :-
    ( member(CondValues, [[], [[]]])
    ->check_asr_props(Asr, cond, PType, PropValues),
      send_check_asr(PType, Asr-PropValues)
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
