:- module(rtchecks_rt, [rtcheck_goal/2,
			rtcheck_goal/4,
			rtc_call/2
		       ]).

:- use_module(library(assertions)).
:- use_module(library(termtyping), []). % assertions about builtins
:- use_module(library(context_values)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(implementation_module)).

:- reexport(library(send_check)).
:- reexport(library(nativeprops)).
:- reexport(library(basicprops)).

:- doc(author, "Edison Mera").

%:- doc(author, "Based on David Trallero's rtchecks package.").

:- doc(module, "This module contains the predicates that are
	required to implement run-time checks.").

:- meta_predicate checkif_modl(?, ?, 0, ?, 0).
checkif_modl(M, M,    _,    _, Goal) :- !, call(Goal).
checkif_modl(_, _, GMod, Goal, Goal) :- call(GMod).

:- public with_assertion/2.
:- meta_predicate with_assertion(0, ?).
with_assertion(Comp, Asr) :-
    with_value(Comp, '$with_assertion', Asr).

:- public with_gloc/2.
:- meta_predicate with_gloc(0, ?).
with_gloc(Comp, GLoc) :-
    with_value(Comp, '$with_gloc', GLoc).

rtcheck_cond(Cond, Check, PredName) :-
    ( Cond
    ->send_rtcheck([_/Check-[]], pp_check, PredName, _)
    ; true
    ).

check_asr_props(Asr, Part, Check, Mult, PropValues) :-
    findall(From/Prop-[],
	    ( asr_aprop(Asr, Part, Prop, From),
	      \+ check_prop(Check, Prop),
	      (Mult = once -> ! ; true)
	    ),
	    PropValues).

check_prop(compat,   Prop) :- compat(  Prop).
check_prop(instance, Prop) :- instance(Prop).

type_part_check_call(compat,    comp, compat,   call).
type_part_check_call(calls,     call, instance, call).
type_part_check_call(success,   call, instance, once).
type_part_check_call(compatpos, comp, compat,   once).
type_part_check_call(comp,      call, instance, once).

check_asrs_props(PType, AsrPVL) :-
    type_part_check_call(PType, Part, Check, Call),
    maplist(check_asr_props(Part, Check, Call), AsrPVL),
    ( maplist(\=(_-[]), AsrPVL)
    ->maplist(send_rtcheck_asr(PType), AsrPVL)
    ; true
    ).

typeif_part_check_call(compat,  comp, compat,   call).
typeif_part_check_call(success, succ, instance, call).

checkif_asrs_props(PType, AsrPVL) :-
    typeif_part_check_call(PType, Part, Check, Call),
    maplist(checkif_asr_props(PType, Part, Check, Call), AsrPVL).

checkif_asr_props(PType, Part, Check, Call, Asr-CondValues) :-
    checkif_asr_props(CondValues, Asr, PType, Part, Check, Call).

check_asr_props(Part, Check, Call, Asr-PropValues) :-
    check_asr_props(Asr, Part, Check, Call, PropValues).

send_rtcheck_asr(PType, Asr-PropValues) :-
    asr_aprop(Asr, head, _:Pred, ALoc), !,
    send_rtcheck(PropValues, PType, Pred, ALoc).

:- meta_predicate checkif_asr_props(+,+,+,+,+,1).
checkif_asr_props([_|_], _, _, _, _, _).
checkif_asr_props([], Asr, PType, Part, Check, Call) :-
    check_asr_props(Asr, Part, Check, Call, PropValues),
    send_rtcheck_asr(PType, Asr-PropValues).

:- meta_predicate checkif_asrs_comp(+, 0).
checkif_asrs_comp([], Goal) :- call(Goal).
checkif_asrs_comp([Asr-PVL|AsrL], Goal1) :-
    notrace(checkif_asr_comp(PVL, Asr, Goal1, Goal)),
    checkif_asrs_comp(AsrL, Goal).

valid_command(times(_, _)).
valid_command(try_sols(_, _)). % Legacy

checkif_asr_comp([_|_], _, Goal,  Goal).
checkif_asr_comp([],  Asr, Goal1, with_assertion(Goal, Asr)) :-
    findall(g(Asr, M, Glob, Loc),
	    ( asr_aprop(Asr, glob, M:Glob, Loc),
	      \+ valid_command(Glob)
	    ), GlobL),
    comps_to_goal(GlobL, comp_pos_to_goal(Asr), Goal, Goal1).

comp_pos_to_goal(Asr, g(Asr, M, Glob, Loc), with_gloc(M:Glob, Loc), Goal) :-
    arg(1, Glob, Goal).

:- meta_predicate rtcheck_goal(0, +).
rtcheck_goal(CM:Goal, AsrL) :-
    implementation_module(CM:Goal, M),
    rtcheck_goal(Goal, M, CM, AsrL).

rtcheck_goal(Goal, M, CM, AsrL) :-
    checkif_modl(M, CM,
		 check_asrs(step1, AsrL, G2), G2,
		 check_asrs(step2, AsrL, CM:Goal)).

% ----------------------------------------------------------------------------
% assrt_op(Part, Step, Level, Type).

assrt_op(comp, step1, exports, pred).
assrt_op(comp, step2, inner,   pred).
assrt_op(call, step1, _,       entry).
assrt_op(call, step1, exports, calls).
assrt_op(call, step1, exports, pred).
assrt_op(call, step2, inner,   calls).
assrt_op(call, step2, inner,   pred).
assrt_op(succ, step1, _,       exit).
assrt_op(succ, step1, exports, success).
assrt_op(succ, step1, exports, pred).
assrt_op(succ, step2, inner,   test).
assrt_op(succ, step2, inner,   success).
assrt_op(succ, step2, inner,   pred).
assrt_op(glob, step1, exports, comp).
assrt_op(glob, step1, exports, pred).
assrt_op(glob, step2, inner,   test).
assrt_op(glob, step2, inner,   comp).
assrt_op(glob, step2, inner,   pred).

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
rtcheck_assr_type(test) :- current_prolog_flag(rtchecks_test, yes).
rtcheck_assr_type(comp).
rtcheck_assr_type(exit) :- current_prolog_flag(rtchecks_exit, yes).
rtcheck_assr_type(success).
% ----------------------------------------------------------------------------

check_asrs(Step, AsrL, Goal) :-
    notrace(check_asrs_pre(Step, AsrL,
			   AsrGlobL, AsrCompL, AsrSuccL)),
    checkif_asrs_comp(AsrGlobL, Goal),
    notrace(check_asrs_pos(AsrCompL, AsrSuccL)).

check_asrs_pos(AsrCompL, AsrSuccL) :-
    checkif_asrs_props(compat,  AsrCompL),
    checkif_asrs_props(success, AsrSuccL).

check_asrs_pre(Step, AsrL, AsrGlobL, AsrCompL, AsrSuccL) :-
    current_prolog_flag(rtchecks_level, Level),
    prop_rtchecks(AsrL, comp, Step, Level, AsrCompL),
    prop_rtchecks(AsrL, call, Step, Level, AsrCallL),
    prop_rtchecks(AsrL, succ, Step, Level, AsrSuccL),
    subtract(AsrSuccL,  AsrCallL, DAsrSuccL),
    prop_rtchecks(AsrL, glob, Step, Level, AsrGlobL),
    subtract(AsrGlobL,  AsrCallL, DAsrGlobL),
    check_asrs_props(compat,  AsrCompL ),
    check_asrs_props(calls,   AsrCallL ),
    check_asrs_props(success, DAsrSuccL),
    check_asrs_props(comp,    DAsrGlobL).

prop_rtchecks(AsrL0, Part, Step, Level, AsrPVL) :-
    include(is_prop_rtcheck(Part, Step, Level), AsrL0, AsrL),
    pairs_keys_values(AsrPVL, AsrL, _PValuesL).

is_prop_rtcheck(Part, Step, Level, Asr) :-
    asr_aprop(Asr, stat, Status, _),
    asr_aprop(Asr, type,   Type, _),
    assrt_op(Part, Step, Level, Type),
    is_valid_status_type(Status, Type), !.

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
