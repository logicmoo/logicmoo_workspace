:- module(rtchecks_rt, [rtcheck_goal/4,
			rtc_call/2
		       ]).

:- use_module(library(assertions)).
:- use_module(library(termtyping), []). % assertions about builtins
:- use_module(library(plprops)).
:- use_module(library(context_values)).
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

rtcheck_cond(Cond, Check, PredName) :-
    ( Cond
    ->send_rtcheck([Check-[]], pp_check, PredName, _)
    ; true
    ).

check_asr_props(Asr, Part, Check, Mult, PropValues) :-
    findall(PropN-NVL,
	    ( prop_asr(Part, Prop, _, Asr),
	      \+ check_prop(Check, Prop),
	      no_bindings_asr(Asr, AsrN),
	      prop_asr(Part, PropN, _, AsrN),
	      term_variables(PropN, NL),
	      copy_term(PropN-NL, Prop-VL),
	      maplist(varnamep, VL, NL, NVL),
	      (Mult = once -> ! ; true)
	    ),
	    PropValues).

varnamep(Var, Name, Name=Var).

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
    asr_head_prop(Asr, _, Pred, _, _, _, ALoc),
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

valid_commands([_:times(_, _), _:try_sols(_, _)]). % Legacy

checkif_asr_comp([_|_], _, Goal,  Goal).
checkif_asr_comp([],  Asr, Goal1, with_assertion(Goal, Asr)) :-
    collect_prop(asr_glob(Asr), user, GlobL1),
    valid_commands(CommL),
    subtract(GlobL1, CommL, GlobL),
    comps_to_goal(GlobL, Goal, Goal1).

rtcheck_goal(Goal, M, CM, AsrL) :-
    checkif_modl(M, CM,
		 check_asrs(step1, AsrL, G2), G2,
		 check_asrs(step2, AsrL, CM:Goal)).

check_asrs(Step, AsrL, Goal) :-
    step_rtchecks_options(Step, CompST, CallST, SuccST, GlobST),
    check_asrs(AsrL, CompST, CallST, SuccST, GlobST, Goal).

% ----------------------------------------------------------------------------
step_rtchecks_options(step1, comp_assrt1, call_assrt1, succ_assrt1, glob_assrt1).
step_rtchecks_options(step2, comp_assrt2, call_assrt2, succ_assrt2, glob_assrt2).

neg_level(inner,   exports).
neg_level(exports, inner).

comp_assrt1(exports, pred).

comp_assrt2(inner, pred).

call_assrt1(_,       entry).
call_assrt1(exports, calls).
call_assrt1(exports, pred).

call_assrt2(inner, calls).
call_assrt2(inner, pred).

succ_assrt1(_,     exit).
succ_assrt1(exports, success).
succ_assrt1(exports, pred).

succ_assrt2(inner, test).
succ_assrt2(inner, success).
succ_assrt2(inner, pred).

glob_assrt1(exports, comp).
glob_assrt1(exports, pred).

glob_assrt2(inner, test).
glob_assrt2(inner, comp).
glob_assrt2(inner, pred).

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

check_asrs(AsrL, CompST, CallST, SuccST, GlobST, Goal) :-
    notrace(check_asrs_pre(AsrL, CompST, CallST, SuccST, GlobST,
			   AsrGlobL, AsrCompL, AsrSuccL)),
    checkif_asrs_comp(AsrGlobL, Goal),
    notrace(check_asrs_pos(AsrCompL, AsrSuccL)).

check_asrs_pos(AsrCompL, AsrSuccL) :-
    checkif_asrs_props(compat,  AsrCompL),
    checkif_asrs_props(success, AsrSuccL).

check_asrs_pre(AsrL, CompST, CallST, SuccST, GlobST,
	       AsrGlobL, AsrCompL, AsrSuccL) :-
    current_prolog_flag(rtchecks_level, Level),
    prop_rtchecks(AsrL, CompST,   Level, AsrCompL ),
    prop_rtchecks(AsrL, CallST,   Level, AsrCallL ),
    prop_rtchecks(AsrL, SuccST,   Level, AsrSuccL ),
    subtract(AsrSuccL,  AsrCallL, DAsrSuccL),
    prop_rtchecks(AsrL, GlobST,   Level, AsrGlobL ),
    subtract(AsrGlobL,  AsrCallL, DAsrGlobL),
    check_asrs_props(compat,    AsrCompL ),
    check_asrs_props(calls,     AsrCallL ),
    check_asrs_props(success,   DAsrSuccL),
    check_asrs_props(comp,      DAsrGlobL).

prop_rtchecks(AsrL0, IsStatusType, Level, AsrPVL) :-
    include(is_prop_rtcheck(IsStatusType, Level), AsrL0, AsrL),
    pairs_keys_values(AsrPVL, AsrL, _PValuesL).

is_prop_rtcheck(IsValidType, Level, Asr) :-
    asr_head_prop(Asr, _, _, Status, Type, _, _),
    call(IsValidType, Level, Type),
    is_valid_status_type(Status, Type).

no_bindings_asr(Asr, AsrN) :-
    functor(Asr,  I, N),
    functor(AsrN, I, N).

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
