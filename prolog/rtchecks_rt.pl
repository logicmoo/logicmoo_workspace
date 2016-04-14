:- module(rtchecks_rt, [rtcheck_goal/1,
			rtcheck_goal/2,
			rtcheck_goal/4,
			rtc_call/2,
			with_gloc/2,
			with_assertion/2,
			collect_assertions/4,
			current_assertion/4
		       ]).

:- use_module(library(assertions)).
:- use_module(library(termtyping), []). % assertions about builtins
:- use_module(library(context_values)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(implementation_module)).
:- use_module(library(resolve_calln)).

:- reexport(library(send_check)).
:- reexport(library(nativeprops)).
:- reexport(library(basicprops)).

:- doc(author, "Edison Mera").

%:- doc(author, "Based on David Trallero's rtchecks package.").

:- doc(module, "This module contains the predicates that are
	required to implement run-time checks.").

/*

Algorithm:

pred :- body.

is transformed in:

pred :-                        \
	"check entry...",       \___________ STEP
	"check exit...",        /            ONE
	'pred$rtc1'.           /

'pred$rtc1' :-                            \
	"check compat pre..."              \
	"check calls...",                   \
	"check success pre",                 \__________ STEP
	"check comp..."(                     /           TWO
	'pred$rtc2',                        /
	"check success pos",               /
	"check compat pos..."             /

'pred$rtc2' :-
	body.

And goals preds are renamed to 'pred$rtc1'.  There are other steps in
order to simplify the generated code as far as possible.

*/

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

ppassertion_type_goal(check(Goal), check, Goal).
ppassertion_type_goal(trust(Goal), trust, Goal).
ppassertion_type_goal(true( Goal), true,  Goal).
ppassertion_type_goal(false(Goal), false, Goal).

:- meta_predicate rtcheck_goal(0).
rtcheck_goal(CM:Goal0 ) :-
    notrace(resolve_calln(Goal0, Goal)),
    ( ppassertion_type_goal(Goal, Type, Pred)
    ->rtc_call(Type, CM:Pred)
    ; notrace(collect_rtasr(Goal, CM, Pred, M, RAsrL)),
      rtcheck_goal(Pred, M, CM, RAsrL)
    ).

collect_rtasr(Goal, CM, Pred, M, RAsrL) :-
    implementation_module(CM:Goal, M),
    qualify_meta_goal(Goal, M, CM, Pred),
    collect_assertions(Pred, M, rtcheck, AsrL),
    maplist(wrap_asr_rtcheck, AsrL, RAsrL).
      
wrap_asr_rtcheck(Asr, rtcheck(Asr)).

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

/*
  Combination of status and rtcheck indicators, to control the compile
  and run-time checking:

  ===========+============++===========+==========
  Status     | + Command  || ctchecked | rtchecked
  ===========+============++===========+==========
  true/trust | -          || no        | no
  true/trust | rtcheck    || no        | yes
  check      | no_rtcheck || yes       | no
  check      | -          || yes       | yes
  ===========+============++===========+==========
  Note: Is weird to preserve ciao-compatibility
*/

valid_ctcheck_assertions(Status, Type) :-
    ctcheck_assr_status(Status),
    ctcheck_assr_type(Type).

ctcheck_assr_status(trust).
ctcheck_assr_status(check).

ctcheck_assr_type(calls).
ctcheck_assr_type(entry).
ctcheck_assr_type(pred).
ctcheck_assr_type(prop).
ctcheck_assr_type(exit).
ctcheck_assr_type(success).

black_list_pred(_=_).

assertion_is_valid(ctcheck, Status, Type, _) :-
    valid_ctcheck_assertions(Status, Type).
assertion_is_valid(rtcheck, Status, Type, Asr) :-
    ( \+ asr_glob(Asr, _, rtcheck(_), _)
    ->rtchecks_rt:is_valid_status_type(Status, Type),
      \+ asr_glob(Asr, _, no_rtcheck(_), _)
    ; true % Force run-time checking
    ).

current_assertion(Pred, M, TimeCheck, Asr) :-
    \+ ( TimeCheck = rtcheck,
	 asr_head_prop(_, AM, Pred, _, prop, _, _),
	 implementation_module(AM:Pred, M)
       ),
    asr_head_prop(Asr, CM, Pred, Status, Type, _, _),
    assertion_is_valid(TimeCheck, Status, Type, Asr),
    ( current_prolog_flag(rtchecks_level, inner)
    ->true
    ; current_prolog_flag(rtchecks_level, exports),
      predicate_property(CM:Pred, export)
    ->true
    ),
    \+ black_list_pred(Pred),
    implementation_module(CM:Pred, M).

pred_assertion(Pred, Pred-Asr, Asr).

collect_assertions(Pred, M, TimeCheck, AsrL) :-
    findall(Pred-Asr, current_assertion(Pred, M, TimeCheck, Asr), Pairs),
    maplist(pred_assertion(Pred), Pairs, AsrL).
