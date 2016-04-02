:- module(rtchecks_gen, [collect_assertions/4,
			 current_assertion/4,
			 generate_ctchecks/4,
			 proc_ppassertion/3,
			 current_assertion/8]).

:- use_module(library(assertions)).
:- use_module(library(assrt_lib)).
:- use_module(library(basicprops)).
:- use_module(library(lists)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(implementation_module)).

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

current_assertion(Asr, Pred, CM, TimeCheck, Status, Type, Dict, Loc) :-
    asr_head_prop(Asr, CM, Pred, Status, Type, Dict, Loc),
    assertion_is_valid(TimeCheck, Status, Type, Asr),
    ( current_prolog_flag(rtchecks_level, inner)
    ->true
    ; current_prolog_flag(rtchecks_level, exports),
      predicate_property(CM:Pred, export)
    ->true
    ),
    \+ black_list_pred(Pred).

current_assertion(Pred, M, TimeCheck, Asr) :-
    current_assertion(Asr, Pred, CM, TimeCheck, _, _, _, _),
    implementation_module(CM:Pred, M).

pred_assertion(Pred, Pred-Asr, Asr).

collect_assertions(Pred, M, TimeCheck, AsrL) :-
    findall(Pred-Asr, current_assertion(Pred, M, TimeCheck, Asr), Pairs),
    maplist(pred_assertion(Pred), Pairs, AsrL).

ppassertion_type_goal(check(Goal), check, Goal).
ppassertion_type_goal(trust(Goal), trust, Goal).
ppassertion_type_goal(true( Goal), true,  Goal).
ppassertion_type_goal(false(Goal), false, Goal).

proc_ppassertion(PPAssertion, CM, rtc_call(Type, CM:Goal)) :-
    ppassertion_type_goal(PPAssertion, Type, Goal).

% Generate compile-time checks, currently only compatibility is checked,
% fails if no ctchecks can be applied to Pred
%
generate_ctchecks(Pred, M, CM, Lits) :-
    functor(Pred, F, A),
    functor(Head, F, A),
    collect_assertions(Head, M, ctcheck, Assertions0),
    maplist(abstract_assertions, Assertions0, Assertions),
				% Abstraction step, here we lose precision
				% but we gain computability of checks at
				% earlier, even compile-time. --EMM
    compat_rtchecks(Assertions, _, [], _, [], _, Lits, []),
    qualify_meta_goal(Pred, M, CM, Head). % ???

/*
%% Trivial abstraction: Check for compatibility issues in properties,
%% compatibility is an abstraction that makes static check decidable.
%% The pay-off is a lose of precision. TBD: Formal demostration. --EMM
abstract_assertions(assr(Asr, Pred, Status, Type, Compat0, Call, Succ, _Comp, ALoc, PredName),
		    assr(Asr, Pred, Status, Type, Compat, [], [], [], ALoc, PredName)) :-
    append([Compat0, Call, Succ], Compat).
*/
