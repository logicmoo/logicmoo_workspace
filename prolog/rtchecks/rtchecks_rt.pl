:- module(rtchecks_rt, [condition/1,
			checkif_comp/5,
			rtcheck/3,
			call_stack/2,
			'$meta$rtc'/2
		       ]).

:- use_module(library(swi/assertions)).
:- use_module(library(intercept)).
:- use_module(library(engine/term_typing), []). % assertions about builtins
:- use_module(library(context_values)).
:- reexport(rtchecks(rtchecks_send)).

:- doc(author, "Edison Mera").

%:- doc(author, "Based on David Trallero's rtchecks package.").

:- doc(module, "This module contains the predicates that are
	required to implement run-time checks.").

:- prop condition/1 + regtype.

condition(true).
condition(fail).

:- pred checkif_comp(Condition, _, CompGoal, CompGoalArg, Head)

# "If ~w is @tt{true} then the ~w containing
the nested comp predicate calls is called with ~w as
argument. To allow efficient implementation, ~w is the
last nested argument of ~w, so unifiying with ~w
we have the comp check, and calling directly to ~w we skip the
test. An example call could be:

@begin{verbatim}
checkif_comp(C,not_fails(is_det(A)),A,partition(_1,_2,_3,_4))
@end{verbatim}

so if C is true (it should come from @pred{get_checkc/N} predicate), then
A is unified with partition(_1,_2,_3,_4) and
not_fails(is_det(partition(_1,_2,_3,_4))) is called. Else, just
partiton(_1,_2,_3,_4) is called."-[Condition, CompGoal, Head, CompGoalArg,
	 CompGoal, Head, Head].

% :- trust pred checkif_comp(Condition, CompGoal, CompGoalArg, Head)
% 	:: condition * callable * term * callable.

:- doc(bug, "checkif_comp/4 generates a unnecessary run-time
	module expansion").

:- meta_predicate checkif_comp(?, ?, 0, ?, 0).
checkif_comp([], Info, Comp, Goal, Goal) :- with_info(Comp, Info).
checkif_comp([_|_], _, _,    _,    Goal) :- call(Goal).

:- meta_predicate with_info(0, ?).
with_info(Comp, Info) :-
    with_context_value(Comp, comp_info, Info).

rtcheck_ifnot(Check, PredName, Loc) :-
    rtcheck_cond(\+ Check, Check, PredName, Loc).

rtcheck_cond(Cond, Check, PredName, Loc) :-
    ( Cond
    ->send_rtcheck([Check-[]], pp_check, PredName, [], [pploc(Loc)])
    ; true
    ).
    

:- meta_predicate rtcheck(+, 0, ?).

rtcheck(Type, Check, Loc) :-
    ignore(do_rtcheck(Type, Check, Loc)).

do_rtcheck(check, Check, Loc) :-
    rtcheck_ifnot(Check, check/1, Loc).
do_rtcheck(trust, Check, Loc) :-
    current_prolog_flag(rtchecks_trust, yes),
    rtcheck_ifnot(Check, trust/1, Loc).
do_rtcheck(true, Check, Loc) :-
    current_prolog_flag(rtchecks_true, yes),
    rtcheck_ifnot(Check, true/1, Loc).
do_rtcheck(false, Check, Loc) :-
    current_prolog_flag(rtchecks_false, yes),
    rtcheck_cond(Check, Check, false/1, Loc),
    fail.

:- meta_predicate call_stack(0, ?).
call_stack(Goal, Pos) :-
	intercept(Goal, rtcheck(Type, PredName, Dict, PropNames, Poss),
	    send_rtcheck(PropNames, Type, PredName, Dict, [Pos|Poss])).
% :- meta_predicate '$meta$rtc'(0, -).
'$meta$rtc'(Goal, Goal).
