:- module(rtchecks_rt, [condition/1,
			checkif_comp/4,
			rtcheck/4,
			rttrust/4,
			call_stack/2,
			'$meta$rtc'/2,
			with_goal/2,
			check/1,
			trust/1,
			true/1,
			false/1
		       ]).

:- use_module(library(swi/assertions)).
:- use_module(library(intercept)).
:- use_module(library(engine/term_typing), []). % assertions about builtins
:- reexport(rtchecks(rtchecks_send)).

:- doc(author, "Edison Mera").

%:- doc(author, "Based on David Trallero's rtchecks package.").

:- doc(module, "This module contains the predicates that are
	required to implement run-time checks.").

:- prop condition/1 + regtype.

condition(true).
condition(fail).

:- meta_predicate with_goal(0, ?).
with_goal(Comp, Goal) :-
    setup_call_cleanup(b_setval(rtchecks_goal, Goal),
		       Comp,
		       nb_delete(rtchecks_goal)).

:- pred checkif_comp(Condition, CompGoal, CompGoalArg, Head)

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

:- meta_predicate checkif_comp(?, 0, ?, 0).
checkif_comp([],    Comp, Goal, Goal) :- call(Comp).
checkif_comp([_|_], _,    _,    Goal) :- call(Goal).

:- meta_predicate rttrust(0, ?, ?, ?).
rttrust(Check, PredName, Dict, Loc) :-
    ( current_prolog_flag(rtchecks_trust, yes)
    ->rtcheck(Check, PredName, Dict, Loc)
    ; true
    ).

:- meta_predicate rtcheck(0, ?, ?, ?).
rtcheck(Check, PredName, Dict, Loc) :-
	rtcheck_(Check, PredName, Dict, Loc),
	fail.
rtcheck(_, _, _, _).

:- meta_predicate rtcheck_(0, ?, ?, ?).
rtcheck_(Check, _, _, _) :-
	Check,
	!.
rtcheck_(Check, PredName, Dict, Loc) :-
	send_rtcheck([Check-[]], pp_check, PredName, Dict, [pploc(Loc)]).

:- meta_predicate call_stack(0, ?).
call_stack(Goal, Pos) :-
	intercept(Goal, rtcheck(Type, PredName, Dict, PropNames, Poss),
	    send_rtcheck(PropNames, Type, PredName, Dict, [Pos|Poss])).
% :- meta_predicate '$meta$rtc'(0, -).
'$meta$rtc'(Goal, Goal).

:- meta_predicate check(0).
check(_).

:- meta_predicate trust(0).
trust(_).

:- meta_predicate true(0).
true(_).

:- meta_predicate false(0).
false(_).
