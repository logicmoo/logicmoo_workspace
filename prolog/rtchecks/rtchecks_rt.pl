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
		       ],
	  [assertions, nortchecks, hiord]).

:- use_module(library(terms_vars)).
:- use_module(library(freeze)).
:- if(current_prolog_flag(dialect, swi)).
:-  abolish(send_signal/1),
    abolish(intercept/3).
:- use_module(library(intercept)).
:- use_module(library(engine/term_typing)).
:- endif.
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

# "If @var{Condition} is @tt{true} then the @var{CompGoal} containing
the nested comp predicate calls is called with @var{Head} as
argument. To allow efficient implementation, @var{CompGoalArg} is the
last nested argument of @var{CompGoal}, so unifiying with @var{Head}
we have the comp check, and calling directly to @var{Head} we skip the
test. An example call could be:

@begin{verbatim}
checkif_comp(C,not_fails(is_det(A)),A,partition(_1,_2,_3,_4))
@end{verbatim}

so if C is true (it should come from @pred{get_checkc/N} predicate), then
A is unified with partition(_1,_2,_3,_4) and
not_fails(is_det(partition(_1,_2,_3,_4))) is called. Else, just
partiton(_1,_2,_3,_4) is called.".

% :- trust pred checkif_comp(Condition, CompGoal, CompGoalArg, Head)
% 	:: condition * callable * term * callable.

:- doc(bug, "checkif_comp/4 generates a unnecessary run-time
	module expansion").

:- meta_predicate checkif_comp(?, goal, ?, goal).
checkif_comp([],    Comp, Goal, Goal) :- call(Comp).
checkif_comp([_|_], _,    _,    Goal) :- call(Goal).

:- meta_predicate rttrust(goal, ?, ?, ?).
rttrust(Check, PredName, Dict, Loc) :-
    ( current_prolog_flag(rtchecks_trust, yes)
    ->rtcheck(Check, PredName, Dict, Loc)
    ; true
    ).

:- meta_predicate rtcheck(goal, ?, ?, ?).
rtcheck(Check, PredName, Dict, Loc) :-
	rtcheck_(Check, PredName, Dict, Loc),
	fail.
rtcheck(_, _, _, _).

:- meta_predicate rtcheck_(goal, ?, ?, ?).
rtcheck_(Check, _, _, _) :-
	Check,
	!.
rtcheck_(Check, PredName, Dict, Loc) :-
	send_rtcheck([Check-[]], pp_check, PredName, Dict, [pploc(Loc)]).

:- meta_predicate call_stack(goal, ?).
call_stack(Goal, Pos) :-
	intercept(Goal, rtcheck(Type, PredName, Dict, PropNames, Poss),
	    send_rtcheck(PropNames, Type, PredName, Dict, [Pos|Poss])).
% :- meta_predicate '$meta$rtc'(0, -).
'$meta$rtc'(Goal, Goal).

:- meta_predicate check(goal).
check(_).

:- meta_predicate trust(goal).
trust(_).

:- meta_predicate true(goal).
true(_).

:- meta_predicate false(goal).
false(_).
