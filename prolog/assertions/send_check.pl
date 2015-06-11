:- module(send_check, [send_comp_rtcheck/3,
		       get_root_goal/2]).

:- if(current_prolog_flag(dialect, swi)).
:-  abolish(send_signal/1),
    abolish(intercept/3).
:- use_module(library(intercept)).
:- endif.

get_root_goal(Goal, Goal0) :-
	( nb_current(rtchecks_goal, Goal0)
	->true
	; Goal0 = Goal
	).

send_comp_rtcheck(Goal, PropName, FailName) :-
	FailName =.. [F|Args],
	FailProp =.. [F, PredName|Args],
	get_root_goal(Goal, Goal0),
	send_signal(rtcheck(comp, PredName, [Goal0=PredName],
			    [PropName-[FailProp]], [])).
