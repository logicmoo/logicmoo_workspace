:- module(send_check, [send_comp_rtcheck/3]).

:- use_module(engine(exceptions)).

send_comp_rtcheck(Goal, PropName, FailName) :-
	FailName =.. [F|Args],
	FailProp =.. [F, PredName|Args],
	( b_getval(rtchecks_goal, Goal0) -> true ; Goal0 = Goal ),
	send_signal(rtcheck(comp, PredName, [Goal0=PredName],
			    [PropName-[FailProp]], [])).
