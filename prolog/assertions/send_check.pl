:- module(send_check, [send_comp_rtcheck/3]).

:- use_module(engine(exceptions)).

send_comp_rtcheck(Goal, PropName, FailName) :-
	FailName =.. [F|Args],
	FailProp =.. [F, PredName|Args],
	send_signal(rtcheck(comp, PredName, [Goal=PredName],
			    [PropName-[FailProp]], [])).
