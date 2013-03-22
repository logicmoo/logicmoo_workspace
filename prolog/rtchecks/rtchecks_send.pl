:- module(rtchecks_send, [send_rtcheck/5, send_comp_rtcheck/3],
	  [assertions, nortchecks, dcg]).

send_rtcheck([], _, _, _, _) :- !.
send_rtcheck(Props, ErrType, PredName, Dict, AsrLocs) :-
	send_signal(rtcheck(ErrType, PredName, Dict, Props, AsrLocs)).

send_comp_rtcheck(Goal, PropName, FailName) :-
	FailName =.. [F|Args],
	FailProp =.. [F, PredName|Args],
	send_signal(rtcheck(comp, PredName, [Goal=PredName], [PropName-[FailProp]], [])).
