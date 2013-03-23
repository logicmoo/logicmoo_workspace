:- module(rtchecks_send, [send_rtcheck/5], [assertions, nortchecks, dcg]).

send_rtcheck([], _, _, _, _) :- !.
send_rtcheck(Props, ErrType, PredName, Dict, AsrLocs) :-
	send_signal(rtcheck(ErrType, PredName, Dict, Props, AsrLocs)).
