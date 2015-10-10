:- module(rtchecks_send, [send_rtcheck/6]).

:- use_module(xlibrary(intercept)).

send_rtcheck([], _, _, _, _, _) :- !.
send_rtcheck(Props, ErrType, PredName, Dict, PLoc, ALoc) :-
	send_signal(rtcheck(ErrType, PredName, Dict, Props, PLoc, ALoc)).
