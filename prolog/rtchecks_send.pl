:- module(rtchecks_send, [send_rtcheck/5]).

:- use_module(xlibrary(intercept)).

send_rtcheck([], _, _, _, _) :- !.
send_rtcheck(Props, ErrType, PredName, Dict, ALoc) :-
	send_signal(rtcheck(asr, ErrType, PredName, Dict, Props, ALoc)).
