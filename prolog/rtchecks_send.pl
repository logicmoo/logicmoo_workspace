:- module(rtchecks_send, [send_rtcheck/4]).

:- use_module(xlibrary(intercept)).

send_rtcheck([], _, _, _) :- !.
send_rtcheck(Props, ErrType, PredName, ALoc) :-
	send_signal(assrchk(asr, error(ErrType, PredName, Props, ALoc))).
