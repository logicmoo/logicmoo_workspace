:- module(rtchecks_send, [send_rtcheck/5], [assertions, nortchecks, dcg]).

:- if(current_prolog_flag(dialect, swi)).
:-  abolish(send_signal/1),
    abolish(intercept/3).
:- use_module(library(intercept)).
:- endif.
:- use_module(rtchecks(rtchecks_utils)).

send_rtcheck([], _, _, _, _) :- !.
send_rtcheck(Props, ErrType, PredName, Dict, AsrLocs) :-
	send_signal(rtcheck(ErrType, PredName, Dict, Props, AsrLocs)).
