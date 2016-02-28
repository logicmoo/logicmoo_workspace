:- module(send_check, [get_comp_rtcheck_info/2,
		       send_rtcheck/4,
		       send_comp_rtcheck/3]).

:- use_module(library(context_values)).
:- use_module(library(intercept)).

get_comp_rtcheck_info(Goal, Info) :-
    ( current_context_value(rtchecks_rt:comp_info, Info)
    ->true
    ; Info = info(Goal, _)
    ).

send_comp_rtcheck(Goal, PropName, FailName) :-
    FailName =.. [F|Args],
    FailProp =.. [F, PredName|Args],
    get_comp_rtcheck_info(Goal, info(PredName, ALoc)),
    send_rtcheck([PropName-[FailProp]], comp, PredName, ALoc).

send_rtcheck([], _, _, _) :- !.
send_rtcheck(Props, ErrType, PredName, ALoc) :-
	send_signal(assrchk(asr, error(ErrType, PredName, Props, ALoc))).
