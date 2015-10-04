:- module(send_check, [send_comp_rtcheck/3]).

:- use_module(library(intercept)).
:- use_module(library(context_values)).

get_comp_rtcheck_info(Goal, Info) :-
    ( current_context_value(rtchecks_rt:comp_info, Info)
    ->true
    ; Info = info(Goal, [], _, _)
    ).

send_comp_rtcheck(Goal, PropName, FailName) :-
    FailName =.. [F|Args],
    FailProp =.. [F, PredName|Args],
    get_comp_rtcheck_info(Goal, info(PredName, Dict, PLoc, ALoc)),
    send_signal(rtcheck(comp, PredName, Dict, [PropName-[FailProp]], PLoc, ALoc)).
