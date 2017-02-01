:- module(send_check, [get_comp_rtcheck_info/3,
                       send_rtcheck/4,
                       send_comp_rtcheck/3]).

:- use_module(library(assrt_lib)).
:- use_module(library(intercept)).

get_comp_rtcheck_info(Goal, Name, From) :-
    ( nb_current('$with_asr_head', Asr-Name)
    ->asr_aprop(Asr, head, _:Name, From)
    ; Name = Goal
    ).

send_comp_rtcheck(Goal, Prop, Fail) :-
    get_comp_rtcheck_info(Goal, PredName, ALoc),
    ignore(nb_current('$with_gloc', GLoc)),
    send_rtcheck([GLoc/Prop-[Fail]], comp, PredName, ALoc).

send_rtcheck([], _, _, _) :- !.
send_rtcheck(Props, ErrType, PredName, ALoc) :-
        send_signal(assrchk(asr, error(ErrType, PredName, Props, ALoc))).
