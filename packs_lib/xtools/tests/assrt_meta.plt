:- begin_tests(assrt_meta).

:- use_module(library(rtchecks_utils)).
:- use_module(assrt_meta_ex).
:- use_module(library(assrt_meta)).
:- use_module(library(rtchecks)).

:- set_prolog_flag(rtchecks_check,  yes).

run_amt(RTChecks) :-
    catch(save_rtchecks(with_rtchecks(amtestf)),
          E,
          true),
    assertion(E=error(existence_error(procedure,
                                      assrt_meta_ex:undefined_proc/1),
                      context(_,_))),
    load_rtchecks(RTChecks).

test(assrt_meta) :-
    save_rtchecks(with_rtchecks(amtest)),
    load_rtchecks(RTChecks),
    assertion(RTChecks == []).

test(assrt_meta_f_1) :-
    set_prolog_flag(assrt_meta_pred, none),
    run_amt(RTChecks),
    assertion(RTChecks = [_]).

test(assrt_meta_f_2) :-
    set_prolog_flag(assrt_meta_pred, all),
    % call_rtc(with_rtchecks(amtestf)),
    run_amt(RTChecks),
    assertion(RTChecks = [_, _]).

test(assrt_meta_f_2b) :-
    set_prolog_flag(assrt_meta_pred, all),
    save_rtchecks(with_rtchecks((amtestf2,amtestf2))),
    load_rtchecks(RTChecks),
    assertion(RTChecks == []).

test(assrt_meta_f_3) :-
    set_prolog_flag(assrt_meta_pred, specific),
    % call_rtc(with_rtchecks(amtestf)),
    run_amt(RTChecks),
    assertion(RTChecks = [_, _]).

:- end_tests(assrt_meta).
