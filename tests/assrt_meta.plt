:- begin_tests(assrt_meta).

:- use_module(library(rtchecks)).
:- use_module(library(rtchecks_tracer)).
:- use_module(assrt_meta_ex).
:- use_module(library(assrt_meta)).
:- set_prolog_flag(rtchecks_check,  yes).

test(assrt_meta) :-
    save_rtchecks(do_trace_rtc(amtest)),
    load_rtchecks(RTChecks),
    assertion(RTChecks == []).

run_amt(RTChecks) :-
    catch(save_rtchecks(do_trace_rtc(amtestf)),
	  E,
	  true),
    assertion(E=error(existence_error(procedure,
				      assrt_meta_ex:undefined_proc/1),
		      context(_,_))),
    load_rtchecks(RTChecks).

test(assrt_meta_f_1) :-
    set_prolog_flag(assrt_meta_pred, none),
    run_amt(RTChecks),
    assertion(RTChecks = [_]).

test(assrt_meta_f_2) :-
    set_prolog_flag(assrt_meta_pred, all),
    run_amt(RTChecks),
    assertion(RTChecks = [_, _]).

test(assrt_meta_f_3) :-
    set_prolog_flag(assrt_meta_pred, specific),
    run_amt(RTChecks),
    assertion(RTChecks = [_, _]).

:- end_tests(assrt_meta).
