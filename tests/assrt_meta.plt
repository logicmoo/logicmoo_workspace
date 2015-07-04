:- begin_tests(assrt_meta).

:- use_module(rtchecks(rtchecks_utils)).
:- use_module(library(rtchecks_tracer)).

:- use_module(assrt_meta_ex).

test(assrt_meta) :-
    save_rtchecks(do_trace_rtc(amtest)),
    load_rtchecks(RTChecks),
    assertion(RTChecks == []).

test(assrt_meta_f) :-
    catch(save_rtchecks(do_trace_rtc(amtestf)),
	  E,
	  true),
    assertion(E=error(existence_error(procedure,
				      assrt_meta_ex:undefined_proc/1),
		      context(assrt_meta_ex:metapred/4,_))),
    load_rtchecks(RTChecks),
    assertion(RTChecks = [_, _]).

:- end_tests(assrt_meta).
