:- begin_tests(rtchecks).

:- use_module(library(swi/rtchecks)).

test(rtcompile) :-
    %set_prolog_flag(check_assertions, [defined, is_prop, ctcheck]),
    expects_dialect(ciao),
    [rtchecks/examples/rtc_external],
    use_module(rtchecks/examples/rtchecks_disc, []),
    [rtchecks/examples/rtchecks_example2],
    use_module(rtchecks/examples/rtchecks_example, []),
    [rtchecks/examples/rtchecks_inline],
    %set_prolog_flag(check_assertions, []).
    retractall(user:error_on_co).

test(rtexec3) :-
    [rtchecks/examples/rtchecks_example3],
    catch(test1,E,true),
    assertion(E=error(unintercepted_signal(rtcheck(pp_check, square(_, _),
						   ['X'=0, 'X2'=0 ],
						   [(rtchecks_example3: (0>0 ))-[]],
						   _)), signal/1-1)),
    assertion((clause(rtchecks_example3:fullasr(_,_),B1), B1\=true)),
    assertion((clause(rtchecks_example3:'fullasr/2$rtc1'(_, _),B2), B2\=true)),
    assertion((clause(rtchecks_example3:'fullasr/2$rtc2'(_, _),B3), B3==true)).

:- end_tests(rtchecks).
