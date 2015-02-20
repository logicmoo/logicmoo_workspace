:- begin_tests(rtchecks).

:- multifile
    user:message_property/2.

:- dynamic
    user:error_on_co/0.

:- use_module(library(call_in_module_file)).
:- use_module(library(swi/rtchecks)).

user:message_property(_, location_prefix(_, '', 'ERROR: ')).
user:message_property(_, stream(current_output)) :- user:error_on_co.

test(rtcompile) :-
    %set_prolog_flag(check_assertions, [defined, is_prop, ctcheck]),
    expects_dialect(ciao),
    call_in_module_file(plunit_rtchecks,
			( ['../examples/rtc_external'],
			  use_module('../examples/rtchecks_disc', []),
			  ['../examples/rtchecks_example2'],
			  use_module('../examples/rtchecks_example', [])
			)),
    expects_dialect(swi),
    %set_prolog_flag(check_assertions, []).
    retractall(user:error_on_co).

test(rtexec3) :-
    expects_dialect(ciao),
    call_in_module_file(plunit_rtchecks,
			( ['../examples/rtchecks_example3'],
			  catch(test1,E,true))),
    expects_dialect(swi),
    assertion(E=error(unintercepted_signal(rtcheck(pp_check, square(_, _),
						   ['X'=0, 'X2'=0 ],
						   [(rtchecks_example3: (0>0 ))-[]],
						   _)), signal/1-1)),
    H1 = rtchecks_example3:fullasr(_,_),
    clause(H1, B1), assertion((H1 :- B1)\=(H1 :- true)),
    H2 = rtchecks_example3:'fullasr/2$rtc1'(_, _),
    clause(H2, B2), assertion((H2 :- B2)\=(H2 :- true)),
    H3 = rtchecks_example3:'fullasr/2$rtc2'(_, _),
    clause(H3, B3), assertion((H3 :- B3)==(H3 :- true)).

:- end_tests(rtchecks).
