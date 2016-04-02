:- begin_tests(rtchecks).

:- multifile
    user:message_property/2.

:- dynamic
    user:error_on_co/0.

:- use_module(library(call_in_dir)).
:- use_module(library(substitute)).
:- use_module(library(rtchecks_eval)).
:- use_module(library(rtchecks_utils)).
:- use_module(library(rtchecks_tracer)).
:- use_module(library(intercept)).
:- use_module(library(libprops)).

user:message_property(_, location_prefix(_, '', 'ERROR: ')).
user:message_property(_, stream(current_output)) :- user:error_on_co.

:- set_prolog_flag(runtime_checks, yes).
:- set_prolog_flag(rtchecks_check, yes).

test(rtc_external) :-
    call_in_module_dir(plunit_rtchecks,
		       ( ['../examples/rtc_external'],
			 save_rtchecks(do_trace_rtc(test_ex)),
			 load_rtchecks(E),
			 % Unload it to avoid further problems with format/3:
			 unload_file('../examples/rtc_external')
		       )),
    assertion(E = [assrchk(ppt(_,_),
			   error(comp,
				 functor(A, B, C),
				 [fails-[not_fails(functor(A, B, C))]],
				 _)),
		   assrchk(ppt(_,_),
		   	   error(success,
		   		 functor(A, B, C),
		   		 [(libprops:atm(B))-[B=0 ]],
		   		 _)),
		   assrchk(ppt(_,_),
			   error(success,
				 functor(A, B, C),
				 [(rtc_external:atom(B))-[B=0 ]],
				 _))]).

test(rtcompile) :-
    %set_prolog_flag(check_assertions, [defined, is_prop, ctcheck]),
    call_in_module_dir(plunit_rtchecks,
		       ( use_module('../examples/rtchecks_disc', []),
			 ['../examples/rtchecks_example2'],
			 use_module('../examples/rtchecks_example', [])
		       )),
    %set_prolog_flag(check_assertions, []).
    retractall(user:error_on_co).

:- ['../examples/rtchecks_example3'].

test(rtexec1) :-
    save_rtchecks(do_trace_rtc(test1)),
    load_rtchecks(E),
    assertion(E=[assrchk(ppt(_,_),
			 error(pp_check, check/1,
			       [(rtchecks_example3: (0>0 ))-[]], _))]).

test(rtexec2) :-
    intercept(do_trace_rtc(test1), Error, print_message(information, Error)).

test(rtexec3) :-
    ignore(save_rtchecks(do_trace_rtc(p(_)))),
    load_rtchecks(E),
    assertion(E=[assrchk(ppt(rtchecks_example3:r/0,
			     clause_pc(_, 7)), error(comp, qq, [not_fails-[fails(qq)]],
						     file(_, _, _, _))),
		 assrchk(ppt(rtchecks_example3:p/1,
			     clause_pc(_, 3)), error(comp, r, [det-[fails(r)]],
						     file(_, _, _, _)))]).

test(rtgen) :-
    generate_rtchecks(fullasr(_X, _Y), rtchecks_example3, RTChecks),
    assertion(RTChecks = rtchecks_rt:rtcheck_goal(fullasr(_A, _B),
						  rtchecks_example3,
						  rtchecks_example3,
						  [_, _, _, _, _, _])).

:- end_tests(rtchecks).
