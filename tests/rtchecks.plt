:- begin_tests(rtchecks).

:- multifile
    user:message_property/2.

:- dynamic
    user:error_on_co/0.

:- use_module(library(call_in_module_file)).
:- use_module(rtchecks(rtchecks_eval)).
:- use_module(rtchecks(rtchecks_utils)).
:- use_module(rtchecks(rtchecks_tracer)).

user:message_property(_, location_prefix(_, '', 'ERROR: ')).
user:message_property(_, stream(current_output)) :- user:error_on_co.

:- set_prolog_flag(runtime_checks, yes).
:- set_prolog_flag(rtchecks_check, yes).

test(rtc_external) :-
    call_in_module_file(plunit_rtchecks,
			( ['../examples/rtc_external'],
			  save_rtchecks(do_trace_rtc(test_ex)),
			  load_rtchecks(E),
			  % Unload it to avoid further problems with format/3:
			  unload_file('../examples/rtc_external')
			)),
    assertion(E = [rtcheck(comp,
			   functor(A,B,C),
			   [A=0,B=0,C=0 ],
			   [fails-[not_fails(functor(A,B,C))]],
			   _,
			   _),
		   rtcheck(success,
			   functor(A, B, C),
			   [A=0, B=0, C=0 ],
			   [(rtc_external:atom(B))-[B=0 ]],
			   _,
			   _)]).

test(rtcompile) :-
    %set_prolog_flag(check_assertions, [defined, is_prop, ctcheck]),
    call_in_module_file(plunit_rtchecks,
			( 
			  use_module('../examples/rtchecks_disc', []),
			  ['../examples/rtchecks_example2'],
			  use_module('../examples/rtchecks_example', [])
			)),
    %set_prolog_flag(check_assertions, []).
    retractall(user:error_on_co).

:- ['../examples/rtchecks_example3'].

test(rtexec) :-
    save_rtchecks(do_trace_rtc(test1)),
    load_rtchecks(E),
    assertion(E=[rtcheck(pp_check, check/1, [],
			 [(rtchecks_example3: (0>0 ))-[]],
			 _,
			 _)]).

pretty_display(RTChecks) :-
    RTChecks = rtchecks_rt:checkif_modl(M1, M2, G1, G2, G),
    numbervars(RTChecks, 20, _,
	       [ singletons(true)
	       ]),
    with_output_to(string(S1),
		   prolog_listing:portray_body(G1, 10, noindent, 1200, current_output, [])),
    with_output_to(string(S2),
		   prolog_listing:portray_body(G2, 10, noindent, 1200, current_output, [])),
    with_output_to(string(S),
		   prolog_listing:portray_body(G,  10, noindent, 1200, current_output, [])),
    format("rtchecks_rt:checkif_modl(~w, ~w,~n       (~s),~n       ~s,~n       (~s))",
	   [M1, M2, S1, S2, S]),fail.
pretty_display(_).

test(rtgen) :-
    generate_rtchecks(fullasr(_X,_Y), rtchecks_example3, _Loc, RTChecks),
    rtc_expected(ERTChecks),
    ( RTChecks \= ERTChecks
    ->pretty_display(RTChecks)
    ; true
    ),
    assertion(RTChecks = ERTChecks).

rtc_expected(rtchecks_rt:checkif_modl(rtchecks_example3, rtchecks_example3,
       (findall(A,
	(   \+ instance(rtchecks_example3:animal(B)),
	    A=animal(A)-['A'=B]
	;   \+ instance(rtchecks_example3:var(C)),
	    A=var(B)-['B'=C]
	),
	D),
	  (   D\=[]
	  ->  send_rtcheck(D,
			   (calls),
			   fullasr(A, B),
			   ['A'=B, 'B'=C],
			   J,
			   file('/home/edison/apps/pl-tests/rtchecks/examples/rtchecks_example3.pl', 34, 9, _))
	  ;   true
	  ),
	  E),
       E,
       (findall(F,
	( \+ compat(rtchecks_example3:atm(B)),
	  F=atm(A)-['A'=B]
	),
	H),
	  findall(G,
		  ( \+ compat(rtchecks_example3:int(B)),
		    G=int(A)-['A'=B]
		  ),
		  I),
	  (   H\=[],
	      I\=[]
	  ->  send_rtcheck(H,
			   compat,
			   fullasr(A, B),
			   ['A'=B, 'B'=C],
			   J,
			   file('/home/edison/apps/pl-tests/rtchecks/examples/rtchecks_example3.pl', 37, 8, U)),
	      send_rtcheck(I,
			   compat,
			   fullasr(A, B),
			   ['A'=B, 'B'=C],
			   J,
			   file('/home/edison/apps/pl-tests/rtchecks/examples/rtchecks_example3.pl', 38, 8, O))
	  ;   true
	  ),
	  findall(K,
		  ( \+ instance(rtchecks_example3:animal(B)),
		    K=animal(A)-['A'=B]
		  ),
		  N),
	  findall(L,
		  (   \+ instance(rtchecks_example3:animal(B)),
		      L=animal(A)-['A'=B]
		  ;   \+ instance(rtchecks_example3:atm(B)),
		      L=atm(A)-['A'=B]
		  ),
		  M),
	  (   M\=[],
	      N\=[]
	  ->  send_rtcheck(M,
			   (calls),
			   fullasr(A, B),
			   ['A'=B, 'B'=C],
			   J,
			   file('/home/edison/apps/pl-tests/rtchecks/examples/rtchecks_example3.pl', 36, 8, P)),
	      send_rtcheck(N,
			   (calls),
			   fullasr(A, B),
			   ['A'=B, 'B'=C],
			   J,
			   file('/home/edison/apps/pl-tests/rtchecks/examples/rtchecks_example3.pl', 38, 8, O))
	  ;   true
	  ),
	  checkif_comp(M,
		       info(fullasr(A, B),
			    ['A'=B, 'B'=C],
			    J,
			    file('/home/edison/apps/pl-tests/rtchecks/examples/rtchecks_example3.pl', 36, 8, P)),
		       not_fails(Q),
		       Q,
		       checkif_comp(N,
				    info(fullasr(A, B),
					 ['A'=B, 'B'=C],
					 J,
					 file('/home/edison/apps/pl-tests/rtchecks/examples/rtchecks_example3.pl', 38, 8, O)),
				    is_det(R),
				    R,
				    rtchecks_example3:fullasr(B, C))),
	  (   H=[]
	  ->  findall(S,
		      ( \+ compat(rtchecks_example3:atm(B)),
			S=atm(A)-['A'=B]
		      ),
		      T),
	      send_rtcheck(T,
			   (success),
			   fullasr(A, B),
			   ['A'=B, 'B'=C],
			   J,
			   file('/home/edison/apps/pl-tests/rtchecks/examples/rtchecks_example3.pl', 37, 8, U))
	  ;   true
	  ),
	  (   I=[]
	  ->  findall(V,
		      ( \+ compat(rtchecks_example3:int(B)),
			V=int(A)-['A'=B]
		      ),
		      W),
	      send_rtcheck(W,
			   (success),
			   fullasr(A, B),
			   ['A'=B, 'B'=C],
			   J,
			   file('/home/edison/apps/pl-tests/rtchecks/examples/rtchecks_example3.pl', 38, 8, O))
	  ;   true
	  ),
	  (   M=[]
	  ->  findall(X,
		      ( \+ instance(rtchecks_example3:family(C)),
			X=family(B)-['B'=C]
		      ),
		      Y),
	      send_rtcheck(Y,
			   (success),
			   fullasr(A, B),
			   ['A'=B, 'B'=C],
			   J,
			   file('/home/edison/apps/pl-tests/rtchecks/examples/rtchecks_example3.pl', 36, 8, P))
	  ;   true
	  ),
	  (   N=[]
	  ->  findall(Z,
		      ( \+ instance(rtchecks_example3:family(C)),
			Z=family(B)-['B'=C]
		      ),
		      A1),
	      send_rtcheck(A1,
			   (success),
			   fullasr(A, B),
			   ['A'=B, 'B'=C],
			   J,
			   file('/home/edison/apps/pl-tests/rtchecks/examples/rtchecks_example3.pl', 38, 8, O))
	  ;   true
	  )))).

:- end_tests(rtchecks).
