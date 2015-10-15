:- begin_tests(rtchecks).

:- multifile
    user:message_property/2.

:- dynamic
    user:error_on_co/0.

:- use_module(library(call_in_module_file)).
:- use_module(xlibrary(substitute)).
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
    assertion(E = [assrchk(ppt(_,_),
			   error(comp,
				 functor(A,B,C),
				 [fails-[not_fails(functor(A,B,C))]],
				 _)),
		   assrchk(ppt(_,_),
			   error(success,
				 functor(A, B, C),
				 [(rtc_external:atom(B))-[B=0 ]],
				 _))]).

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
    assertion(E=[assrchk(ppt(_,_),
			 error(pp_check, check/1,
			       [(rtchecks_example3: (0>0 ))-[]],
			       _))]).

substitute_file_4(File4, _) :-
    nonvar(File4),
    File4 = file(_, _, _, _).

pretty_display(RTChecks0 ) :-
    substitute(substitute_file_4, RTChecks0, RTChecks),
    RTChecks = checkif_modl(M1, M2, MG1:G1, G2, MG2:G),
    numbervars(RTChecks, 20, _,
	       [ singletons(true)
	       ]),
    with_output_to(string(S1),
		   prolog_listing:portray_body(G1, 10, noindent, 1200, current_output, [])),
    with_output_to(string(S2),
		   prolog_listing:portray_body(G2, 10, noindent, 1200, current_output, [])),
    with_output_to(string(S),
		   prolog_listing:portray_body(G,  10, noindent, 1200, current_output, [])),
    format("checkif_modl(~w, ~w,~n       ~a:(~s),~n       ~s,~n       ~a:(~s))~n",
	   [M1, M2, MG1, S1, S2, MG2, S]),fail.
pretty_display(_).

test(rtgen) :-
    generate_rtchecks(fullasr(_X,_Y), rtchecks_example3, RTChecks),
    rtc_expected(ERTChecks),
    ( RTChecks \= ERTChecks
    ->pretty_display(RTChecks)
    ; true
    ),
    assertion(RTChecks = ERTChecks).

rtc_expected(checkif_modl(rtchecks_example3, rtchecks_example3,
       rtchecks_example3:(findall(A,
	(   \+ instance(rtchecks_example3:animal(B)),
	    A=animal(A)-['A'=B]
	;   \+ instance(rtchecks_example3:var(C)),
	    A=var(B)-['B'=C]
	),
	D),
	  (   D\=[]
	  ->  send_rtcheck(D, calls, fullasr(A, B), _)
	  ;   true
	  ),
	  E),
       E,
       rtchecks_example3:(findall(F,
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
	  ->  send_rtcheck(H, compat, fullasr(A, B), _),
	      send_rtcheck(I, compat, fullasr(A, B), _)
	  ;   true
	  ),
	  findall(J,
		  ( \+ instance(rtchecks_example3:animal(B)),
		    J=animal(A)-['A'=B]
		  ),
		  M),
	  findall(K,
		  (   \+ instance(rtchecks_example3:animal(B)),
		      K=animal(A)-['A'=B]
		  ;   \+ instance(rtchecks_example3:atm(B)),
		      K=atm(A)-['A'=B]
		  ),
		  L),
	  (   L\=[],
	      M\=[]
	  ->  send_rtcheck(L, calls, fullasr(A, B), _),
	      send_rtcheck(M, calls, fullasr(A, B), _)
	  ;   true
	  ),
	  checkif_comp(L,
		       info(fullasr(A, B), _),
		       rtchecks_example3:not_fails(N),
		       N,
		       rtchecks_example3:checkif_comp(M, info(fullasr(A, B), _), rtchecks_example3:is_det(O), O, rtchecks_example3:rtchecks_example3:fullasr(B, C))),
	  (   H=[]
	  ->  findall(P,
		      ( \+ compat(rtchecks_example3:atm(B)),
			P=atm(A)-['A'=B]
		      ),
		      Q),
	      send_rtcheck(Q, success, fullasr(A, B), _)
	  ;   true
	  ),
	  (   I=[]
	  ->  findall(R,
		      ( \+ compat(rtchecks_example3:int(B)),
			R=int(A)-['A'=B]
		      ),
		      S),
	      send_rtcheck(S, success, fullasr(A, B), _)
	  ;   true
	  ),
	  (   L=[]
	  ->  findall(T,
		      ( \+ instance(rtchecks_example3:family(C)),
			T=family(B)-['B'=C]
		      ),
		      U),
	      send_rtcheck(U, success, fullasr(A, B), _)
	  ;   true
	  ),
	  (   M=[]
	  ->  findall(V,
		      ( \+ instance(rtchecks_example3:family(C)),
			V=family(B)-['B'=C]
		      ),
		      W),
	      send_rtcheck(W, success, fullasr(A, B), _)
	  ;   true
	  )))).

:- end_tests(rtchecks).
