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
			   [posloc(functor(A,B,C),
				   clause_pc(_,_)),
			    asrloc(loc(_,_,_))]),
		   rtcheck(success,
			   functor(A, B, C),
			   [A=0, B=0, C=0 ],
			   [(rtc_external:atom(B))-[B=0 ]],
			   [posloc(functor(A, B, C),
				   clause_pc(_, _)),
			    asrloc(loc(_, _, _))])]).

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
			 [pploc(clause_pc(_,_))])]).

test(rtgen) :-
    generate_rtchecks(_Loc, rtchecks_example3, fullasr(A,B), RTChecks),
    % portray_clause(true :- RTChecks), % Uncomment this if you want to update this test
    assertion(RTChecks
	     = ( fullasr(_, _)=F,
		 asrloc(loc(_, _, _))=N,
		 asrloc(loc(_, _, _))=G,
		 asrloc(loc(_, _, _))=I,
		 findall(A,
			 ( \+ native_props:compat(rtchecks_example3:atm(B)),
			   A=atm(_)-['A'=B]
			 ),
			 D),
		 findall(C,
			 ( \+ native_props:compat(rtchecks_example3:int(B)),
			   C=int(_)-['A'=B]
			 ),
			 E),
		 (   D\=[],
		     E\=[]
		 ->  rtchecks_send:send_rtcheck(D, compat, F, ['A'=B, 'B'=H], [G]),
		     rtchecks_send:send_rtcheck(E, compat, F, ['A'=B, 'B'=H], [I])
		 ;   true
		 ),
		 findall(J,
			 ( \+ native_props:instance(rtchecks_example3:animal(B)),
			   J=animal(_)-['A'=B]
			 ),
			 M),
		 findall(K,
			 (   \+ native_props:instance(rtchecks_example3:animal(B)),
			     K=animal(_)-['A'=B]
			 ;   \+ native_props:instance(rtchecks_example3:atm(B)),
			     K=atm(_)-['A'=B]
			 ),
			 L),
		 (   L\=[],
		     M\=[]
		 ->  rtchecks_send:send_rtcheck(L, calls, F, ['A'=B, 'B'=H], [N]),
		     rtchecks_send:send_rtcheck(M, calls, F, ['A'=B, 'B'=H], [I])
		 ;   true
		 ),
		 @(rtchecks_rt:checkif_comp(L, info(F, ['A'=B, 'B'=H], [N]), not_fails(O), O,
					    @(rtchecks_rt:checkif_comp(M, info(F, ['A'=B, 'B'=H], [I]), is_det(P), P,
								       rtchecks_example3:fullasr(B, H)), rtchecks_example3)),
	  rtchecks_example3),
		 (   D==[]
		 ->  findall(Q,
			     ( \+ native_props:compat(rtchecks_example3:atm(B)),
			       Q=atm(_)-['A'=B]
			     ),
			     R),
		     rtchecks_send:send_rtcheck(R, success, F, ['A'=B, 'B'=H], [G])
		 ;   true
		 ),
		 (   E==[]
		 ->  findall(S,
			     ( \+ native_props:compat(rtchecks_example3:int(B)),
			       S=int(_)-['A'=B]
			     ),
			     T),
		     rtchecks_send:send_rtcheck(T, success, F, ['A'=B, 'B'=H], [I])
		 ;   true
		 ),
		 (   L==[]
		 ->  findall(U,
			     ( \+ native_props:instance(rtchecks_example3:family(H)),
			       U=family(_)-['B'=H]
			     ),
			     V),
		     rtchecks_send:send_rtcheck(V, success, F, ['A'=B, 'B'=H], [N])
		 ;   true
		 ),
		 (   M==[]
		 ->  findall(W,
			     ( \+ native_props:instance(rtchecks_example3:family(H)),
			       W=family(_)-['B'=H]
			     ),
			     X),
		     rtchecks_send:send_rtcheck(X, success, F, ['A'=B, 'B'=H], [I])
		 ;   true
		 )
	       )).

:- end_tests(rtchecks).
