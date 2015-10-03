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
    generate_rtchecks(_Loc, rtchecks_example3, fullasr(A,C), RTChecks),
    % portray_clause(true :- RTChecks), % Uncomment this if you want to update this test
    assertion(RTChecks = rtchecks_rt:checkif_modl(rtchecks_example3, rtchecks_example3,
	( 
	  findall(B,
		  (   \+ instance(rtchecks_example3:animal(A)),
		      B=animal(A)-['A'=A]
		  ;   \+ instance(rtchecks_example3:var(C)),
		      B=var(B)-['B'=C]
		  ),
		  D),
	  (   D\=[]
	  ->  send_rtcheck(D,
			   (calls),
			   fullasr(A, B),
			   ['A'=A, 'B'=C],
			   
			   [ asrloc(loc('/home/edison/apps/pl-tests/rtchecks/examples/rtchecks_example3.pl',
					34,
				      34))
			   ])
	  ;   true
	  ),
	  E
	),
	E,
	( fullasr(_, _)=J,
	  asrloc(loc(_, _, _))=Q,
	  asrloc(loc(_, _, _))=K,
	  asrloc(loc(_, _, _))=L,
	  findall(F,
		  ( \+ compat(rtchecks_example3:atm(A)),
		    F=atm(A)-['A'=A]
		  ),
		  H),
	  findall(G,
		  ( \+ compat(rtchecks_example3:int(A)),
		    G=int(A)-['A'=A]
		  ),
		  I),
	  (   H\=[],
	      I\=[]
	  ->  send_rtcheck(H,
			   compat,
			   J,
			   ['A'=A, 'B'=C],
			   [K]),
	      send_rtcheck(I,
			   compat,
			   J,
			   ['A'=A, 'B'=C],
			   [L])
	  ;   true
	  ),
	  findall(M,
		  ( \+ instance(rtchecks_example3:animal(A)),
		    M=animal(A)-['A'=A]
		  ),
		  P),
	  findall(N,
		  (   \+ instance(rtchecks_example3:animal(A)),
		      N=animal(A)-['A'=A]
		  ;   \+ instance(rtchecks_example3:atm(A)),
		      N=atm(A)-['A'=A]
		  ),
		  O),
	  (   O\=[],
	      P\=[]
	  ->  send_rtcheck(O,
			   (calls),
			   J,
			   ['A'=A, 'B'=C],
			   [Q]),
	      send_rtcheck(P,
			   (calls),
			   J,
			   ['A'=A, 'B'=C],
			   [L])
	  ;   true
	  ),
	  checkif_comp(O,
		       info(J, ['A'=A, 'B'=C], [Q]),
		       not_fails(R),
		       R,
		       checkif_comp(P,
				    info(J,
					 ['A'=A, 'B'=C],
					 [L]),
				    is_det(S),
				    S,
				    rtchecks_example3:fullasr(A, C))),
	  (   H==[]
	  ->  findall(T,
		      ( \+ compat(rtchecks_example3:atm(A)),
			T=atm(A)-['A'=A]
		      ),
		      U),
	      send_rtcheck(U,
			   (success),
			   J,
			   ['A'=A, 'B'=C],
			   [K])
	  ;   true
	  ),
	  (   I==[]
	  ->  findall(V,
		      ( \+ compat(rtchecks_example3:int(A)),
			V=int(A)-['A'=A]
		      ),
		      W),
	      send_rtcheck(W,
			   (success),
			   J,
			   ['A'=A, 'B'=C],
			   [L])
	  ;   true
	  ),
	  (   O==[]
	  ->  findall(X,
		      ( \+ instance(rtchecks_example3:family(C)),
			X=family(B)-['B'=C]
		      ),
		      Y),
	      send_rtcheck(Y,
			   (success),
			   J,
			   ['A'=A, 'B'=C],
			   [Q])
	  ;   true
	  ),
	  (   P==[]
	  ->  findall(Z,
		      ( \+ instance(rtchecks_example3:family(C)),
			Z=family(B)-['B'=C]
		      ),
		      A1),
	      send_rtcheck(A1,
			   (success),
			   J,
			   ['A'=A, 'B'=C],
			   [L])
	  ;   true
	  )
	))).

:- end_tests(rtchecks).
