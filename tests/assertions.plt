:- begin_tests(assertions).

:- use_module(library(assertions), except([(test)/1])).
				% :- use_module(library(assertions/assrt_lib)).

				% Test the assertion reader:

test(assrt_lib_1) :-
    assrt_lib:assertion_records(m, [], (pred p(A, B)
				::int(A):(gnd(A),var(B))
				=>(gnd(A), gnd(B))
				+ (not_fails,is_det)), _, R, _),
    assertion(R=[(assrt_lib:assertion_head(p(A, B), m, check, pred, "", [], _):-
		 call(int(A)),
		  call((gnd(A), var(B))),
		  call((gnd(A), gnd(B))),
		  call((not_fails(_), is_det(_))))]).

test(assrt_lib_2) :-
    assrt_lib:assertion_records(m, [], (pred system:p(A, B)
				::int(A):(gnd(A), var(B))
				=>(gnd(A), gnd(B))
				+ not_fails), _, R, _),
    assertion(R=[(assrt_lib:assertion_head(p(A, B), system, check, pred, "", [], _):-
		 call(int(A)),
		  call((gnd(A), var(B))),
		  call((gnd(A), gnd(B))),
		  call(not_fails(_)))]).

% for a normal expression without syntax sugar:
test(assrt_lib_simple) :-
    assrt_lib:assrt_lib_tr((:- pred atomic_list_concat(A, B)
			   :: (list(A, ground), atom(B))
			   :  (list(A, atom),   term(B))
			   => (list(A, atom),   atom(B))
			   +  (is_det, iso) # "Write live comments here"),
			   Record, a, Dict),
    assertion(Record=[(assrt_lib:assertion_head(atomic_list_concat(A, B), system, check,
						pred, "Write live comments here", Dict, _):-
		      call((list(A, ground), atom(B))),
		       call((list(A, atom), term(B))),
		       call((list(A, atom), atom(B))),
		       call((is_det(_), iso(_))))]).

test(assrt_lib_comp) :-
    assrt_lib:assertion_records(m, [], true comp nfi(G,V) + (sideff(free), no_rtcheck), _, R, _),
    assertion(R=[(assrt_lib:assertion_head(nfi(G, V), m, true, comp, "", [], _) :-
		 call(true),
		  call(true),
		  call(true),
		  call((sideff(_, free), no_rtcheck(_))))]).

% for a complex expression with syntax sugar:
test(assrt_lib_sugar) :-
    assrt_lib:assrt_lib_tr((:- pred atomic_list_concat/2
			   :: list(ground) * atom
			   :  list(atom)   * term
			   => list(atom)   * atom
			   +  (is_det, iso) # "Write live comments here"),
			   Record, a, Dict),
    assertion(Record=[(assrt_lib:assertion_head(atomic_list_concat(A, B), system, check,
						pred, "Write live comments here", Dict, _):-
		      call((list(A, ground), atom(B))),
		       call((list(A, atom), term(B))),
		       call((list(A, atom), atom(B))),
		       call((is_det(_), iso(_))))]).

% a complex expression that compact multiple assertions:
test(assrt_lib_multi) :-
    assrt_lib:assrt_lib_tr((:- pred [(q:a/1+kbmask([+])), b/2+hidden]+kbrule), Records, m, []),
    assertion(Records=[(assrt_lib:assertion_head(a(A), q, check, pred, "", [], _) :-
		       call(true),
			call(true),
			call(true),
			call((kbrule(_), q:kbmask(_, [ (+)])))),
		       (assrt_lib:assertion_head(b(A, _B), m, check, pred, "", [], _) :-
		       call(true),
			call(true),
			call(true),
			call((kbrule(_), hidden(_))))]).

test(assrt_lib_mode1) :-
    assrt_lib:assertion_records(u, [], pred dict(+int), _, Re, _),
    assertion(Re=[(assrt_lib:assertion_head(dict(A), u, check, pred, "", [], _) :-
		  call(true),
		   call(int(A)),
		   call(true),
		   call(true))]).

test(assrt_lib_is_plus) :-
    As1 = (prop (hidden)/1 : callable  + no_rtcheck # "Specifies a hidden rule."),
    As2 = (prop (hidden)/1 : callable is no_rtcheck # "Specifies a hidden rule."),
    assrt_lib:assertion_records(user, [], As1, _, R1, _),
    assrt_lib:assertion_records(user, [], As2, _, R2, _),
    assertion(R1=R2).

test(assrt_lib_oddity_1) :-
    assrt_lib:assertion_records(m, [], (pred a:b/2 : e * l + n), _, R, _),
    assertion(R=[(assrt_lib:assertion_head(b(A, B), a, check, pred, "", [], _) :-
		 call(true),
		  call((a:e(A), a:l(B))),
		  call(true),
		  call(a:n(_)))]).

test(assrt_lib_oddity_2) :-
    assrt_lib:assertion_records(m, [], (pred (a:b/2) : e * l + n), _, R, _),
    assertion(R=[(assrt_lib:assertion_head(b(A, B), a, check, pred, "", [], _) :-
		 call(true),
		  call((e(A), l(B))),
		  call(true),
		  call(n(_)))]).

test(assrt_lib_abridged_notation) :-
    assrt_lib:assertion_records(rt, [], pred check_to_messages(+Time      :ctime_t,
							       +RTCheck   :rtcheck_error,
							       ?Messages0 :list(message_info),
							       ?Messages  :list(message_info))#"c", _, R, _),
    assertion(R=[(assrt_lib:assertion_head(check_to_messages(Time,
							     RTCheck,
							     Messages0,
							     Messages),
					   rt, check, pred, "c", [], _) :-
		 call((list(Messages0, message_info),
		       list(Messages,  message_info))),
		  call((ctime_t(Time),
			rtcheck_error(RTCheck))),
		  call(true),
		  call(true))]).

:- end_tests(assertions).
