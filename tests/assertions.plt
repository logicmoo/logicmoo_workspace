:- begin_tests(assertions).

:- use_module(library(swi/assertions), except([(test)/1])).
% :- use_module(library(assertions/assrt_lib)).

% Test the assertion reader:

test(assrt_lib_1) :-
    assrt_lib:assertion_records((pred p(A, B)
				::int(A):(gnd(A),var(B))
				=>(gnd(A), gnd(B))
				+ (not_fails,is_det)),R,m,[]),
    assertion(R=[assrt_lib:assertion_db(p(A, B),
					m, check, pred,
					[int(A)],
					[gnd(A), var(B)],
					[gnd(A), gnd(B)],
					[not_fails, is_det], "", [])]).

test(assrt_lib_2) :-
    assrt_lib:assertion_records((pred system:p(A, B)
				::int(A):(gnd(A), var(B))
				=>(gnd(A), gnd(B))
				+ not_fails),R,m,[]),
    assertion(R=[assrt_lib:assertion_db(p(A, B),
					system, check, pred,
					[m:int(A)],
					[m:gnd(A), m:var(B)],
					[m:gnd(A), m:gnd(B)],
					[m:not_fails], "", [])]).

% for a normal expression without syntax sugar:
test(assrt_lib_simple) :-
    assrt_lib:assrt_lib_tr((:- pred atomic_list_concat(A, B)
			   :: (list(A, ground), atom(B))
			   :  (list(A, atom),   term(B))
			   => (list(A, atom),   atom(B))
			   +  (is_det, iso) # "Write live comments here"),
			   Record, a, Dict),
    assertion(Record=[assrt_lib:assertion_db(atomic_list_concat(A, B),
					     a,
					     check, pred,
					     [list(A, ground), atom(B)],
					     [list(A, atom),   term(B)],
					     [list(A, atom),   atom(B)],
					     [is_det, iso],
					     "Write live comments here", Dict)]).

test(assrt_lib_comp) :-
    assrt_lib:assertion_records(true comp nfi(G,V) + (sideff(free), no_rtcheck),R,m,[]),
    assertion(R=[assrt_lib:assertion_db(nfi(G, V), m, true, comp, [], [], [],
					[sideff(free), no_rtcheck], "", [])]).

% for a complex expression with syntax sugar:
test(assrt_lib_sugar) :-
    assrt_lib:assrt_lib_tr((:- pred atomic_list_concat/2
			   :: list(ground) * atom
			   :  list(atom)   * term
			   => list(atom)   * atom
			   +  (is_det, iso) # "Write live comments here"),
			   Record, a, Dict),
    assertion(Record=[assrt_lib:assertion_db(atomic_list_concat(A, B),
					     a, check, pred,
					     [list(A, ground), atom(B)],
					     [list(A, atom),   term(B)],
					     [list(A, atom),   atom(B)],
					     [is_det, iso],
					     "Write live comments here", Dict)]).

% a complex expression that compact multiple assertions:
test(assrt_lib_multi) :-
    assrt_lib:assrt_lib_tr((:- pred [(q:a/1+kbmask([+])), b/2+hidden]+kbrule), Records, m, []),
    assertion(Records=[assrt_lib:assertion_db(a(A),    q, check, pred, [], [],
					      [], [m:kbrule, kbmask([+])], "", []),
		       assrt_lib:assertion_db(b(A, _B), m, check, pred, [], [],
					      [], [kbrule, hidden], "", [])]).

test(assrt_lib_mode1) :-
    assrt_lib:assertion_records(pred dict(+int),Re, u,[]),
    assertion(Re=[assrt_lib:assertion_db(dict(A), u, check, pred, [],
					 [int(A)], [], [], "", [])]).

test(assrt_lib_is_plus) :-
    As1 = (prop (hidden)/1 : callable  + no_rtcheck # "Specifies a hidden rule."),
    As2 = (prop (hidden)/1 : callable is no_rtcheck # "Specifies a hidden rule."),
    assrt_lib:assertion_records(As1, R1, user, []),
    assrt_lib:assertion_records(As2, R2, user, []),
    assertion(R1=R2).

test(assrt_lib_oddity_1) :-
    assrt_lib:assertion_records((pred a:b/2 : e * l + n),R,m,[]),
    assertion(R=[assrt_lib:assertion_db(b(A, B), a, check, pred,
					[], [e(A), l(B)], [], [n], "", [])]).

test(assrt_lib_oddity_2) :-
    assrt_lib:assertion_records((pred (a:b/2) : e * l + n),R,m,[]),
    assertion(R=[assrt_lib:assertion_db(b(A, B), a, check, pred,
					[], [m:e(A), m:l(B)], [], [m:n], "", [])]).

test(assrt_lib_abridged_notation) :-
    assrt_lib:assertion_records(pred check_to_messages(+Time      :ctime_t,
						       +RTCheck   :rtcheck_error,
						       ?Messages0 :list(message_info),
						       ?Messages  :list(message_info))#"c",
				R,rt,[]),
    assertion(R=[assrt_lib:assertion_db(check_to_messages(Time,
							  RTCheck,
							  Messages0,
							  Messages),
					rt, check, pred,
					[list(Messages0, message_info),
					 list(Messages, message_info)],
					[ctime_t(Time),
					 rtcheck_error(RTCheck)],
					[], [], "c", [])]).

:- end_tests(assertions).
