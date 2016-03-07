:- begin_tests(assertions).

:- use_module(library(substitute)).
:- use_module(library(assertions), except([(test)/1])).
				% :- use_module(library(assertions/assrt_lib)).

				% Test the assertion reader:

test(assrt_lib_1) :-
    assrt_lib:assertion_records(m, [], (pred p(A, B)
				::int(A):(gnd(A),var(B))
				=>(gnd(A), gnd(B))
				+ (not_fails,is_det)), _, R, _),
    assertion(R=[assrt_lib:head_prop_asr(p(A, B), m, check, pred, "", [], _, Idx),
		 assrt_lib:asr_comp(Idx, m, int(A)),
		 assrt_lib:asr_call(Idx, m, gnd(A)),
		 assrt_lib:asr_call(Idx, m, var(B)),
		 assrt_lib:asr_succ(Idx, m, gnd(A)),
		 assrt_lib:asr_succ(Idx, m, gnd(B)),
		 assrt_lib:asr_glob(Idx, m, not_fails),
		 assrt_lib:asr_glob(Idx, m, is_det)
		]).

test(assrt_lib_2) :-
    assrt_lib:assertion_records(m, [], (pred system:p(A, B)
				::int(A):(gnd(A), var(B))
				=>(gnd(A), gnd(B))
				+ not_fails), _, R, _),
    assertion(R=[assrt_lib:head_prop_asr(p(A, B), system, check, pred, "", [],_, Idx),
		 assrt_lib:asr_comp(Idx, m, int(A)),
		 assrt_lib:asr_call(Idx, m, gnd(A)),
		 assrt_lib:asr_call(Idx, m, var(B)),
		 assrt_lib:asr_succ(Idx, m, gnd(A)),
		 assrt_lib:asr_succ(Idx, m, gnd(B)),
		 assrt_lib:asr_glob(Idx, m, not_fails)
		]).

% for a normal expression without syntax sugar:
test(assrt_lib_simple) :-
    assrt_lib:assrt_lib_tr((:- pred atomic_list_concat(A, B)
			   :: (list(A, ground), atom(B))
			   :  (list(A, atom),   term(B))
			   => (list(A, atom),   atom(B))
			   +  (is_det, iso) # "Write live comments here"),
			   Record, a, Dict),
    assertion(Record=[assrt_lib:head_prop_asr(atomic_list_concat(A, B), a, check, pred, "Write live comments here", Dict, _, Idx),
		      assrt_lib:asr_comp(Idx, a, list(A, ground)),
		      assrt_lib:asr_comp(Idx, a, atom(B)),
		      assrt_lib:asr_call(Idx, a, list(A, atom)),
		      assrt_lib:asr_call(Idx, a, term(B)),
		      assrt_lib:asr_succ(Idx, a, list(A, atom)),
		      assrt_lib:asr_succ(Idx, a, atom(B)),
		      assrt_lib:asr_glob(Idx, a,is_det),
		      assrt_lib:asr_glob(Idx, a, iso)
		     ]).

test(assrt_lib_comp) :-
    assrt_lib:assertion_records(m, [], true comp nfi1(G,V) + (sideff(free), no_rtcheck), _, R, _),
    assertion(R=[assrt_lib:head_prop_asr(nfi1(G, V), m, true, comp, "", [], _, Idx),
		 assrt_lib:asr_glob(Idx, m, sideff(free)),
		 assrt_lib:asr_glob(Idx, m, no_rtcheck)
		]).

% for a complex expression with syntax sugar:
test(assrt_lib_sugar) :-
    assrt_lib:assrt_lib_tr((:- pred atomic_list_concat/2
			   :: list(ground) * atom
			   :  list(atom)   * term
			   => list(atom)   * atom
			   +  (is_det, iso) # "Write live comments here"),
			   Record, a, Dict),
    assertion(Record=[assrt_lib:head_prop_asr(atomic_list_concat(A, B), a, check, pred, "Write live comments here", Dict, _, Idx),
		      assrt_lib:asr_comp(Idx, a, list(A, ground)),
		      assrt_lib:asr_comp(Idx, a, atom(B)),
		      assrt_lib:asr_call(Idx, a, list(A, atom)),
		      assrt_lib:asr_call(Idx, a, term(B)),
		      assrt_lib:asr_succ(Idx, a, list(A, atom)),
		      assrt_lib:asr_succ(Idx, a, atom(B)),
		      assrt_lib:asr_glob(Idx, a, is_det),
		      assrt_lib:asr_glob(Idx, a, iso)
		     ]).

% a complex expression that compact multiple assertions:
test(assrt_lib_multi) :-
    assrt_lib:assrt_lib_tr((:- pred [(q:a/1+kbmask([+])), b/2+hidden]+kbrule), Records, m, []),
    assertion(Records=[assrt_lib:head_prop_asr(a(_), q, check, pred, "", [], _, Idx1),
		       assrt_lib:asr_glob(Idx1, m, kbrule),
		       assrt_lib:asr_glob(Idx1, q, kbmask([(+)])),
		       assrt_lib:head_prop_asr(b(_, _), m, check, pred, "", [], _, Idx2),
		       assrt_lib:asr_glob(Idx2, m, kbrule),
		       assrt_lib:asr_glob(Idx2, m, hidden)
		      ]).

test(assrt_lib_mode1) :-
    assrt_lib:assertion_records(u, [], pred dict(+int), _, Re, _),
    assertion(Re=[assrt_lib:head_prop_asr(dict(A), u, check, pred, "", [], _, Idx),
		  assrt_lib:asr_call(Idx, u, int(A))]).

test(assrt_lib_is_plus) :-
    As1 = (prop (hidden)/1 : callable  + no_rtcheck # "Specifies a hidden rule."),
    As2 = (prop (hidden)/1 : callable is no_rtcheck # "Specifies a hidden rule."),
    assrt_lib:assertion_records(user, [], As1, _, R1, _),
    assrt_lib:assertion_records(user, [], As2, _, R2, _),
    R1 = [_, _:asr_call(Idx1, _, _)|_],
    R2 = [_, _:asr_call(Idx2, _, _)|_],
    assertion(substitute(substitute_idx(Idx1, Idx2), R1, R2)).

substitute_idx(Idx1, Idx2, Term, Idx2) :-
    nonvar(Term),
    Term = Idx1.

test(assrt_lib_oddity_1) :-
    assrt_lib:assertion_records(m, [], (pred a:b/2 : e * l + n), _, R, _),
    assertion(R=[assrt_lib:head_prop_asr(b(A, B), a, check, pred, "", [], _, Idx),
		 assrt_lib:asr_call(Idx, a, e(A)),
		 assrt_lib:asr_call(Idx, a, l(B)),
		 assrt_lib:asr_glob(Idx, a, n)
		]).

test(assrt_lib_oddity_2) :-
    assrt_lib:assertion_records(m, [], (pred (a:b/2) : e * l + n), _, R, _),
    assertion(R=[assrt_lib:head_prop_asr(b(A, B), a, check, pred, "", [], _, Idx),
		 assrt_lib:asr_call(Idx, m, e(A)),
		 assrt_lib:asr_call(Idx, m, l(B)),
		 assrt_lib:asr_glob(Idx, m, n)
		]).

test(assrt_lib_abridged_notation) :-
    assrt_lib:assertion_records(rt, [], pred check_to_messages(+Time      :ctime_t,
							       +RTCheck   :rtcheck_error,
							       ?Messages0 :list(message_info),
							       ?Messages  :list(message_info))#"c", _, R, _),
    assertion(R=[assrt_lib:head_prop_asr(check_to_messages(Time,
							   RTCheck,
							   Messages0,
							   Messages),
					 rt, check, pred, "c", [], _, Idx),
		 assrt_lib:asr_comp(Idx, rt, list(Messages0, message_info)),
		 assrt_lib:asr_comp(Idx, rt, list(Messages,  message_info)),
		 assrt_lib:asr_call(Idx, rt, ctime_t(Time)),
		 assrt_lib:asr_call(Idx, rt, rtcheck_error(RTCheck))]).

:- end_tests(assertions).
