:- begin_tests(assertions).

:- use_module(library(substitute)).
:- use_module(library(assertions)).

% Test the assertion expander:

test(assertions_1) :-
    assertions:expand_assertion(m, [], (pred p(A, B)
                                       ::int(A):(gnd(A),var(B))
                                                =>(gnd(A), gnd(B))
                                                + (not_fails,is_det)), _, R, _),
    assertion(R=[assertions:asr_head_prop(Idx, m, p(A, B), check, pred, []),
                 assertions:asr_comp(Idx, m, int(A)),
                 assertions:asr_call(Idx, m, gnd(A)),
                 assertions:asr_call(Idx, m, var(B)),
                 assertions:asr_succ(Idx, m, gnd(A)),
                 assertions:asr_succ(Idx, m, gnd(B)),
                 assertions:asr_glob(Idx, m, not_fails(_)),
                 assertions:asr_glob(Idx, m, is_det(_))
                ]).

test(assertions_2) :-
    assertions:expand_assertion(
                  m, [],
                  (pred [system:p(A, B)]
                               ::int(A):(gnd(A), var(B))
                                        =>(gnd(A), gnd(B))
                                        + not_fails), _, R, _),
    assertion(R=[assertions:asr_head_prop(Idx, system, p(A, B), check, pred,[]),
                 assertions:asr_comp(Idx, m, int(A)),
                 assertions:asr_call(Idx, m, gnd(A)),
                 assertions:asr_call(Idx, m, var(B)),
                 assertions:asr_succ(Idx, m, gnd(A)),
                 assertions:asr_succ(Idx, m, gnd(B)),
                 assertions:asr_glob(Idx, m, not_fails(_))
                ]).

% for a normal expression without syntax sugar:
test(assertions_simple) :-
    assertions:expand_assertion(
                  a, Dict,
                  (pred atomic_list_concat(A, B)
                   :: (list(A, ground), atom(B))
                   :  (list(A, atom),   term(B))
                      => (list(A, atom),   atom(B))
                      +  (is_det, iso) # "Write live comments here"),
                  _, Records, _),
    assertion(Records=[assertions:asr_head_prop(Idx, a, atomic_list_concat(A, B), check, pred, Dict),
                       assertions:asr_comm(Idx, "Write live comments here"),
                       assertions:asr_comp(Idx, a, list(A, ground)),
                       assertions:asr_comp(Idx, a, atom(B)),
                       assertions:asr_call(Idx, a, list(A, atom)),
                       assertions:asr_call(Idx, a, term(B)),
                       assertions:asr_succ(Idx, a, list(A, atom)),
                       assertions:asr_succ(Idx, a, atom(B)),
                       assertions:asr_glob(Idx, a, is_det(_)),
                       assertions:asr_glob(Idx, a, iso(_))
                      ]).

test(assertions_comp) :-
    assertions:expand_assertion(
                  m, [], true comp nfi1(G,V) + (sideff(free), no_acheck), _, R, _),
    assertion(R=[assertions:asr_head_prop(Idx, m, nfi1(G, V), true, comp, []),
                 assertions:asr_glob(Idx, m, sideff(free, _)),
                 assertions:asr_glob(Idx, m, no_acheck(_))
                ]).

% for a complex expression with syntax sugar:
test(assertions_sugar) :-
    assertions:expand_assertion(
                  a, Dict,
                  (pred atomic_list_concat/2:: list(ground) * atom
                   :  list(atom)   * term
                                     => list(atom)   * atom
                      +  (is_det, iso) # "Write live comments here"), _,
                  Records, _),
    assertion(Records=[assertions:asr_head_prop(Idx, a, atomic_list_concat(A, B), check, pred, Dict),
                       assertions:asr_comm(Idx, "Write live comments here"),
                       assertions:asr_comp(Idx, a, list(ground, A)),
                       assertions:asr_comp(Idx, a, atom(B)),
                       assertions:asr_call(Idx, a, list(atom, A)),
                       assertions:asr_call(Idx, a, term(B)),
                       assertions:asr_succ(Idx, a, list(atom, A)),
                       assertions:asr_succ(Idx, a, atom(B)),
                       assertions:asr_glob(Idx, a, is_det(_)),
                       assertions:asr_glob(Idx, a, iso(_))
                      ]).

% a complex expression that compact multiple assertions:
test(assertions_multi) :-
    assertions:expand_assertion(m, [], (pred [(q:a/1+kbmask([+])), b/2+hidden]+kbrule),
                               _, Records, _),
    assertion(Records=[assertions:asr_head_prop(Idx1, q, a(_), check, pred, []),
                       assertions:asr_glob(Idx1, m, kbrule(_)),
                       assertions:asr_glob(Idx1, q, kbmask([(+)], _)),
                       assertions:asr_head_prop(Idx2, m, b(_, _), check, pred, []),
                       assertions:asr_glob(Idx2, m, kbrule(_)),
                       assertions:asr_glob(Idx2, m, hidden(_))
                      ]).

test(assertions_mode1) :-
    assertions:expand_assertion(u, [], pred dict(+int), _, Re, _),
    assertion(Re=[assertions:asr_head_prop(Idx, u, dict(A), check, pred, []),
                  assertions:asr_call(Idx, u, int(A))]).

test(assertions_is_plus) :-
    As1 = (prop (hidden)/1 : callable  + no_acheck(rt) # "Specifies a hidden rule."),
    As2 = (prop (hidden)/1 : callable is no_acheck(rt) # "Specifies a hidden rule."),
    assertions:expand_assertion(user, [], As1, _, R1, _),
    assertions:expand_assertion(user, [], As2, _, R2, _),
    R1 = [_, _, _:asr_call(Idx1, _, _)|_],
    R2 = [_, _, _:asr_call(Idx2, _, _)|_],
    assertion(substitute(substitute_idx(Idx1, Idx2), R1, R2)).

substitute_idx(Idx1, Idx2, Term, Idx2) :-
    nonvar(Term),
    Term = Idx1.

test(assertions_oddity_1) :-
    assertions:expand_assertion(m, [], (pred a:b/2 : e * l + n), _, R, _),
    assertion(R=[assertions:asr_head_prop(Idx, a, b(A, B), check, pred, []),
                 assertions:asr_call(Idx, a, e(A)),
                 assertions:asr_call(Idx, a, l(B)),
                 assertions:asr_glob(Idx, a, n(_))
                ]).

test(assertions_oddity_2) :-
    assertions:expand_assertion(m, [], (pred (a:b/2) : e * l + n), _, R, _),
    assertion(R=[assertions:asr_head_prop(Idx, a, b(A, B), check, pred, []),
                 assertions:asr_call(Idx, m, e(A)),
                 assertions:asr_call(Idx, m, l(B)),
                 assertions:asr_glob(Idx, m, n(_))
                ]).

test(assertions_abridged_notation) :-
    assertions:expand_assertion(rt, [], pred check_to_messages(+Time      :ctime_t,
                                                              +RTCheck   :rtcheck_error,
                                                              ?Messages1 :list(message_info),
                                                              ?Messages  :list(message_info))#"c", _, R, _),
    assertion(R=[assertions:asr_head_prop(Idx,
                                         rt, check_to_messages(Time,
                                                               RTCheck,
                                                               Messages1,
                                                               Messages), check, pred, []),
                 assertions:asr_comm(Idx, "c"),
                 assertions:asr_comp(Idx, rt, list(message_info, Messages1)),
                 assertions:asr_comp(Idx, rt, list(message_info, Messages)),
                 assertions:asr_call(Idx, rt, ctime_t(Time)),
                 assertions:asr_call(Idx, rt, rtcheck_error(RTCheck))]).

:- end_tests(assertions).
