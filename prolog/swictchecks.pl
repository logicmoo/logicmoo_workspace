:- module(swictchecks, []).

:- use_module(library(prolog_codewalk),  []). % for message_location
:- use_module(library(compound_expand)).
:- use_module(library(compact_pi_list)).
:- use_module(library(swirtchecks)).
:- reexport(library(swirtchecks)).

:- multifile prolog:message/3.

issue_format(defined, '\tUsing undefined: ~w').
issue_format(is_prop, '\tNot properties : ~w').

issue_message(ctcheck-RTChecksL) --> !,
    {append(RTChecksL, RTChecks)},
    prolog:message(acheck(checks(ctcheck), RTChecks)).
issue_message(Issue-Props) -->
    {compact_pi_list(Props, Compacted),
     issue_format(Issue, Format)},
    [Format -[Compacted], nl].

issue_messages([]) --> [].
issue_messages([IssuePIs|IssuePIsL]) -->
    issue_message(IssuePIs),
    issue_messages(IssuePIsL).

prolog:message(acheck(prop_issue(Heads, IssuePIsL))) -->
    {sort(Heads, Sorted), compact_pi_list(Sorted, Compacted)},
    ['In assertions for ~w'-[Compacted], nl],
    issue_messages(IssuePIsL).

ctcheck_head((M:T --> B), M) :- !, ctcheck_head((T --> B), M).
ctcheck_head((H0  --> _), M) :- !,
    H0 =.. L0,
    append(L0, [_, _], L),
    H =.. L,
    ctcheck_head(H, M).
ctcheck_head((T :- _), M) :- !, ctcheck_head(T, M).
ctcheck_head(M:T,      _) :- !, ctcheck_head(T, M).
ctcheck_head(H,        M) :-
    ctcheck_goal(H, M, M).

property_issue(IssuePIsL-Heads) :-
    print_message(error, acheck(prop_issue(Heads, IssuePIsL))).

ctcheck_goal(Goal, M, IM) :-
    functor(Goal, F, A),
    check_property(ctcheck, M:Goal, IM, CTChecks),
    property_issue([ctcheck-[CTChecks]]-[M:F/A]).

:- create_prolog_flag(check_assertions, [], [type(term)]).

checker_t(defined).
checker_t(is_prop).
checker_t(ctcheck).

resolve_head(M:H0, _, H) :- !,
    resolve_head(H0, M, H).
resolve_head((A,B), M, H) :- !,
    ( resolve_head(A, M, H)
    ; resolve_head(B, M, H)
    ).
resolve_head((A;B), _, H) :- !,
    ( resolve_head(A, M, H)
    ; resolve_head(B, M, H)
    ).
resolve_head(H, M, M:H).

current_property(Head, M, Type, Cp, Ca, Su, Gl, Issues, PI-(Issue-Values)) :-
    Type \= (test),
    functor(Head, HF,HA),
    PI=M:HF/HA,
    ( ( member(Prop, Cp)
      ; member(Prop, Ca)
      ; member(Prop, Su)
      ),
      resolve_head(Prop, M, N:H)
    ; member(Glob, Gl),
      resolve_head(Glob, M, N:H0),
      H0 =.. [F|Args],
      H =.. [F, Head|Args]
    ),
    member(Issue, Issues),
    checker_t(Issue),
    (predicate_property(N:H, imported_from(IM)) -> true ; IM = N),
    check_property(Issue, N:H, IM, Values).

% :- meta_predicate check_properties(?, ?).

check_properties(Head, M, Type, Cp, Ca, Su, Gl, Issues) :-
    findall(Pair, current_property(Head, M, Type, Cp, Ca, Su, Gl, Issues, Pair),
	    Pairs),
    report_issues(Pairs).

report_issues(Pairs) :-
    sort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    maplist(\ (K-L)^ (G-K)^ group_pairs_by_key(L, G), Grouped, Trans),
    keysort(Trans, TSorted),
    group_pairs_by_key(TSorted, TGrouped),
    maplist(property_issue, TGrouped).

check_property(defined, M:H, _, M:F/A) :-
    functor(H, F, A),
    \+ current_predicate(M:F/A).
check_property(is_prop, M:H, IM, M:F/A) :-
    functor(H, F, A),
    \+ verif_is_property(IM, F, A).
check_property(ctcheck, M:H, IM, CTChecks) :-
				% compile-time checks. Currently only
				% compatibility checks.
    % rtchecks_tr:location(Loc),
    rtchecks_tr:generate_ctchecks(H, IM, _Loc, Goal),
    save_rtchecks(M:Goal),	% Now execute the checks
    load_rtchecks(CTChecks),	% and collect the failures
    CTChecks \= [].

verif_is_property(_, call, N) :- N > 0, !. % meta checks not supported yet --EMM
verif_is_property(IM, F, A) :-
    functor(H, F, A),
    assertions:assertion_db(H, AM, _, prop, _, _, _, _, _, _),
    ( AM = IM -> true
    ; predicate_property(AM:H, imported_from(IM))
    ).

term_expansion(assertions:assertion_db(Head, M, _Status, Type, Cp, Ca,
				       Su, Gl, _Co, _), _) :-
    !,
    current_prolog_flag(check_assertions, Issues),
    Issues \== [],
    check_properties(Head, M, Type, Cp, Ca, Su, Gl, Issues),
    fail.

term_expansion(Term, _) :-
    nonvar(Term),
    current_prolog_flag(check_assertions, Issues),
    memberchk(ctcheck, Issues),
    '$set_source_module'(M, M),
    ctcheck_head(Term, M),
    fail.

goal_expansion(Goal, _) :-
    current_prolog_flag(check_assertions, Issues),
    memberchk(ctcheck, Issues),
    '$set_source_module'(M, M),
    (predicate_property(M:Goal, imported_from(IM)) -> true ; IM = M),
    ctcheck_goal(Goal, M, IM),
    fail.
