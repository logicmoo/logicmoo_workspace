:- module(ctchecks, []).

:- use_module(library(lists)).
:- use_module(library(maplist_dcg)).
:- use_module(library(prolog_codewalk),  []). % for message_location
:- use_module(library(compound_expand)).
:- use_module(library(compact_pi_list)).
:- use_module(library(implementation_module)).
:- use_module(library(assertions/assrt_lib)).
:- reexport(library(swi/rtchecks)).
:- use_module(library(rtchecks/rtchecks_gen)).

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

prolog:message(acheck(prop_issue(Heads, IssuePIsL))) -->
    {sort(Heads, Sorted), compact_pi_list(Sorted, Compacted)},
    ['In assertions for ~w'-[Compacted], nl],
    maplist_dcg(issue_message, IssuePIsL).

ctcheck_head(((M:T --> B)), M, P) :- !, ctcheck_head(((T --> B)), M, P).
ctcheck_head(((H0  --> _)), M, P) :- !,
    H0 =.. L0,
    append(L0, [_, _], L),
    H =.. L,
    ctcheck_head(H, M, P).
ctcheck_head(((T :- _)), M, P) :- !, ctcheck_head(T, M, P).
ctcheck_head(M:T,      _, P) :- !, ctcheck_head(T, M, P).
ctcheck_head(H,        M, P) :-
    ctcheck_goal(H, M, P).

property_issue(IssuePIsL-Heads) :-
    print_message(error, acheck(prop_issue(Heads, IssuePIsL))).

ctcheck_goal(Goal, M, Pos) :-
    functor(Goal, F, A),
    check_property(ctcheck, Goal, M, Pos, CTChecks),
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
resolve_head((A;B), M, H) :- !,
    ( resolve_head(A, M, H)
    ; resolve_head(B, M, H)
    ).
resolve_head(H, M, M:H).

current_property(Head, M, Type, Cp, Ca, Su, Gl, Pos, Issues, PI-(Issue-Values)) :-
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
    implementation_module(N:H, IM),
    check_property(Issue, H, IM, Pos, Values).

check_properties(Head, M, Type, Cp, Ca, Su, Gl, Pos, Issues) :-
    findall(Pair,
	    current_property(Head, M, Type, Cp, Ca, Su, Gl, Pos, Issues, Pair),
	    Pairs),
    report_issues(Pairs).

group_pairs_2(K-L, G-K) :- group_pairs_by_key(L, G).

report_issues(Pairs) :-
    trans_group(Pairs, TGrouped),
    maplist(property_issue, TGrouped).

trans_group(Pairs, TGrouped) :-
    sort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    maplist(group_pairs_2, Grouped, Trans),
    keysort(Trans, TSorted),
    group_pairs_by_key(TSorted, TGrouped).

is_location(Loc) :- clause(prolog:message_location(Loc, _, _), _).

curr_location(Pos, Loc) :-
    ( var(Pos)
    ->rtchecks_tr:location(Loc)
    ; is_location(Pos)
    ->Loc = Pos
    ; rtchecks_tr:location(loc(Src, _, _)),
      ( var(Src)
      ->source_location(Src, _)
      ; true
      ),
      Loc = file_term_position(Src, Pos)
    ).

check_property(defined, H, M, _, M:F/A) :-
    functor(H, F, A),
    \+ current_predicate(M:F/A).
check_property(is_prop, H, M, _, M:F/A) :-
    functor(H, F, A),
    \+ verif_is_property(M, F, A).
check_property(ctcheck, H, M, Pos, CTChecks) :-
				% compile-time checks. Currently only
				% compatibility checks.
    curr_location(Pos, Loc),
    generate_ctchecks(H, M, Loc, Goal),
    save_rtchecks(M:Goal),	% Now execute the checks
    load_rtchecks(CTChecks),	% and collect the failures
    CTChecks \= [].

verif_is_property(_, call, N) :- N > 0, !. % meta checks not supported yet --EMM
verif_is_property(system, true, 0) :- !.   % ignore true (identity)
verif_is_property(IM, F, A) :-
    functor(H, F, A),
    assertion_db(H, AM, _, prop, _, _, _, _, _, _, _),
    ( AM = IM -> true
    ; predicate_property(AM:H, imported_from(IM))
    ).

term_expansion((assrt_lib:assertion_head(Head, M, _Status, Type, _, _, Pos)
	      :- Body), _, _, _) :-
    !,
    current_prolog_flag(check_assertions, Issues),
    Issues \== [],
    a_fake_body(Cp, Ca, Su, Gl, Body),
    check_properties(Head, M, Type, Cp, Ca, Su, Gl, Pos, Issues),
    fail.

term_expansion(Term, Pos, _, _) :-
    nonvar(Term),
    current_prolog_flag(check_assertions, Issues),
    memberchk(ctcheck, Issues),
    '$set_source_module'(M, M),
    ctcheck_head(Term, M, Pos),
    fail.

ct_black_list(basic_props).

goal_expansion(Goal, Pos, _, _) :-
    current_prolog_flag(check_assertions, Issues),
    memberchk(ctcheck, Issues),
    '$set_source_module'(M, M),
    \+ ct_black_list(M),
    implementation_module(M:Goal, IM),
    ctcheck_goal(Goal, IM, Pos),
    fail.
