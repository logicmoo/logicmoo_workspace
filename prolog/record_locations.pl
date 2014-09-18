:- module(record_locations,
	  [extra_location/4]).

:- dynamic
    extra_location/4.

:- multifile
    system:term_expansion/4,
    system:goal_expansion/4,
    extra_location/4.

:- discontiguous
    extra_location/4.

:- volatile rl_tmp/2.
:- dynamic rl_tmp/2. % trick to detect if term_expansion was applied

% Extra location for assertions of a given predicate
extra_location(Head, M, assertion(Status, Type), From) :-
    clause(assrt_lib:assertion_head(Head, M, Status, Type, _, _, From), _).

record_extra_location((:- module(M, L)))      :- assert_declaration(export, M, L).
record_extra_location((:- volatile(L)))       :- assert_declaration(volatile, L).
record_extra_location((:- dynamic(L)))        :- assert_declaration(dynamic, L).
record_extra_location((:- reexport(U, L)))    :- assert_declaration(reexport(U), L).
record_extra_location((:- export(L)))         :- assert_declaration(export, L).
record_extra_location((:- public(L)))         :- assert_declaration(public, L).
record_extra_location((:- multifile(L)))      :- assert_declaration(multifile, L).
record_extra_location((:- discontiguous(L)))  :- assert_declaration(discontiguous, L).
record_extra_location((:- meta_predicate(L))) :- assert_declaration(meta_predicate, L).
record_extra_location((:- include(U)))        :- assert_declaration((-), include(U)).
record_extra_location((:- use_module(A)))     :- assert_declaration((-), use_module(A)).
record_extra_location((:- use_module(A, L)))  :- assert_declaration((-), use_module(A, L)).
record_extra_location((:- initialization(_))).
record_extra_location((:- G)) :-
    nonvar(G),
    '$set_source_module'(M, M),
    functor(G, F, A),
    current_predicate(M:F/A),
    retractall(rl_tmp(_, _)).

:- meta_predicate mapsequence(+, 1).
mapsequence(A, _) :-
    var(A),
    !.
    % call(G, A).
mapsequence([],    _) :- !.
mapsequence([A|B], G) :- !,
    mapsequence(A, G),
    mapsequence(B, G).
mapsequence((A, B), G) :- !,
    mapsequence(A, G),
    mapsequence(B, G).
mapsequence(A, G) :-
    call(G, A).

assert_declaration(Declaration, Sequence) :-
    '$set_source_module'(M, M),
    assert_declaration(Declaration, M, Sequence).

assert_declaration(Declaration, M, Sequence) :-
    source_location(File, Line),
    L = file(File, Line, -1, 0),
    mapsequence(Sequence, assert_declaration_one(Declaration, L, M)).

assert_location(G, T) :-
    source_location(File, Line),
    '$set_source_module'(M, M),
    L = file(File, Line, -1, 0),
    assert_location(G, M, T, L).

assert_declaration_one(reexport(U), L, M, PI) :- !,
    assert_reexport_declaration_2(PI, U, L, M).
assert_declaration_one(Declaration, L, _, M:PI) :- !,
    assert_declaration_one(Declaration, L, M, PI).
assert_declaration_one(Declaration, L, M, F/A) :- !,
    functor(H, F, A),
    assert_location(H, M, Declaration, L).
assert_declaration_one(Declaration, L, M, H) :-
    assert_location(H, M, Declaration, L).

assert_reexport_declaration_2((F/A as G), U, L, M) :-
    functor(H, G, A),
    assert_location(H, M, reexport(U, [F/A as G]), L).
assert_reexport_declaration_2(F/A, U, L, M) :-
    functor(H, F, A),
    assert_location(H, M, reexport(U, [F/A]), L).
assert_reexport_declaration_2(op(_, _, _), _, _, _).
assert_reexport_declaration_2(except(_),   _, _, _).

assert_location(H, M, Type, Loc) :-
    compile_aux_clauses(record_locations:extra_location(H, M, Type, Loc)).

system:term_expansion(Term, _, _, _) :-
    source_location(File, Line),
    retractall(rl_tmp(_, _)),
    asserta(rl_tmp(File, Line)),
    once(record_extra_location(Term)),
    fail.

redundant((_,_)).
redundant((_;_)).
redundant((_:_)).

rl_goal_expansion(Goal) :-
    callable(Goal),
    \+ redundant(Goal),
    source_location(File, Line),
    \+ rl_tmp(File, Line),
    assert_location(Goal, goal),
    !.

system:goal_expansion(Goal, _, _, _) :-
    rl_goal_expansion(Goal),
    fail.
