:- module(record_locations,
	  [declaration_location/4]).

:- dynamic
    declaration_location/4.

:- multifile
    user:term_expansion/2,
    user:goal_expansion/2,
    declaration_location/4.

:- discontiguous
    declaration_location/4.

:- volatile rl_tmp/2.
:- dynamic rl_tmp/2. % trick to detect if term_expansion was applied

record_location_term((:- module(_, L)))      :- assert_declaration(export, L).
record_location_term((:- volatile(L)))       :- assert_declaration(volatile, L).
record_location_term((:- dynamic(L)))        :- assert_declaration(dynamic, L).
record_location_term((:- reexport(U, L)))    :- assert_declaration(reexport(U), L).
record_location_term((:- export(L)))         :- assert_declaration(export, L).
record_location_term((:- public(L)))         :- assert_declaration(public, L).
record_location_term((:- multifile(L)))      :- assert_declaration(multifile, L).
record_location_term((:- discontiguous(L)))  :- assert_declaration(discontiguous, L).
record_location_term((:- meta_predicate(L))) :- assert_declaration(meta_predicate, L).
record_location_term((:- include(U)))        :- assert_declaration(include(U)).
record_location_term((:- use_module(A)))     :- assert_declaration(use_module(A)).
record_location_term((:- use_module(A, L)))  :- assert_declaration(use_module(A, L)).
record_location_term((:- initialization(_))).
record_location_term((:- G)) :-
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
    source_location(File, Line),
    '$set_source_module'(M, M),
    L = file(File, Line, -1, 0),
    mapsequence(Sequence, assert_declaration_one(Declaration, L, M)).

assert_declaration(T) :- assert_location((-), T).

assert_location(G, T) :-
    source_location(File, Line),
    '$set_source_module'(M, M),
    L = file(File, Line, -1, 0),
    assert_location(G, M, T, L).

assert_declaration_one(reexport(U), L, M, PI) :-
    !,
    assert_reexport_declaration_2(PI, U, L, M).
assert_declaration_one(Declaration, L, _, M:PI) :-
    !,
    assert_declaration_one(Declaration, L, M, PI).
assert_declaration_one(Declaration, L, M, F/A) :-
    !,
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

assert_location(H, M, Declaration, Loc) :-
    compile_aux_clauses(record_locations:declaration_location(H, M, Declaration, Loc)).

user:term_expansion(Term, _) :-
    source_location(File, Line),
    retractall(rl_tmp(_, _)),
    asserta(rl_tmp(File, Line)),
    once(record_location_term(Term)),
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

user:goal_expansion(Goal, _) :-
    rl_goal_expansion(Goal),
    fail.
