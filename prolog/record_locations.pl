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

record_location_term((:- module(_, L))) :-
    assert_declaration(export, L).
record_location_term((:- dynamic(L))) :-
    assert_declaration(dynamic, L).
record_location_term((:- reexport(U, L))) :-
    assert_declaration(reexport(U), L).
record_location_term((:- export(L))) :-
    assert_declaration(export, L).
record_location_term((:- multifile(L))) :-
    assert_declaration(multifile, L).
record_location_term((:- discontiguous(L))) :-
    assert_declaration(discontiguous, L).
record_location_term((:- include(U))) :-
    assert_include_declaration(U).
record_location_term((:- meta_predicate(L))) :-
    assert_declaration(meta_predicate, L).

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

assert_include_declaration(U) :-
    source_location(File, Line),
    '$set_source_module'(M, M),
    L = file(File, Line, -1, 0),
    assert_location((-), M, include(U), L).

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
    record_location_term(Term),
    fail.
