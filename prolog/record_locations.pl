:- module(record_locations, []).

:- dynamic
    declaration_location/3.

:- multifile
    user:term_expansion/2,
    user:goal_expansion/2,
    declaration_location/3.

cleanup_db(Module) :-
	retractall(declaration_location(Module:_, _, _)).

record_location_term((:- module(M, L))) :-
	cleanup_db(M),
	assert_declaration(export, L).
record_location_term((:- module(Module))) :-
	cleanup_db(Module).
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

/*
record_location_goal(Call) :-
	'$set_source_module'(SM, SM),
	implementation_module(SM:Call, IM),
	database_fact_ort(Def, IM:Call, MFact),
	nonvar(MFact),
	( MFact = M:Fact -> true
	;
	  implementation_module(SM:MFact, M),
	  Fact = MFact
	),
	nonvar(M),
	nonvar(Fact),
	functor(Fact, F, A),
	source_location(File, Line),
	record_location(M:F/A, dynamic(Def, Call), file(File, Line, -1, 0))).
*/

user:term_expansion(Term, _) :-
	record_location_term(Term),
	fail.
/*
user:goal_expansion(Call, _) :-
	record_location_goal(Call),
	fail.
*/
:- meta_predicate mapsequence(+, 1).
mapsequence(A, G) :-
	var(A),
	!,
	call(G, A).
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
	assert_location(M:(-), include(U), L).

assert_declaration_one(reexport(U), L, M, PI) :-
	!,
	assert_reexport_declaration_2(PI, U, L, M).
assert_declaration_one(Declaration, L, _, M:PI) :-
	!,
	assert_declaration_one(Declaration, L, M, PI).
assert_declaration_one(Declaration, L, M, PI) :-
	assert_location(M:PI, Declaration, L).

assert_reexport_declaration_2((F/A as G), U, L, M) :-
	assert_location(M:G/A, reexport(U, [F/A as G]), L).
assert_reexport_declaration_2(F/A, U, L, M) :-
	assert_location(M:F/A, reexport(U, [F/A]), L).
assert_reexport_declaration_2(op(_, _, _), _, _, _).
assert_reexport_declaration_2(except(_),   _, _, _).

assert_location(PI, Declaration, Loc) :-
    retractall(declaration_location(PI, Declaration, Loc)),
    assertz(declaration_location(PI, Declaration, Loc)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
