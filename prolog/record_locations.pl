:- module(record_locations,
	  [extra_location/4]).

:- use_module(library(apply)).

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

:- public record_extra_location/2.

% record_extra_location((:- module(M, L)),
% 		      term_position(_, _, _, _,
% 				    [term_position(_, _, _, _,
% 						   [_, list_position(_,_,PosL,_)])]))
% :- !,
%     maplist(assert_declaration(export, M), L, PosL).
record_extra_location((:- Decl),
		      term_position(_, _, _, _,
				    [DPos])) :-
    record_extra_location_d(Decl, DPos).

record_extra_location_d(initialization(_)) :- !.
record_extra_location_d(Decl, DPos) :-
    '$set_source_module'(SM, SM),
    forall(declaration_pos(Decl, DPos, Id, SM, M, Arg, Pos),
	   assert_declaration(Id, M, Arg, Pos)),
    !.
record_extra_location(G) :-
    nonvar(G),
    '$set_source_module'(M, M),
    functor(G, F, A),
    current_predicate(M:F/A),
    retractall(rl_tmp(_, _)).

declaration_pos(module(M, L), DPos, module_2, _, M, module(M, L), DPos).
declaration_pos(module(M, L),
		term_position(_, _, _, _, [_, Pos]),
		export, _, M, L, Pos).
declaration_pos(volatile(L),
		term_position(_, _, _, _, [Pos]),
		volatile, M, M, L, Pos).
declaration_pos(dynamic(L),
		term_position(_, _, _, _, [Pos]),
		dynamic, M, M, L, Pos).
declaration_pos(reexport(U, L),
		term_position(_, _, _, _, [_, Pos]),
		reexport(U), M, M, L, Pos).
declaration_pos(public(L),
		term_position(_, _, _, _, [Pos]),
		pulic, M, M, L, Pos).
declaration_pos(multifile(L),
		term_position(_, _, _, _, [Pos]),
		multifile, M, M, L, Pos).
declaration_pos(discontiguous(L),
		term_position(_, _, _, _, [Pos]),
		discontiguous, M, M, L, Pos).
declaration_pos(meta_predicate(L),
		term_position(_, _, _, _, [Pos]),
		meta_predicate, M, M, L, Pos).
declaration_pos(include(U),       DPos, include,      M, M, U, DPos).
declaration_pos(use_module(U),    DPos, use_module,   M, M, U, DPos).
declaration_pos(consult(U),       DPos, consult,      M, M, U, DPos).
declaration_pos(use_module(U, L), DPos, use_module_2, M, M, use_module(U, L), DPos).
declaration_pos(use_module(U, L), term_position(_, _, _, _, [_, Pos]),
		import(U), M, M, L, Pos).

:- meta_predicate mapsequence(2,?,?).
mapsequence(_, A, _) :-
    var(A),
    !.
    % call(G, A).
mapsequence(_, [], _) :- !.
mapsequence(G, L, list_position(_, _, PosL, _)) :- !,
    maplist(mapsequence(G), L, PosL).
mapsequence(G, (A, B), term_position(_, _, _, _, [PA, PB])) :- !,
    mapsequence(G, A, PA),
    mapsequence(G, B, PB).
mapsequence(G, A, PA) :-
    call(G, A, PA).

assert_declaration(Declaration, Sequence) :-
    assert_declaration(Declaration, Sequence, _).

assert_declaration(Declaration, Sequence, Pos) :-
    '$set_source_module'(M, M),
    assert_declaration(Declaration, M, Sequence, Pos).

assert_declaration(Declaration, M, Sequence, Pos) :-
    mapsequence(assert_declaration_one(Declaration, M), Sequence, Pos).

assert_declaration_one(reexport(U), M, PI, Pos) :- !,
    assert_reexport_declaration_2(PI, U, Pos, M).
assert_declaration_one(Declaration, _, M:PI,
		       term_position(_, _, _, _, [_, Pos])) :- !,
    assert_declaration_one(Declaration, M, PI, Pos).
assert_declaration_one(Declaration, M, F/A, Pos) :- !,
    functor(H, F, A),
    assert_position(H, M, Declaration, Pos).
assert_declaration_one(Declaration, M, F//A0, Pos) :- !,
    A is A0+2,
    functor(H, F, A),
    assert_position(H, M, Declaration, Pos).
assert_declaration_one(Declaration, M, H, Pos) :-
    assert_position(H, M, Declaration, Pos).

assert_reexport_declaration_2((F/A as G), U, Pos, M) :-
    functor(H, G, A),
    assert_position(H, M, reexport(U, [F/A as G]), Pos).
assert_reexport_declaration_2(F/A, U, Pos, M) :-
    functor(H, F, A),
    assert_position(H, M, reexport(U, [F/A]), Pos).
assert_reexport_declaration_2(op(_, _, _), _, _, _).
assert_reexport_declaration_2(except(_),   _, _, _).

assert_position(H, M, Type, Pos) :-
    source_location(File, Line),
    ( nonvar(Pos)
    ->Loc = file_term_position(File, Pos)
    ; Loc = file(File, Line, -1, 0)
    ),
    assert_location(H, M, Type, Loc).

assert_location(H, M, Type, Loc) :-
    ( \+ extra_location(H, M, Type, Loc) ->
      compile_aux_clauses(record_locations:extra_location(H, M, Type, Loc))
    ; true
    ).

system:term_expansion(Term, Pos, _, _) :-
    source_location(File, Line),
    retractall(rl_tmp(_, _)),
    asserta(rl_tmp(File, Line)),
    once(record_extra_location(Term, Pos)),
    fail.

redundant((_,_)).
redundant((_;_)).
redundant((_:_)).
redundant(true).
redundant(!).

assert_position(G, Pos, T) :-
    '$set_source_module'(M, M),
    assert_position(G, M, T, Pos).

:- public rl_goal_expansion/2.
rl_goal_expansion(Goal, Pos) :-
    callable(Goal),
    \+ redundant(Goal),
    source_location(File, Line),
    \+ rl_tmp(File, Line),
    assert_position(Goal, Pos, goal),
    !.

system:goal_expansion(Goal, Pos, _, _) :-
    rl_goal_expansion(Goal, Pos),
    fail.
