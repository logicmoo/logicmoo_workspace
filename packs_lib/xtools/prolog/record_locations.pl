/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(record_locations, [record_location/0]).

:- use_module(library(extra_location)). % shold be the first
:- use_module(library(apply)).
:- use_module(library(filepos_line)).
:- use_module(library(from_utils)).

:- multifile
    system:term_expansion/4,
    system:goal_expansion/4.

:- dynamic record_location/0.
record_location. % Enable recording of locations

:- thread_local rl_tmp/3. % trick to detect if term_expansion was applied

% Extra location for assertions of a given predicate
extra_location:loc_declaration(Head, M, assertion(Status, Type), From) :-
    assertions:asr_head_prop(_, CM, Head, Status, Type, _, From),
    predicate_property(CM:Head, implementation_module(M)).

:- multifile skip_record_decl/1.

skip_record_decl(initialization(_)) :- !.
skip_record_decl(Decl) :-
    nonvar(Decl),
    '$current_source_module'(M),
    predicate_property(M:Decl, imported_from(assertions)),
    functor(Decl, Type, Arity),
    memberchk(Arity, [1, 2]),
    assertions:assrt_type(Type), !.

:- public record_extra_location/4.

record_extra_location((:- Decl),
                      term_position(_, _, _, _, [DPos])) -->
    ( {\+ skip_record_decl(Decl)}
    ->record_extra_decl(Decl, DPos)
    ; []
    ).

record_extra_decl(Decl, DPos) -->
    { '$current_source_module'(SM),
      declaration_pos(Decl, DPos, SM, M, IdL, ArgL, PosL)
    },
    foldl(assert_declaration(M), IdL, ArgL, PosL),
    !.
record_extra_decl(Goal, Pos) -->
    { nonvar(Goal),
      source_location(File, Line),
      retractall(rl_tmp(File, Line, _)),
      asserta(rl_tmp(File, Line, 1)),
      assert_position(Goal, Pos, body)
    }.

declaration_pos(DM:Decl, term_position(_, _, _, _, [_, DPos]), _, M, ID, U, Pos) :-
    declaration_pos(Decl, DPos, DM, M, ID, U, Pos).
declaration_pos(module(M, L), DPos,
                _, M, [module_2, export], [module(M, L), L], [DPos, Pos]) :-
    DPos = term_position(_, _, _, _, [_, Pos]).
declaration_pos(volatile(L), term_position(_, _, _, _, PosL),
                M, M, [volatile], [L], PosL).
declaration_pos(dynamic(L), term_position(_, _, _, _, PosL),
                M, M, [dynamic], [L], PosL).
declaration_pos(thread_local(L), term_position(_, _, _, _, PosL),
                M, M, [thread_local], [L], PosL).
declaration_pos(public(L), term_position(_, _, _, _, PosL),
                M, M, [public], [L], PosL).
declaration_pos(export(L), term_position(_, _, _, _, PosL),
                M, M, [export], [L], PosL).
declaration_pos(multifile(L), term_position(_, _, _, _, PosL),
                M, M, [multifile], [L], PosL).
declaration_pos(discontiguous(L), term_position(_, _, _, _, PosL),
                M, M, [discontiguous], [L], PosL).
declaration_pos(meta_predicate(L), term_position(_, _, _, _, PosL),
                M, M, [meta_predicate], [L], PosL).
declaration_pos(reexport(SM:DU),  DPos,  _, M, ID, U, Pos) :- !,
    declaration_pos(reexport(DU), DPos, SM, M, ID, U, Pos).
declaration_pos(use_module(SM:DU),  DPos,  _, M, ID, U, Pos) :- !,
    declaration_pos(use_module(DU), DPos, SM, M, ID, U, Pos).
declaration_pos(use_module(SM:DU, L), DPos, ID, _, M, U, Pos) :- !,
    declaration_pos(use_module(DU, L), DPos, ID, SM, M, U, Pos).
declaration_pos(reexport(SM:DU, L), DPos, ID, _, M, U, Pos) :- !,
    declaration_pos(reexport(DU, L), DPos, ID, SM, M, U, Pos).
declaration_pos(include(U),    DPos, M, M, [include],    [U], [DPos]).
declaration_pos(use_module(U), DPos, M, M, [use_module], [U], [DPos]).
declaration_pos(reexport(U),   DPos, M, M, [reexport],   [U], [DPos]).
declaration_pos(consult(U),    DPos, M, M, [consult],    [U], [DPos]).
declaration_pos(reexport(U, L), DPos, M, M,
                [reexport_2, reexport(U)], [reexport(U, L), L], [DPos, Pos]) :-
    DPos = term_position(_, _, _, _, [_, Pos]).
declaration_pos(use_module(U, L), DPos, M, M,
                [use_module_2, import(U)], [use_module(U, L), L], [DPos, Pos]) :-
    DPos = term_position(_, _, _, _, [_, Pos]).

:- meta_predicate foldsequence(4,?,?,?,?).

foldsequence(G, A, B) --> foldsequence_(A, G, B).

foldsequence_(A, _, _) -->
    {var(A)},
    !.
    % call(G, A).
foldsequence_([], _, _) --> !.
foldsequence_([E|L], G, list_position(_, _, PosL, _)) -->
    !,
    foldl(foldsequence(G), [E|L], PosL).
foldsequence_((A, B), G, term_position(_, _, _, _, [PA, PB])) -->
    !,
    foldsequence_(A, G, PA),
    foldsequence_(B, G, PB).
foldsequence_(A, G, PA) --> call(G, A, PA).

assert_declaration(M, Declaration, Sequence, Pos) -->
    foldsequence(assert_declaration_one(Declaration, M), Sequence, Pos).

assert_declaration_one(reexport(U), M, PI, Pos) -->
    !,
    assert_reexport_declaration_2(PI, U, Pos, M).
assert_declaration_one(Declaration, _, M:PI,
                       term_position(_, _, _, _, [_, Pos])) -->
    !,
    assert_declaration_one(Declaration, M, PI, Pos).
assert_declaration_one(Declaration, M, F/A, Pos) -->
    { atom(F),
      integer(A)
    },
    !,
    {functor(H, F, A)},
    assert_position(H, M, Declaration, Pos).
assert_declaration_one(Declaration, M, F//A1, Pos) -->
    { atom(F),
      integer(A1)
    },
    !,
    { A is A1+2,
      functor(H, F, A)
    },
    assert_position(H, M, Declaration, Pos).
assert_declaration_one(Declaration, M, H, Pos) -->
    assert_position(H, M, Declaration, Pos).

assert_reexport_declaration_2((F/A as G), U, Pos, M) -->
    {functor(H, G, A)},
    assert_position(H, M, reexport(U, [F/A as G]), Pos).
assert_reexport_declaration_2(F/A, U, Pos, M) -->
    {functor(H, F, A)},
    assert_position(H, M, reexport(U, [F/A]), Pos).
assert_reexport_declaration_2(op(_, _, _), _, _, _) --> [].
assert_reexport_declaration_2(except(_),   _, _, _) --> [].

assert_position(H, M, Type, TermPos) :-
    assert_position(H, M, Type, TermPos, Clauses, []),
    compile_aux_clauses(Clauses).

assert_position(H, M, Type, TermPos) -->
    { source_location(File, Line1),
      ( nonvar(TermPos)
      ->arg(1, TermPos, Chars),
        filepos_line(File, Chars, Line, LinePos)
        % Meld TermPos because later the source code will not be available and
        % therefore we will not be able to get LinePos
      ; Line = Line1,
        LinePos = -1
      )
    },
    assert_location(H, M, Type, File, Line, file(File, Line, LinePos, Chars)).

assert_location(H, M, Type, File, Line, From) -->
    ( {\+ have_extra_location(From, H, M, Type)}
    ->['$source_location'(File, Line):extra_location:loc_declaration(H, M, Type, From)]
    ; []
    ).

/*
have_extra_location(file(File, Line, _, _), H, M, Type) :- !,
    extra_location(H, M, Type, From),
    from_to_file(From, File),
    from_to_line(From, Line).
have_extra_location(From, H, M, Type) :-
    extra_location(H, M, Type, From).
*/

have_extra_location(From1, H, M, Type) :-
    extra_location(H, M, Type, From),
    subsumes_from(From1, From).

system:term_expansion(Term, Pos, [Term|Clauses], Pos) :-
    record_location,
    source_location(File, Line),
    ( rl_tmp(File, Line, _)
    ->fail
    ; retractall(rl_tmp(_, _, _)),
      asserta(rl_tmp(File, Line, 0 )),
      record_extra_location(Term, Pos, Clauses, []),
      Clauses \= []
    ).

redundant((_,_)).
redundant((_;_)).
redundant((_:_)).
redundant(true).
redundant(!).

assert_position(G, Pos, T) :-
    '$current_source_module'(M),
    assert_position(G, M, T, Pos).

:- public rl_goal_expansion/2.
rl_goal_expansion(Goal, Pos) :-
    callable(Goal),
    \+ redundant(Goal),
    source_location(File, Line),
    ( rl_tmp(File, Line, Flag)
    ->Flag == 1
    ; true
    ),
    b_getval('$term', Term),
    memberchk(Term, [(:-_), []]),
    \+ clause(declaration_pos(Goal, _, _, _, _, _, _), _),
    \+ skip_record_decl(Goal),
    assert_position(Goal, Pos, goal),
    !.

system:goal_expansion(Goal, Pos, _, _) :-
    record_location,
    rl_goal_expansion(Goal, Pos),
    fail.
