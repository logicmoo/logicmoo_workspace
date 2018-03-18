/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2014, Process Design Center, Breda, The Netherlands.
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

:- module(goalstub, [register_stub/1,
                     stub_term_expansion/5]).

:- use_module(library(lists)).
:- use_module(library(sequence_list)).

:- dynamic
    stub/2,
    requires_stub/4.

:- multifile
    stub/2,
    requires_stub/4.

:- meta_predicate register_stub(:).
register_stub(M:Stub) :-
    % M:export(op(1150, fx, Stub)),
    compile_aux_clauses(goalstub:stub(M, Stub)).

stub_term_expansion((:- Decl), _, EM, CL, _) :-
    Decl =.. [Stub, S],
    stub(EM, Stub), !,
    sequence_list(S, L, []),
    '$current_source_module'(M),
    findall(goalstub:requires_stub(F, A, M, Stub), member(F/A, L), CL).

stub_term_expansion(end_of_file, P, _, CL, P) :- !,
    '$current_source_module'(M),
    module_property(M, file(File)),
    prolog_load_context(file, File),
    findall((H :- G),
            ( findall(F/A, requires_stub(F, A, M, _), PIU),
              sort(PIU, PIL),   % remove duplicates
              member(F/A, PIL),
              functor(H, F, A),
              stub_head(H, HS),
              findall(S, requires_stub(F, A, M, S), StubL),
              concat_stubs(StubL, H, G, HS)
            ), CL, [end_of_file]).
stub_term_expansion(Term1, P, _, Term, P) :-
    '$current_source_module'(M),
    requires_stub_rename_head(Term1, Term, M).

concat_stubs([],    _, G,  G).
concat_stubs([S|L], H, G1, G) :-
    G1 =.. [S, H, G2],
    concat_stubs(L, H, G2, G).

stub_head(Head1, Head) :-
    Head1 =.. [F1|Args],
    atom_concat(F1, '$stub', F),
    Head  =.. [F |Args].

requires_stub_rename_head(M:Term1, Term, _) :- !,
    requires_stub_rename_head(Term1, Term, M).
requires_stub_rename_head((Head1 :- Body),
                          (Head  :- Body),
                          M) :- !,
    requires_stub_rename_head_(Head1, Head, 0, M).
requires_stub_rename_head((Head1 --> Body),
                          (Head  --> Body),
                          M) :- !,
    requires_stub_rename_head_(Head1, Head, 2, M).


requires_stub_rename_head_(M:Head1, Head, N, _) :- !,
    requires_stub_rename_head_(Head1, Head, N, M).

requires_stub_rename_head_(Head1, Head, N, M) :-
    functor(Head1, F, A1 ),
    A is A1 + N,
    requires_stub(F, A, M, _), !,
    stub_head(Head1, Head).
