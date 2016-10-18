/*  Part of Extended libraries for Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2014, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
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
	      sort(PIU, PIL),	% remove duplicates
	      member(F/A, PIL),
	      functor(H, F, A),
	      stub_head(H, HS),
	      findall(S, requires_stub(F, A, M, S), StubL),
	      concat_stubs(StubL, H, G, HS)
	    ), CL, [end_of_file]).
stub_term_expansion(Term0, P, _, Term, P) :-
    '$current_source_module'(M),
    requires_stub_rename_head(Term0, Term, M).

concat_stubs([],    _, G,  G).
concat_stubs([S|L], H, G0, G) :-
    G0 =.. [S, H, G1],
    concat_stubs(L, H, G1, G).

stub_head(Head0, Head) :-
    Head0 =.. [F0|Args],
    atom_concat(F0, '$stub', F),
    Head  =.. [F |Args].

requires_stub_rename_head(M:Term0, Term, _) :- !,
    requires_stub_rename_head(Term0, Term, M).
requires_stub_rename_head((Head0 :- Body),
			  (Head  :- Body),
			  M) :- !,
    requires_stub_rename_head_(Head0, Head, 0, M).
requires_stub_rename_head((Head0 --> Body),
			  (Head  --> Body),
			  M) :- !,
    requires_stub_rename_head_(Head0, Head, 2, M).


requires_stub_rename_head_(M:Head0, Head, N, _) :- !,
    requires_stub_rename_head_(Head0, Head, N, M).

requires_stub_rename_head_(Head0, Head, N, M) :-
    functor(Head0, F, A0 ),
    A is A0 + N,
    requires_stub(F, A, M, _), !,
    stub_head(Head0, Head).
