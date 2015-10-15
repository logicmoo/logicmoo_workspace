/*  Part of Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.

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

:- module(extra_codewalk, [extra_walk_code/4,
			   extra_wcsetup/3,
			   extra_walk_module_body/2,
			   decl_walk_code/2,
			   record_issues/1]).

:- use_module(library(prolog_codewalk)).
:- use_module(xtools(extra_location)).
:- use_module(xtools(option_utils)).

:- thread_local
    issues/1.

:- meta_predicate
    decl_walk_code(3,-),
    extra_walk_code(+,3,-,-).

extra_walk_module_body(M, OptionL0 ) :-
    select_option(module(M), OptionL0, OptionL1, M),
    ( nonvar(M)
    ->findall(Ref, current_clause_module_body(M, Ref), RefU),
      sort(RefU, RefL),
      prolog_walk_code([source(false), clauses(RefL)|OptionL1])
    ; true
    ).

extra_walk_code(OptionL0, Tracer, M, FromChk) :-
    extra_wcsetup(OptionL0, OptionL1, FromChk),
    select_option(source(S), OptionL1, OptionL, false),
    extra_walk_module_body(M, [on_trace(Tracer)|OptionL]),
    optimized_walk_code(S, [on_trace(Tracer)|OptionL]),
    decl_walk_code(Tracer, M),
    assr_walk_code(Tracer, M).

current_clause_module_body(CM, Ref) :-
    current_predicate(M:F/A),
    M \= CM,
    functor(H, F, A),
    \+ predicate_property(M:H,imported_from(_)),
    nth_clause(M:H, _I, Ref),
    clause_property(Ref, module(CM)).

optimized_walk_code(false, OptionL) :-
    prolog_walk_code([source(false)|OptionL]).
optimized_walk_code(true, OptionL) :-
    prolog_walk_code([source(false)|OptionL]),
    findall(CRef, retract(issues(CRef)), ClausesU),
    sort(ClausesU, Clauses),
    ( Clauses==[]
    ->true
    ; prolog_walk_code([clauses(Clauses)|OptionL])
    ).

extra_wcsetup(OptionL0, OptionL, FromChk) :-
    option_fromchk(OptionL0, OptionL1, FromChk),
    merge_options(OptionL1,
		  [infer_meta_predicates(false),
		   autoload(false),
		   evaluate(false),
		   trace_reference(_),
		   module_class([user, system, library])
		  ], OptionL).

decl_walk_code(Tracer, M) :-
    forall(loc_declaration(Head, M, goal, From),
	   ignore(call(Tracer, M:Head, _:'<declaration>', From))).

assr_walk_code(Tracer, M) :-
    forall(assertion_db(Head, _, M, _, _, _, _, _, _, _, _, From),
	   ignore(call(Tracer, M:Head, _:'<assertion>', From))).

record_issues(CRef) :-
    assertz(issues(CRef)).
