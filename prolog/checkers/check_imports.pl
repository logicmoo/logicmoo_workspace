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

:- module(check_imports, []).

:- use_module(checkers(checker)).
:- use_module(library(apply)).
:- use_module(library(clambda)).
:- use_module(library(expansion_module)).
:- use_module(library(implementation_module)).
:- use_module(library(extra_codewalk)).
:- use_module(library(extra_location)).
:- use_module(library(from_utils)).
:- use_module(library(location_utils)).

:- multifile
    prolog:message//1.

prolog:message(acheck(imports)) -->
    ['--------------',nl,
     'Unused Imports',nl,
     '--------------',nl,
     'The predicates or modules below has been imported, however they', nl,
     'are never used in the importing module, or they do not implement', nl,
     'new clauses for multifile predicates.  Note that modules that', nl,
     'export operators, or that do not export any predicate are not', nl,
     'reported.', nl,
     'You can silent the warnings by declaring use_module/2 with an',nl,
     'empty import list. If they have desirable side effects and still', nl,
     'needs to be imported, you can refactorize you program so that', nl,
     'such side effects are not required anymore.', nl, nl].
prolog:message(acheck(imports, c(Class, Type, Name)-LocElemL)) -->
    ['~w ~w have unused ~w:'-[Class, Name, Type], nl],
    foldl(unused_import(Type), LocElemL).

unused_import(Type, Loc/Elem) -->
    Loc,
    ['unused ~w ~w'-[Type, Elem], nl].

:- dynamic
    used_import/1,
    used_usemod/2.

checker:check(imports, Result, OptionL) :-
    check_imports(OptionL, Result).

check_imports(OptionL, Pairs) :-
    extra_walk_code([source(false)|OptionL], collect_imports(M, FromChk), M, FromChk),
    collect_imports(M, FromChk, Pairs, Tail),
    collect_usemods(M, FromChk, Tail, []),
    cleanup_imports.

:- meta_predicate collect_imports(?,1,+,+,+).
collect_imports(M, FromChk, M:Goal, Caller, From) :-
    call(FromChk, From),
    record_location_meta(M:Goal, _, From, all_call_refs, mark_import),
    ( nonvar(Caller),
      Caller = MC:_,
      M \= MC,
      \+ used_usemod(M, MC)
    ->assertz(used_usemod(M, MC))
    ; true
    ).

collect_imports(M, FromChk, Pairs, Tail) :-
    findall(warning-(c(use_module, import, U)-(Loc/(F/A))),
	    ( clause(loc_declaration(Head, M, import(U), From), _, CRef),
	      call(FromChk, From),
	      M \= user,
	      \+ memberchk(Head, [term_expansion(_,_),
				  term_expansion(_,_,_,_),
				  goal_expansion(_,_),
				  goal_expansion(_,_,_,_),
				  except(_)
				 ]),
	      \+ used_import(CRef),
	      \+ loc_declaration(Head, M, goal, _),
	      module_property(M, class(Class)),
	      memberchk(Class, [user]),
	      functor(Head, F, A),
	      from_location(From, Loc)
	    ),
	    Pairs, Tail).

:- multifile ignore_import/2.

ignore_import(_, rtchecks_rt).
ignore_import(M, IM) :- expansion_module(M, IM).

collect_usemods(M, FromChk, Pairs, Tail) :-
    findall(warning-(c(module, use_module, M)-(Loc/U)),
%	    [M,FromChk,U,Loc] +\
	   ( ( loc_declaration(U, M, use_module, From),
	       ExL = []
	     ; loc_declaration(use_module(U, except(ExL)), M, use_module_2, From)
	     ),
	     call(FromChk, From),
	     M \= user,
	     module_property(M, class(Class)),
	     memberchk(Class, [user]),
	     from_to_file(From, File),
	     \+ findall(I, source_file_property(File, included_in(I, _)),
			[_, _|_]),
	     absolute_file_name(U, UFile, [file_type(prolog), access(exist),
					   file_errors(fail)]),
	     current_module(UM, UFile),
	     \+ ignore_import(M, UM),
	     module_property(UM, exports(EL)),
	     EL \= [],
	     subtract(EL, ExL, PIL),
	     \+ ( module_property(UM, exported_operators(OL)),
		  OL \= []
		),
	     \+ ( member(F/A, PIL),
		  functor(Head, F, A),
		  implementation_module(UM:Head, IM),
		  used_usemod(M, IM)
		),
	     from_location(From, Loc)
	   ), Pairs, Tail).

mark_import(M:Head, CM, _, _, _, _) :-
    nonvar(M),
    callable(Head),
    mark_import(Head, M, CM).

mark_import(Head, M, CM) :-
    forall(( clause(loc_declaration(Head, CM, import(_), _), _, CRef),
	     \+ used_import(CRef)),
	   assertz(used_import(CRef))),
    ( M \= CM,
      \+used_usemod(CM, M)
    ->assertz(used_usemod(CM, M))
    ; true
    ).

cleanup_imports :-
    retractall(used_import(_)),
    retractall(used_usemod(_, _)).
