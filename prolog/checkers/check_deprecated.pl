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

:- module(check_deprecated, []).

:- use_module(checkers(checker)).
:- use_module(library(check), []).
:- use_module(library(prolog_codewalk)).
:- use_module(xlibrary(implementation_module)).
:- use_module(xtools(location_utils)).
:- use_module(xtools(option_utils)).
:- use_module(xtools(referenced_by)).

:- dynamic
    deprecated_db/1,
    deprecated_db/4.

:- multifile
    prolog:message//1,
    deprecated_predicate/2.

checker:check(deprecated, Result, OptionL0) :-
    option_allchk(OptionL0, OptionL, FileChk),
    check_deprecated(from_chk(FileChk), OptionL, Result).

check_deprecated(FromChk, OptionL0, Pairs) :-
    select_option(module(M), OptionL0, OptionL1, M),
    merge_options(OptionL1,
		  [infer_meta_predicates(false),
		   autoload(false),
		   evaluate(false),
		   trace_reference(_)
		  ], OptionL),
    prolog_walk_code([source(false),
		      on_trace(have_deprecated(M, FromChk))
		     |OptionL]),
    findall(CRef, retract(deprecated_db(clause(CRef))), Clauses),
    ( Clauses==[]
    ->Pairs=[]
    ; prolog_walk_code([clauses(Clauses),
			on_trace(collect_deprecated(M))
		       |OptionL]),
      findall(information-(((IM:Call)/Alt)-(Loc/CI)),
	      ( retract(deprecated_db(Call, IM, Alt, From)),
		from_location(From, Loc),
		check:predicate_indicator(From, CI, [])
	      ), Pairs)
    ).

predicate_head(Module:Head) -->
    { nonvar(Head),
      arg(_, Head, Arg),
      nonvar(Arg)
    },
    !,
    ['~w'-[Module:Head]].
predicate_head(Head) -->
    check:predicate(Head).

prolog:message(acheck(deprecated)) -->
    ['---------------------',nl,
     'Deprecated Predicates',nl,
     '---------------------',nl,
     'The predicates below are marked as deprecated, so you have to', nl,
     'avoid its usage in new code, and to refactorize old code.', nl, nl].
prolog:message(acheck(deprecated, (PI/Alt)-LocCIs)) -->
    predicate_head(PI),
    [' deprecated, use ~q instead. Referenced by'-[Alt], nl],
    referenced_by(LocCIs).

:- public have_deprecated/5.
:- meta_predicate have_deprecated(?, 1, +, +, +).

have_deprecated(M, FromChk, MGoal, _, From) :-
    call(FromChk, From),
    MGoal = _:Goal,
    implementation_module(MGoal, M),
    deprecated_predicate(M:Goal, _),
    assertz(deprecated_db(From)),
    fail.
have_deprecated(_, _, _, _, _).

:- public collect_deprecated/4.

collect_deprecated(M, MGoal, _, From) :-
    MGoal = _:Goal,
    implementation_module(MGoal, M),
    deprecated_predicate(M:Goal, Alt),
    assertz(deprecated_db(Goal, M, Alt, From)),
    fail.
collect_deprecated(_, _, _, _).
