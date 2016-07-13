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
:- use_module(library(implementation_module)).
:- use_module(library(location_utils)).
:- use_module(library(referenced_by)).
:- use_module(library(assrt_lib)).
:- use_module(library(extra_codewalk)).
:- use_module(library(from_utils)).

:- dynamic
    deprecated_db/6.

:- multifile
    prolog:message//1,
    deprecated_predicate/3.

deprecated_predicate(MGoal, Comment, DFrom, CFrom) :-
    prop_asr(head, MGoal, _, DFrom, Asr),
    prop_asr(glob, basicprops:deprecated(_), _, _GFrom, Asr),
    asr_comm(Asr, Comment, CFrom).
deprecated_predicate(M:Goal, " Use ~q instead."-[Alt], [], []) :-
    implementation_module(M:Goal, IM),
    deprecated_predicate(Goal, IM, Alt).

checker:check(deprecated, Result, OptionL) :-
    check_deprecated(OptionL, Result).

check_deprecated(OptionL0, Pairs) :-
    merge_options(OptionL0,
		  [source(true),
		   infer_meta_predicates(false),
		   autoload(false),
		   evaluate(false),
		   trace_reference(_),
		   on_etrace(collect_deprecated)],
		  OptionL),
    extra_walk_code(OptionL),
    findall(information-((DLoc/(IM:F/A))-((CLoc/Comment)-(Loc/CI))),
	    ( retract(deprecated_db(Call, M, Comment, DFrom, CFrom, From)),
	      implementation_module(M:Call, IM),
	      functor(Call, F, A),
	      from_location(DFrom, DLoc),
	      from_location(CFrom, CLoc),
	      from_location(From, Loc),
	      check:predicate_indicator(From, CI, [])
	    ), Pairs).

prolog:message(acheck(deprecated)) -->
    ['---------------------',nl,
     'Deprecated Predicates',nl,
     '---------------------',nl,
     'The predicates below are marked as deprecated, so you have to', nl,
     'avoid its usage in new code, and to refactorize old code.', nl, nl].
prolog:message(acheck(deprecated, (Loc/PI)-CommentLocCIL)) -->
    Loc,
    ["~w deprecated."-[PI], nl],
    foldl(comment_referenced_by, CommentLocCIL).

comment_referenced_by((Loc/Comment)-LocCIL) -->
    ["    "], Loc, [Comment, " Referenced by", nl],
    referenced_by(LocCIL).

:- public collect_deprecated/3.
:- meta_predicate collect_deprecated(0,?,?).
collect_deprecated(M:Goal, _, From) :-
    deprecated_predicate(M:Goal, Comment, DFrom, CFrom),
    % counter intuitive optimization: we save M (context module) instead of
    % implementation module since M is more discriminative
    update_fact_from(deprecated_db(Goal, M, Comment, DFrom, CFrom), From).
