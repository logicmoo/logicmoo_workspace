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

:- module(check_assertions, []).

:- use_module(library(prolog_codewalk)).
:- use_module(library(check), []).
:- use_module(library(implementation_module)).
:- use_module(library(maplist_dcg)).
:- use_module(library(normalize_pi)).
:- use_module(library(option_utils)).
:- use_module(library(location_utils)).
:- use_module(library(swi/ctchecks)).
:- use_module(library(rtchecks/rtchecks_gen)).
:- use_module(library(audit/audit)).

:- dynamic
    assertions_db/1,
    violation_db/2.

:- multifile
    prolog:message//1.

audit:check(assertions, Result, OptionL0) :-
    option_allchk(OptionL0, OptionL, FileChk),
    check_assertions(from_chk(FileChk), OptionL, Result).

check_assertions(FromChk, OptionL0, Pairs) :-
    select_option(module(M), OptionL0, OptionL1, M),
    merge_options(OptionL1,
		  [infer_meta_predicates(false),
		   autoload(false),
		   evaluate(false),
		   trace_reference(_)
		  ], OptionL),
    prolog_walk_code([source(false),
		      on_trace(have_assertions(M, FromChk))
		     |OptionL]),
    findall(CRef, retract(assertions_db(clause(CRef))), Clauses),
    ( Clauses==[]
    ->Pairs=[]
    ; prolog_walk_code([clauses(Clauses),
			on_trace(collect_violations(M))
		       |OptionL]),
      head_ctcheck(M, FromChk),
      findall(error-(Prop-i(Issue)),
	      ( retract(violation_db(Prop, Issue))
	      ; Prop = prop(Loc),
		current_prop_ctcheck(M, FromChk, From, Issue),
		from_location(From, Loc)
	      ),
	      Pairs)
    ).

head_ctcheck(M, FromChk) :-
    ( current_predicate(M:F/A),
      functor(H, F, A),
      implementation_module(M:H, M),
      nth_clause(M:H, _, Clause),
      clause(M:H, _, Clause),
      From = clause(Clause),
      call(FromChk, From),
      % current_assertion(H, ctcheck, _, _, _, _, _, _, _, _, _, _, _, _, _, M),
      ctchecks:check_property(ctcheck, H, M, From, CTChecks),
      assertz(violation_db(head, [ctcheck-[CTChecks]]-[M:F/A])),
      fail
    ; true
    ).

current_prop_ctcheck(M, FromChk, From, Group) :-
    assertion_db(Head, M, _, Type, Cp, Ca, Su, Gl, _, _, From),
    call(FromChk, From),
    findall(Pair,
	    ctchecks:current_property(Head, M, Type, Cp, Ca, Su, Gl, From,
				      [defined, ctcheck, is_prop], Pair),
	    Pairs),
    Pairs \= [],
    ctchecks:trans_group(Pairs, Groups),
    member(Group, Groups).

prolog:message(acheck(assertions)) -->
    ['-----------------',nl,
     'Check asssertions',nl,
     '---------------------',nl,
     'The predicates below contains assertions that are inconsistent', nl,
     'with the  implementation. The reason is explained there.', nl, nl].
prolog:message(acheck(assertions, Type-IssuesHeads)) -->
    type_message(Type),
    maplist_dcg(prop_issue, IssuesHeads).

prop_issue(i(Issues-Heads)) -->
    prolog:message(acheck(prop_issue(Heads, Issues))).

type_message(body(PI)) -->
    ['In the body of ~q:'-[PI], nl].
type_message(head) --> ['In the head:', nl].
type_message(prop(Loc)) --> Loc.

:- public have_assertions/5.
:- meta_predicate have_assertions(?, 1, +, +, +).

have_assertions(M, FromChk, MGoal, _, From) :-
    \+ \+ have_assertions(M, FromChk, MGoal, From).

have_assertions(M, FromChk, CM:Goal, From) :-
    call(FromChk, From),
    implementation_module(CM:Goal, M),
    once(current_assertion(Goal, ctcheck, _, _, _, _, _, _, _, _, _, _, _, _, _, M)),
    assertz(assertions_db(From)).

:- public collect_violations/4.

collect_violations(M, MGoal, Caller, From) :-
    \+ \+ collect_violations_(M, MGoal, Caller, From).

collect_violations_(M, MGoal, Caller, From) :-
    MGoal = _:Goal,
    implementation_module(MGoal, M),
    ctchecks:check_property(ctcheck, Goal, M, From, CTChecks),
    normalize_pi(MGoal, _:PI),
    normalize_pi(Caller, CPI),
    assertz(violation_db(body(CPI), [ctcheck-[CTChecks]]-[M:PI])).
