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

:- use_module(library(lists)).
:- use_module(library(prolog_codewalk)).
:- use_module(library(check), []).
:- use_module(library(implementation_module)).
:- use_module(library(maplist_dcg)).
:- use_module(library(normalize_pi)).
:- use_module(library(option_utils)).
:- use_module(library(location_utils)).
:- use_module(library(compact_pi_list)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(assertions/assrt_lib)).
:- use_module(library(swi/rtchecks)).
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
    findall(CRef, retract(assertions_db(clause(CRef))), ClausesU),
    sort(ClausesU, Clauses),
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
      from_location(From, Loc),
      check_property(ctcheck, H, M, CTChecks),
      assertz(violation_db(head(Loc), [ctcheck-[CTChecks]]-[M:F/A])),
      fail
    ; true
    ).

current_prop_ctcheck(M, FromChk, From, Group) :-
    assertion_db(Head, M, _, Type, Cp, Ca, Su, Gl, _, _, From),
    call(FromChk, From),
    findall(Pair,
	    current_property(Head, M, Type, Cp, Ca, Su, Gl,
			     [defined, ctcheck, is_prop], Pair),
	    Pairs),
    Pairs \= [],
    trans_group(Pairs, Groups),
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

type_message(body(Loc, PI)) --> Loc, ['In  ~q:'-[PI], nl].
type_message(head(Loc)) --> Loc.
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
    qualify_meta_goal(MGoal, Goal),
    implementation_module(MGoal, M),
    check_property(ctcheck, Goal, M, CTChecks),
    functor(Goal, F, A),
    normalize_pi(Caller, CPI),
    from_location(From, Loc),
    assertz(violation_db(body(Loc, CPI), [ctcheck-[CTChecks]]-[M:F/A])).

issue_format(defined, '\tUsing undefined: ~w').
issue_format(is_prop, '\tNot properties : ~w').

issue_message(ctcheck-RTChecksL) --> !,
    {append(RTChecksL, RTChecks)},
    prolog:message(acheck(checks(ctcheck), RTChecks)).
issue_message(Issue-Props) -->
    {compact_pi_list(Props, Compacted),
     issue_format(Issue, Format)},
    [Format -[Compacted], nl].

prolog:message(acheck(prop_issue(Heads, IssuePIsL))) -->
    {sort(Heads, Sorted), compact_pi_list(Sorted, Compacted)},
    ['In assertions for ~w'-[Compacted], nl],
    maplist_dcg(issue_message, IssuePIsL).

checker_t(defined).
checker_t(is_prop).
checker_t(ctcheck).

resolve_head(M:H0, _, H) :- !,
    resolve_head(H0, M, H).
resolve_head((A,B), M, H) :- !,
    ( resolve_head(A, M, H)
    ; resolve_head(B, M, H)
    ).
resolve_head((A;B), M, H) :- !,
    ( resolve_head(A, M, H)
    ; resolve_head(B, M, H)
    ).
resolve_head(H, M, M:H).

current_property(Head, M, Type, Cp, Ca, Su, Gl, Issues, PI-(Issue-Values)) :-
    Type \= (test),
    functor(Head, HF,HA),
    PI=M:HF/HA,
    ( ( member(Prop, Cp)
      ; member(Prop, Ca)
      ; member(Prop, Su)
      ),
      resolve_head(Prop, M, N:H)
    ; member(Glob, Gl),
      resolve_head(Glob, M, N:H0),
      H0 =.. [F|Args],
      H =.. [F, Head|Args]
    ),
    member(Issue, Issues),
    checker_t(Issue),
    implementation_module(N:H, IM),
    check_property(Issue, H, IM, Values).

group_pairs_2(K-L, G-K) :- group_pairs_by_key(L, G).

trans_group(Pairs, TGrouped) :-
    sort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Grouped),
    maplist(group_pairs_2, Grouped, Trans),
    keysort(Trans, TSorted),
    group_pairs_by_key(TSorted, TGrouped).

is_location(Loc) :- clause(prolog:message_location(Loc, _, _), _).

check_property(defined, H, M, M:F/A) :-
    functor(H, F, A),
    \+ current_predicate(M:F/A).
check_property(is_prop, H, M, M:F/A) :-
    functor(H, F, A),
    \+ verif_is_property(M, F, A).
check_property(ctcheck, H, M, CTChecks) :-
				% compile-time checks. Currently only
				% compatibility checks.
    generate_ctchecks(H, M, _, Goal),
    save_rtchecks(M:Goal),	% Now execute the checks
    load_rtchecks(CTChecks),	% and collect the failures
    CTChecks \= [].

verif_is_property(_, call, N) :- N > 0, !. % meta checks not supported yet --EMM
verif_is_property(system, true, 0) :- !.   % ignore true (identity)
verif_is_property(IM, F, A) :-
    functor(H, F, A),
    assertion_db(H, AM, _, prop, _, _, _, _, _, _, _),
    ( AM = IM -> true
    ; predicate_property(AM:H, imported_from(IM))
    ).
