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

:- use_module(assertions(assrt_lib)).
:- use_module(checkers(checker)).
:- use_module(library(apply)).
:- use_module(library(check), []).
:- use_module(library(lists)).
:- use_module(library(prolog_codewalk)).
:- use_module(rtchecks(rtchecks)).
:- use_module(rtchecks(rtchecks_gen)).
:- use_module(xlibrary(clambda)).
:- use_module(xlibrary(compact_pi_list)).
:- use_module(xlibrary(implementation_module)).
:- use_module(xlibrary(intercept)).
:- use_module(xlibrary(normalize_pi)).
:- use_module(xlibrary(resolve_calln)).
:- use_module(xtools(location_utils)).
:- use_module(xtools(option_utils)).

:- dynamic
    tablecheck_db/3,
    assertions_db/1,
    violations_db/1,
    rtcheck_db_1/3,
    rtcheck_db/3.

:- multifile
    prolog:message//1.

checker:check(assertions, Result, OptionL0) :-
    option_allchk(OptionL0, OptionL, FileChk),
    cleanup_db,
    check_assertions(from_chk(FileChk), OptionL, Result).

cleanup_db :-
        retractall(tablecheck_db(_, _, _)),
	retractall(assertions_db(_)),
	retractall(violations_db(_)).

check_assertions(FromChk, OptionL0, Pairs) :-
    ignore(option(module(M), OptionL0 )),
    merge_options(OptionL0,
		  [infer_meta_predicates(false),
		   autoload(false),
		   evaluate(false),
		   trace_reference(_)
		  ], OptionL),
    prolog_walk_code([source(false),
		      on_trace(record_checks(M, FromChk))
		     |OptionL]),
    findall(CRef, retract(assertions_db(clause(CRef))), ClausesU),
    sort(ClausesU, Clauses),
    ( Clauses==[]
    ->Pairs=[]
    ; prolog_walk_code([clauses(Clauses),
			on_trace(collect_violations(M))
		       |OptionL]),
      findall(error-Issue,
	      ( retract(violations_db(Issue))
	      ; current_head_ctcheck(M, FromChk, Issue)
	      ),
	      Pairs, Props),
      prop_ctcheck(M, FromChk, Props)
    ).

current_head_ctcheck(M, FromChk, head(Loc-PI)-AssrErrorL) :-
    PI=M:F/A,
    current_predicate(M:F/A),
    functor(H, F, A),
    \+ predicate_property(M:H, imported_from(_)),
    tabled_generate_ctchecks(H, M, CTCheck),
    CTCheck \= _:true,
    nth_clause(M:H, _, Clause),
    From = clause(Clause),
    call(FromChk, From),
    clause(M:H, _, Clause),
    do_check_property_ctcheck(H, M, CTCheck, AssrErrorL),
    % Although we have duplicated logic, we don't call check_property_ctcheck/3
    % here because is too slow:
    % check_property_ctcheck(H, M, CTChecks),
    AssrErrorL \= [],
    from_location(From, Loc).

prop_ctcheck(M, FromChk, Trans) :-
    findall(Pair, current_prop_ctcheck(M, FromChk, Pair), Pairs),
    sort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Groups),
    maplist(\ (K-L)^(error-(prop(G)-K))
	   ^group_pairs_by_key(L, G), Groups, Trans).

current_prop_ctcheck(M, FromChk, (Checker-Issues)-(Loc-PI)) :-
    assertion_db(Head, M, CM, _, Type, Cp, Ca, Su, Gl, _, _, From),
    Type \= (test),
    call(FromChk, From),
    functor(Head, HF,HA),
    PI=M:HF/HA,
    ( ( member(Prop, Cp)
      ; member(Prop, Ca)
      ; member(Prop, Su)
      ),
      resolve_head(Prop, CM, N:H)
    ; member(Glob, Gl),
      resolve_head(Glob, CM, N:H0 ),
      H0 =.. [F|Args],
      H =.. [F, Head|Args]
    ),
    checker_t(Checker),
    implementation_module(N:H, IM),
    check_property(Checker, H, IM, Issues),
    from_location(From, Loc).

prolog:message(acheck(assertions)) -->
    ['-----------------',nl,
     'Check asssertions',nl,
     '---------------------',nl,
     'The predicates below contains assertions that are inconsistent', nl,
     'with the  implementation. The reason is explained there.', nl, nl].
prolog:message(acheck(assertions, Type-IssueL)) -->
    type_message(Type),
    {type_issue_t(Type, IssueT)},
    foldl(prop_issue(IssueT), IssueL).

type_issue_t(body(_), ctchecks).
type_issue_t(head(_), ctchecks).
type_issue_t(prop(_), property).

prop_issue(ctchecks, CTChecks) -->
    prolog:message(acheck(checks, CTChecks)).
prop_issue(property, Checker-IssueL) -->
    property_issue(Checker, IssueL).

property_issue(ctcheck, CTChecksL) -->
    {append(CTChecksL, CTChecks)},
    prop_issue(ctchecks, CTChecks).
property_issue(defined, PropL) -->
    {compact_pi_list(PropL, PropC)},
    ["\t~w are undefined"-[PropC], nl].
property_issue(is_prop, PropL) -->
    {compact_pi_list(PropL, PropC)},
    ["\t~w are not properties"-[PropC], nl].

type_message(body(Loc-PI)) --> Loc, ['In the body of ~q:'-[PI], nl].
type_message(head(Loc-PI)) --> Loc, ['In the head of ~q:'-[PI], nl].
type_message(prop(LocPIL)) --> foldl(type_message_prop, LocPIL).

type_message_prop(Loc-PIL) -->
    {compact_pi_list(PIL, PIC)},
    Loc, ['In assertions of ~q:'-[PIC], nl].

black_list(assertion_head(_, _, _, _, _, _, _), assrt_lib).
				% Issues in the assertion body will be reported
				% when checking properties.
black_list(M:Call) :- black_list(Call, M).

:- public record_checks/5.
:- meta_predicate record_checks(?, 1, +, +, +).

record_checks(M, FromChk, CM:Goal, Caller, From) :-
    \+ black_list(Caller),
    call(FromChk, From),
    implementation_module(CM:Goal, M),
    check_property_ctcheck(Goal, M, CTChecks),
    CTChecks \= [],
    assertz(assertions_db(From)).

:- public collect_violations/4.
:- meta_predicate collect_violations(?, 0, +, +).
collect_violations(M, CM:Goal, Caller, From) :-
    implementation_module(CM:Goal, M),
    check_property_ctcheck(Goal, M, CTChecks),
    CTChecks \= [],
    normalize_pi(Caller, CPI),
    from_location(From, Loc),
    assertz(violations_db(body(Loc-CPI)-CTChecks)).

check_property_ctcheck(Goal, M, AssrErrorL) :-
    tabled_generate_ctchecks(Goal, M, CTCheck),
    CTCheck \= _:true, % Skip lack of assertions or assertions that will not
                       % trigger violations
    do_check_property_ctcheck(Goal, M, CTCheck, AssrErrorL).

do_check_property_ctcheck(Goal, M, CTCheck, AssrErrorL) :-
    ( copy_term_nat(Goal, Goal2),
      rtcheck_db(Goal, M, AssrErrorL),
      Goal =@= Goal2
    ->true
    ; check_property_ctcheck_1st_time(Goal, M, CTCheck, AssrErrorL)
    ).

check_property_ctcheck_1st_time(Goal, M, CTCheck, AssrErrorL) :- 
    assertz(rtcheck_db_1(Goal, M, [])),
    AssrError = assrchk(_, _),
    intercept(CTCheck, AssrError, % Now execute the checks
	      ( retract(rtcheck_db_1(Goal, M, AssrErrorL)),
		assertz(rtcheck_db_1(Goal, M, [AssrError|AssrErrorL]))
	      )),
    retract(rtcheck_db_1(Goal, M, AssrErrorL)),
    assertz(rtcheck_db(Goal, M, AssrErrorL)).

checker_t(defined).
checker_t(is_prop).
checker_t(ctcheck).

check_property(defined, H, M, M:F/A) :- % This will also be reported by
                                        % check_undefined, but is here to avoid
                                        % dependency with other analysis.
    functor(H, F, A),
    \+ current_predicate(M:F/A).
check_property(is_prop, H, M, M:F/A) :-
    resolve_calln(M:H, M:G),
    functor(G, F, A),
    \+ verif_is_property(M, F, A).
check_property(ctcheck, H, M, CTChecks) :- % compile-time checks. Currently only
                                           % compatibility checks.
    check_property_ctcheck(H, M, CTChecks),
    CTChecks \= [].

%% tabled_generate_ctchecks(+, +, -) is det
%
tabled_generate_ctchecks(H, M, Goal) :-
    ( tablecheck_db(H, M, Goal)
    ->true
    ; functor(H, F, A),
      functor(P, F, A),
      generate_ctchecks(P, M, Goal),
      assertz(tablecheck_db(P, M, Goal)),
      P = H
    ).

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

verif_is_property(system, true, 0) :- !.   % ignore true (identity)
verif_is_property(IM, F, A) :-
    functor(H, F, A),
    assertion_db(H, AM, _, _, prop, _, _, _, _, _, _, _),
    ( AM = IM -> true
    ; predicate_property(AM:H, imported_from(IM))
    ).
