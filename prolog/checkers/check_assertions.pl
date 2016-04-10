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

:- use_module(library(assrt_lib)).
:- use_module(checkers(checker)).
:- use_module(library(apply)).
:- use_module(library(check), []).
:- use_module(library(extra_codewalk)).
:- use_module(library(rtchecks)).
:- use_module(library(clambda)).
:- use_module(library(compact_pi_list)).
:- use_module(library(implementation_module)).
:- use_module(library(intercept)).
:- use_module(library(normalize_pi)).
:- use_module(library(resolve_calln)).
:- use_module(library(location_utils)).
:- use_module(library(from_utils)).
:- use_module(library(qualify_meta_goal)).

:- dynamic
    tablecheck_db/4,
    assertions_db/1,
    violations_db/3,
    rtcheck_db_1/3,
    rtcheck_db/4.

:- multifile
    prolog:message//1.

checker:check(assertions, Result, OptionL) :-
    cleanup_db,
    check_assertions(OptionL, Result).

cleanup_db :-
        retractall(tablecheck_db(_, _, _, _)),
	retractall(assertions_db(_)),
	retractall(violations_db(_, _, _)).

check_assertions(OptionL0, Pairs) :-
    merge_options(OptionL0,
		  [on_etrace(collect_violations(M)),
		   walkextras([declaration]) % TODO: use asrparts([head, body])
		  ], OptionL),
    extra_walk_code(OptionL, M, FromChk),
    findall(error-Issue,
	    ( retract(violations_db(CPI, CTChecks, From)),
	      from_location(From, Loc),
	      Issue = body(Loc-CPI)-CTChecks
	    ; current_head_ctcheck(M, FromChk, Issue)
	    ),
	    Pairs, Props),
    prop_ctcheck(M, FromChk, Props).

current_head_ctcheck(M, FromChk, head(Loc-PI)-AssrErrorL) :-
    PI=M:F/A,
    current_predicate(M:F/A),
    functor(H, F, A),
    \+ predicate_property(M:H, imported_from(_)),
    \+ predicate_property(M:H, built_in),
    \+ predicate_property(M:H, foreign),
    tabled_generate_ctchecks(H, M, _CM, CTCheck), % Keep _CM uninstantiated
    CTCheck \= _:true,
    clause(M:H, _, Clause),
    From = clause(Clause),
    call(FromChk, From),
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

current_prop_ctcheck(M, FromChk, (Checker-PLoc/Issues)-(Loc-PI)) :-
    asr_head_prop(Asr, CM, Head, _, Type, _, From),
    Type \= (test),
    implementation_module(CM:Head, M),
    call(FromChk, From),
    functor(Head, HF,HA),
    PI=M:HF/HA,
    ( ( asr_comp(Asr, PM, Prop, PFrom)
      ; asr_call(Asr, PM, Prop, PFrom)
      ; asr_succ(Asr, PM, Prop, PFrom)
      ; asr_glob(Asr, PM, Prop, PFrom)
      ),
      resolve_head(Prop, PM, N:H)
    ),
    checker_t(Checker),
    implementation_module(N:H, IM),
    check_property(Checker, H, IM, PM, Issues),
    numbervars(Issues, 0, _),
    from_location(PFrom, PLoc),
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
    foldl(property_issue(Checker), IssueL).

property_issue(ctcheck, Loc/(PI-CTChecks)) -->
    ["\t"], Loc, ["In call to ~w:"-[PI], nl],
    prop_issue(ctchecks, CTChecks).
property_issue(defined, Loc/Prop) -->
    ["\t"], Loc, ["~w is undefined"-[Prop], nl].
property_issue(is_prop, Loc/Prop) -->
    ["\t"], Loc, ["~w is not a property"-[Prop], nl].

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

:- public collect_violations/4.
collect_violations(M, CM:Goal, Caller, From) :-
    \+ black_list(Caller),
    implementation_module(CM:Goal, M),
    check_property_ctcheck(Goal, M, CM, CTChecks),
    CTChecks \= [],
    normalize_pi(Caller, CPI),
    update_fact_from(violations_db(CPI, CTChecks), From).

check_property_ctcheck(Goal, M, CM, AssrErrorL) :-
    tabled_generate_ctchecks(Goal, M, CM, CTCheck),
    CTCheck \= _:true, % Skip lack of assertions or assertions that will not
                       % trigger violations
    do_check_property_ctcheck(Goal, M, CTCheck, AssrErrorL).

do_check_property_ctcheck(Goal, M, CTCheck, AssrErrorL) :-
    ( variant_sha1(Goal, SHA1),
      rtcheck_db(SHA1, Goal, M, AssrErrorL)
    ->true
    ; check_property_ctcheck_1st_time(Goal, M, CTCheck, AssrErrorL)
    ).

check_property_ctcheck_1st_time(Goal, M, CTCheck, AssrErrorL) :-
    variant_sha1(Goal, SHA1),
    assertz(rtcheck_db_1(Goal, M, [])),
    AssrError = assrchk(_, _),
    intercept(CTCheck, AssrError, % Now execute the checks
	      ( retract(rtcheck_db_1(Goal, M, AssrErrorL)),
		assertz(rtcheck_db_1(Goal, M, [AssrError|AssrErrorL]))
	      )),
    retract(rtcheck_db_1(Goal, M, AssrErrorL)),
    assertz(rtcheck_db(SHA1, Goal, M, AssrErrorL)).

checker_t(defined).
checker_t(is_prop).
checker_t(ctcheck).

check_property(defined, H, M, _, M:F/A) :-
				% Also reported by check_undefined, but is here
				% to avoid dependency with other analysis.
    functor(H, F, A),
    \+ current_predicate(M:F/A).
check_property(is_prop, H, M, _, M:F/A) :-
    resolve_calln(M:H, M:G),
    functor(G, F, A),
    \+ verif_is_property(M, F, A).
check_property(ctcheck, H, M, CM, (M:F/A)-CTChecks) :-
				% compile-time checks. Currently only
				% compatibility checks.
    check_property_ctcheck(H, M, CM, CTChecks),
    CTChecks \= [],
    resolve_calln(M:H, M:G),
    functor(G, F, A).

%% tabled_generate_ctchecks(+, +, ?, -) is det
%
tabled_generate_ctchecks(H, M, CM, Goal) :-
    ( tablecheck_db(H, M, CM, Goal)
    ->true
    ; functor(H, F, A),
      functor(P, F, A),
      generate_ctchecks(P, M, CM0, Goal),
      assertz(tablecheck_db(P, M, CM0, Goal)),
      CM0 = CM,
      P = H
    ).

% Generate compile-time checks, currently only compatibility is checked, fails
% if no ctchecks can be applied to Pred. Note that CM can be a variable, to
% allow tabling of the result and CM to be instantiated later.
%
generate_ctchecks(Goal, M, CM, CTChecks) :-
    qualify_meta_goal(Goal, M, CM, Pred),
    collect_assertions(Pred, M, ctcheck, AsrL),
    ( AsrL \= []
    ->
      maplist(wrap_asr_ctcheck, AsrL, PAsrL),
      CTChecks = check_assertions:ctcheck_goal(PAsrL)
    ; CTChecks = check_assertions:true
    ).

wrap_asr_ctcheck(Asr, ctcheck(Asr)).

:- public ctcheck_goal/1.
ctcheck_goal(AsrL) :-
    pairs_keys_values(AsrPVL, AsrL, _),
    rtchecks_rt:check_asrs_props(compat, AsrPVL).

% Trivial abstraction: Check for compatibility issues in properties,
% compatibility is an abstraction that makes static check decidable.  Here we
% lose precision but we gain computability of checks at earlier, even
% compile-time. TBD: Formal demostration. --EMM
assrt_lib:asr_aprop(ctcheck(Asr), Key, Prop, From) :-
    prop_abstraction(Key, Abst),
    prop_asr(Abst, Prop, From, Asr).

prop_abstraction(head, head).
prop_abstraction(stat, stat).
prop_abstraction(type, type).
prop_abstraction(dict, dict).
prop_abstraction(comm, comm).
prop_abstraction(comp, comp).
prop_abstraction(comp, call).
prop_abstraction(comp, succ).

verif_is_property(system, true, 0) :- !.   % ignore true (identity)
verif_is_property(IM, F, A) :-
    functor(H, F, A),
    asr_head_prop(_, CM, H, _, prop, _, _),
    implementation_module(CM:H, AM),
    ( AM = IM
    ->true
    ; predicate_property(IM:H, imported_from(AM))
    ).
