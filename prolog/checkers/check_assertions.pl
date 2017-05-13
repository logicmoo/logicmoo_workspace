/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.
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

:- module(check_assertions, []).

:- use_module(library(assrt_lib)).
:- use_module(checkers(checker)).
:- use_module(library(apply)).
:- use_module(library(check), []).
:- use_module(library(source_codewalk)).
:- use_module(library(rtchecks_rt)).
:- use_module(library(clambda)).
:- use_module(library(compact_pi_list)).
:- use_module(library(implementation_module)).
:- use_module(library(intercept)).
:- use_module(library(normalize_pi)).
:- use_module(library(resolve_calln)).
:- use_module(library(location_utils)).
:- use_module(library(from_utils)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(tabling)).
:- use_module(library(assertions)).
:- use_module(library(basicprops)).
:- use_module(library(option_utils)).

:- dynamic
       violations_db/3.

:- multifile
       prolog:message//1.

:- table
       generate_ctchecks/4,
       do_check_property_ctcheck/2.

checker:check(assertions, Result, OptionL) :-
    cleanup_db,
    check_assertions(OptionL, Result).

cleanup_db :-
        retractall(violations_db(_, _, _)).

check_assertions(OptionL1, Pairs) :-
    select_option(module(M), OptionL1, OptionL2, M),
    merge_options(OptionL2,
                  [module(M),
                   on_trace(collect_violations(M))
                  ], OptionL),
    source_codewalk(OptionL),
    option_fromchk(OptionL, _, FromChk),
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
    generate_ctchecks(H, M, [], CTCheck),
    CTCheck \= _:true,
    clause(M:H, _, Clause),
    From = clause(Clause),
    call(FromChk, From),
    do_check_property_ctcheck(CTCheck, AssrErrorL),
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
    term_variables(Head, Vars),
    '$expand':mark_vars_non_fresh(Vars),
    check_property(Checker, H, IM, PM, M:Head, Issues),
    numbervars(Issues, 0, _),
    from_location(PFrom, PLoc),
    from_location(From, Loc).

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
    ["    "], Loc, ["In call to ~w:"-[PI], nl],
    prop_issue(ctchecks, CTChecks).
property_issue(defined, Loc/Prop) -->
    ["    "], Loc, ["~w is undefined"-[Prop], nl].
property_issue(is_prop, Loc/Prop) -->
    ["    "], Loc, ["~w is not a property"-[Prop], nl].

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

:- meta_predicate collect_violations(+,0,0,+).
collect_violations(M, CM:Goal, Caller, From) :-
    \+ black_list(Caller),
    implementation_module(CM:Goal, M),
    check_property_ctcheck(Goal, M, CM, Caller, CTChecks),
    CTChecks \= [],
    normalize_pi(Caller, CPI),
    update_fact_from(violations_db(CPI, CTChecks), From).

check_property_ctcheck(Goal, M, CM, Caller, AssrErrorL) :-
    tabled_generate_ctchecks(Goal, M, CM, Caller, CTCheck),
    CTCheck \= _:true,
    % Skip lack of assertions or assertions that will not
    % trigger violations
    do_check_property_ctcheck(CTCheck, AssrErrorL),
    ignore(( nb_current('$variable_names', VNL),
             maplist(set_variable_names, VNL)
           )).

set_variable_names(Name=Variable) :- ignore(Variable = '$VAR'(Name)).

:- dynamic
    assrt_error_db/2.

do_check_property_ctcheck(CTCheck, AssrErrorL) :-
    AssrError = assrchk(_, _),
    retractall(assrt_error_db(_, _)),
    intercept(CTCheck, AssrError, CTCheck, assertz(assrt_error_db(CTCheck, AssrError))),
    findall(CTCheck-AssrError,
            retract(assrt_error_db(CTCheck, AssrError)), CAssrErrorL),
    maplist(collect_assr_error(CTCheck), CAssrErrorL, AssrErrorL).

collect_assr_error(CTCheck, CTCheck-AssrError, AssrError).

checker_t(defined).
checker_t(is_prop).
checker_t(ctcheck).

check_property(defined, H, M, _, _, M:F/A) :-
    % Also reported by check_undefined, but is here to avoid dependency with
    % other analysis.
    functor(H, F, A),
    \+ current_predicate(M:F/A).
check_property(is_prop, H, M, _, _, M:F/A) :-
    resolve_calln(M:H, M:G),
    functor(G, F, A),
    \+ verif_is_property(M, F, A).
check_property(ctcheck, H, M, CM, Caller, (M:F/A)-CTChecks) :-
    % compile-time checks. Currently only compatibility checks.
    check_property_ctcheck(H, M, CM, Caller, CTChecks),
    CTChecks \= [],
    resolve_calln(M:H, M:G),
    functor(G, F, A).

var_info(A, P) -->
    ( { var_property(A, fresh(Fresh)) }
    ->[P=Fresh]
    ; []
    ).

rm_var_info(Var) :- del_attr(Var, '$var_info').

%!  tabled_generate_ctchecks(+Head, +Module, ?Context, +Caller, -Goal) is det
%
tabled_generate_ctchecks(H, M, CM, Caller, Goal) :-
    functor(H, F, A),
    functor(P, F, A),
    H =.. [F|Args],
    P =.. [F|PInf],
    foldl(var_info, Args, PInf, VInf, []),
    term_variables(H, Vars),
    maplist(rm_var_info, Vars),
    ( meta_call_goal(H, M, Caller, Meta)
    ->qualify_meta_goal(CM1:P, Meta, G)
    ; G = P
    ),
    generate_ctchecks(G, M, VInf, Goal),
    CM = CM1,
    P = H.

%!  generate_ctchecks(+Goal, +M, +VInf, -CTChecks) is det
%
%   Generate compile-time checks, currently only compatibility is checked, fails
%   if no ctchecks can be applied to Pred.
%
generate_ctchecks(Goal, M, VInf, CTChecks) :-
    % writeln(user_error, generate_ctchecks(Goal, M, VInf, CTChecks)),
    collect_assertions(Goal, M, ctcheck, AsrL),
    ( AsrL \= []
    ->maplist(wrap_asr_ctcheck(VInf), AsrL, PAsrL),
      CTChecks = check_assertions:ctcheck_goal(PAsrL, Goal)
    ; CTChecks = check_assertions:true
    ).

wrap_asr_ctcheck(VInf, Asr, ctcheck(VInf, Asr)).

:- public ctcheck_goal/2.
ctcheck_goal(AsrL, Goal) :-
    % To catch more errors we can use a partial evaluator instead of true:
    check_asrs(check_assertions:is_prop_ctcheck, AsrL, Goal, true).

assrt_op(call, entry).
assrt_op(call, calls).
assrt_op(call, pred).
assrt_op(call, prop).
assrt_op(succ, exit).
assrt_op(succ, success).
assrt_op(succ, pred).
assrt_op(succ, prop).
% assrt_op(glob, comp).
% assrt_op(glob, pred).

is_prop_ctcheck(Part, Asr) :-
    asr_aprop(Asr, type, Type, _),
    assrt_op(Part, Type), !.

% Trivial abstraction: Check for compatibility issues in properties,
% compatibility is an abstraction that makes static check decidable.  Here we
% lose precision but we gain computability of checks at compile-time.
% TBD: Formal demonstration. --EMM
assrt_lib:asr_aprop(ctcheck(VInf, Asr), Key, Prop, From) :-
    asr_aprop_ctcheck(Key, VInf, Asr, Prop, From).

asr_aprop_ctcheck(head, _, A, P, F) :- curr_prop_asr(head, P, F, A).
asr_aprop_ctcheck(stat, _, A, P, F) :- curr_prop_asr(stat, P, F, A).
asr_aprop_ctcheck(type, _, A, P, F) :- curr_prop_asr(type, P, F, A).
asr_aprop_ctcheck(dict, _, A, P, F) :- curr_prop_asr(dict, P, F, A).
asr_aprop_ctcheck(comm, _, A, P, F) :- curr_prop_asr(comm, P, F, A).
asr_aprop_ctcheck(comp, _, A, P, F) :- curr_prop_asr(comp, P, F, A).
asr_aprop_ctcheck(comp, _, A, P, F) :- curr_prop_asr(succ, P, F, A). % TBD: Key = succ
asr_aprop_ctcheck(Key,  L, Asr, Prop, From) :-
    asr_aprop_ctcheck_abstraction(Key, L, Asr, Prop, From).

prop_abstraction(call, true).
% prop_abstraction(succ, false).

asr_aprop_ctcheck_abstraction(Key, L, Asr, Prop, From) :-
    prop_abstraction(RKey, Fresh),
    curr_prop_asr(RKey, Prop, From, Asr),
    term_variables(Prop, Vars),
    ( member(Var=Fresh, L),
      member(Arg, Vars),
      Arg==Var
    ->Key = RKey
    ; Key = comp
    ).

verif_is_property(system, true, 0) :- !. % ignore true (identity)
verif_is_property(M, F, A) :-
    functor(H, F, A),
    prop_asr(H, M, _, prop, _, _, _).
