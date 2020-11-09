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

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(yall)).
:- use_module(library(pairs)).
:- use_module(library(checkers/checker)).
:- use_module(library(assertions)).
:- use_module(library(check), []).
:- use_module(library(codewalk)).
:- use_module(library(compact_pi_list)).
:- use_module(library(intercept)).
:- use_module(library(normalize_pi)).
:- use_module(library(resolve_calln)).
:- use_module(library(location_utils)).
:- use_module(library(from_utils)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(assertions)).
:- use_module(library(option_utils)).
:- use_module(library(checkable_predicate)).
:- use_module(library(ctrtchecks)).
:- use_module(library(rtchecks_rt)).
:- use_module(library(rtchecks_utils), []).

:- dynamic
       violations_db/3.

:- multifile
       ignore_predicate/2,
       prolog:message//1.

% :- table
%        generate_ctchecks/4,
%        do_check_property_ctcheck/2.

checker:check(assertions, Result, Options) :-
    cleanup_db,
    check_assertions(Options, Result).

cleanup_db :-
    retractall(violations_db(_, _, _)).

check_assertions(Options1, Pairs) :-
    foldl(select_option_default,
          [method(Method1)-clause],
          Options1, Options2),
    ( \+ memberchk(Method1, [source, clause]) % only these methods are supported
    ->Method = clause,
      print_message(
          warning,
          format("Method `~w' not supported, using `~w' instead",
                 [Method1, Method]))
    ; Method = Method1
    ),
    merge_options(Options2,
                  [method(Method),
                   trace_variables([non_fresh]),
                   on_trace(collect_violations)
                  ], Options),
    option_module_files(Options, MFileD),
    walk_code([module_files(MFileD)|Options]),
    findall(error-Issue,
            ( retract(violations_db(CPI, CTChecks, From)),
              from_location(From, Loc),
              Issue = body(Loc-CPI)-CTChecks
            ; current_head_ctcheck(MFileD, Issue)
            ), Pairs, Props),
    prop_ctcheck(MFileD, Props).

current_head_ctcheck(MFileD, head(Loc-PI)-AssrErrorL) :-
    PI=M:F/A,
    get_dict(M, MFileD, FileD),
    current_predicate(M:F/A),
    functor(H, F, A),
    MH = M:H,
    \+ predicate_property(MH, imported_from(_)),
    \+ is_built_in(MH),
    \+ predicate_property(MH, foreign),
    generate_ctchecks(H, M, [], CTCheck),
    CTCheck \= _:true,
    clause(MH, _, Clause),
    clause_property(Clause, file(File)),
    get_dict(File, FileD, _),
    do_check_property_ctcheck(CTCheck, AssrErrorL),
    % Although we have duplicated logic, we don't call check_property_ctcheck/3
    % here because is too slow:
    % check_property_ctcheck(H, M, CTChecks),
    AssrErrorL \= [],
    from_location(clause(Clause), Loc).

prop_ctcheck(MFileD, Trans) :-
    findall(Pair, current_prop_ctcheck(MFileD, Pair), Pairs),
    sort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Groups),
    maplist([K-L, (error-(prop(G)-K))]
            >>group_pairs_by_key(L, G), Groups, Trans).

current_prop_ctcheck(MFileD, (Checker-PLoc/Issues)-(Loc-PI)) :-
    prop_asr(head, M:Head, From, Asr),
    get_dict(M, MFileD, FileD),
    from_to_file(From, File),
    get_dict(File, FileD, _),
    functor(Head, HF,HA),
    PI=M:HF/HA,
    ( member(Part, [comp, call, succ, glob]),
      curr_prop_asr(Part, PM:Prop, PFrom, Asr),
      resolve_head(Prop, PM, N:H)
    ),
    checker_t(Checker),
    term_variables(Head, Vars),
    '$expand':mark_vars_non_fresh(Vars),
    check_property(Checker, H, N, M:Head, Issues),
    numbervars(Issues, 0, _),
    from_location(PFrom, PLoc),
    from_location(From, Loc).

resolve_head(V, M, M:V) :-
    var(V),
    % Note: this should not happen
    !,
    fail.
resolve_head(M:H1, _, H) :- !,
    resolve_head(H1, M, H).
resolve_head((A,B), M, H) :- !,
    ( resolve_head(A, M, H)
    ; resolve_head(B, M, H)
    ).
resolve_head((A;B), M, H) :-
    !,
    %% ERROR: When Checker=ctcheck, we are reporting an issue even if one of the
    %% next two branches does not have any:
    ( resolve_head(A, M, H)
    ; resolve_head(B, M, H)
    ).
resolve_head(H, M, M:H).

prolog:message(acheck(assertions)) -->
    ['Check asssertions',nl,
     '-----------------',nl,
     'The predicates contain assertions that are inconsistent', nl,
     'with the implementation.', nl, nl].
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

ignore_predicate(assertion_head(_, _, _, _, _, _, _), assertions).
ignore_predicate(_, M) :- ignore_module(M).

ignore_module(extend_args).

% Issues in the assertion body will be reported when checking properties.
ignore_predicate(M:Call) :- ignore_predicate(Call, M).

:- public collect_violations/3.

%!  collect_violations(+Module, :Goal, +Caller, +From)
%
%   Collect the assertion violations of a given Goal. Note that Module refer to
%   the module of the source code, while Goal could have another context module,
%   for instance, if module qualification was used in the body of a predicate.

:- meta_predicate collect_violations(0,0,+).
collect_violations(M:Goal, Caller, From) :-
    ( \+ ignore_predicate(Caller),
      check_property_ctcheck(Goal, M, Caller, CTChecks),
      CTChecks \= []
    ->normalize_pi(Caller, CPI),
      update_fact_from(violations_db(CPI, CTChecks), From)
    ; true
    ).

check_property_ctcheck(Goal, M, Caller, AssrErrorL) :-
    tabled_generate_ctchecks(Goal, M, Caller, CTCheck),
    CTCheck \= _:true,
    % Skip lack of assertions or assertions that will not
    % trigger violations
    do_check_property_ctcheck(CTCheck, AssrErrorL),
    ignore(( nb_current('$variable_names', VNL),
             maplist(set_variable_names, VNL)
           )).

set_variable_names(Name=Variable) :- ignore(Variable = '$VAR'(Name)).

do_check_property_ctcheck(CTCheck, AssrErrorL) :-
    SErrors = s([]),
    intercept(catch(CTCheck, Error, send_signal(Error)),
              AssrError, cpc_handler(AssrError), SErrors-CTCheck),
    SErrors = s(CAssrErrorL),
    maplist(collect_assr_error(CTCheck), CAssrErrorL, AssrErrorL).

cpc_handler(AssrError, SErrors-CTCheck) :-
    SErrors = s(CAssrErrorL1),
    nb_setarg(1, SErrors, [CTCheck-AssrError|CAssrErrorL1]).

collect_assr_error(CTCheck, CTCheck-AssrError, AssrError).

checker_t(defined).
checker_t(is_prop).
checker_t(ctcheck).

check_property(defined, H, M, _, M:F/A) :-
    % Also reported by check_undefined, but is here to avoid dependency with
    % other analysis.
    functor(H, F, A),
    \+ current_predicate(M:F/A).
check_property(is_prop, H, M, _, M:F/A) :-
    resolve_calln(M:H, M:G),
    functor(G, F, A),
    \+ verif_is_property(M, F, A).
check_property(ctcheck, H, M, Caller, (M:F/A)-CTChecks) :-
    % compile-time checks. Currently only compatibility checks.
    check_property_ctcheck(H, M, Caller, CTChecks),
    CTChecks \= [],
    resolve_calln(M:H, M:G),
    functor(G, F, A).

var_info(A, P) -->
    ( { var_property(A, fresh(Fresh)) }
    ->[P=Fresh]
    ; []
    ).

%!  tabled_generate_ctchecks(+Head, ?Context, +Caller, -Goal) is det
%
tabled_generate_ctchecks(H, M, Caller, Goal) :-
    functor(H, F, A),
    functor(P, F, A),
    H =.. [F|Args],
    P =.. [F|PInf],
    foldl(var_info, Args, PInf, VInf, []),
    term_attvars(M:H, Vars),
    maplist(del_attrs, Vars),
    ( meta_call_goal(H, M, Caller, Meta)
    ->qualify_meta_goal(CM:P, Meta, G)
    ; G = P
    ),
    generate_ctchecks(G, M, VInf, Goal),
    CM = M,
    P = H.

%!  generate_ctchecks(+Goal, +Context, +VInf, -CTChecks) is det
%
%   Generate compile-time checks, currently only compatibility is checked, fails
%   if no ctchecks can be applied to Pred. VInf contains information about fresh
%   variables.
%
generate_ctchecks(Goal, M, VInf, CTChecks) :-
    collect_assertions(ct, Goal, M, AsrL),
    ( AsrL \= []
    ->maplist(wrap_asr_ctcheck(VInf), AsrL, PAsrL),
      CTChecks = ctrtchecks:check_call(ct, PAsrL, M:Goal)
    ; CTChecks = check_assertions:true
    ).

wrap_asr_ctcheck(VInf, Asr, ctcheck(VInf, Asr)).

assertions:asr_aprop(ctcheck(VInf, Asr), Key, Prop, From) :-
    asr_aprop_ctcheck(Key, VInf, Asr, Prop, From).

%!  asr_aprop_ctcheck(Asr, Section, Property, From)
%
%   Assertion abstraction: If we can not determine the mode at compile time, at
%   least check for compatibility (instead of instantiation).  This abstraction
%   makes static check decidable, the tradeoff is that we lose precision but we
%   gain computability of checks at compile-time.

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
