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

:- module(check_trivial_fails, []).

:- use_module(library(checkers/checker)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(abstract_interpreter)).
:- use_module(library(from_utils)).
:- use_module(library(location_utils)).
:- use_module(library(codewalk)).
:- use_module(library(dynamic_locations)).
:- use_module(library(checkable_predicate)).

:- multifile
    prolog:message//1.

:- dynamic
    trivial_fail/2,
    ai_cache_result/2.

checker:check(trivial_fails, Result, Options) :-
    check_trivial_fails(Options, Result).

check_trivial_fails(Options1, Pairs) :-
    select_option(match_ai(MatchAI), Options1, Options2, match_head),
    merge_options(Options2,
                  [infer_meta_predicates(false),
                   autoload(false),
                   evaluate(false),
                   trace_reference(_),
                   module_class([user, system, library])
                  ], Options),
    dynamic_locations(Options),
    walk_code([on_trace(collect_trivial_fails(MatchAI))|Options]),
    findall(warning-(Loc-Args),
            ( retract(trivial_fail(Args, From)),
              from_location(From, Loc)
            ), Pairs),
    cleanup_f,
    !.

cleanup_f :-
    retractall(ai_cache_result(_, _)).

prolog:message(acheck(trivial_fails)) -->
    ['Trivial Fails',nl,
     '-------------',nl,
     'The literals below always fails, due to there are no', nl,
     'matching clauses for such calls, which is reported as', nl,
     'a trivial fail, or because all paths leads to dead', nl,
     'points, in such case the warning reports also the', nl,
     'biggest failure chain found', nl, nl].
prolog:message(acheck(trivial_fails, Loc-Args)) -->
    Loc,
    foldl(show_trivial_fail, Args).

show_trivial_fail(trivial_fail(Caller, MGoal)) -->
    ['In ~q, trivial fail for ~q'-[Caller, MGoal], nl].
show_trivial_fail(failure(Caller, MGoal, S)) -->
    ['In ~q, failure for ~q, biggest failure chain was ~q'-[Caller, MGoal, S], nl].

:- multifile ignore_predicate/2.
ignore_predicate(H, M) :- \+ predicate_property(M:H, defined), !.
ignore_predicate(H, M) :- predicate_property(M:H, multifile), !.
ignore_predicate(pce_class(_, _, template, _, _, _), pce_expansion).
ignore_predicate(property(system_source_prefix(_)), pce_host).
ignore_predicate(verbose, pce_expansion).
ignore_predicate(inferred_meta_pred(_, _, _), prolog_metainference).

:- public collect_trivial_fails/4.
:- meta_predicate collect_trivial_fails(7,+,+,+).
collect_trivial_fails(MatchAI, MGoal, Caller, From) :-
    record_location_meta(MGoal, _, From, all_call_refs, cu_caller_hook(MatchAI, Caller)).

cu_caller_hook(MatchAI, Caller, H, CM, Type, _, _, From) :-
    ground(CM),
    callable(H),
    predicate_property(CM:H, implementation_module(M)),
    MGoal = M:H,
    checkable_predicate(MGoal),
    \+ ignore_predicate(H, M),
    copy_term_nat(CM:H, Term),
    variant_sha1(Term, Hash),
    ( ai_cache_result(Hash, Data)
    ->true
    ; ( abstract_interpreter(Term, MatchAI, [location(From)], Result)
      ->Data = true(Result)
      ; Data = fail
      ),
      assertz(ai_cache_result(Hash, Data))
    ),
    Data = fail,
    N = 0,
    S = [],
    ( N = 0
    ->Args = trivial_fail(Caller, MGoal)
    ; Args = failure(Caller, MGoal, S) % General failure not implemented yet
    ),
    memberchk(Type, [lit, use]),
    update_fact_from(trivial_fail(Args), From).
