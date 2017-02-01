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

:- module(check_trivial_fails, []).

:- use_module(library(checkers/checker)).
:- use_module(library(apply)).
:- use_module(library(abstract_interpreter)).
:- use_module(library(from_utils)).
:- use_module(library(location_utils)).
:- use_module(library(extra_codewalk)).
:- use_module(library(dynamic_locations)).

:- multifile
    prolog:message//1.

:- dynamic
    trivial_fail/2,
    ai_cache_result/2.

checker:check(trivial_fails, Result, OptionL) :-
    check_trivial_fails(OptionL, Result).

check_trivial_fails(OptionL1, Pairs) :-
    select_option(match_ai(MatchAI), OptionL1, OptionL2, match_head),
    merge_options(OptionL2,
                  [infer_meta_predicates(false),
                   autoload(false),
                   evaluate(false),
                   trace_reference(_),
                   module_class([user, system, library])
                  ], OptionL),
    dynamic_locations(OptionL),
    extra_walk_code([on_etrace(collect_trivial_fails(MatchAI))|OptionL]),
    findall(warning-(Loc-Args),
            ( retract(trivial_fail(Args, From)),
              from_location(From, Loc)
            ), Pairs),
    cleanup_f,
    !.

cleanup_f :-
    retractall(ai_cache_result(_, _)).

prolog:message(acheck(trivial_fails)) -->
    ['-------------',nl,
     'Trivial Fails',nl,
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
ignore_predicate(_=_, _) :- !, fail.
ignore_predicate(H, M) :- \+ predicate_property(M:H, defined), !.
ignore_predicate(H, M) :-
    predicate_property(M:H, built_in),
    \+ predicate_property(M:H, dynamic), !.
ignore_predicate(H, M) :- predicate_property(M:H, multifile), !.
ignore_predicate(pce_class(_, _, template, _, _, _), pce_expansion).
ignore_predicate(property(system_source_prefix(_)), pce_host).
ignore_predicate(verbose, pce_expansion).
ignore_predicate(inferred_meta_pred(_, _, _), prolog_metainference).

:- public collect_trivial_fails/4.
:- meta_predicate collect_trivial_fails(7,+,+,+).
collect_trivial_fails(MatchAI, MGoal, Caller, From) :-
    record_location_meta(MGoal, _, From, all_call_refs, cu_caller_hook(MatchAI, Caller)).

cu_caller_hook(MatchAI, Caller, MGoal, CM, Type, _, _, From) :-
    atom(CM),
    MGoal = M:H,
    callable(H),
    \+ ignore_predicate(H, M),
    variant_sha1(ai(H, CM), Hash),
    ( ai_cache_result(Hash, Data) -> true
    ; once(abstract_interpreter(CM:H, MatchAI, [location(From)], Data)),
      assertz(ai_cache_result(Hash, Data))
    ),
    Data = data(N, S, fail),
    ( N = 0
    ->Args = trivial_fail(Caller, MGoal)
    ; Args = failure(Caller, MGoal, S)
    ),
    memberchk(Type, [lit, use]),
    update_fact_from(trivial_fail(Args), From).
