/*  Part of Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
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

:- module(check_wrong_dynamic, []).

:- use_module(checkers(checker)).
:- use_module(library(apply)).
:- use_module(library(check), []).
:- use_module(library(clambda)).
:- use_module(library(compact_pi_list)).
:- use_module(library(normalize_head)).
:- use_module(library(normalize_pi)).
:- use_module(library(checkable_predicate)).
:- use_module(library(current_defined_predicate)).
:- use_module(library(infer_meta_if_required)).
:- use_module(library(database_fact)).
:- use_module(library(extra_codewalk)).
:- use_module(library(location_utils)).
:- use_module(library(compact_goal)).
:- use_module(library(from_utils)).

:- multifile
    prolog:message//1,
    hide_wrong_dynamic/2,
    hide_var_dynamic_hook/2.

hide_var_dynamic(Call, M) :-
    ( Call1 = Call
    ; Call =.. [F|Args],
      atom_concat(F1, ' tabled', F),
      Call1 =.. [F1|Args]
    ),
    hide_var_dynamic_hook(Call1, M).

hide_var_dynamic_hook(list_strings(_), check).
hide_var_dynamic_hook(collect_non_mutually_exclusive(_, _, _, _), check_non_mutually_exclusive).
hide_var_dynamic_hook(current_used_use_module(_, _, _, _), check_imports).
hide_var_dynamic_hook(mutually_exclusive(_, _, _), check_non_mutually_exclusive).
hide_var_dynamic_hook(cu_caller_hook(_, _, _, _, _, _, _), check_trivial_fails).
hide_var_dynamic_hook(implemented_in(_, _, _), implemented_in).
hide_var_dynamic_hook(unfold_goal(_, _), ref_scenarios).
hide_var_dynamic_hook(match_head_clause(_, _), check_unused).
hide_var_dynamic_hook(unmarked(_, _, _, _, _), check_unused).
hide_var_dynamic_hook(duptype_elem(_, _, _, _, _, _), check_dupcode).
hide_var_dynamic_hook(bind_type_names(_, _, _, _), foreign_generator).
hide_var_dynamic_hook(bind_tn_clause(_, _, _, _), foreign_generator).
hide_var_dynamic_hook(call_ref(_, _), foreign_generator).
hide_var_dynamic_hook(no_backtrace_entry(_), filtered_backtrace).
hide_var_dynamic_hook(mark_to_head(_, _), check_unused).
hide_var_dynamic_hook(current_arc(_, _, _), check_unused).
hide_var_dynamic_hook(match_clause(_, _, _, _, _), ontrace).
hide_var_dynamic_hook(type_desc(_, _), foreign_props).
hide_var_dynamic_hook(prepare_results(_, _, _), checker).
hide_var_dynamic_hook(current_edge(_, _, _), check_unused).
hide_var_dynamic_hook(commited_retract(_), commited_retract).
hide_var_dynamic_hook(tabling(_, _), ntabling).
hide_var_dynamic_hook(match_head_body(_, _, _, _), abstract_interpreter).
hide_var_dynamic_hook(is_entry_caller(_), check_unused).
hide_var_dynamic_hook(caller_ptr(_, _, _), check_unused).
hide_var_dynamic_hook(match_head_1st_arg(_, _, _), check_text).
hide_var_dynamic_hook(current_head_ctcheck(_, _, _), check_assertions).
hide_var_dynamic_hook(unfold_call(_, _, _, _, _), unfold_calls).
hide_var_dynamic_hook(walk_from_assertion(_, _, _, _), extra_codewalk).
hide_var_dynamic_hook(current_clause_module_body(_, _), extra_codewalk).
hide_var_dynamic_hook(update_fact_from(_, _), from_utils).
hide_var_dynamic_hook(dupclauses(_), plprops).

:- dynamic
    wrong_dynamic_db/4,
    var_dynamic_db/2.

hide_wrong_dynamic(prolog_trace_interception(_, _, _, _), user).

cleanup_dynamic_db :-
    retractall(wrong_dynamic_db(_, _, _, _)),
    retractall(var_dynamic_db(_, _)).

checker:check(wrong_dynamic, Result, OptionL) :-
    check_wrong_dynamic(OptionL, Result).

check_wrong_dynamic(OptionL0, Pairs) :-
    infer_meta_if_required,
    merge_options(OptionL0,
                  [infer_meta_predicates(false),
                   autoload(false),
                   evaluate(false),
                   trace_reference(_),
                   on_etrace(collect_wrong_dynamic(M))],
                  OptionL),
    extra_walk_code(OptionL, M, FromChk),
    collect_result(M, FromChk, Pairs),
    cleanup_dynamic_db.

collect_result(M, FromChk, Pairs) :-
    findall(Type-(as_dynamic(DType)-((Loc/PI)-(MLoc/MPI))),
            ( current_static_as_dynamic(Type, DType, Loc, PI, From, MPI),
              from_location(From, MLoc)), Pairs, Pairs1),
    findall(warning-(dynamic_as_static-(Loc-PI)),
            current_dynamic_as_static(M:_, FromChk, Loc, PI), Pairs1, Pairs2),
    findall(warning-(var_as_dynamic-(PI-(Loc/CI))),
            ( retract(var_dynamic_db(PI, From)),
              check:predicate_indicator(From, CI, []),
              from_location(From, Loc)), Pairs2, []).

current_static_as_dynamic(Type, DType, Loc, PI, MFrom, MPI) :-
    wrong_dynamic_db(TypeDB, PI, MPI, MFrom),
    memberchk(TypeDB, [def, dec, retract]),
    PI = M:F/A,
    functor(H, F, A),
    \+ hide_wrong_dynamic(H, M),
    Ref = M:H,
    \+ predicate_property(Ref, dynamic),
    \+ predicate_property(Ref, volatile),
    ( predicate_property(Ref, number_of_clauses(N)),
      N > 0 ->
      Type = error,
      DType = static,
      predicate_location(Ref, Loc)
    ; Type = warning,
      DType  = unknown,
      once(property_location(PI, _, Loc))
    ).

:- meta_predicate current_dynamic_as_static(?, 1, -, ?).
current_dynamic_as_static(Ref, FromChk, Loc, PI) :-
    Ref = M:H,
    PI = M:F/A,
    ( var(H) ->
      current_defined_predicate(PI),
      functor(H, F, A)
    ; functor(H, F, A),
      current_defined_predicate(PI)
    ),
    \+ hide_wrong_dynamic(H, M),
    checkable_predicate(Ref),
    predicate_property(Ref, dynamic),
    property_from(PI, dynamic, From),
    call(FromChk, From),
    %% ignore predicates with the following properties:
    \+ predicate_property(Ref, multifile),
    % \+ predicate_property(Ref, exported),
    \+ predicate_property(Ref, public),
    \+ ( wrong_dynamic_db(Type, PI, _, _),
         memberchk(Type, [def, dec, retract])
       ),
    from_location(From, Loc).

prolog:message(acheck(wrong_dynamic, Type-List)) -->
    wrong_dynamic_message(Type, List).

as_dynamic(DType, Loc/PI-MLocPIs) -->
    ['\t'|Loc], ['~w ~q modified by'-[DType, PI], nl],
    foldl(show_locpi, MLocPIs).

show_locpi(Loc/PI) --> ['\t\t'|Loc], check:predicate(PI), [nl].

show_locci(Loc/CI) --> ['\t\t'|Loc], CI, [nl].

dynamic_as_static(Loc-PIs) -->
    {compact_pi_list(PIs, CPIs)},
    ['\t'|Loc], ['predicates ~w'-[CPIs], nl].

wrong_dynamic_message(as_dynamic(DType), LocPIs) -->
    ['Predicates are ~w, but never declared dynamic and modified:'-DType, nl],
    foldl(as_dynamic(DType), LocPIs).
wrong_dynamic_message(dynamic_as_static, LocPIs) -->
    ['Predicates declared dynamic, but never modified:', nl],
    foldl(dynamic_as_static, LocPIs).
wrong_dynamic_message(var_as_dynamic, PILocCIs) -->
    ['Predicates called with a variable in a module-sensitive argument:', nl],
    foldl(var_as_dynamic, PILocCIs).

var_as_dynamic(PI-LocCIs) -->
    ['\t~w called with a variable in'-[PI], nl],
    foldl(show_locci, LocCIs).

prolog:message(acheck(wrong_dynamic)) -->
    ['--------------------------', nl,
     'Wrong Dynamic Declarations', nl,
     '--------------------------', nl,
     'The predicates below present inconsistencies between its', nl,
     'usage and the dynamic declarations. Could be that they are', nl,
     'being used as dynamic without a proper declaration, being', nl,
     'declared as dynamic but never asserted, retracted, or using', nl,
     'a variable argument in a database predicate, making it', nl,
     'difficult to analyze.', nl, nl].

:- public collect_wrong_dynamic/4.
collect_wrong_dynamic(M, MGoal, Caller, From) :-
    record_location_meta(MGoal, M, From, \T^G^M^_^F^database_fact_ort(T,G,M,F),
                         record_location_wd(Caller)).

record_location_wd(Caller, M:Fact, CM, Type, MGoal, _, From) :-
    compact_goal(MGoal, Comp),
    normalize_pi(Comp, MPI),
    ( atom(M),
      callable(Fact)
    ->functor(Fact, F, A),
      record_location_goal(Fact, M, Type, CM, Comp, From),
      update_fact_from(wrong_dynamic_db(Type, M:F/A, MPI), From)
    ; \+ database_fact(Caller)
    ->normalize_head(Caller, CM:HC),
      \+ hide_var_dynamic(HC, CM),
      \+ ( predicate_property(CM:HC, meta_predicate(Meta)),
           arg(Pos, Meta, Spec),
           '$expand':meta_arg(Spec),
           arg(Pos, HC, Arg),
           Arg == Fact
         ),
      update_fact_from(var_dynamic_db(MPI), From)
    ; true
    ).
