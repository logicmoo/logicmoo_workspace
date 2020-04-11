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

:- module(check_wrong_dynamic, []).

:- use_module(library(checkers/checker)).
:- use_module(library(apply)).
:- use_module(library(check), []).
:- use_module(library(clambda)).
:- use_module(library(compact_pi_list)).
:- use_module(library(normalize_head)).
:- use_module(library(normalize_pi)).
:- use_module(library(checkable_predicate)).
:- use_module(library(current_defined_predicate)).
:- use_module(library(database_fact)).
:- use_module(library(codewalk)).
:- use_module(library(location_utils)).
:- use_module(library(predicate_from)).
:- use_module(library(option_utils)).
:- use_module(library(compact_goal)).
:- use_module(library(from_utils)).

:- multifile
    prolog:message//1,
    hide_wrong_dynamic/2,
    hide_var_dynamic_hook/2.

hide_var_dynamic(Call1, M) :-
    ( \+ ( current_module(M:'$tabled'/1),
           M:'$tabled'(Call1)
         )
    ->Call = Call1
    ; Call1 =.. [F1|Args],
      atom_concat(F, ' tabled', F1),
      Call =.. [F|Args]
    ),
    hide_var_dynamic_hook(Call, M).

hide_var_dynamic_hook(match_clause(_, _, _, _, _, _, _), ontrace).
hide_var_dynamic_hook(collect_non_mutually_exclusive(_, _, _, _), check_non_mutually_exclusive).
hide_var_dynamic_hook(ignore_import(_, _), check_imports).
hide_var_dynamic_hook(current_head_body(_, _, _, _), codewalk_clause).
hide_var_dynamic_hook(walk_from_assertion(_, _, _), codewalk_prolog).
hide_var_dynamic_hook(current_head_ctcheck(_, _), check_assertions).
hide_var_dynamic_hook(unfold_call(_, _, _, _, _), unfold_calls).
hide_var_dynamic_hook(no_backtrace_entry(_), filtered_backtrace).
hide_var_dynamic_hook(det_clause(_, _), check_useless_cuts).
hide_var_dynamic_hook(dyn_rtcheck_record(_, _), rtchecks).
hide_var_dynamic_hook(unrtcheck2(_, _), rtchecks).

:- dynamic
    wrong_dynamic_db/4,
    var_dynamic_db/2.

hide_wrong_dynamic(prolog_trace_interception(_, _, _, _), user).
hide_wrong_dynamic(Call, _) :-
    functor(Call, Name, _),
    member(Prefix, ['__wrap$',
                    '$wrap$']),
    atom_concat(Prefix, _, Name).

cleanup_dynamic_db :-
    retractall(wrong_dynamic_db(_, _, _, _)),
    retractall(var_dynamic_db(_, _)).

checker:check(wrong_dynamic, Result, Options) :-
    check_wrong_dynamic(Options, Result).

check_wrong_dynamic(Options1, Pairs) :-
    option(module(M), Options1, M),
    merge_options(Options1,
                  [infer_meta_predicates(false),
                   autoload(false),
                   evaluate(false),
                   trace_variables([meta_arg,
                                    non_fresh]),
                   module_class([user, system, library]),
                   on_trace(collect_wrong_dynamic(M))],
                  Options),
    option_module_files(Options, MFileD),
    walk_code([module_files(MFileD)|Options]),
    collect_result(MFileD, Pairs),
    cleanup_dynamic_db.

collect_result(MFileD, Pairs) :-
    findall(Type-(modified_nondynamic(DType)-((Loc/PI)-(MLoc/MPI))),
            ( current_modified_nondynamic(Type, DType, Loc, PI, From, MPI),
              from_location(From, MLoc)), Pairs, Pairs1),
    findall(warning-(unmodified_dynamic-(Loc-PI)),
            current_unmodified_dynamic(MFileD, Loc, PI), Pairs1, Pairs2),
    findall(warning-(var_as_dynamic-(PI-(Loc/CI))),
            ( retract(var_dynamic_db(PI, From)),
              check:predicate_indicator(From, CI, []),
              from_location(From, Loc)), Pairs2, []).

current_modified_nondynamic(Type, DType, Loc, PI, MFrom, MPI) :-
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

current_unmodified_dynamic(MFileD, Loc, PI) :-
    Ref = M:H,
    PI = M:F/A,
    get_dict(M, MFileD, FileD),
    current_defined_predicate(Ref),
    \+ hide_wrong_dynamic(H, M),
    checkable_predicate(Ref),
    predicate_property(Ref, dynamic),
    functor(H, F, A),
    once(( property_from(PI, dynamic, From)
         ; predicate_from(Ref, From)
         )),
    from_to_file(From, File),
    get_dict(File, FileD, _),
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

modified_nondynamic(DType, Loc/PI-MLocPIs) -->
    ['\t'|Loc], ['~w ~q modified by'-[DType, PI], nl],
    foldl(show_locpi, MLocPIs).

show_locpi(Loc/PI) --> ['\t\t'|Loc], check:predicate(PI), [nl].

show_locci(Loc/CI) --> ['\t\t'|Loc], CI, [nl].

unmodified_dynamic(Loc-PIs) -->
    {compact_pi_list(PIs, CPIs)},
    ['\t'|Loc], ['predicates ~w'-[CPIs], nl].

wrong_dynamic_message(modified_nondynamic(DType), LocPIs) -->
    ['Predicates are ~w, but never declared dynamic and modified:'-DType, nl],
    foldl(modified_nondynamic(DType), LocPIs).
wrong_dynamic_message(unmodified_dynamic, LocPIs) -->
    ['Predicates declared dynamic, but never modified:', nl],
    foldl(unmodified_dynamic, LocPIs).
wrong_dynamic_message(var_as_dynamic, PILocCIs) -->
    ['Predicates called with a variable in a module-sensitive argument:', nl],
    foldl(var_as_dynamic, PILocCIs).

var_as_dynamic(PI-LocCIs) -->
    ['\t~w called with a variable in'-[PI], nl],
    foldl(show_locci, LocCIs).

prolog:message(acheck(wrong_dynamic)) -->
    ['Wrong Dynamic Declarations', nl,
     '--------------------------', nl,
     'The predicates present inconsistencies between its', nl,
     'usage and the dynamic declarations. Could be that they are', nl,
     'being used as dynamic without a proper declaration, being', nl,
     'declared as dynamic but never asserted, retracted, or using', nl,
     'a variable argument in a database predicate, making it', nl,
     'difficult to analyze.', nl, nl].

:- public collect_wrong_dynamic/4.
:- meta_predicate collect_wrong_dynamic(?,0,+,+).
collect_wrong_dynamic(M, MGoal, Caller, From) :-
    ignore(record_location_meta(MGoal, M, From, \T^G^MG^_^F^database_fact_ort(T,G,MG,F),
                                record_location_wd(Caller))).

record_location_wd(Caller, Fact, CM, Type, MGoal, _, From) :-
    compact_goal(MGoal, Comp),
    normalize_pi(Comp, MPI),
    ( callable(Fact),
      atom(CM)
    ->functor(Fact, F, A),
      predicate_property(CM:Fact, implementation_module(M)),
      record_location_goal(Fact, M, Type, CM, Comp, From),
      update_fact_from(wrong_dynamic_db(Type, M:F/A, MPI), From)
    ; \+ database_fact(Caller)
    ->normalize_head(Caller, CMHC),
      strip_module(CMHC, CM, HC),
      \+ hide_var_dynamic(HC, CM),
      \+ ( get_attr(Fact, meta_args, Spec),
           prolog_metainference:is_meta(Spec)
         ),
      update_fact_from(var_dynamic_db(MPI), From)
    ; true
    ).
