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

:- module(checker,
          [check_wrapper/1, showcheck/1, showcheck/2, checkallc/1, checkeach/2,
           checkall/0, checkall/1, body_report/1, body_report/2, full_report/1,
           check/3, check_results/2, check_results/3, available_checker/1,
           head_report/1
          ]).

:- use_module(library(lists)).
:- use_module(library(atomics_atom)).
:- use_module(library(thread)).
:- use_module(library(group_pairs_or_sort)).
:- use_module(library(infer_meta)).
:- use_module(library(dynamic_locations)).
% This provides extra information to prolog_codewalk but will not be required if
% you use source_codewalk instead:
:- use_module(library(ai_extra_clauses), []).

user:file_search_path(checkers, library(checkers)).

:- multifile
    prepare_results/3,  % Custom preparation method
    check/3.            % Hook to define new analyses

:- public
    prepare_results/3,
    check/3.

:- meta_predicate
    with_prolog_flag(+, +, 0 ).

prolog:called_by(Goal, _, M, [M:Macro]) :-
    functor(Goal, F, A),
    \+ blob(F, closure),
    once(atomics_atom(['__aux_', Name, '/', AN, '_', CF, '+', EN], F)),
    atom_number(AN, N),
    atom_number(EN, E),
    A =:= E + N - 1,
    length(EL, E),
    Goal =.. [F|AL],
    append(TL, EL, AL),
    trim_args(Name, N, C, CF, EL, [C|TL], TT),
    Macro =.. [Name|TT].

% This is a kludge to bypass the fact that maplist/N, N>5 does not exist:
trim_args(maplist, N, C, CF, EL, AL, AT) :-
    N > 5, !,
    length(AT, 5),
    append(AT, AR, AL),
    length(AR, RN),
    length(ER, RN),
    append(ER, EL, CL),
    C =.. [CF|CL].
trim_args(_, _, C, CF, EL, AL, AL) :-
    C =.. [CF|EL].

/*
user:prolog_clause_name(Ref, Name) :-
    nth_clause(M:H, N, Ref), !,
    functor(H, F, A),
    Name = M:F/A-N.
user:prolog_clause_name(Ref, Name) :-
    clause_property(Ref, erased), !,
    clause_property(Ref, predicate(M:PI)),
    Name = erased(M:PI).
user:prolog_clause_name(_, '<meta-call>').
*/

showcheck(Checker) :-
    showcheck(Checker, []).

available_checker(Checker) :-
    clause(check(Checker, _, _), _).

showcheck(Checker, Options) :-
    check_results(Checker, Results, Options),
    full_report(Checker-Results).

with_prolog_flag(Flag, Value, Goal) :-
    current_prolog_flag(Flag, Old),
    setup_call_cleanup(
        set_prolog_flag(Flag, Value),
        Goal,
        set_prolog_flag(Flag, Old)).

head_report(Checker-Pairs) :-
    ( Pairs \= []
    ->print_message(warning, acheck(Checker))
    ; true
    ).

full_report(CheckerPairs) :-
    head_report(CheckerPairs),
    body_report(CheckerPairs).

body_report(CheckerPairs) :-
    body_report(CheckerPairs, report_record_message).

:- meta_predicate body_report(+, 3).

body_report(Checker-Pairs, Printer) :-
    ( prepare_results(Checker, Pairs, Prepared)
    ->true
    ; Prepared = Pairs
    ),
    group_pairs_or_sort(Prepared, Results),
    maplist(report_analysis_results(Checker, Printer), Results).
    
report_analysis_results(Checker, Printer, Type-ResultL) :-
    maplist(call(Printer, Checker, Type), ResultL).

report_record_message(Checker, Type, Result) :-
    \+ ( copy_term_nat(acheck(Checker, Result), Term),
         numbervars(Term, 0, _,
                    [ singletons(true)
                    ]),
         print_message(Type, Term),
         fail
       ).

check_results(Checker, Result) :-
    check_results(Checker, Result, []).

checkall :-
    checkall([]).

infocheck(Checker, T) :-
    get_time(T),
    print_message(information, format('Running Checker ~w', [Checker])).

donecheck(Checker, T) :-
    get_time(T2),
    DT is T2-T,
    print_message(information, format('Done ~w (~3f s)', [Checker, DT])).

checkall(Options) :- checkall(maplist, Options).

checkallc(Options) :- checkall(concurrent_maplist, Options).

:- meta_predicate checkall(2, +).
checkall(Mapper, Options) :-
    findall(C, available_checker(C), CL),
    check_wrapper(call(Mapper, checkeach(Options), CL)).

:- meta_predicate check_wrapper(0 ).

check_wrapper(Goal) :-
    with_prolog_flag(
        check_database_preds, true,
        with_prolog_flag(
            verbose, silent,
            setup_call_cleanup(
                infer_meta_if_required,
                Goal,
                cleanup_dynl_db))).

checkeach(Options, Checker) :-
    infocheck(Checker, T),
    check(Checker, Results, Options),
    full_report(Checker-Results),
    donecheck(Checker, T).

check_results(Checker, Results, Options) :-
    check_wrapper(check(Checker, Results, Options)).
