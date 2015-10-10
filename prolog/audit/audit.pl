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

:- module(audit,
	  [showcheck/1, showcheck/2, checkall/0, checkall/1, checkallc/1,
	  check_results/2, check_results/3, report_list/2, full_report/1,
	  simple_report/1, available_checker/1]).

:- use_module(library(thread)).
:- use_module(library(clambda)).
:- use_module(library(group_pairs_or_sort)).
:- use_module(library(location_utils)).
:- use_module(library(infer_meta_if_required)).

:- multifile
    prepare_results/3,	% Custom preparation method
    check/3.		% Hook to define new analyses

:- public
    prepare_results/3,
    check/3.

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

cleanup_db :-
    cleanup_loc_dynamic(_, _, dynamic(_, _, _), _).

showcheck(Checker) :-
    showcheck(Checker, []),
    cleanup_db.

available_checker(Checker) :-
    clause(check(Checker, _, _), _).

showcheck(Checker, OptionL) :-
    check_results(Checker, Results, OptionL),
    full_report(Checker-Results).

full_report(Checker-Pairs) :-
    ( Pairs == []
    ->true
    ; print_message(warning, acheck(Checker)),
      simple_report(Checker-Pairs)
    ).

simple_report(Checker-Pairs) :-
    ( prepare_results(Checker, Pairs, Prepared)
    ->true
    ; Prepared = Pairs
    ),
    group_pairs_or_sort(Prepared, Results),
    maplist(report_analysis_results(Checker), Results).

report_analysis_results(Checker, Type-ResultL) :-
    maplist(report_record_message(Checker, Type), ResultL).

report_record_message(Checker, Type, Result) :-
    print_message(Type, acheck(Checker, Result)).

:- meta_predicate report_list(?,1).
report_list(Pairs, PrintMethod) :-
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Results),
    maplist(PrintMethod, Results).

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

checkall(OptionL) :- checkall(maplist, OptionL).

checkallc(OptionL) :- checkall(concurrent_maplist, OptionL).

:- meta_predicate checkall(2, +).
checkall(Mapper, OptionL) :-
    findall(C, available_checker(C), CL),
    setup_call_cleanup(infer_meta_if_required,
		       call(Mapper, checkeach(OptionL), CL),
		       cleanup_db).

:- public checkeach/2.
checkeach(OptionL, Checker) :-
     infocheck(Checker, T),
     showcheck(Checker, OptionL),
     donecheck(Checker, T).

check_results(Checker, Results, OptionL) :-
    current_prolog_flag(check_database_preds, F),
    setup_call_cleanup(
	set_prolog_flag(check_database_preds, true),
	check(Checker, Results, OptionL),
	set_prolog_flag(check_database_preds, F)).
