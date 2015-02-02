:- module(audit, [showcheck/1, showcheck/2, showcheck/3, checkall/0, checkall/2,
		  checkallc/2, check_results/2, check_results/4, report_list/2,
		  full_report/1, simple_report/1, available_checker/1]).

:- use_module(library(thread)).
:- use_module(library(clambda)).
:- use_module(library(group_pairs_or_sort)).
:- use_module(library(location_utils)).

:- multifile
    prepare_results/3,	% Custom preparation method
    check/4.		% Hook to a new analysis

cleanup_db :-
    cleanup_locations(_, _, dynamic(_, _, _), _).

showcheck(Checker, OptionL) :-
    showcheck(Checker, _, OptionL),
    cleanup_db.

showcheck(Checker) :-
    showcheck(Checker, []).

available_checker(Checker) :-
    clause(check(Checker, _, _, _), _).

showcheck(Checker, Ref, OptionL) :-
    check_results(Checker, Ref, Results, OptionL),
    full_report(Checker-Results).

full_report(Checker-Pairs) :-
    ( Pairs == []
    ->true
    ; print_message(warning, acheck(Checker)),
      simple_report(Checker-Pairs)
    ).

simple_report(Checker-Pairs) :-
    ( audit:prepare_results(Checker, Pairs, Prepared)
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
    check_results(Checker, _, Result, []).

checkall :-
    checkall(_, []).

infocheck(Checker) :-
    print_message(information, format('Running Checker ~w', [Checker])).

checkall(Ref, OptionL) :-
    available_checker(Checker),
    infocheck(Checker),
    showcheck(Checker, Ref, OptionL),
    fail.
checkall(_, _) :-
    cleanup_db.

checkallc(Ref, OptionL) :-
    findall(C, available_checker(C), CL),
    concurrent_maplist([Ref, OptionL]+\ C^ ( infocheck(C),
					     showcheck(C, Ref, OptionL)
					   ), CL),
    cleanup_db.

check_results(Checker, Ref, Results, OptionL) :-
    current_prolog_flag(check_database_preds, F),
    setup_call_cleanup(
	set_prolog_flag(check_database_preds, true),
	check(Checker, Ref, Results, OptionL),
	set_prolog_flag(check_database_preds, F)).
