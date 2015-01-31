:- module(audit, [showcheck/1, showcheck/3, available_checker/1,
		  report_list/2, full_report/1, simple_report/1,
		  check_results/2, check_results/4,
		  checkall/0, checkall/2]).

:- use_module(library(group_pairs_or_sort)).

:- multifile
    audit:prepare_results/3,	% Custom preparation method
    audit:check/4.		% Hook to a new analysis

showcheck(Analysis) :-
    showcheck(Analysis, _, []).

available_checker(Checker) :-
    clause(check(Checker, _, _, _), _).

showcheck(Analysis, Ref, OptionL) :-
    check_results(Analysis, Ref, Results, OptionL),
    full_report(Analysis-Results).

full_report(Analysis-Pairs) :-
    ( Pairs == []
    ->true
    ; print_message(warning, acheck(Analysis)),
      simple_report(Analysis-Pairs)
    ).

simple_report(Analysis-Pairs) :-
    ( audit:prepare_results(Analysis, Pairs, Prepared)
    ->true
    ; Prepared = Pairs
    ),
    group_pairs_or_sort(Prepared, Results),
    maplist(report_analysis_results(Analysis), Results).

report_analysis_results(Analysis, Type-ResultL) :-
    maplist(report_record_message(Analysis, Type), ResultL).

report_record_message(Analysis, Type, Result) :-
    print_message(Type, acheck(Analysis, Result)).

:- meta_predicate report_list(?,1).
report_list(Pairs, PrintMethod) :-
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Results),
    maplist(PrintMethod, Results).

check_results(Analysis, Result) :-
    check_results(Analysis, _, Result, []).

checkall :-
    checkall(_, []).

checkall(Ref, OptionL) :-
    available_checker(Checker),
    print_message(information, format('Running Checker ~w', [Checker])),
    showcheck(Checker, Ref, OptionL),
    fail.
checkall(_, _).

check_results(Analysis, Ref, Results, OptionL) :-
    current_prolog_flag(check_database_preds, F),
    setup_call_cleanup(
	set_prolog_flag(check_database_preds, true),
	check(Analysis, Ref, Results, OptionL),
	set_prolog_flag(check_database_preds, F)).
