:- module(audit,
	  [showcheck/1, showcheck/2, checkall/0, checkall/1, checkallc/1,
	  check_results/2, check_results/3, report_list/2, full_report/1,
	  simple_report/1, available_checker/1]).

:- use_module(library(thread)).
:- use_module(library(clambda)).
:- use_module(library(group_pairs_or_sort)).
:- use_module(library(location_utils)).

:- multifile
    prepare_results/3,	% Custom preparation method
    check/3.		% Hook to define new analyses

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

% TODO: Ref argument is odd, may is better to pass only the context module that
% we want to analyze, and may be in OptionL instead of in an extra argument.
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

checkall(OptionL) :-
    available_checker(Checker),
    infocheck(Checker, T),
    showcheck(Checker, OptionL),
    donecheck(Checker, T),
    fail.
checkall(_) :-
    cleanup_db.

checkallc(OptionL) :-
    findall(C, available_checker(C), CL),
    concurrent_maplist([OptionL]+\ C^ ( infocheck(C, T),
					showcheck(C, OptionL),
					donecheck(C, T)
				      ), CL),
    cleanup_db.

check_results(Checker, Results, OptionL) :-
    current_prolog_flag(check_database_preds, F),
    setup_call_cleanup(
	set_prolog_flag(check_database_preds, true),
	check(Checker, Results, OptionL),
	set_prolog_flag(check_database_preds, F)).
