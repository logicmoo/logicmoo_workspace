:- begin_tests(checkers).

:- use_module(library(infer_meta)).
:- use_module(library(checkers/checker)).
:- use_module(library(checkers/check_wrong_dynamic)).

checkable_predicate:application_predicate(M:_) :-
    application_module(M).

application_module(cwda).
application_module(cwdb).

:- use_module(cwda).
:- use_module(cwdb).
:- use_module(cwdc).
:- use_module(cwde).

test(cwd_1) :-
    check_results(wrong_dynamic, Results, [files([xtools/tests/cwdb])]),
    assertion(Results = [_]).

test(cwd_2) :-
    check_results(wrong_dynamic, Results, [files([xtools/tests/cwda, xtools/tests/cwdb])]),
    assertion(Results = []).

test(cwd_3) :-
    check_results(wrong_dynamic, Results, [files([xtools/tests/cwda])]),
    assertion(Results = []).

test(cwd_4) :- % There is an issue that does not refers to module cwda
    check_results(wrong_dynamic, Results, [module(cwda), files([xtools/tests/cwdb])]),
    assertion(Results = []).

test(cwd_5, [setup(cleanup_inferred_meta)]) :-
    check_results(wrong_dynamic, Results, [files([xtools/tests/cwdc])]),
    assertion(Results = []).

test(cwd_6) :-
    check_results(wrong_dynamic, Results, [module(cwde)]),
    assertion(Results = [_]).

:- end_tests(checkers).
