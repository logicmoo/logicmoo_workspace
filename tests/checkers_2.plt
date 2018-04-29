:- begin_tests(checkers_2).

:- use_module(library(infer_meta)).
:- use_module(library(checkers/checker)).
:- use_module(library(checkers/check_wrong_dynamic)).
:- use_module(checkers_hooks).

:- use_module(cwda).
:- use_module(cwdb).

test(cwd_2) :-
    check_results(wrong_dynamic, Results, [files([xtools/tests/cwda, xtools/tests/cwdb])]),
    assertion(Results = []).

:- end_tests(checkers_2).
