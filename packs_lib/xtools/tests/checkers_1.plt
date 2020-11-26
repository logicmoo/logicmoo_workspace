:- begin_tests(checkers_1).

:- use_module(library(infer_meta)).
:- use_module(library(checkers/checker)).
:- use_module(library(checkers/check_wrong_dynamic)).
:- use_module(checkers_hooks).

:- use_module(cwda).
:- use_module(cwdb).

test(cwd_1) :-
    check_results(wrong_dynamic, Results, [files([xtools/tests/cwdb])]),
    assertion(Results = [_]).

:- end_tests(checkers_1).
