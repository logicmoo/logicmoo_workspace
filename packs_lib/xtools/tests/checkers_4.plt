:- begin_tests(checkers_4).

:- use_module(library(infer_meta)).
:- use_module(library(checkers/checker)).
:- use_module(library(checkers/check_wrong_dynamic)).
:- use_module(checkers_hooks).

:- use_module(cwda).
:- use_module(cwdb).

test(cwd_4) :- % There is an issue that does not refers to module cwda
    check_results(wrong_dynamic, Results, [module(cwda), files([xtools/tests/cwdb])]),
    assertion(Results = []).

:- end_tests(checkers_4).
