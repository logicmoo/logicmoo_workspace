:- begin_tests(checkers_5).

:- use_module(library(infer_meta)).
:- use_module(library(checkers/checker)).
:- use_module(library(checkers/check_wrong_dynamic)).
:- use_module(checkers_hooks).

:- use_module(cwdc).

test(cwd_5, [setup(cleanup_inferred_meta)]) :-
    check_results(wrong_dynamic, Results, [files([xtools/tests/cwdc])]),
    assertion(Results = []).

:- end_tests(checkers_5).
