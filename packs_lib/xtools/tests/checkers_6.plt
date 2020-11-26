:- begin_tests(checkers_6).

:- use_module(library(infer_meta)).
:- use_module(library(checkers/checker)).
:- use_module(library(checkers/check_wrong_dynamic)).

:- use_module(cwde).

test(cwd_6) :-
    check_results(wrong_dynamic, Results, [module(cwde)]),
    assertion(Results = [_]).

:- end_tests(checkers_6).
