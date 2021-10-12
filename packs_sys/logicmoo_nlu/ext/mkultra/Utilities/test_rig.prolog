%% test(*Name, +Options)
%  Body of this rule is a test that should be run with the specified options.

run_tests(Test) :-
   forall(test_body(Name, Options, Body),
	  run_test(Name, Options, Body)).

test_body(Name, Options, Body) :-
   clause(test(Name, Options), Body).
test_body(Name, [ ], Body) :-
   clause(test(Name), Body).

	   