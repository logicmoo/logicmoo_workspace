
:- use_module(test_phil).


:- format(user_error,
	  'phil test suite.  To run all tests run ?- test.~n~n', []).


test:-
  test_phil.
