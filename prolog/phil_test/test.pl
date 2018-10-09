
:- use_module(phil).


#:- format(user_error,
#	  'phil.  To run a test run ?- test.~n~n', []).
test:- writeln("phil loaded").
