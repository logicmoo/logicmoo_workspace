

:- format(user_error,
	  'trill test suite.  To run all tests run ?- test.~n~n', []).
test:-
  use_module(test_trill),
  test_trill,
  unload_file(test_trill),
  use_module(test_trillp),
  test_trillp,
  unload_file(test_trillp),
  use_module(test_tornado),
  test_tornado.
