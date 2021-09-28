

:- format(user_error,
	  'TRILL test suite.  To run all tests run ?- test.~n~n', []).
test:-
  use_module(library(trill_test/test_trill)),
  test_trill,
  unload_file(library(trill_test/test_trill)),
  use_module(library(trill_test/test_trillp)),
  test_trillp,
  unload_file(library(trill_test/test_trillp)),
  use_module(library(trill_test/test_tornado)),
  test_tornado.
