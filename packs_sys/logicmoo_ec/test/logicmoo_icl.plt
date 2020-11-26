%:- module(logicmoo_ec,[test_logicmoo_ec/0]).

% [Required] Load the Logicmoo Library Utils
:- include(sanity_tests).

:- ensure_loaded(library(logicmoo_ec)).

test(test_logicmoo_ec):- test_logicmoo_ec_sanity.
test(test_logicmoo_ec_lps_reader):- test_logicmoo_ec_lps_reader.


