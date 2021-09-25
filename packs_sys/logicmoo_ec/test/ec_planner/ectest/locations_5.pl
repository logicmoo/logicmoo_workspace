:- include(library(logicmoo_test_header)).



test_logicmoo_ec_lps_reader_here(X):- mpred_test(test_logicmoo_ec_lps_reader(X)).

:- prolog_load_context(directory,X), cd(X),
   expand_file_name('locations_5.e',Files),
   maplist(test_logicmoo_ec_lps_reader_here,Files).


