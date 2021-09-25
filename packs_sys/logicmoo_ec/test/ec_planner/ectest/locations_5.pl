:- include(library(logicmoo_test_header)).


test_logicmoo_ec_lps_reader_here(X):- 
  retractall(tmp_ec:done_it(_)),
  test_logicmoo_ec_lps_reader(X).

:- prolog_load_context(directory,X), cd(X),
   expand_file_name('locations_5.e',[File]),!,  
   mpred_test(test_logicmoo_ec_lps_reader_here(File)),!.


