:- include(library(logicmoo_test_header)).


test_logicmoo_ec_lps_reader_here(X):- 
  retractall(tmp_ec:done_it(_)),
  test_logicmoo_ec_lps_reader(X).

:- prolog_load_context(directory,X), cd(X),
   expand_file_name('locations_5.e',[File]),!,
   cvt_e_pl(File),
   test_logicmoo_ec_lps_reader_here(File),!.



% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_ec/test/ec_planner/ectest/locations_5.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.ec.ec_planner.ectest/LOCATIONS_5/logicmoo_ec_ec_planner_ectest_LOCATIONS_5/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ALOCATIONS_5 

% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/658
