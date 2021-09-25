
:- include(library(logicmoo_test_header)).

:- mpred_test(
  test_logicmoo_ec_lps_reader([ ec('ecnet/Diving.e'),
                                  ec('foundations/*.e'),
                                  ec('ecnet/*.e')
                                ])).

test_logicmoo_ec_lps_reader_here(X):- mpred_test(test_logicmoo_ec_lps_reader(X)).

:- prolog_load_context(directory,X), cd(X), 
   expand_file_name('*.e',Files),
   maplist(test_logicmoo_ec_lps_reader_here,Files).


% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_ec/test/ec_planner/ectest/convert_e_to_lps_01.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.ec.ec_planner.ectest/CONVERT_E_TO_LPS_01/logicmoo_ec_ec_planner_ectest_CONVERT_E_TO_LPS_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ACONVERT_E_TO_LPS_01 

% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/647
