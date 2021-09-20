:- include(test_header).


%  was_module(adb_pfc,[]).



 ~P,P ==> contrradiction.

bird(X), \+ ~fly(X) ==> fly(X).

penguin(X) ==> bird(X).

penguin(X) ==> ~fly(X).

bird(X), injured(X) ==> ~fly(X).

bird(X), dead(X) ==> ~fly(X).

% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/birds_01c.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/BIRDS_01C/logicmoo_base_examples_fol_BIRDS_01C_JUnit/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ABIRDS_01C 

% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/616
