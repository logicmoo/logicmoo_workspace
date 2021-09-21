:- include(test_header).

% :- process_this_script.

% Initially Dmiles thought LEM was the culprit, it was not.
% this is a more generalized problem in Nomics


*/
% ==============================================================
% Section 3: Decidablity?
% ==============================================================

% Is the existence of cute puppies really even knowable/decidable in an "Open World"?

% Rule 4: Cute puppies exists or not (LEM)
nesc(exists(X,cute_puppy(X)) v ~exists(X,cute_puppy(X))).

% ~exists(X,




% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/429 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/unit_projection_lem_01.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/UNIT_PROJECTION_LEM_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3AUNIT_PROJECTION_LEM_01 

