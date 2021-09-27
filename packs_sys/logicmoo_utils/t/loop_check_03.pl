
:- include('loop_check_tests.plt').

loop_inf41(X) :- loop_check(loop_inf41(X),X=1).
loop_inf41(2).

test(loop_inf41):- mpred_test((   loop_inf41(X),X=1)),!.

test(loop_inf4a):- must((   loop_inf41(X),X=2,!)).

% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master//var/lib/jenkins/workspace/logicmoo_workspace/packs_sys/logicmoo_utils/t/loop_check_03.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.utils.utils.t/LOOP_CHECK_03/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ALOOP_CHECK_03 
% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/ 

