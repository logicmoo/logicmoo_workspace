:- include(test_header).


 

all([X,Y,R],
  exists(RPlus, 
   holds(RPlus,X,Y) & holds(Ri,Y,Z) => 
           ( holds(RPlus,X,Z) & ~holds(Ri,X,Z)))).




% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/430 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/tc_example_01.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/TC_EXAMPLE_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ATC_EXAMPLE_01 

