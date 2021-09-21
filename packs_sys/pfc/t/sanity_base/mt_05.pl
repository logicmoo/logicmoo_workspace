/* <module>
%
%  PFC is a language extension for prolog.
%
%  It adds a new type of module inheritance
%
% Dec 13, 2035
% Douglas Miles
*/
%  was_module(mt_01,[]).

:- include(library(logicmoo_test_header)).


%:- add_import_module(mt_01,baseKB,end).

:- set_defaultAssertMt(myMt).

mtProlog(code1).
mtHybrid(kb2).
mtHybrid(kb3).

% code1: (a <- b).
code1: (a:-b).


kb2: (b).

genlMt(code1,kb2).

:- code1:import(kb2:b/0).

kb2: (:- a).

genlMt(kb3,kb2).


baseKB:predicateConventionMt(a,code1).

kb3: (a==>c).



% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/387 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/pfc/t/sanity_base/mt_05.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.pfc.test.sanity_base/MT_05/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3AMT_05 

