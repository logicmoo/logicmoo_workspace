/* <module>
%
%  PFC is a language extension for prolog.
%
%  It adds a new type of module inheritance
%
% Dec 13, 2035
% Douglas Miles
*/
%  was_module(header_sane,[]).

:- include(library(logicmoo_test_header)).

:- expects_dialect(pfc).

:- set_defaultAssertMt(cycKB1).

loves(sally,joe).

:- mpred_test(clause_u(cycKB1:loves(_,_))).

:- mpred_test(\+ clause_u(baseKB:loves(_,_))).

:- pfc_test_feature(mt,\+ clause_u(header_sane:loves(_,_))).

:- mpred_test(clause_u(loves(_,_))).


:- mpred_test(call_u(cycKB1:loves(_,_))).

:- pfc_test_feature(mt,\+ call_u(baseKB:loves(_,_))).

:- pfc_test_feature(mt,\+ call_u(header_sane:loves(_,_))).

:- pfc_test_feature(mt,call_u(loves(_,_))).





% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/403 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/pfc/t/sanity_base/mt_01c.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.pfc.test.sanity_base/MT_01C/logicmoo_pfc_test_sanity_base_MT_01C_JUnit/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3AMT_01C 

