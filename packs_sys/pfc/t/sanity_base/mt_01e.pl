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

:- mpred_trace_exec.
:- expects_dialect(pfc).

%:- sanity(current_prolog_flag(retry_undefined, kb_shared)).
%

:- pfc_test_feature(mt,\+ mtHybrid(header_sane)).
:- mpred_test(\+ mtProlog(header_sane)).
%:- mpred_test(tMicrotheory(header_sane)).

genlMt(kb1,header_sane).

:- mpred_test(\+ mtProlog(kb1)).
:- pfc_test_feature(mt,\+ mtHybrid(kb1)).

%:- mpred_test(tMicrotheory(kb1)).



% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/pfc/t/sanity_base/mt_01e.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.pfc.test.sanity_base/MT_01E/logicmoo_pfc_test_sanity_base_MT_01E_JUnit/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3AMT_01E 

% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/592
