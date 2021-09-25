

:- expects_dialect(swi).

'==>'(a,b).

:- ensure_loaded(library(logicmoo_test)).
:- expects_dialect(pfc).

(a ==> b).



% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/325 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/pfc/t/sanity_base/dia_pfc_mod_01c.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.pfc.test.sanity_base/DIA_PFC_MOD_01C/logicmoo_pfc_test_sanity_base_DIA_PFC_MOD_01C_JUnit/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ADIA_PFC_MOD_01C 

