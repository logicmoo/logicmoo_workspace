
:- ensure_loaded(library(pfc_test)).
:- expects_dialect(pfc).

==> a(1).
==> a(2).

a(2) <==> b(1).

% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/370 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/pfc/t/sanity_base/dia_pfc_mod_01b.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.pfc.test.sanity_base/DIA_PFC_MOD_01B/logicmoo_pfc_test_sanity_base_DIA_PFC_MOD_01B_JUnit/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ADIA_PFC_MOD_01B 

