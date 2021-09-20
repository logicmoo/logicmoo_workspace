
:- module(bar,[]).

:- ensure_loaded(library(pfc_test)).
% :- set_prolog_flag(pfc_version,1.8).
:- set_prolog_flag(pfc_version,v(1,8,2)).

:- expects_dialect(pfc).

a(1).
a(2).

%:- trace.
a(2) <==> b(1).

:- listing(b/1).



% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/326 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/pfc/t/sanity_base/dia_pfc_mod_01a.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.pfc.test.sanity_base/DIA_PFC_MOD_01A/logicmoo_pfc_test_sanity_base_DIA_PFC_MOD_01A_JUnit/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ADIA_PFC_MOD_01A 

