
:- module(foo,[a/1, b/1]).

%:- set_prolog_flag(pfc_version,1.8).
:- set_prolog_flag(pfc_version,v(1,8,2)).

:- ensure_loaded(library(logicmoo_test)).
:- expects_dialect(pfc).

%:- trace.
a(1).
a(2).
a(2) <==> b(1).

:- listing(b/1).



% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/64 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/pfc/t/sanity_base/dia_pfc_mod_01.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.pfc.test.sanity_base/DIA_PFC_MOD_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ADIA_PFC_MOD_01 

