#!/usr/bin/env lmoo-junit
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles

% Tests if Rule database can clear correctly

:- include(library(logicmoo_test_header)).
:- set_file_abox_module(user).
:- expects_dialect(pfc).
:- include(library(logicmoo_test_header)).


:- dynamic(cond_POST/1).
:- dynamic(cond_PRE/1).

cond_PRE ==> cond_POST.
cond_PRE.

cond_PRE ==> child_POST.
cond_PRE_D ==> cond_POST.

:- mpred_why(cond_POST).

:- mpred_trace_exec.

aaa.

bbbb.

:- pp_DB.

:- mpred_reset_kb(user).

:- listing('$spft').

:- pp_DB.



% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/337 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/pfc/t/sanity_base/reset_db_01_user.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.pfc.test.sanity_base/RESET_DB_01_USER/logicmoo_pfc_test_sanity_base_RESET_DB_01_USER_JUnit/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ARESET_DB_01_USER 

