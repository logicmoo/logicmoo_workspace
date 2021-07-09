#!/usr/bin/env swipl
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles

% Tests if Rule database can clear correctly

:- include(test_header).
:- set_file_abox_module(user).
:- file_begin(pfc).
:- include(test_header).


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


