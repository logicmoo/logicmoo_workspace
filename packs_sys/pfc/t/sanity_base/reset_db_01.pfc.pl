#!/usr/bin/env swipl
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles

% Tests if Finin Backchaining memo idea is working


%  was_module(bc_01,[]).

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

:- mpred_reset.

:- pp_DB.

