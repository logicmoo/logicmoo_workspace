#!/usr/bin/env lmoo-junit

%  was_module(sanity_ks_two,[]).

:- include(library(logicmoo_test_header)).

:- if(\+ current_module(attvar_reader)).
:- use_module(library(logicmoo/attvar_reader)).
:- endif.

% Tests assertable attributed variables

:- debug_logicmoo(_).
:- nodebug_logicmoo(http(_)).
:- debug_logicmoo(logicmoo(_)).
 % :- mpred_trace_exec.

:- dynamic(sk_out/1).
:- dynamic(sk_in/1).

% :- ain(baseKB:rtArgsVerbatum(my_sk)).

:- read_attvars(true).

% :- expects_dialect(swi).
my_sk(aVar([vn='Ex',sk='SKF-666'])).

:- mpred_test((my_sk(Ex),get_attr(Ex,sk,What),What=='SKF-666')).



% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/pfc/t/sanity_base/attvar_04.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.pfc.test.sanity_base/ATTVAR_04/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3AATTVAR_04 

% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/548
