#!/usr/bin/env lmoo-junit

:- module(t123,[]).

:- include(test_header).
:- module(t123).

:- dynamic(t123:ttExpressionType/1).

% :- process_this_script.

:- statistics.

:- test_boxlog(all(R,exists(D,implies(room(R), and(door(D), has(R, D)))))).


:- test_boxlog(all(R,implies(room(R),exists(D,and(door(D), has(R, D)))))).


:- test_boxlog(exists(D, all(R, implies(room(R), and(door(D), hasShared(R, D)))))).



% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/scope_exists_01.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/SCOPE_EXISTS_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ASCOPE_EXISTS_01 

% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/609
