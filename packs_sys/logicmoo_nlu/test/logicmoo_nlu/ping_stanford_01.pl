#!/usr/bin/env lmoo-junit

:- include(library(logicmoo_test_header)).

% =============================================
% File 'mpred_builtin.pfc'
% Purpose: Agent Reactivity for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net %
% Version: 'interface' 1.0.0
% Revision: $Revision: 1.9 $
% Revised At: $Date: 2002/06/27 14:13:20 $
% =============================================
%

%:- ensure_loaded(library(parser_chat80)).

:- mpred_test(test_corenlp_parse1).

:- mpred_test(test_corenlp1).

test_here(X):- mpred_test(test_corenlp(X)).

:- forall(parser_tests(X),test_here(X)).


% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_nlu/test/logicmoo_nlu/ping_stanford_01.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.nlu.test.nlu/PING_STANFORD_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3APING_STANFORD_01 

% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/640
