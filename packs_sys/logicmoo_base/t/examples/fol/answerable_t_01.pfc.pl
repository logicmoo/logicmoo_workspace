#!/usr/bin/env lmoo-junit

:- module(t123,[]).

%:- ensure_loaded(library(logicmoo_test)).
%:- use_module(library(logicmoo_clif)).
%:- use_module(library(logicmoo_plarkc)).
:- include(test_header).



:- test_boxlog(( ~fallacy_t(PROP) => unknown_t(PROP) v false_t(PROP) v true_t(PROP) )).
:- test_boxlog(( ~unknown_t(PROP) => true_t(PROP) v false_t(PROP)  )).
:- test_boxlog(( ~false_t(PROP) => fallacy_t(PROP) v unknown_t(PROP) v true_t(PROP) )).
:- test_boxlog(( answerable_t(PROP) <=> askable_t(PROP) & ~unknown_t(PROP) )).
:- test_boxlog(( answerable_t(PROP) => true_t(PROP) v false_t(PROP)  )).
:- test_boxlog(( askable_t(PROP) <=> ~fallacy_t(PROP) )).
:- test_boxlog(( askable_t(PROP) => true_t(PROP) v unknown_t(PROP) v false_t(PROP)  )).
:- test_boxlog(( askable_t(PROP) v fallacy_t(PROP) )).
:- test_boxlog(( asserted_t(PROP) => true_t(PROP) )).
:- test_boxlog(( fallacy_t(PROP) => false_t(PROP) & true_t(PROP) & ~unknown_t(PROP) & ~possible_t(PROP) )).   
:- test_boxlog(( true_t(PROP) & false_t(PROP) => fallacy_t(PROP) )).
:- test_boxlog(( true_t(PROP) v unknown_t(PROP) v false_t(PROP)  )).

:- test_boxlog(( true_t(PROP) => possible_t(PROP) )).
:- test_boxlog(( possible_t(PROP) => ~false_t(PROP) & ~fallacy_t(PROP)  )).

:- test_boxlog(( ~true_t(PROP) => false_t(PROP) v fallacy_t(PROP) v possible_t(PROP) )).
:- test_boxlog(( false_t(PROP) <=> ~true_t(PROP) & ~possible_t(PROP) & ~unknown_t(PROP) )).
:- test_boxlog(( true_t(PROP) => ~false_t(PROP) & possible_t(PROP) & ~unknown_t(PROP) )).
:- test_boxlog(( ~asserted_t(PROP) => possible_t(PROP) v false_t(PROP) v fallacy_t(PROP) )).
:- test_boxlog(( ~possible_t(PROP) => false_t(PROP) v fallacy_t(PROP) )).
:- test_boxlog(( possible_t(PROP) => ~false_t(PROP) & ~fallacy_t(PROP)  )).            
:- test_boxlog(( unknown_t(PROP) => ~true_t(PROP) & possible_t(PROP) & ~asserted_t(PROP) & ~false_t(PROP) )).
%:- test_boxlog(( ist(MT1,askable_t(PROP))  & genlMt(MT1,MT2) => ist(MT2, (true_t(PROP) v unknown_t(PROP) v false_t(PROP)  )))).
% :- test_boxlog(( ist(MT1,asserted_t(PROP)) & genlMt(MT1,MT2) => ist(MT2,true_t(PROP)) )).




% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/458 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/answerable_t_01.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/ANSWERABLE_T_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3AANSWERABLE_T_01 

