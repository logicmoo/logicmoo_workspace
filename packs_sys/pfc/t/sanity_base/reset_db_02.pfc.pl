#!/usr/bin/env lmoo-junit
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles


%  cls ; kill -9 %1 ; swipl -g "ensure_loaded(pack(logicmoo_base/t/examples/base/'sanity_abc.pfc'))."

%  was_module(header_sane,[]).

:- include(library(logicmoo_test_header)).

:- expects_dialect(pfc).

header_sane:(must_clause_asserted(G):- cwc, must(clause_asserted_u(G))).
:-   ain((must_clause_asserted(G):- cwc, must(clause_asserted_u(G)))).

must_clause_asserted(G):- cwc, must(clause_asserted_u(G)).

:- listing(must_clause_asserted).

:- sanity(predicate_property(must_clause_asserted(_),number_of_clauses(_))).

a.

:- header_sane:listing(a).

:- must_clause_asserted(a).

:- mpred_reset.


% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/311 
% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/pfc/t/sanity_base/reset_db_02.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.pfc.test.sanity_base/RESET_DB_02/logicmoo_pfc_test_sanity_base_RESET_DB_02_JUnit/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ARESET_DB_02 

