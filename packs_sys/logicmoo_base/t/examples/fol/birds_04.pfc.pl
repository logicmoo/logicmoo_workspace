:- include(test_header).


%  was_module(adb_pfc,[]).


:- expects_dialect(pfc).

 ~P,P ==> contrradiction.


:- expects_dialect(clif).

bird(X), \+ ~fly(X) => fly(X).

penguin(X) => bird(X).

penguin(X) => ~fly(X).

bird(X), injured(X) => ~fly(X).

bird(X), dead(X) => ~fly(X).

% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/birds_04.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/BIRDS_04/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ABIRDS_04 

% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/615
