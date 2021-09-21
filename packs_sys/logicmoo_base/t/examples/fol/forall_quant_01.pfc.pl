/*

Make sure quantifiers make sense

*/

:- include(test_header).

:- set_prolog_flag(gc,true).

% =================================================================================
% Set our engine up
% =================================================================================

:- expects_dialect(pfc).


% =================================================================================
% Begin program
% =================================================================================

% Let same/2 be our unification identify 
:- test_boxlog(forall(X,same(X,X))).

% same/2 implies not differnt
:- test_boxlog(all(X,all(Y,same(X,Y) => ~ different(X,Y)))).

:- test_boxlog(forall([X,Y], same(X,Y) => ~ different(X,Y))).

end_of_file.


% Make each key unique depending on its label
:- test_boxlog(forall([Key1,Key2,Label1,Label2],
  glythed(Key1,Label1) & glythed(Key2,Label2) & different(Label1,Label2) => different(Key1,Key2))).



                                              

% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_base/t/examples/fol/forall_quant_01.pfc.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.base.examples.fol/FORALL_QUANT_01/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3AFORALL_QUANT_01 

% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/613
