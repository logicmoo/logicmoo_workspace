
:- include('../ec_test_incl').

:- module(ec).

do_test_cakes(N) :- abdemo_special(loops,[holds_at(num_cakes(N),t)],R).

%do_test(1) :- do_test_cakes(1).
%do_test(0) :- do_test_cakes(0).
do_test(4) :- do_test_cakes(4).
do_test(5) :- do_test_cakes(5).
do_test(6) :- \+ do_test_cakes(6).

fluent(num_cakes(_)).

executable(eat_cakes(1)).

initially(num_cakes(5)).

axiom( initiates(eat_cakes(Eat),num_cakes(Remaining),T), 
 [ call((   
   plus(Remaining, Eat, Start),
   Start>=0,Remaining>=0,Eat>=0)),
   holds_at(num_cakes(Start), T)]).

axiom(terminates(eat_cakes(Eat),num_cakes(N),T), [call((number(E),Eat>0)),holds_at(num_cakes(N),T)]).

/* Abduction policy */

abducible(dummy).


:- listing(ec:ec_current_domain_db/1).
:- listing(ec_current_domain_db/2).


:- run_ec_tests.




% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_ec/test/ec_planner/ectest/test_eat_cake_1.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.ec.ec_planner.ectest/TEST_EAT_CAKE_1/logicmoo_ec_ec_planner_ectest_TEST_EAT_CAKE_1/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ATEST_EAT_CAKE_1 

% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/646
