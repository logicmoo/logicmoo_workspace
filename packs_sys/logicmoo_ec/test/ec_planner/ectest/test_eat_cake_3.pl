
:- include('../ec_test_incl').

:- module(ec).

do_test_cakes(N) :- abdemo_special(loops,[holds_at(num_cakes(N),t)],R).

%do_ec_test(1) :- do_test_cakes(1).
%do_ec_test(0) :- do_test_cakes(0).
%do_ec_test(4) :- do_test_cakes(4).
%do_ec_test(5) :- do_test_cakes(5).
%do_ec_test(6) :- \+ do_test_cakes(6).
do_ec_test(3) :- do_test_cakes(3).

fluent(num_cakes(_)).

executable(eat_cakes(1)).

initially(num_cakes(5)).

axiom(initiates(eat_cakes(Eat),num_cakes(Remaining),T), 
 [ holds_at(num_cakes(Start), T),
   call((   
   plus(Remaining, Eat, Start),
   Start>=0,Remaining>=0,Eat>=0)),
   holds_at(num_cakes(Start), T)]).

axiom(terminates(eat_cakes(Eat),num_cakes(N),T), [call((number(E),Eat>0)),holds_at(num_cakes(N),T)]).

/* Abduction policy */

abducible(dummy).


:- mpred_test(show_ec_current_domain_db).


:- run_ec_tests.





% EDIT: https://github.com/logicmoo/logicmoo_workspace/edit/master/packs_sys/logicmoo_ec/test/ec_planner/ectest/test_eat_cake_3.pl 
% JENKINS: https://jenkins.logicmoo.org/job/logicmoo_workspace/lastBuild/testReport/logicmoo.ec.ec_planner.ectest/TEST_EAT_CAKE_3/ 
% ISSUE_SEARCH: https://github.com/logicmoo/logicmoo_workspace/issues?q=is%3Aissue+label%3ATEST_EAT_CAKE_3 
% ISSUE: https://github.com/logicmoo/logicmoo_workspace/issues/ 

