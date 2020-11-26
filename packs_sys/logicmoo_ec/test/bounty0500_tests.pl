/*

root@gitlab:/opt/logicmoo_workspace/packs_sys/planner_api# cd t
root@gitlab:/opt/logicmoo_workspace/packs_sys/planner_api/t# swipl bounty0500_tests.pl

*/
% DEFINITION OF DELIVERY ROBOT WORLD IN STRIPS NOTATION
:- use_module(library(planner_api)).

:- planner_add_workspace(delrob_problem).

:- include(prolog_files/delrob/delrob_strips).

:- WS = delrob_problem,
   NewWS = delrob_problem_after_step_1,
   planner_get_plan(WS,at(parcel,o111),[Step1|Rest]),
   planner_debug(plan=[Step1|Rest]),
   planner_apply_step(WS,Step1,NewWS),
   planner_get_plan(NewWS,at(parcel,o111),NewPlan),
   planner_debug(nextplan=NewPlan),
   assertion(Rest = NewPlan).

:- planner_add_workspace(delrob_repeat).

:- include(prolog_files/delrob/delrob_strips).

:- WS = delrob_repeat,
   NewWS = delrob_repeat_after_step_1,
   planner_get_plan(WS,at(parcel,o111),[Step1|Rest]),
   planner_debug(plan=[Step1|Rest]),
   planner_apply_step(WS,Step1,NewWS),
   planner_get_plan(NewWS,at(parcel,o111),NewPlan),
   planner_debug(nextplan=NewPlan),
   assertion(Rest = NewPlan).

