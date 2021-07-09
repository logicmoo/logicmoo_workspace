
%:- consult(eventCalculusPlanner).
%:- consult(planner19a).
%:- consult(planner115).
%:- consult(ec_common).

:- ec:use_module(ec_planner_dmiles).

:- discontiguous do_test/1.
:- multifile do_test/1.
:- dynamic(do_test/1).
                          
:- style_check(-singleton).

:- use_module('./ec_loader').




