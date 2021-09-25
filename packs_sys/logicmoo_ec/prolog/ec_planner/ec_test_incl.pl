
%:- consult(eventCalculusPlanner).
%:- consult(planner19a).
%:- consult(planner115).
%:- consult(ec_common).

:- include(library(logicmoo_test_header)).

:- ec:use_module(ec_planner_dmiles).

:- discontiguous(do_ec_test/1).
:- multifile(do_ec_test/1).
:- dynamic(do_ec_test/1).
                          
:- style_check(-singleton).

:- use_module('./ec_loader').




