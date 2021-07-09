:- include('ec_test_incl').
/*
   Test queries:

*/

do_test(plant1) :- abdemo_special(easy,[holds_at(plant_safe,t)],R).


axiom(holds_at(plant_safe,T),
     [holds_at(tank_empty,T),holds_at(temperature_low,T)]).

axiom(initiates(drain_tank,tank_empty,T),
     [holds_at(pressure_normal,T)]).

axiom(initiates(cool_tank,temperature_low,T),[]).

axiom(holds_at(pressure_normal,T),[holds_at(valve_open,T)]).

axiom(holds_at(pressure_normal,T),[holds_at(boiler_off,T)]).

axiom(initiates(open_valve,valve_open,T),[]).

axiom(initiates(turn_off_boiler,boiler_off,T),[]).


/* Abduction policy */

abducible(dummy).

executable(drain_tank).

executable(cool_tank).

executable(open_valve).

executable(turn_off_boiler).




:- run_tests.

% :- halt.

