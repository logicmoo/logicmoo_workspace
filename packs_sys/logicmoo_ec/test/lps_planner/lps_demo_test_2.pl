
:- expects_dialect(lps).

% Your program goes here
maxTime(10).

actions drain_tank, cool_tank, open_valve, turn_off_boiler.

fluents
  plant_safe,
  tank_empty,
  temperature_low,
  pressure_normal,
  valve_open,
  boiler_off.


plant_safe at T if tank_empty at T, temperature_low at T.
drain_tank initiates tank_empty if pressure_normal.
cool_tank initiates temperature_low.
pressure_normal at T if valve_open at T.
pressure_normal at T if boiler_off at T.
open_valve initiates valve_open.
turn_off_boiler initiates boiler_off.

if true at 8 then plant_safe at 8.

baseKB:process_ec_2:- process_ec([
  axiom(holds(plant_safe, T), [holds(tank_empty, T), holds(temperature_low, T)]),
  axiom(initiates(drain_tank, tank_empty, T), [holds(pressure_normal, T)]),
  axiom(initiates(cool_tank, temperature_low, _T), []),
  axiom(holds(pressure_normal, T), [holds(valve_open, T)]),
  axiom(holds(pressure_normal, T), [holds(boiler_off, T)]),
  axiom(initiates(open_valve, valve_open, _T), []),
  axiom(initiates(turn_off_boiler, boiler_off, _T), [])]).


/*
event(drain_tank).
event(cool_tank).
event(open_valve).
event(turn_off_boiler).
*/




:- multifile(ec:demo_test/3).
ec:demo_test(lps_demo_tests_run, lps_demo, [holds(plant_safe,8)]).

baseKB:lps_demo_test_2_run:- abdemo([holds(plant_safe,8)]).

/** <examples>
?- go(Timeline).
*/


