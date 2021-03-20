:- expects_dialect(lps).

maxTime(10).

actions drain_tank, cool_tank, open_valve, turn_off_boiler.

events plant_safe.

fluents
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

observe plant_safe from 8.


:- multifile(ec:demo_test/1).
ec:demo_test(lps_demo_tests_run, lps_demo, [holds(plant_safe,9)]).

baseKB:lps_demo_tests_run:- abdemo([holds(plant_safe,9)]).

/** <examples>
?- go(Timeline).
*/


baseKB:lps_demo_tests_run_all:- demo_test(lps_demo).

