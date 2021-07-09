
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


%:- multifile(ec:demo_test/3).
ec:demo_test(lps_demo_test_1, lps_demo, [holds(plant_safe,9)]).

%baseKB:lps_demo_test_1_run:- abdemo([holds(plant_safe,9)]).

/** <examples>
?- go(Timeline).
*/

end_of_file.

?- lps_demo_test_1.

OUTPUTS AS:

% % From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14
% running('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/test/lps_planner/lps_demo_test_1.pl').
% Preparing visualization...
% Collected events and fluents...
% Prepared internal structures...
% Doing something else...
% Finding the end of time...
% Almost done...
lps_visualization(
   Dict{groups:[ Dict2{
                      content:"Events",
                      id:"event",
                      order:1} ],
        items:[ Dict1{
                     content:"plant_safe",
                     group:"event",
                     id:0,
                     start:9,
                     style:"color:#E19735",
                     title:"happens(plant_safe,8,9)",
                     type:"point"} ]},
   []).
 %  do(demo_test(lps_demo_test_1,lps_demo)).

 %  showing_ec_current_domain_db.

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

predicate(dummy).

mpred_prop(dummy,predicate).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

event(plant_safe).

event(plant_safe).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

action(drain_tank).

action(cool_tank).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

action(open_valve).

action(turn_off_boiler).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

action(drain_tank).

action(cool_tank).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

action(open_valve).

action(turn_off_boiler).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

axiom(happens(plant_safe, 9),
    []).

axiom(happens(plant_safe, 9),
    []).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

axiom(initiates(drain_tank, tank_empty, Time_at_Initiates),
    [holds(pressure_normal, Time_at_Initiates)]).

axiom(initiates(cool_tank, temperature_low, Initiates),
    []).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

axiom(initiates(open_valve, valve_open, Initiates),
    []).

axiom(initiates(turn_off_boiler, boiler_off, Initiates),
    []).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

axiom(initiates(drain_tank, tank_empty, Time_at_Initiates),
    [holds(pressure_normal, Time_at_Initiates)]).

axiom(initiates(cool_tank, temperature_low, Initiates),
    []).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

axiom(initiates(open_valve, valve_open, Initiates),
    []).

axiom(initiates(turn_off_boiler, boiler_off, Initiates),
    []).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

axiom(holds(plant_safe, Time_at),
    [holds(tank_empty, Time_at), holds(temperature_low, Time_at)]).

axiom(holds(pressure_normal, Time_at),
    [holds(valve_open, Time_at)]).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

axiom(holds(pressure_normal, Time_at),
    [holds(boiler_off, Time_at)]).

axiom(holds(plant_safe, Time_at),
    [holds(tank_empty, Time_at), holds(temperature_low, Time_at)]).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

axiom(holds(pressure_normal, Time_at),
    [holds(valve_open, Time_at)]).

axiom(holds(pressure_normal, Time_at),
    [holds(boiler_off, Time_at)]).

 %  shown_ec_current_domain_db.

 %  ?-do_abdemo([holds(plant_safe,9)]).

 %  showing_ec_current_domain_db.

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

predicate(dummy).

mpred_prop(dummy,predicate).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

event(plant_safe).

event(plant_safe).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

action(drain_tank).

action(cool_tank).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

action(open_valve).

action(turn_off_boiler).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

action(drain_tank).

action(cool_tank).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

action(open_valve).

action(turn_off_boiler).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

axiom(happens(plant_safe, 9),
    []).

axiom(happens(plant_safe, 9),
    []).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

axiom(initiates(drain_tank, tank_empty, Time_at_Initiates),
    [holds(pressure_normal, Time_at_Initiates)]).

axiom(initiates(cool_tank, temperature_low, Initiates),
    []).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

axiom(initiates(open_valve, valve_open, Initiates),
    []).

axiom(initiates(turn_off_boiler, boiler_off, Initiates),
    []).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

axiom(initiates(drain_tank, tank_empty, Time_at_Initiates),
    [holds(pressure_normal, Time_at_Initiates)]).

axiom(initiates(cool_tank, temperature_low, Initiates),
    []).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

axiom(initiates(open_valve, valve_open, Initiates),
    []).

axiom(initiates(turn_off_boiler, boiler_off, Initiates),
    []).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

axiom(holds(plant_safe, Time_at),
    [holds(tank_empty, Time_at), holds(temperature_low, Time_at)]).

axiom(holds(pressure_normal, Time_at),
    [holds(valve_open, Time_at)]).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

axiom(holds(pressure_normal, Time_at),
    [holds(boiler_off, Time_at)]).

axiom(holds(plant_safe, Time_at),
    [holds(tank_empty, Time_at), holds(temperature_low, Time_at)]).

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

axiom(holds(pressure_normal, Time_at),
    [holds(valve_open, Time_at)]).

axiom(holds(pressure_normal, Time_at),
    [holds(boiler_off, Time_at)]).

 %  shown_ec_current_domain_db.
Total time taken 0.05 !!!! For Plan:

% From /mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/prolog/logicmoo_planner.pl:14

 /*   [ [happens(cool_tank, t27), happens(open_valve, t26), happens(drain_tank, t17)],
          [b(t27, 9), b(t26, 9), b(t26, t17), b(t17, 9)],
          [ [clipped(t27, temperature_low, 9)],
            [clipped(t26, valve_open, t17)],
            [clipped(t17, tank_empty, 9)]
          ]
        ].
 */

 %  success(do_abdemo([holds(plant_safe,9)])).

 lps_sanity=100
true.

baseKB:  ?-

