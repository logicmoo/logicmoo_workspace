:- expects_dialect(lps).

maxTime(10).

requirement(':negative_preconditions').
requirement(':conditional_effects').
requirement(':equality').
requirement(':typing').
requirement(':fluents').
requirement(':durative_actions').
requirement(':derived_predicates').

subtype(intelligentagent, object).
subtype(residence, object).
subtype(vehicle, object).
subtype(tool, object).
subtype(container, object).
subtype(modeoftransportation, category).
subtype(isa, thing).
subtype(physicallocation, thing).
subtype(person, intelligentagent).
subtype(residence, physicallocation).
subtype(vehicle, container).

fluents autonomous(intelligentagent).
fluents location(isa, physicallocation).
fluents contains(container, object).
fluents mobile(isa).
fluents directly_holding(intelligentagent, object).
fluents travel_path(modeoftransportation, physicallocation, physicallocation).
fluents driving_p(modeoftransportation).
fluents walking_p(modeoftransportation).

function(travel_distance(modeoftransportation, physicallocation, physicallocation), any).
function(travel_duration(modeoftransportation, physicallocation, physicallocation), any).

fluents enabled_durative_action_walk(A, L0, L1, M, Duration).
actions walk(A, L0, L1, M).
enabled_durative_action_walk(A, L0, L1, M, Duration)at T0 if isa(A, intelligentagent), isa(L0, physicallocation), isa(L1, physicallocation), isa(M, modeoftransportation).
walk(A, L0, L1, M)initiates location(A, L1)if enabled_durative_action_walk(A, L0, L1, M, Duration).
walk(A, L0, L1, M)terminates location(A, L0)if enabled_durative_action_walk(A, L0, L1, M, Duration).

fluents enabled_durative_action_pick_up(A, O, L, Duration).
actions pick_up(A, O, L).
enabled_durative_action_pick_up(A, O, L, Duration)at T0 if isa(A, intelligentagent), isa(O, object), isa(L, physicallocation).
pick_up(A, O, L)initiates directly_holding(A, O)if enabled_durative_action_pick_up(A, O, L, Duration).

fluents enabled_durative_action_set_down(A, O, L, Duration).
actions set_down(A, O, L).
enabled_durative_action_set_down(A, O, L, Duration)at T0 if isa(A, intelligentagent), isa(O, object), isa(L, physicallocation).
set_down(A, O, L)initiates location(O, L)if enabled_durative_action_set_down(A, O, L, Duration).
set_down(A, O, L)terminates directly_holding(A, O)if enabled_durative_action_set_down(A, O, L, Duration).

fluents enabled_durative_action_carry(A, O, L0, L1, M, Duration).
actions carry(A, O, L0, L1, M).
enabled_durative_action_carry(A, O, L0, L1, M, Duration)at T0 if isa(A, intelligentagent), isa(O, object), isa(L0, physicallocation), isa(L1, physicallocation), isa(M, modeoftransportation).
carry(A, O, L0, L1, M)initiates location(A, L1)if enabled_durative_action_carry(A, O, L0, L1, M, Duration).
carry(A, O, L0, L1, M)initiates location(O, L1)if enabled_durative_action_carry(A, O, L0, L1, M, Duration).
carry(A, O, L0, L1, M)terminates location(A, L0)if enabled_durative_action_carry(A, O, L0, L1, M, Duration).
carry(A, O, L0, L1, M)terminates location(O, L0)if enabled_durative_action_carry(A, O, L0, L1, M, Duration).

fluents enabled_durative_action_place_into(A, O, C, L, Duration).
actions place_into(A, O, C, L).
enabled_durative_action_place_into(A, O, C, L, Duration)at T0 if isa(A, intelligentagent), isa(O, object), isa(C, container), isa(L, physicallocation).
place_into(A, O, C, L)initiates contains(C, O)if enabled_durative_action_place_into(A, O, C, L, Duration).
place_into(A, O, C, L)terminates directly_holding(A, O)if enabled_durative_action_place_into(A, O, C, L, Duration).

fluents enabled_durative_action_drive(A, V, L0, L1, M, Duration).
actions drive(A, V, L0, L1, M).
enabled_durative_action_drive(A, V, L0, L1, M, Duration)at T0 if isa(A, intelligentagent), isa(V, vehicle), isa(L0, physicallocation), isa(L1, physicallocation), isa(M, modeoftransportation).
drive(A, V, L0, L1, M)initiates location(A, L1)if enabled_durative_action_drive(A, V, L0, L1, M, Duration).
drive(A, V, L0, L1, M)initiates location(V, L1)if enabled_durative_action_drive(A, V, L0, L1, M, Duration).
drive(A, V, L0, L1, M)terminates location(A, L0)if enabled_durative_action_drive(A, V, L0, L1, M, Duration).
drive(A, V, L0, L1, M)terminates location(V, L0)if enabled_durative_action_drive(A, V, L0, L1, M, Duration).

isa(driving, modeoftransportation).
isa(walking, modeoftransportation).
isa(bluetoothkeyboard, object).
isa(tissues, object).
isa(andrewdougherty, person).
isa(meredithmcghan, person).
isa(auroraillinois, physicallocation).
isa(flintmichigan, physicallocation).
isa(townhomeofeleanorandandrewandmeredith, residence).
isa(bluetoothkeyboard, tool).
isa(andrewdoughertyshypotheticalcar, vehicle).
isa(meredithmcghanscar, vehicle).

initially autonomous(andrewdougherty).
initially autonomous(meredithmcghan).
initially location(andrewdougherty, townhomeofeleanorandandrewandmeredith).
initially location(andrewdoughertyshypotheticalcar, auroraillinois).
initially location(bluetoothkeyboard, townhomeofeleanorandandrewandmeredith).
initially location(meredithmcghan, flintmichigan).
initially location(meredithmcghanscar, flintmichigan).
initially mobile(andrewdoughertyshypotheticalcar).
initially mobile(bluetoothkeyboard).
initially mobile(meredithmcghanscar).
initially travel_path(driving, auroraillinois, townhomeofeleanorandandrewandmeredith).
initially travel_path(driving, flintmichigan, auroraillinois).
initially travel_distance(driving, auroraillinois, townhomeofeleanorandandrewandmeredith)=5.
initially travel_distance(driving, flintmichigan, auroraillinois)=500.
initially travel_duration(driving, auroraillinois, townhomeofeleanorandandrewandmeredith)=0.15.
initially travel_duration(driving, flintmichigan, auroraillinois)=7.

metric(pddl_minimize(pddl_total_time)).

:- multifile(ec:demo_test/3).
ec:demo_test(lps_demo_test_9_run, lps_demo, [holds(directly_holding(andrewdougherty, bluetoothkeyboard),8)]).

baseKB:lps_demo_test_9_run :- abdemo([directly_holding(andrewdougherty, bluetoothkeyboard)at 8]).


