
:- expects_dialect(lps).

maxTime(50).


/*
lps_pddl_convert:test_logicmoo_lps_pddl_readerA :-
    pre_pddl_tests,
    test_logicmoo_lps_pddl_reader(pddl('assembly/domain*5.pddl')),
    test_logicmoo_lps_pddl_reader(pddl('assembly/prob20.pddl')),
    !.
*/
subtype(assembly, object).
subtype(resource, object).

fluents available(resource).
fluents available(assembly).
fluents complete(assembly).
fluents requires(assembly, resource).
fluents committed(resource, assembly).
fluents incorporated(assembly, assembly).
fluents part_of(assembly, assembly).
fluents to_be_removed(assembly, assembly).
fluents assemble_order(assembly, assembly, assembly).
fluents transient_part(assembly, assembly).
fluents remove_order(assembly, assembly, assembly).

fluents enabled_action_commit(Res, As).
actions commit(Res, As).
enabled_action_commit(Res, As)at T0 if isa(Res, resource), isa(As, assembly), available(Res)at T0.
commit(Res, As)initiates committed(Res, As)if enabled_action_commit(Res, As).
commit(Res, As)terminates available(Res)if enabled_action_commit(Res, As).

fluents enabled_action_release(Res, As).
actions release(Res, As).
enabled_action_release(Res, As)at T0 if isa(Res, resource), isa(As, assembly), committed(Res, As)at T0.
release(Res, As)initiates available(Res)if enabled_action_release(Res, As).
release(Res, As)terminates committed(Res, As)if enabled_action_release(Res, As).

fluents enabled_action_assemble(Part, Whole, Res, Prev, P, Tp).
actions assemble(Part, Whole).
enabled_action_assemble(Part, Whole, Res, Prev, P, Tp)at T0 if isa(Part, assembly), isa(Whole, assembly), all(typed(resource, Res), pddl_imply(requires(Whole, Res), committed(Res, Whole)))at T0, (part_of(Part, Whole);transient_part(Part, Whole))at T0, available(Part)at T0, all(typed(assembly, Prev), pddl_imply(assemble_order(Prev, Part, Whole), incorporated(Prev, Whole)))at T0.
assemble(Part, Whole)initiates incorporated(Part, Whole) if enabled_action_assemble(Part, Whole, Res, Prev, P, Tp).
assemble(Part, Whole)initiates complete(Whole) if enabled_action_assemble(Part, Whole, Res, Prev, P, Tp), not exists(typed(assembly, P),  (part_of(P, Whole), not P=Part, not incorporated(P, Whole))), not exists(typed(assembly, Tp),  (transient_part(Tp, Whole), incorporated(Tp, Whole))).
assemble(Part, Whole)initiates available(Whole)if enabled_action_assemble(Part, Whole, Res, Prev, P, Tp), not exists(typed(assembly, P),  (part_of(P, Whole), not P=Part, not incorporated(P, Whole))), not exists(typed(assembly, Tp),  (transient_part(Tp, Whole), incorporated(Tp, Whole))).
assemble(Part, Whole)terminates available(Part)if enabled_action_assemble(Part, Whole, Res, Prev, P, Tp).

fluents enabled_action_remove(Part, Whole, Res, Prev, P, Tp).
actions remove(Part, Whole).
enabled_action_remove(Part, Whole, Res, Prev, P, Tp)at T0 if isa(Part, assembly), isa(Whole, assembly), isa(Res, resource),
  all(typed(resource, Res), pddl_imply(requires(Whole, Res), committed(Res, Whole)))at T0, incorporated(Part, Whole)at T0, (transient_part(Part, Whole), all(typed(assembly, Prev), pddl_imply(remove_order(Prev, Part, Whole), incorporated(Prev, Whole)));part_of(Part, Whole), not exists(typed(assembly, Prev),  (assemble_order(Prev, Part, Whole), incorporated(Prev, Whole))))at T0.
remove(Part, Whole)initiates available(Part)if enabled_action_remove(Part, Whole, Res, Prev, P, Tp).
remove(Part, Whole)initiates complete(Whole)if enabled_action_remove(Part, Whole, Res, Prev, P, Tp), not exists(typed(assembly, P),  (part_of(P, Whole), not incorporated(P, Whole))), not exists(typed(assembly, Tp),  (transient_part(Tp, Whole), not Tp=Part, incorporated(Tp, Whole))).
remove(Part, Whole)initiates available(Whole)if enabled_action_remove(Part, Whole, Res, Prev, P, Tp), not exists(typed(assembly, P),  (part_of(P, Whole), not incorporated(P, Whole))), not exists(typed(assembly, Tp),  (transient_part(Tp, Whole), not Tp=Part, incorporated(Tp, Whole))).
remove(Part, Whole)terminates incorporated(Part, Whole)if enabled_action_remove(Part, Whole, Res, Prev, P, Tp).

isa(socket, assembly).
isa(device, assembly).
isa(doodad_3, assembly).
isa(kludge_4, assembly).
isa(hack_5, assembly).
isa(gimcrack_2, assembly).
isa(frob, assembly).
isa(valve, assembly).
isa(tube, assembly).
isa(plug, assembly).
isa(doodad, assembly).
isa(kludge, assembly).
isa(sprocket, assembly).
isa(wire, assembly).
isa(hack, assembly).
isa(bracket, assembly).
isa(widget, assembly).
isa(unit, assembly).
isa(connector, assembly).
isa(hoozawhatsie, assembly).
isa(thingumbob, assembly).
isa(fastener, assembly).
isa(gimcrack, assembly).
isa(contraption, assembly).
isa(foobar, assembly).
isa(whatsis, assembly).
isa(coil, assembly).
isa(mount, assembly).
isa(hammock, resource).
isa(clamp, resource).

initially available(doodad_3).
initially available(kludge_4).
initially available(hack_5).
initially available(gimcrack_2).
initially available(valve).
initially available(tube).
initially available(doodad).
initially available(kludge).
initially available(wire).
initially available(hack).
initially available(bracket).
initially available(unit).
initially available(connector).
initially available(hoozawhatsie).
initially available(thingumbob).
initially available(gimcrack).
initially available(contraption).
initially available(foobar).
initially available(whatsis).
initially available(coil).
initially available(mount).
initially requires(device, clamp).
initially requires(frob, hammock).
initially requires(plug, clamp).
initially requires(sprocket, clamp).
initially requires(widget, clamp).
initially requires(fastener, clamp).
initially part_of(device, socket).
initially part_of(frob, socket).
initially part_of(plug, socket).
initially part_of(widget, socket).
initially part_of(fastener, socket).
initially part_of(doodad_3, device).
initially part_of(kludge_4, device).
initially part_of(hack_5, device).
initially part_of(gimcrack_2, device).
initially part_of(valve, frob).
initially part_of(tube, frob).
initially part_of(doodad, plug).
initially part_of(kludge, plug).
initially part_of(sprocket, plug).
initially part_of(wire, sprocket).
initially part_of(hack, sprocket).
initially part_of(bracket, sprocket).
initially part_of(unit, widget).
initially part_of(connector, widget).
initially part_of(hoozawhatsie, widget).
initially part_of(thingumbob, widget).
initially part_of(gimcrack, fastener).
initially part_of(contraption, fastener).
initially part_of(foobar, fastener).
initially part_of(whatsis, fastener).
initially part_of(coil, fastener).
initially part_of(mount, fastener).
initially assemble_order(plug, device, socket).
initially assemble_order(widget, plug, socket).
initially assemble_order(doodad_3, kludge_4, device).
initially assemble_order(hack_5, doodad_3, device).
initially assemble_order(gimcrack_2, doodad_3, device).
initially assemble_order(valve, tube, frob).
initially assemble_order(doodad, sprocket, plug).
initially assemble_order(hoozawhatsie, unit, widget).
initially assemble_order(thingumbob, hoozawhatsie, widget).

:- multifile(ec:demo_test/3).
ec:demo_test(lps_demo_test_5_run, lps_demo, [holds(complete(socket),8)]).

baseKB:lps_demo_test_5_run :- abdemo([holds(complete(socket),8)]).


