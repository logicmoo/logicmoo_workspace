/**
 *  All rights reserved. Use of this software is permitted for non-commercial
 *  research purposes, and it may be copied only for that use.  All copies must
 *  include this copyright message.  This software is made available AS IS, and
 *  neither the GIPO team nor the University of Huddersfield make any warranty
 *  about the software or its performance.
 *
 *  Automatically generated OCL Domain from  GIPO Version 2.0
 *
 *  Author: Beth Richardson
 *  Institution: University of Huddersfield
 *  Date created: April 2000
 *  Date last modified: 2008/02/04 at 12:23:54 PM GMT
 *  Description:
 *    This tyre domain has extra objects and actions. Now, instead of just two
 *    wheels there are five with five tyres, four wheel trims and four named
 *    hubs. There are also four sets of wheel nuts. The pump now has a use. If
 *    a tyre is low it may be 'flat' or 'punctured'. If it is flat then a prevail of
 *    inflate_tyre is have(pump).
 */

domain_name(tyre_extended).

option(hierarchical).

% Sorts
sorts(primitive_sorts,[container,nuts,hub,pump,wheel,wrench,jack,wheel_trim,tyre]).

% Objects
objects(container,[boot]).
objects(nuts,[nuts1,nuts2,nuts3,nuts4]).
objects(hub,[hub1,hub2,hub3,hub4]).
objects(pump,[pump0]).
objects(wheel,[wheel1,wheel2,wheel3,wheel4,wheel5]).
objects(wrench,[wrench0]).
objects(jack,[jack0]).
objects(wheel_trim,[trim1,trim2,trim3,trim4]).
objects(tyre,[tyre1,tyre2,tyre3,tyre4,tyre5]).

% Predicates
predicates([
    closed(container),
    open(container),
    tight(nuts,hub),
    loose(nuts,hub),
    have_nuts(nuts),
    on_ground(hub),
    fastened(hub),
    jacked_up(hub,jack),
    free(hub),
    unfastened(hub),
    have_pump(pump),
    pump_in(pump,container),
    have_wheel(wheel),
    wheel_in(wheel,container),
    wheel_on(wheel,hub),
    have_wrench(wrench),
    wrench_in(wrench,container),
    have_jack(jack),
    jack_in_use(jack,hub),
    jack_in(jack,container),
    trim_on(wheel_trim,wheel),
    trim_off(wheel_trim),
    fits_on(tyre,wheel),
    full(tyre),
    flat(tyre),
    punctured(tyre)]).

% Object Class Definitions
substate_classes(container,C,[
    [closed(C)],
    [open(C)]]).
substate_classes(nuts,N,[
    [tight(N,H)],
    [loose(N,H)],
    [have_nuts(N)]]).
substate_classes(hub,H,[
    [on_ground(H),fastened(H)],
    [jacked_up(H,J),fastened(H)],
    [free(H),jacked_up(H,J),unfastened(H)],
    [unfastened(H),jacked_up(H,J)]]).
substate_classes(pump,Pu,[
    [have_pump(Pu)],
    [pump_in(Pu,C)]]).
substate_classes(wheel,Wh,[
    [have_wheel(Wh)],
    [wheel_in(Wh,C)],
    [wheel_on(Wh,H)]]).
substate_classes(wrench,Wr,[
    [have_wrench(Wr)],
    [wrench_in(Wr,C)]]).
substate_classes(jack,J,[
    [have_jack(J)],
    [jack_in_use(J,H)],
    [jack_in(J,C)]]).
substate_classes(wheel_trim,WT,[
    [trim_on(WT,Wh)],
    [trim_off(WT)]]).
substate_classes(tyre,Ty,[
    [fits_on(Ty,Wh)],
    [full(Ty)],
    [flat(Ty)],
    [punctured(Ty)]]).

% Atomic Invariants
atomic_invariants([
    fits_on(tyre1,wheel1),
    fits_on(tyre2,wheel2),
    fits_on(tyre3,wheel3),
    fits_on(tyre4,wheel4),
    fits_on(tyre5,wheel5)]).

% Implied Invariants

% Inconsistent Constraints
inconsistent_constraint([have_nuts(N),tight(N,_)]).
inconsistent_constraint([have_nuts(N),loose(N,_)]).
inconsistent_constraint([loose(_,H),tight(_,H)]).
inconsistent_constraint([unfastened(H),tight(_,H)]).
inconsistent_constraint([unfastened(H),loose(_,H)]).
inconsistent_constraint([wheel_in(Wh,_),wheel_on(Wh,_)]).
inconsistent_constraint([wheel_in(Wh,_),have_wheel(Wh)]).
inconsistent_constraint([jack_in(J,_),have_jack(J)]).
inconsistent_constraint([pump_in(Pu,_),have_pump(Pu)]).
inconsistent_constraint([wrench_in(Wr,_),have_wrench(Wr)]).
inconsistent_constraint([open(C),closed(C)]).
inconsistent_constraint([full(Ty),flat(Ty)]).
inconsistent_constraint([full(Ty),punctured(Ty)]).
inconsistent_constraint([flat(Ty),punctured(Ty)]).
inconsistent_constraint([fastened(H),unfastened(H)]).
inconsistent_constraint([jacked_up(H,J),on_ground(H)]).
inconsistent_constraint([free(H),wheel_on(_,H)]).
inconsistent_constraint([free(X),fastened(X)]).
inconsistent_constraint([free(X),tight(Nuts,X)]).
inconsistent_constraint([free(X),loose(Nuts,X)]).
inconsistent_constraint([wheel_on(W1,X),wheel_on(W2,X),ne(W1,W2)]).
inconsistent_constraint([wheel_on(W,H1),wheel_on(W,H2),ne(H1,H2)]).
inconsistent_constraint([wheel_on(W,H1),have_wheel(W)]).
inconsistent_constraint([jacked_up(H,J),jack_in(J,_)]).
inconsistent_constraint([jacked_up(H,J),have_jack(J)]).
inconsistent_constraint([fastened(H),have_nuts(N)]).
inconsistent_constraint([jack_in_use(J,_),jack_in(J,_)]).
inconsistent_constraint([jack_in_use(J,_),have_jack(J)]).
inconsistent_constraint([jack_in_use(J,H),on_ground(H)]).
inconsistent_constraint([trim_on(WT,Wh),loose(N,H)]).
inconsistent_constraint([trim_on(Wheel_trim,Wheel),jack_in_use(Jack,Hub)]).
inconsistent_constraint([trim_on(Wheel_trim,Wheel),have_nuts(Nuts)]).
inconsistent_constraint([trim_on(Wheel_trim,Wheel),free(Hub)]).
inconsistent_constraint([trim_on(Wheel_trim,Wheel),unfastened(Hub)]).

% Operators
/****
 * The nuts attach the wheel then the hub is jacked down. The nuts are tightened and the trim applied.
 * method(attach_hub(Nuts1,Hub1,Jack0,Trim1,Wheel5),
 * % pre-condition
 * [
 * se(wrench,Wrench0,[have_wrench(Wrench0)]),
 * se(wheel,Wheel5,[wheel_on(Wheel5,Hub1)])],
 * % Index Transitions
 * [
 * sc(nuts,Nuts1,[have_nuts(Nuts1)]=>[tight(Nuts1,Hub1)]),
 * sc(hub,Hub1,[unfastened(Hub1),jacked_up(Hub1,Jack0)]=>[on_ground(Hub1),fastened(Hub1)]),
 * sc(jack,Jack0,[jack_in_use(Jack0,Hub1)]=>[have_jack(Jack0)]),
 * sc(wheel_trim,Trim1,[trim_off(Trim1)]=>[trim_on(Trim1,Wheel5)])],
 * % Static
 * [
 * ],
 * % Temporal Constraints
 * [
 * before(1,2),
 * before(2,3),
 * before(3,4)],
 * % Decomposition
 * [
 * do_up(Wr,WT,H,J,N),
 * jack_down(N,H,J),
 * tighten(Wr,H,WT,N),
 * apply_trim(H,WT,Wh)]
 * ).
 * The jack and wrench are replaced in the boot which remains open.
 * method(putaway_tools(Wrench0,C,Jack0),
 * % pre-condition
 * [
 * se(container,Boot,[open(Boot)])],
 * % Index Transitions
 * [
 * sc(wrench,Wrench0,[have_wrench(Wrench0)]=>[wrench_in(Wrench0,C)]),
 * sc(jack,Jack0,[have_jack(Jack0)]=>[jack_in(Jack0,C)])],
 * % Static
 * [
 * ],
 * % Temporal Constraints
 * [
 * before(1,2)],
 * % Decomposition
 * [
 * putaway_wrench(C,Wr),
 * putaway_jack(C,J)]
 * ).
 */
operator(open_container(C),
    % prevail
    [],
    % necessary
    [     sc(container,C,[closed(C)]=>[open(C)])],
    % conditional
    []).
operator(close_container(C),
    % prevail
    [],
    % necessary
    [     sc(container,C,[open(C)]=>[closed(C)])],
    % conditional
    []).
operator(fetch_jack(C,J),
    % prevail
    [     se(container,C,[open(C)])],
    % necessary
    [     sc(jack,J,[jack_in(J,C)]=>[have_jack(J)])],
    % conditional
    []).
operator(fetch_wheel(C,Wh),
    % prevail
    [     se(container,C,[open(C)])],
    % necessary
    [     sc(wheel,Wh,[wheel_in(Wh,C)]=>[have_wheel(Wh)])],
    % conditional
    []).
operator(fetch_wrench(C,Wr),
    % prevail
    [     se(container,C,[open(C)])],
    % necessary
    [     sc(wrench,Wr,[wrench_in(Wr,C)]=>[have_wrench(Wr)])],
    % conditional
    []).
operator(fetch_pump(C,Pu),
    % prevail
    [     se(container,C,[open(C)])],
    % necessary
    [     sc(pump,Pu,[pump_in(Pu,C)]=>[have_pump(Pu)])],
    % conditional
    []).
operator(putaway_wheel(C,Wh),
    % prevail
    [     se(container,C,[open(C)])],
    % necessary
    [     sc(wheel,Wh,[have_wheel(Wh)]=>[wheel_in(Wh,C)])],
    % conditional
    []).
operator(putaway_wrench(C,Wr),
    % prevail
    [     se(container,C,[open(C)])],
    % necessary
    [     sc(wrench,Wr,[have_wrench(Wr)]=>[wrench_in(Wr,C)])],
    % conditional
    []).
operator(putaway_jack(C,J),
    % prevail
    [     se(container,C,[open(C)])],
    % necessary
    [     sc(jack,J,[have_jack(J)]=>[jack_in(J,C)])],
    % conditional
    []).
operator(putaway_pump(C,Pu),
    % prevail
    [     se(container,C,[open(C)])],
    % necessary
    [     sc(pump,Pu,[have_pump(Pu)]=>[pump_in(Pu,C)])],
    % conditional
    []).
operator(loosen(Wr,H,WT,N),
    % prevail
    [     se(wrench,Wr,[have_wrench(Wr)]),
     se(hub,H,[on_ground(H),fastened(H)]),
     se(wheel_trim,WT,[trim_off(WT)])],
    % necessary
    [     sc(nuts,N,[tight(N,H)]=>[loose(N,H)])],
    % conditional
    []).
operator(tighten(Wr,H,WT,N),
    % prevail
    [     se(wrench,Wr,[have_wrench(Wr)]),
     se(hub,H,[on_ground(H),fastened(H)]),
     se(wheel_trim,WT,[trim_off(WT)])],
    % necessary
    [     sc(nuts,N,[loose(N,H)]=>[tight(N,H)])],
    % conditional
    []).
operator(jack_up(N,H,J),
    % prevail
    [     se(nuts,N,[loose(N,H)])],
    % necessary
    [     sc(hub,H,[on_ground(H),fastened(H)]=>[jacked_up(H,J),fastened(H)]),
     sc(jack,J,[have_jack(J)]=>[jack_in_use(J,H)])],
    % conditional
    []).
operator(jack_down(N,H,J),
    % prevail
    [     se(nuts,N,[loose(N,H)])],
    % necessary
    [     sc(hub,H,[jacked_up(H,J),fastened(H)]=>[on_ground(H),fastened(H)]),
     sc(jack,J,[jack_in_use(J,H)]=>[have_jack(J)])],
    % conditional
    []).
operator(do_up(Wr,WT,H,J,N),
    % prevail
    [     se(wrench,Wr,[have_wrench(Wr)]),
     se(wheel_trim,WT,[trim_off(WT)])],
    % necessary
    [     sc(hub,H,[unfastened(H),jacked_up(H,J)]=>[jacked_up(H,J),fastened(H)]),
     sc(nuts,N,[have_nuts(N)]=>[loose(N,H)])],
    % conditional
    []).
operator(remove_wheel(WT,Wh,H,J),
    % prevail
    [     se(wheel_trim,WT,[trim_off(WT)])],
    % necessary
    [     sc(wheel,Wh,[wheel_on(Wh,H)]=>[have_wheel(Wh)]),
     sc(hub,H,[unfastened(H),jacked_up(H,J)]=>[free(H),jacked_up(H,J),unfastened(H)])],
    % conditional
    []).
operator(put_on_wheel(WT,Wh,H,J),
    % prevail
    [     se(wheel_trim,WT,[trim_off(WT)])],
    % necessary
    [     sc(wheel,Wh,[have_wheel(Wh)]=>[wheel_on(Wh,H)]),
     sc(hub,H,[free(H),jacked_up(H,J),unfastened(H)]=>[unfastened(H),jacked_up(H,J)])],
    % conditional
    []).
operator(undo(Wr,WT,H,J,N),
    % prevail
    [     se(wrench,Wr,[have_wrench(Wr)]),
     se(wheel_trim,WT,[trim_off(WT)])],
    % necessary
    [     sc(hub,H,[jacked_up(H,J),fastened(H)]=>[unfastened(H),jacked_up(H,J)]),
     sc(nuts,N,[loose(N,H)]=>[have_nuts(N)])],
    % conditional
    []).
operator(apply_trim(H,WT,Wh),
    % prevail
    [     se(hub,H,[on_ground(H),fastened(H)])],
    % necessary
    [     sc(wheel_trim,WT,[trim_off(WT)]=>[trim_on(WT,Wh)])],
    % conditional
    []).
operator(remove_trim(H,WT,Wh),
    % prevail
    [     se(hub,H,[on_ground(H),fastened(H)])],
    % necessary
    [     sc(wheel_trim,WT,[trim_on(WT,Wh)]=>[trim_off(WT)])],
    % conditional
    []).
operator(inflate_tyre(Pu,Ty),
    % prevail
    [     se(pump,Pu,[have_pump(Pu)])],
    % necessary
    [     sc(tyre,Ty,[flat(Ty)]=>[full(Ty)])],
    % conditional
    []).
operator(find_puncture(Pu,Ty),
    % prevail
    [     se(pump,Pu,[have_pump(Pu)])],
    % necessary
    [     sc(tyre,Ty,[flat(Ty)]=>[punctured(Ty)])],
    % conditional
    []).

% Methods
/****
 * A flat but not punctured tyre is inflated and becomes a full tyre.
 */
method(fix_flat(Tyre2),
    % pre-condition
    [
     se(container,Boot,[closed(Boot)]),
     se(pump,Pump0,[pump_in(Pump0,C)])],
    % Index Transitions
    [
     sc(tyre,Tyre2,[flat(Tyre2)]=>[full(Tyre2)])],
    % Static
    [
],
    % Temporal Constraints
    [
     before(1,2),
     before(2,3),
     before(3,4),
     before(4,5)],
    % Decomposition
    [
     open_container(C),
     fetch_pump(C,Pu),
     inflate_tyre(Pu,Ty),
     putaway_pump(C,Pu),
     close_container(C)]
).
/****
 * A flat tyre can't be inflated so is found to punctured. The boot is left open because the wheel will need to be changed.
 */
method(discover_puncture(Boot,Tyre1),
    % pre-condition
    [
     se(pump,Pump0,[pump_in(Pump0,C)])],
    % Index Transitions
    [
     sc(container,Boot,[closed(Boot)]=>[open(Boot)]),
     sc(tyre,Tyre1,[flat(Tyre1)]=>[punctured(Tyre1)])],
    % Static
    [
],
    % Temporal Constraints
    [
     before(1,2),
     before(2,3),
     before(3,4)],
    % Decomposition
    [
     open_container(C),
     fetch_pump(C,Pu),
     find_puncture(Pu,Ty),
     putaway_pump(C,Pu)]
).
/****
 * The wrench and jack are fetched from the boot ready to change the wheel.
 */
method(fetch_tools(Wrench0,C,Jack0),
    % pre-condition
    [
     se(container,Boot,[open(Boot)])],
    % Index Transitions
    [
     sc(wrench,Wrench0,[wrench_in(Wrench0,C)]=>[have_wrench(Wrench0)]),
     sc(jack,Jack0,[jack_in(Jack0,C)]=>[have_jack(Jack0)])],
    % Static
    [
],
    % Temporal Constraints
    [
     before(1,2)],
    % Decomposition
    [
     fetch_wrench(C,Wr),
     fetch_jack(C,J)]
).
/****
 * The trim is removed and the nuts are loosened. Then the hub is jacked up and the nuts are removed.
 */
method(unfasten_hub(Nuts1,Hub1,Jack0,Trim1,Wheel1),
    % pre-condition
    [
],
    % Index Transitions
    [
     sc(nuts,Nuts1,[tight(Nuts1,Hub1)]=>[have_nuts(Nuts1)]),
     sc(hub,Hub1,[on_ground(Hub1),fastened(Hub1)]=>[unfastened(Hub1),jacked_up(Hub1,Jack0)]),
     sc(jack,Jack0,[have_jack(Jack0)]=>[jack_in_use(Jack0,Hub1)]),
     sc(wheel_trim,Trim1,[trim_on(Trim1,Wheel1)]=>[trim_off(Trim1)])],
    % Static
    [
],
    % Temporal Constraints
    [
     before(1,2),
     before(2,3),
     before(3,4)],
    % Decomposition
    [
     remove_trim(H,WT,Wh),
     loosen(Wr,H,WT,N),
     jack_up(N,H,J),
     undo(Wr,WT,H,J,N)]
).
/****
 * The punctured wheel is exchanged for the spare in the boot.
 */
method(change_wheel(Wheel5,C,H,Wheel1),
    % pre-condition
    [
     se(container,Boot,[open(Boot)]),
     se(nuts,Nuts1,[have_nuts(Nuts1)]),
     se(hub,Hub1,[unfastened(Hub1),jacked_up(Hub1,J)]),
     se(jack,Jack1,[jack_in_use(Jack1,H)]),
     se(wheel_trim,Trim1,[trim_off(Trim1)])],
    % Index Transitions
    [
     sc(wheel,Wheel5,[wheel_in(Wheel5,C)]=>[wheel_on(Wheel5,H)]),
     sc(wheel,Wheel1,[wheel_on(Wheel1,H)]=>[wheel_in(Wheel1,C)])],
    % Static
    [
],
    % Temporal Constraints
    [
     before(1,2),
     before(2,3),
     before(3,4)],
    % Decomposition
    [
     fetch_wheel(C,Wh),
     remove_wheel(WT,Wh,H,J),
     put_on_wheel(WT,Wh,H,J),
     putaway_wheel(C,Wh)]
).
/****
 * The nuts attach the wheel then the hub is jacked down. The nuts are tightened and the trim applied.
 */
method(attach_hub(Nuts1,Hub1,Jack0,Trim1,Wheel5),
    % pre-condition
    [
     se(wrench,Wrench0,[have_wrench(Wrench0)]),
     se(wheel,Wheel5,[wheel_on(Wheel5,Hub1)])],
    % Index Transitions
    [
     sc(nuts,Nuts1,[have_nuts(Nuts1)]=>[tight(Nuts1,Hub1)]),
     sc(hub,Hub1,[unfastened(Hub1),jacked_up(Hub1,Jack0)]=>[on_ground(Hub1),fastened(Hub1)]),
     sc(jack,Jack0,[jack_in_use(Jack0,Hub1)]=>[have_jack(Jack0)]),
     sc(wheel_trim,Trim1,[trim_off(Trim1)]=>[trim_on(Trim1,Wheel5)])],
    % Static
    [
],
    % Temporal Constraints
    [
     before(1,2),
     before(2,3),
     before(3,4)],
    % Decomposition
    [
     do_up(Wr,WT,H,J,N),
     jack_down(N,H,J),
     tighten(Wr,H,WT,N),
     apply_trim(H,WT,Wh)]
).
/****
 * The jack and wrench are replaced in the boot which remains open.
 */
method(putaway_tools(Wrench0,C,Jack0),
    % pre-condition
    [
     se(container,Boot,[open(Boot)])],
    % Index Transitions
    [
     sc(wrench,Wrench0,[have_wrench(Wrench0)]=>[wrench_in(Wrench0,C)]),
     sc(jack,Jack0,[have_jack(Jack0)]=>[jack_in(Jack0,C)])],
    % Static
    [
],
    % Temporal Constraints
    [
     before(1,2)],
    % Decomposition
    [
     putaway_wrench(C,Wr),
     putaway_jack(C,J)]
).

% Domain Tasks
planner_task(1,
    % Goals
    [
     se(container,boot,[closed(boot)]),
     se(wheel,wheel1,[wheel_on(wheel1,hub1)]),
     se(tyre,tyre1,[full(tyre1)]),
     se(wheel_trim,trim1,[trim_on(trim1,wheel1)]),
     se(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     se(pump,pump0,[pump_in(pump0,boot)]),
     se(wrench,wrench0,[wrench_in(wrench0,boot)]),
     se(jack,jack0,[jack_in(jack0,boot)])],
    % INIT States
    [
     ss(container,boot,[closed(boot)]),
     ss(wheel,wheel1,[wheel_on(wheel1,hub1)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(wheel,wheel5,[wheel_in(wheel5,boot)]),
     ss(tyre,tyre1,[flat(tyre1)]),
     ss(wheel_trim,trim1,[trim_on(trim1,wheel1)]),
     ss(jack,jack0,[jack_in(jack0,boot)]),
     ss(wrench,wrench0,[wrench_in(wrench0,boot)]),
     ss(pump,pump0,[pump_in(pump0,boot)]),
     ss(hub,hub1,[on_ground(hub1),fastened(hub1)])]).
planner_task(2,
    % Goals
    [
     se(container,boot,[closed(boot)]),
     se(wheel,wheel1,[wheel_in(wheel1,boot)]),
     se(wheel,wheel5,[wheel_on(wheel5,hub1)]),
     se(tyre,tyre1,[punctured(tyre1)]),
     se(wheel_trim,trim1,[trim_on(trim1,wheel5)]),
     se(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     se(pump,pump0,[pump_in(pump0,boot)]),
     se(wrench,wrench0,[wrench_in(wrench0,boot)]),
     se(jack,jack0,[jack_in(jack0,boot)]),
     se(nuts,nuts1,[tight(nuts1,hub1)])],
    % INIT States
    [
     ss(container,boot,[closed(boot)]),
     ss(wheel,wheel1,[wheel_on(wheel1,hub1)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(wheel,wheel5,[wheel_in(wheel5,boot)]),
     ss(tyre,tyre1,[flat(tyre1)]),
     ss(wheel_trim,trim1,[trim_on(trim1,wheel1)]),
     ss(jack,jack0,[jack_in(jack0,boot)]),
     ss(wrench,wrench0,[wrench_in(wrench0,boot)]),
     ss(pump,pump0,[pump_in(pump0,boot)]),
     ss(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     ss(nuts,nuts1,[tight(nuts1,hub1)]),
     ss(nuts,nuts2,[tight(nuts2,hub2)]),
     ss(nuts,nuts3,[tight(nuts3,hub3)]),
     ss(nuts,nuts4,[tight(nuts4,hub4)])]).
planner_task(3,
    % Goals
    [
     se(container,boot,[closed(boot)]),
     se(tyre,tyre1,[full(tyre1)]),
     se(wheel,wheel1,[wheel_on(wheel1,hub4)]),
     se(wheel,wheel4,[wheel_in(wheel4,boot)]),
     se(wheel_trim,trim4,[trim_on(trim4,wheel1)])],
    % INIT States
    [
     ss(wheel,wheel1,[wheel_in(wheel1,boot)]),
     ss(tyre,tyre1,[flat(tyre1)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(tyre,tyre2,[full(tyre2)]),
     ss(wheel_trim,trim2,[trim_on(trim2,wheel2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(tyre,tyre3,[full(tyre3)]),
     ss(wheel_trim,trim3,[trim_on(trim3,wheel3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(tyre,tyre4,[punctured(tyre4)]),
     ss(wheel_trim,trim4,[trim_on(trim4,wheel4)]),
     ss(wheel,wheel5,[wheel_on(wheel5,hub1)]),
     ss(tyre,tyre5,[full(tyre5)]),
     ss(wheel_trim,trim1,[trim_on(trim1,wheel5)]),
     ss(container,boot,[closed(boot)]),
     ss(nuts,nuts1,[tight(nuts1,hub1)]),
     ss(nuts,nuts2,[tight(nuts2,hub2)]),
     ss(nuts,nuts3,[tight(nuts3,hub3)]),
     ss(nuts,nuts4,[tight(nuts4,hub4)]),
     ss(pump,pump0,[pump_in(pump0,boot)]),
     ss(wrench,wrench0,[wrench_in(wrench0,boot)]),
     ss(jack,jack0,[jack_in(jack0,boot)]),
     ss(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     ss(hub,hub2,[on_ground(hub2),fastened(hub2)]),
     ss(hub,hub3,[on_ground(hub3),fastened(hub3)]),
     ss(hub,hub4,[on_ground(hub4),fastened(hub4)])]).
planner_task(4,
    % Goals
    [
     se(wheel,wheel5,[wheel_on(wheel5,hub3)]),
     se(hub,hub3,[on_ground(hub3),fastened(hub3)]),
     se(wheel_trim,trim3,[trim_on(trim3,wheel5)]),
     se(wheel,wheel3,[wheel_in(wheel3,boot)]),
     se(nuts,nuts3,[tight(nuts3,hub3)])],
    % INIT States
    [
     ss(wheel_trim,trim3,[trim_off(trim3)]),
     ss(tyre,tyre3,[punctured(tyre3)]),
     ss(hub,hub3,[on_ground(hub3),fastened(hub3)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(pump,pump0,[have_pump(pump0)]),
     ss(container,boot,[open(boot)]),
     ss(tyre,tyre5,[full(tyre5)]),
     ss(wheel,wheel5,[wheel_in(wheel5,boot)]),
     ss(wrench,wrench0,[wrench_in(wrench0,boot)]),
     ss(jack,jack0,[jack_in(jack0,boot)]),
     ss(nuts,nuts3,[tight(nuts3,hub3)])]).
planner_task(5,
    % Goals
    [
     se(container,boot,[open(boot)]),
     se(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     se(wheel_trim,trim1,[trim_on(trim1,wheel1)]),
     se(tyre,tyre1,[punctured(tyre1)]),
     se(wheel,wheel1,[wheel_on(wheel1,hub1)]),
     se(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     se(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     se(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     se(wheel,wheel5,[wheel_in(wheel5,boot)]),
     se(pump,pump0,[pump_in(pump0,boot)]),
     se(jack,jack0,[have_jack(jack0)]),
     se(wrench,wrench0,[have_wrench(wrench0)])],
    % INIT States
    [
     ss(container,boot,[closed(boot)]),
     ss(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     ss(wheel_trim,trim1,[trim_on(trim1,wheel1)]),
     ss(tyre,tyre1,[flat(tyre1)]),
     ss(jack,jack0,[jack_in(jack0,boot)]),
     ss(wrench,wrench0,[wrench_in(wrench0,boot)]),
     ss(pump,pump0,[pump_in(pump0,boot)]),
     ss(wheel,wheel1,[wheel_on(wheel1,hub1)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(wheel,wheel5,[wheel_in(wheel5,boot)])]).
planner_task(6,
    % Goals
    [
     se(nuts,nuts1,[have_nuts(nuts1)]),
     se(hub,hub1,[free(hub1),jacked_up(hub1,jack0),unfastened(hub1)]),
     se(wheel,wheel1,[have_wheel(wheel1)]),
     se(wheel_trim,trim1,[trim_off(trim1)])],
    % INIT States
    [
     ss(nuts,nuts1,[tight(nuts1,hub1)]),
     ss(nuts,nuts2,[tight(nuts2,hub2)]),
     ss(nuts,nuts3,[tight(nuts3,hub3)]),
     ss(nuts,nuts4,[tight(nuts4,hub4)]),
     ss(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     ss(wheel,wheel1,[wheel_on(wheel1,hub1)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(wheel,wheel5,[wheel_in(wheel5,boot)]),
     ss(wrench,wrench0,[have_wrench(wrench0)]),
     ss(jack,jack0,[have_jack(jack0)]),
     ss(wheel_trim,trim1,[trim_on(trim1,wheel1)]),
     ss(wheel_trim,trim2,[trim_on(trim2,wheel2)]),
     ss(wheel_trim,trim3,[trim_on(trim3,wheel3)]),
     ss(wheel_trim,trim4,[trim_on(trim4,wheel4)]),
     ss(tyre,tyre1,[punctured(tyre1)])]).
planner_task(7,
    % Goals
    [
     se(tyre,tyre1,[punctured(tyre1)]),
     se(wheel_trim,trim1,[trim_off(trim1)]),
     se(jack,jack0,[jack_in_use(jack0,hub1)]),
     se(wrench,wrench0,[have_wrench(wrench0)]),
     se(wheel,wheel1,[have_wheel(wheel1)]),
     se(hub,hub1,[free(hub1),jacked_up(hub1,jack0),unfastened(hub1)]),
     se(nuts,nuts1,[have_nuts(nuts1)]),
     se(container,boot,[open(boot)])],
    % INIT States
    [
     ss(container,boot,[closed(boot)]),
     ss(nuts,nuts1,[tight(nuts1,hub1)]),
     ss(nuts,nuts2,[tight(nuts2,hub2)]),
     ss(nuts,nuts3,[tight(nuts3,hub3)]),
     ss(nuts,nuts4,[tight(nuts4,hub4)]),
     ss(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     ss(pump,pump0,[pump_in(pump0,boot)]),
     ss(wheel,wheel1,[wheel_on(wheel1,hub1)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(wheel,wheel5,[wheel_in(wheel5,boot)]),
     ss(wrench,wrench0,[wrench_in(wrench0,boot)]),
     ss(jack,jack0,[jack_in(jack0,boot)]),
     ss(wheel_trim,trim1,[trim_on(trim1,wheel1)]),
     ss(wheel_trim,trim2,[trim_on(trim2,wheel2)]),
     ss(wheel_trim,trim3,[trim_on(trim3,wheel3)]),
     ss(wheel_trim,trim4,[trim_on(trim4,wheel4)]),
     ss(tyre,tyre1,[flat(tyre1)])]).
planner_task(8,
    % Goals
    [
     se(container,boot,[closed(boot)]),
     se(nuts,nuts1,[tight(nuts1,hub1)]),
     se(nuts,nuts2,[tight(nuts2,hub2)]),
     se(nuts,nuts3,[tight(nuts3,hub3)]),
     se(nuts,nuts4,[tight(nuts4,hub4)]),
     se(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     se(hub,hub2,[on_ground(hub2),fastened(hub2)]),
     se(hub,hub3,[on_ground(hub3),fastened(hub3)]),
     se(hub,hub4,[on_ground(hub4),fastened(hub4)]),
     se(pump,pump0,[pump_in(pump0,boot)]),
     se(wheel,wheel1,[wheel_in(wheel1,boot)]),
     se(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     se(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     se(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     se(wheel,wheel5,[wheel_on(wheel5,hub1)]),
     se(wrench,wrench0,[wrench_in(wrench0,boot)]),
     se(jack,jack0,[jack_in(jack0,boot)]),
     se(wheel_trim,trim1,[trim_on(trim1,wheel5)]),
     se(wheel_trim,trim2,[trim_on(trim2,wheel2)]),
     se(wheel_trim,trim3,[trim_on(trim3,wheel3)]),
     se(wheel_trim,trim4,[trim_on(trim4,wheel4)]),
     se(tyre,tyre1,[punctured(tyre1)]),
     se(tyre,tyre2,[full(tyre2)]),
     se(tyre,tyre3,[full(tyre3)]),
     se(tyre,tyre4,[full(tyre4)]),
     se(tyre,tyre5,[full(tyre5)])],
    % INIT States
    [
     ss(container,boot,[closed(boot)]),
     ss(nuts,nuts1,[tight(nuts1,hub1)]),
     ss(nuts,nuts2,[tight(nuts2,hub2)]),
     ss(nuts,nuts3,[tight(nuts3,hub3)]),
     ss(nuts,nuts4,[tight(nuts4,hub4)]),
     ss(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     ss(hub,hub2,[on_ground(hub2),fastened(hub2)]),
     ss(hub,hub3,[on_ground(hub3),fastened(hub3)]),
     ss(hub,hub4,[on_ground(hub4),fastened(hub4)]),
     ss(pump,pump0,[pump_in(pump0,boot)]),
     ss(wheel,wheel1,[wheel_on(wheel1,hub1)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(wheel,wheel5,[wheel_in(wheel5,boot)]),
     ss(wrench,wrench0,[wrench_in(wrench0,boot)]),
     ss(jack,jack0,[jack_in(jack0,boot)]),
     ss(wheel_trim,trim1,[trim_on(trim1,wheel1)]),
     ss(wheel_trim,trim2,[trim_on(trim2,wheel2)]),
     ss(wheel_trim,trim3,[trim_on(trim3,wheel3)]),
     ss(wheel_trim,trim4,[trim_on(trim4,wheel4)]),
     ss(tyre,tyre2,[flat(tyre2)]),
     ss(tyre,tyre3,[full(tyre3)]),
     ss(tyre,tyre4,[full(tyre4)]),
     ss(tyre,tyre5,[full(tyre5)]),
     ss(tyre,tyre1,[flat(tyre1)])]).

% HTN Domain Tasks
htn_task(9,
    goal(
          [
            fix_flat(tyre2)],
    % Temporal Constraints
          [
],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(container,boot,[closed(boot)]),
     ss(nuts,nuts1,[tight(nuts1,hub1)]),
     ss(nuts,nuts2,[tight(nuts2,hub2)]),
     ss(nuts,nuts3,[tight(nuts3,hub3)]),
     ss(nuts,nuts4,[tight(nuts4,hub4)]),
     ss(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     ss(hub,hub2,[on_ground(hub2),fastened(hub2)]),
     ss(hub,hub3,[on_ground(hub3),fastened(hub3)]),
     ss(hub,hub4,[on_ground(hub4),fastened(hub4)]),
     ss(pump,pump0,[pump_in(pump0,boot)]),
     ss(wheel,wheel1,[wheel_on(wheel1,hub1)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(wheel,wheel5,[wheel_in(wheel5,boot)]),
     ss(wrench,wrench0,[wrench_in(wrench0,boot)]),
     ss(jack,jack0,[jack_in(jack0,boot)]),
     ss(wheel_trim,trim1,[trim_on(trim1,wheel1)]),
     ss(wheel_trim,trim2,[trim_on(trim2,wheel2)]),
     ss(wheel_trim,trim3,[trim_on(trim3,wheel3)]),
     ss(wheel_trim,trim4,[trim_on(trim4,wheel4)]),
     ss(tyre,tyre1,[flat(tyre1)]),
     ss(tyre,tyre2,[flat(tyre2)]),
     ss(tyre,tyre3,[full(tyre3)]),
     ss(tyre,tyre4,[full(tyre4)]),
     ss(tyre,tyre5,[full(tyre5)])]).
htn_task(10,
    goal(
          [
            discover_puncture(boot,tyre1)],
    % Temporal Constraints
          [
],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(container,boot,[closed(boot)]),
     ss(nuts,nuts1,[tight(nuts1,hub1)]),
     ss(nuts,nuts2,[tight(nuts2,hub2)]),
     ss(nuts,nuts3,[tight(nuts3,hub3)]),
     ss(nuts,nuts4,[tight(nuts4,hub4)]),
     ss(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     ss(hub,hub2,[on_ground(hub2),fastened(hub2)]),
     ss(hub,hub3,[on_ground(hub3),fastened(hub3)]),
     ss(hub,hub4,[on_ground(hub4),fastened(hub4)]),
     ss(pump,pump0,[pump_in(pump0,boot)]),
     ss(wheel,wheel1,[wheel_on(wheel1,hub1)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(wheel,wheel5,[wheel_in(wheel5,boot)]),
     ss(wrench,wrench0,[wrench_in(wrench0,boot)]),
     ss(jack,jack0,[jack_in(jack0,boot)]),
     ss(wheel_trim,trim1,[trim_on(trim1,wheel1)]),
     ss(wheel_trim,trim2,[trim_on(trim2,wheel2)]),
     ss(wheel_trim,trim3,[trim_on(trim3,wheel3)]),
     ss(wheel_trim,trim4,[trim_on(trim4,wheel4)]),
     ss(tyre,tyre1,[flat(tyre1)]),
     ss(tyre,tyre2,[flat(tyre2)]),
     ss(tyre,tyre3,[full(tyre3)]),
     ss(tyre,tyre4,[full(tyre4)]),
     ss(tyre,tyre5,[full(tyre5)])]).
htn_task(11,
    goal(
          [
            fetch_tools(wrench0,boot,jack0)],
    % Temporal Constraints
          [
],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(container,boot,[open(boot)]),
     ss(nuts,nuts2,[tight(nuts2,hub2)]),
     ss(nuts,nuts3,[tight(nuts3,hub3)]),
     ss(nuts,nuts4,[tight(nuts4,hub4)]),
     ss(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     ss(hub,hub2,[on_ground(hub2),fastened(hub2)]),
     ss(hub,hub3,[on_ground(hub3),fastened(hub3)]),
     ss(hub,hub4,[on_ground(hub4),fastened(hub4)]),
     ss(pump,pump0,[pump_in(pump0,boot)]),
     ss(wheel,wheel1,[wheel_on(wheel1,hub1)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(wheel,wheel5,[wheel_in(wheel5,boot)]),
     ss(wrench,wrench0,[wrench_in(wrench0,boot)]),
     ss(jack,jack0,[jack_in(jack0,boot)]),
     ss(wheel_trim,trim1,[trim_off(trim1)]),
     ss(wheel_trim,trim2,[trim_on(trim2,wheel2)]),
     ss(wheel_trim,trim3,[trim_on(trim3,wheel3)]),
     ss(wheel_trim,trim4,[trim_on(trim4,wheel4)]),
     ss(tyre,tyre1,[punctured(tyre1)]),
     ss(tyre,tyre2,[full(tyre2)]),
     ss(tyre,tyre3,[full(tyre3)]),
     ss(tyre,tyre4,[full(tyre4)]),
     ss(tyre,tyre5,[full(tyre5)]),
     ss(nuts,nuts1,[tight(nuts1,hub1)])]).
htn_task(12,
    goal(
          [
            unfasten_hub(nuts1,hub1,jack0,trim1,wheel1)],
    % Temporal Constraints
          [
],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(container,boot,[open(boot)]),
     ss(nuts,nuts2,[tight(nuts2,hub2)]),
     ss(nuts,nuts3,[tight(nuts3,hub3)]),
     ss(nuts,nuts4,[tight(nuts4,hub4)]),
     ss(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     ss(hub,hub2,[on_ground(hub2),fastened(hub2)]),
     ss(hub,hub3,[on_ground(hub3),fastened(hub3)]),
     ss(hub,hub4,[on_ground(hub4),fastened(hub4)]),
     ss(pump,pump0,[pump_in(pump0,boot)]),
     ss(wheel,wheel1,[wheel_on(wheel1,hub1)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(wheel,wheel5,[wheel_in(wheel5,boot)]),
     ss(wrench,wrench0,[have_wrench(wrench0)]),
     ss(jack,jack0,[have_jack(jack0)]),
     ss(wheel_trim,trim1,[trim_on(trim1,wheel1)]),
     ss(wheel_trim,trim2,[trim_on(trim2,wheel2)]),
     ss(wheel_trim,trim3,[trim_on(trim3,wheel3)]),
     ss(wheel_trim,trim4,[trim_on(trim4,wheel4)]),
     ss(tyre,tyre1,[punctured(tyre1)]),
     ss(tyre,tyre2,[full(tyre2)]),
     ss(tyre,tyre3,[full(tyre3)]),
     ss(tyre,tyre4,[full(tyre4)]),
     ss(tyre,tyre5,[full(tyre5)]),
     ss(nuts,nuts1,[tight(nuts1,hub1)])]).
htn_task(13,
    goal(
          [
            change_wheel(wheel5,boot,hub1,wheel1)],
    % Temporal Constraints
          [
],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(container,boot,[open(boot)]),
     ss(nuts,nuts2,[tight(nuts2,hub2)]),
     ss(nuts,nuts3,[tight(nuts3,hub3)]),
     ss(nuts,nuts4,[tight(nuts4,hub4)]),
     ss(hub,hub1,[unfastened(hub1),jacked_up(hub1,jack0)]),
     ss(hub,hub2,[on_ground(hub2),fastened(hub2)]),
     ss(hub,hub3,[on_ground(hub3),fastened(hub3)]),
     ss(hub,hub4,[on_ground(hub4),fastened(hub4)]),
     ss(pump,pump0,[pump_in(pump0,boot)]),
     ss(wheel,wheel1,[wheel_on(wheel1,hub1)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(wheel,wheel5,[wheel_in(wheel5,boot)]),
     ss(wrench,wrench0,[have_wrench(wrench0)]),
     ss(jack,jack0,[jack_in_use(jack0,hub1)]),
     ss(wheel_trim,trim1,[trim_off(trim1)]),
     ss(wheel_trim,trim2,[trim_on(trim2,wheel2)]),
     ss(wheel_trim,trim3,[trim_on(trim3,wheel3)]),
     ss(wheel_trim,trim4,[trim_on(trim4,wheel4)]),
     ss(tyre,tyre1,[punctured(tyre1)]),
     ss(tyre,tyre2,[full(tyre2)]),
     ss(tyre,tyre3,[full(tyre3)]),
     ss(tyre,tyre4,[full(tyre4)]),
     ss(tyre,tyre5,[full(tyre5)]),
     ss(nuts,nuts1,[have_nuts(nuts1)])]).
htn_task(14,
    goal(
          [
            attach_hub(nuts1,hub1,jack0,trim1,wheel5)],
    % Temporal Constraints
          [
],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(container,boot,[open(boot)]),
     ss(nuts,nuts2,[tight(nuts2,hub2)]),
     ss(nuts,nuts3,[tight(nuts3,hub3)]),
     ss(nuts,nuts4,[tight(nuts4,hub4)]),
     ss(hub,hub1,[unfastened(hub1),jacked_up(hub1,jack0)]),
     ss(hub,hub2,[on_ground(hub2),fastened(hub2)]),
     ss(hub,hub3,[on_ground(hub3),fastened(hub3)]),
     ss(hub,hub4,[on_ground(hub4),fastened(hub4)]),
     ss(pump,pump0,[pump_in(pump0,boot)]),
     ss(wheel,wheel1,[wheel_in(wheel1,boot)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(wheel,wheel5,[wheel_on(wheel5,hub1)]),
     ss(wrench,wrench0,[have_wrench(wrench0)]),
     ss(jack,jack0,[jack_in_use(jack0,hub1)]),
     ss(wheel_trim,trim1,[trim_off(trim1)]),
     ss(wheel_trim,trim2,[trim_on(trim2,wheel2)]),
     ss(wheel_trim,trim3,[trim_on(trim3,wheel3)]),
     ss(wheel_trim,trim4,[trim_on(trim4,wheel4)]),
     ss(tyre,tyre1,[punctured(tyre1)]),
     ss(tyre,tyre2,[full(tyre2)]),
     ss(tyre,tyre3,[full(tyre3)]),
     ss(tyre,tyre4,[full(tyre4)]),
     ss(tyre,tyre5,[full(tyre5)]),
     ss(nuts,nuts1,[have_nuts(nuts1)])]).
htn_task(15,
    goal(
          [
            putaway_tools(wrench0,boot,jack0)],
    % Temporal Constraints
          [
],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(container,boot,[open(boot)]),
     ss(nuts,nuts2,[tight(nuts2,hub2)]),
     ss(nuts,nuts3,[tight(nuts3,hub3)]),
     ss(nuts,nuts4,[tight(nuts4,hub4)]),
     ss(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     ss(hub,hub2,[on_ground(hub2),fastened(hub2)]),
     ss(hub,hub3,[on_ground(hub3),fastened(hub3)]),
     ss(hub,hub4,[on_ground(hub4),fastened(hub4)]),
     ss(pump,pump0,[pump_in(pump0,boot)]),
     ss(wheel,wheel1,[wheel_in(wheel1,boot)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(wheel,wheel5,[wheel_on(wheel5,hub1)]),
     ss(wrench,wrench0,[have_wrench(wrench0)]),
     ss(jack,jack0,[have_jack(jack0)]),
     ss(wheel_trim,trim1,[trim_on(trim1,wheel5)]),
     ss(wheel_trim,trim2,[trim_on(trim2,wheel2)]),
     ss(wheel_trim,trim3,[trim_on(trim3,wheel3)]),
     ss(wheel_trim,trim4,[trim_on(trim4,wheel4)]),
     ss(tyre,tyre1,[punctured(tyre1)]),
     ss(tyre,tyre2,[full(tyre2)]),
     ss(tyre,tyre3,[full(tyre3)]),
     ss(tyre,tyre4,[full(tyre4)]),
     ss(tyre,tyre5,[full(tyre5)]),
     ss(nuts,nuts1,[tight(nuts1,hub1)])]).
htn_task(16,
    goal(
          [
            fix_flat(tyre2),
            discover_puncture(boot,tyre1)],
    % Temporal Constraints
          [
            before(1,2)],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(container,boot,[closed(boot)]),
     ss(nuts,nuts1,[tight(nuts1,hub1)]),
     ss(nuts,nuts2,[tight(nuts2,hub2)]),
     ss(nuts,nuts3,[tight(nuts3,hub3)]),
     ss(nuts,nuts4,[tight(nuts4,hub4)]),
     ss(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     ss(hub,hub2,[on_ground(hub2),fastened(hub2)]),
     ss(hub,hub3,[on_ground(hub3),fastened(hub3)]),
     ss(hub,hub4,[on_ground(hub4),fastened(hub4)]),
     ss(pump,pump0,[pump_in(pump0,boot)]),
     ss(wheel,wheel1,[wheel_on(wheel1,hub1)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(wheel,wheel5,[wheel_in(wheel5,boot)]),
     ss(wrench,wrench0,[wrench_in(wrench0,boot)]),
     ss(jack,jack0,[jack_in(jack0,boot)]),
     ss(wheel_trim,trim1,[trim_on(trim1,wheel1)]),
     ss(wheel_trim,trim2,[trim_on(trim2,wheel2)]),
     ss(wheel_trim,trim3,[trim_on(trim3,wheel3)]),
     ss(wheel_trim,trim4,[trim_on(trim4,wheel4)]),
     ss(tyre,tyre1,[flat(tyre1)]),
     ss(tyre,tyre2,[flat(tyre2)]),
     ss(tyre,tyre3,[full(tyre3)]),
     ss(tyre,tyre4,[full(tyre4)]),
     ss(tyre,tyre5,[full(tyre5)])]).
htn_task(17,
    goal(
          [
            discover_puncture(boot,tyre1),
            fetch_tools(wrench0,boot,jack0)],
    % Temporal Constraints
          [
            before(1,2)],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(container,boot,[closed(boot)]),
     ss(nuts,nuts1,[tight(nuts1,hub1)]),
     ss(nuts,nuts2,[tight(nuts2,hub2)]),
     ss(nuts,nuts3,[tight(nuts3,hub3)]),
     ss(nuts,nuts4,[tight(nuts4,hub4)]),
     ss(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     ss(hub,hub2,[on_ground(hub2),fastened(hub2)]),
     ss(hub,hub3,[on_ground(hub3),fastened(hub3)]),
     ss(hub,hub4,[on_ground(hub4),fastened(hub4)]),
     ss(pump,pump0,[pump_in(pump0,boot)]),
     ss(wheel,wheel1,[wheel_on(wheel1,hub1)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(wheel,wheel5,[wheel_in(wheel5,boot)]),
     ss(wrench,wrench0,[wrench_in(wrench0,boot)]),
     ss(jack,jack0,[jack_in(jack0,boot)]),
     ss(wheel_trim,trim1,[trim_on(trim1,wheel1)]),
     ss(wheel_trim,trim2,[trim_on(trim2,wheel2)]),
     ss(wheel_trim,trim3,[trim_on(trim3,wheel3)]),
     ss(wheel_trim,trim4,[trim_on(trim4,wheel4)]),
     ss(tyre,tyre1,[flat(tyre1)]),
     ss(tyre,tyre2,[flat(tyre2)]),
     ss(tyre,tyre3,[full(tyre3)]),
     ss(tyre,tyre4,[full(tyre4)]),
     ss(tyre,tyre5,[full(tyre5)])]).
htn_task(18,
    goal(
          [
            discover_puncture(boot,tyre1),
            fetch_tools(wrench0,boot,jack0),
            unfasten_hub(nuts1,hub1,jack0,trim1,wheel1)],
    % Temporal Constraints
          [
            before(1,2),
            before(2,3)],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(container,boot,[closed(boot)]),
     ss(nuts,nuts1,[tight(nuts1,hub1)]),
     ss(nuts,nuts2,[tight(nuts2,hub2)]),
     ss(nuts,nuts3,[tight(nuts3,hub3)]),
     ss(nuts,nuts4,[tight(nuts4,hub4)]),
     ss(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     ss(hub,hub2,[on_ground(hub2),fastened(hub2)]),
     ss(hub,hub3,[on_ground(hub3),fastened(hub3)]),
     ss(hub,hub4,[on_ground(hub4),fastened(hub4)]),
     ss(pump,pump0,[pump_in(pump0,boot)]),
     ss(wheel,wheel1,[wheel_on(wheel1,hub1)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(wheel,wheel5,[wheel_in(wheel5,boot)]),
     ss(wrench,wrench0,[wrench_in(wrench0,boot)]),
     ss(jack,jack0,[jack_in(jack0,boot)]),
     ss(wheel_trim,trim1,[trim_on(trim1,wheel1)]),
     ss(wheel_trim,trim2,[trim_on(trim2,wheel2)]),
     ss(wheel_trim,trim3,[trim_on(trim3,wheel3)]),
     ss(wheel_trim,trim4,[trim_on(trim4,wheel4)]),
     ss(tyre,tyre1,[flat(tyre1)]),
     ss(tyre,tyre2,[flat(tyre2)]),
     ss(tyre,tyre3,[full(tyre3)]),
     ss(tyre,tyre4,[full(tyre4)]),
     ss(tyre,tyre5,[full(tyre5)])]).
htn_task(19,
    goal(
          [
            discover_puncture(boot,tyre1),
            fetch_tools(wrench0,boot,jack0),
            unfasten_hub(nuts1,hub1,jack0,trim1,wheel1),
            remove_wheel(trim1,wheel1,hub1,jack0),
            fetch_wheel(boot,wheel5),
            putaway_wheel(boot,wheel1),
            put_on_wheel(trim1,wheel5,hub1,jack0)],
    % Temporal Constraints
          [
            before(1,2),
            before(2,3),
            before(3,4),
            before(4,5),
            before(5,6),
            before(6,7)],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(container,boot,[closed(boot)]),
     ss(nuts,nuts1,[tight(nuts1,hub1)]),
     ss(nuts,nuts2,[tight(nuts2,hub2)]),
     ss(nuts,nuts3,[tight(nuts3,hub3)]),
     ss(nuts,nuts4,[tight(nuts4,hub4)]),
     ss(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     ss(hub,hub2,[on_ground(hub2),fastened(hub2)]),
     ss(hub,hub3,[on_ground(hub3),fastened(hub3)]),
     ss(hub,hub4,[on_ground(hub4),fastened(hub4)]),
     ss(pump,pump0,[pump_in(pump0,boot)]),
     ss(wheel,wheel1,[wheel_on(wheel1,hub1)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(wheel,wheel5,[wheel_in(wheel5,boot)]),
     ss(wrench,wrench0,[wrench_in(wrench0,boot)]),
     ss(jack,jack0,[jack_in(jack0,boot)]),
     ss(wheel_trim,trim1,[trim_on(trim1,wheel1)]),
     ss(wheel_trim,trim2,[trim_on(trim2,wheel2)]),
     ss(wheel_trim,trim3,[trim_on(trim3,wheel3)]),
     ss(wheel_trim,trim4,[trim_on(trim4,wheel4)]),
     ss(tyre,tyre1,[flat(tyre1)]),
     ss(tyre,tyre2,[flat(tyre2)]),
     ss(tyre,tyre3,[full(tyre3)]),
     ss(tyre,tyre4,[full(tyre4)]),
     ss(tyre,tyre5,[full(tyre5)])]).
htn_task(20,
    goal(
          [
            discover_puncture(boot,tyre1),
            fetch_tools(wrench0,boot,jack0),
            unfasten_hub(nuts1,hub1,jack0,trim1,wheel1),
            remove_wheel(trim1,wheel1,hub1,jack0),
            fetch_wheel(boot,wheel5),
            putaway_wheel(boot,wheel1),
            put_on_wheel(trim1,wheel5,hub1,jack0),
            attach_hub(nuts1,hub1,jack0,trim1,wheel5)],
    % Temporal Constraints
          [
            before(1,2),
            before(2,3),
            before(3,4),
            before(4,5),
            before(5,6),
            before(6,7),
            before(7,8)],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(container,boot,[closed(boot)]),
     ss(nuts,nuts1,[tight(nuts1,hub1)]),
     ss(nuts,nuts2,[tight(nuts2,hub2)]),
     ss(nuts,nuts3,[tight(nuts3,hub3)]),
     ss(nuts,nuts4,[tight(nuts4,hub4)]),
     ss(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     ss(hub,hub2,[on_ground(hub2),fastened(hub2)]),
     ss(hub,hub3,[on_ground(hub3),fastened(hub3)]),
     ss(hub,hub4,[on_ground(hub4),fastened(hub4)]),
     ss(pump,pump0,[pump_in(pump0,boot)]),
     ss(wheel,wheel1,[wheel_on(wheel1,hub1)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(wheel,wheel5,[wheel_in(wheel5,boot)]),
     ss(wrench,wrench0,[wrench_in(wrench0,boot)]),
     ss(jack,jack0,[jack_in(jack0,boot)]),
     ss(wheel_trim,trim1,[trim_on(trim1,wheel1)]),
     ss(wheel_trim,trim2,[trim_on(trim2,wheel2)]),
     ss(wheel_trim,trim3,[trim_on(trim3,wheel3)]),
     ss(wheel_trim,trim4,[trim_on(trim4,wheel4)]),
     ss(tyre,tyre1,[flat(tyre1)]),
     ss(tyre,tyre2,[flat(tyre2)]),
     ss(tyre,tyre3,[full(tyre3)]),
     ss(tyre,tyre4,[full(tyre4)]),
     ss(tyre,tyre5,[full(tyre5)])]).
htn_task(21,
    goal(
          [
            discover_puncture(boot,tyre1),
            fetch_tools(wrench0,boot,jack0),
            unfasten_hub(nuts1,hub1,jack0,trim1,wheel1),
            remove_wheel(trim1,wheel1,hub1,jack0),
            fetch_wheel(boot,wheel5),
            putaway_wheel(boot,wheel1),
            put_on_wheel(trim1,wheel5,hub1,jack0),
            attach_hub(nuts1,hub1,jack0,trim1,wheel5),
            putaway_tools(wrench0,boot,jack0)],
    % Temporal Constraints
          [
            before(1,2),
            before(2,3),
            before(3,4),
            before(4,5),
            before(5,6),
            before(6,7),
            before(7,8),
            before(8,9)],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(container,boot,[closed(boot)]),
     ss(nuts,nuts1,[tight(nuts1,hub1)]),
     ss(nuts,nuts2,[tight(nuts2,hub2)]),
     ss(nuts,nuts3,[tight(nuts3,hub3)]),
     ss(nuts,nuts4,[tight(nuts4,hub4)]),
     ss(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     ss(hub,hub2,[on_ground(hub2),fastened(hub2)]),
     ss(hub,hub3,[on_ground(hub3),fastened(hub3)]),
     ss(hub,hub4,[on_ground(hub4),fastened(hub4)]),
     ss(pump,pump0,[pump_in(pump0,boot)]),
     ss(wheel,wheel1,[wheel_on(wheel1,hub1)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(wheel,wheel5,[wheel_in(wheel5,boot)]),
     ss(wrench,wrench0,[wrench_in(wrench0,boot)]),
     ss(jack,jack0,[jack_in(jack0,boot)]),
     ss(wheel_trim,trim1,[trim_on(trim1,wheel1)]),
     ss(wheel_trim,trim2,[trim_on(trim2,wheel2)]),
     ss(wheel_trim,trim3,[trim_on(trim3,wheel3)]),
     ss(wheel_trim,trim4,[trim_on(trim4,wheel4)]),
     ss(tyre,tyre1,[flat(tyre1)]),
     ss(tyre,tyre2,[flat(tyre2)]),
     ss(tyre,tyre3,[full(tyre3)]),
     ss(tyre,tyre4,[full(tyre4)]),
     ss(tyre,tyre5,[full(tyre5)])]).
htn_task(22,
    goal(
          [
            discover_puncture(boot,tyre1),
            fetch_tools(wrench0,boot,jack0),
            unfasten_hub(nuts1,hub1,jack0,trim1,wheel1),
            remove_wheel(trim1,wheel1,hub1,jack0),
            fetch_wheel(boot,wheel5),
            putaway_wheel(boot,wheel1),
            put_on_wheel(trim1,wheel5,hub1,jack0),
            attach_hub(nuts1,hub1,jack0,trim1,wheel5),
            putaway_tools(wrench0,boot,jack0),
            close_container(boot)],
    % Temporal Constraints
          [
            before(1,2),
            before(2,3),
            before(3,4),
            before(4,5),
            before(5,6),
            before(6,7),
            before(7,8),
            before(8,9),
            before(9,10)],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(container,boot,[closed(boot)]),
     ss(nuts,nuts1,[tight(nuts1,hub1)]),
     ss(nuts,nuts2,[tight(nuts2,hub2)]),
     ss(nuts,nuts3,[tight(nuts3,hub3)]),
     ss(nuts,nuts4,[tight(nuts4,hub4)]),
     ss(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     ss(hub,hub2,[on_ground(hub2),fastened(hub2)]),
     ss(hub,hub3,[on_ground(hub3),fastened(hub3)]),
     ss(hub,hub4,[on_ground(hub4),fastened(hub4)]),
     ss(pump,pump0,[pump_in(pump0,boot)]),
     ss(wheel,wheel1,[wheel_on(wheel1,hub1)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(wheel,wheel5,[wheel_in(wheel5,boot)]),
     ss(wrench,wrench0,[wrench_in(wrench0,boot)]),
     ss(jack,jack0,[jack_in(jack0,boot)]),
     ss(wheel_trim,trim1,[trim_on(trim1,wheel1)]),
     ss(wheel_trim,trim2,[trim_on(trim2,wheel2)]),
     ss(wheel_trim,trim3,[trim_on(trim3,wheel3)]),
     ss(wheel_trim,trim4,[trim_on(trim4,wheel4)]),
     ss(tyre,tyre1,[flat(tyre1)]),
     ss(tyre,tyre2,[flat(tyre2)]),
     ss(tyre,tyre3,[full(tyre3)]),
     ss(tyre,tyre4,[full(tyre4)]),
     ss(tyre,tyre5,[full(tyre5)])]).
htn_task(23,
    goal(
          [
            discover_puncture(boot,tyre1),
            fetch_tools(wrench0,boot,jack0),
            unfasten_hub(nuts1,hub1,jack0,trim1,wheel1),
            remove_wheel(trim1,wheel1,hub1,jack0),
            fetch_wheel(boot,wheel5),
            putaway_wheel(boot,wheel1),
            put_on_wheel(trim1,wheel5,hub1,jack0),
            attach_hub(nuts1,hub1,jack0,trim1,wheel5),
            putaway_tools(wrench0,boot,jack0),
            fetch_pump(boot,pump0),
            inflate_tyre(pump0,tyre2),
            putaway_pump(boot,pump0),
            close_container(boot)],
    % Temporal Constraints
          [
            before(1,2),
            before(2,3),
            before(3,4),
            before(4,5),
            before(5,6),
            before(6,7),
            before(7,8),
            before(8,9),
            before(9,10),
            before(10,11),
            before(11,12),
            before(12,13)],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(container,boot,[closed(boot)]),
     ss(nuts,nuts1,[tight(nuts1,hub1)]),
     ss(nuts,nuts2,[tight(nuts2,hub2)]),
     ss(nuts,nuts3,[tight(nuts3,hub3)]),
     ss(nuts,nuts4,[tight(nuts4,hub4)]),
     ss(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     ss(hub,hub2,[on_ground(hub2),fastened(hub2)]),
     ss(hub,hub3,[on_ground(hub3),fastened(hub3)]),
     ss(hub,hub4,[on_ground(hub4),fastened(hub4)]),
     ss(pump,pump0,[pump_in(pump0,boot)]),
     ss(wheel,wheel1,[wheel_on(wheel1,hub1)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(wheel,wheel5,[wheel_in(wheel5,boot)]),
     ss(wrench,wrench0,[wrench_in(wrench0,boot)]),
     ss(jack,jack0,[jack_in(jack0,boot)]),
     ss(wheel_trim,trim1,[trim_on(trim1,wheel1)]),
     ss(wheel_trim,trim2,[trim_on(trim2,wheel2)]),
     ss(wheel_trim,trim3,[trim_on(trim3,wheel3)]),
     ss(wheel_trim,trim4,[trim_on(trim4,wheel4)]),
     ss(tyre,tyre1,[flat(tyre1)]),
     ss(tyre,tyre2,[flat(tyre2)]),
     ss(tyre,tyre3,[full(tyre3)]),
     ss(tyre,tyre4,[full(tyre4)]),
     ss(tyre,tyre5,[full(tyre5)])]).
htn_task(24,
    goal(
          [
            open_container(boot),
            fetch_jack(boot,jack0),
            fetch_wheel(boot,wheel5),
            fetch_wrench(boot,wrench0),
            remove_trim(hub1,trim1,wheel1),
            loosen(wrench0,hub1,trim1,nuts1),
            jack_up(nuts1,hub1,jack0),
            undo(wrench0,trim1,hub1,jack0,nuts1),
            remove_wheel(trim1,wheel1,hub1,jack0),
            put_on_wheel(trim1,wheel5,hub1,jack0),
            do_up(wrench0,trim1,hub1,jack0,nuts1),
            jack_down(nuts1,hub1,jack0),
            tighten(wrench0,hub1,trim1,nuts1),
            apply_trim(hub1,trim1,wheel5)],
    % Temporal Constraints
          [
            before(1,2),
            before(2,3),
            before(3,4),
            before(4,5),
            before(5,6),
            before(6,7),
            before(7,8),
            before(8,9),
            before(9,10),
            before(10,11),
            before(11,12),
            before(12,13),
            before(13,14)],
    % Static constraints
          [
]),
    % INIT States
    [
     ss(container,boot,[closed(boot)]),
     ss(nuts,nuts1,[tight(nuts1,hub1)]),
     ss(nuts,nuts2,[tight(nuts2,hub2)]),
     ss(nuts,nuts3,[tight(nuts3,hub3)]),
     ss(nuts,nuts4,[tight(nuts4,hub4)]),
     ss(hub,hub1,[on_ground(hub1),fastened(hub1)]),
     ss(hub,hub2,[on_ground(hub2),fastened(hub2)]),
     ss(hub,hub3,[on_ground(hub3),fastened(hub3)]),
     ss(hub,hub4,[on_ground(hub4),fastened(hub4)]),
     ss(pump,pump0,[pump_in(pump0,boot)]),
     ss(wheel,wheel1,[wheel_on(wheel1,hub1)]),
     ss(wheel,wheel2,[wheel_on(wheel2,hub2)]),
     ss(wheel,wheel3,[wheel_on(wheel3,hub3)]),
     ss(wheel,wheel4,[wheel_on(wheel4,hub4)]),
     ss(wheel,wheel5,[wheel_in(wheel5,boot)]),
     ss(wrench,wrench0,[wrench_in(wrench0,boot)]),
     ss(jack,jack0,[jack_in(jack0,boot)]),
     ss(wheel_trim,trim1,[trim_on(trim1,wheel1)]),
     ss(wheel_trim,trim2,[trim_on(trim2,wheel2)]),
     ss(wheel_trim,trim3,[trim_on(trim3,wheel3)]),
     ss(wheel_trim,trim4,[trim_on(trim4,wheel4)]),
     ss(tyre,tyre3,[full(tyre3)]),
     ss(tyre,tyre4,[full(tyre4)]),
     ss(tyre,tyre5,[full(tyre5)]),
     ss(tyre,tyre2,[full(tyre2)]),
     ss(tyre,tyre1,[full(tyre1)])]).
