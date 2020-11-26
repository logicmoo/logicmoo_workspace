:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Example3.e',25).
:- call_pel_directive(translate(unskipped,
                                '/pack/logicmoo_ec/test/ec_planner/ectest/Example4.e.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(ecalc).
:- call_pel_directive(translate(begining,
                                '/pack/logicmoo_ec/test/ec_planner/ectest/Example4.e.pl')).
%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Example4.e',11).
% load foundations/Root.e
:- call_pel_directive(load('foundations/Root.e')).

% load foundations/EC.e
:- call_pel_directive(load('foundations/EC.e')).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Example4.e',14).
% sort agent
sort(agent).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Example4.e',16).
% fluent Awake(agent)
fluent(awake(agent)).

% event WakeUp(agent)
event(wakeUp(agent)).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Example4.e',19).
% [agent,time]
 % Initiates(WakeUp(agent),Awake(agent),time).
initiates_at(wakeUp(Agent),awake(Agent),Time).


% [agent,time]
 % Happens(WakeUp(agent),time) -> !HoldsAt(Awake(agent),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Example4.e',19).
happens_at(wakeUp(Agent), Time) ->
    holds_at(not(awake(Agent)), Time).

% agent James, Jessie
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Example4.e',21).
t(agent,james).
t(agent,jessie).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Example4.e',23).
% !HoldsAt(Awake(James),0).
holds_at(not(awake(james)),0).


% !HoldsAt(Awake(Jessie),0).
holds_at(not(awake(jessie)),0).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Example4.e',25).
% HoldsAt(Awake(James),1).
holds_at(awake(james),1).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Example4.e',27).
% range time 0 1
:- call_pel_directive(range(time, 0, 1)).

% range offset 1 1
:- call_pel_directive(range(offset, 1, 1)).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Example4.e',27).
:- call_pel_directive(translate(ending,
                                '/pack/logicmoo_ec/test/ec_planner/ectest/Example4.e.pl')).
