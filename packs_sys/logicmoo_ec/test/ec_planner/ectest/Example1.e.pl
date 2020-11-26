:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Example1a.e',43).
:- call_pel_directive(translate(unskipped,
                                '/pack/logicmoo_ec/test/ec_planner/ectest/Example1.e.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(ecalc).
:- call_pel_directive(translate(begining,
                                '/pack/logicmoo_ec/test/ec_planner/ectest/Example1.e.pl')).
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
%; deduction

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Example1.e',14).
% load foundations/Root.e
:- call_pel_directive(load('foundations/Root.e')).

% load foundations/EC.e
:- call_pel_directive(load('foundations/EC.e')).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Example1.e',17).
% sort agent
sort(agent).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Example1.e',19).
% fluent Awake(agent)
fluent(awake(agent)).

% event WakeUp(agent)
event(wakeUp(agent)).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Example1.e',22).
% [agent,time]
 % Initiates(WakeUp(agent),Awake(agent),time).
initiates_at(wakeUp(Agent),awake(Agent),Time).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Example1.e',24).
% agent James
t(agent,james).


% !HoldsAt(Awake(James),0).
holds_at(not(awake(james)),0).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Example1.e',26).
% Delta: 
directive(delta).
 


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Example1.e',26).
% Happens(WakeUp(James),0).
happens_at(wakeUp(james),0).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Example1.e',28).
% completion Delta Happens
:- call_pel_directive(completion(delta)).
:- call_pel_directive(completion(happens_at)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Example1.e',30).
% range time 0 1
:- call_pel_directive(range(time, 0, 1)).

% range offset 1 1
:- call_pel_directive(range(offset, 1, 1)).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Example1.e',30).
:- call_pel_directive(translate(ending,
                                '/pack/logicmoo_ec/test/ec_planner/ectest/Example1.e.pl')).
