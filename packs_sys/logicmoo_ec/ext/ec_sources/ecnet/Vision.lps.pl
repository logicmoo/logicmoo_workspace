% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',371).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.lps.pl')).
% Fri, 26 Mar 2021 01:06:14 GMT File: <stream>(0x555567ca9700)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; The Vision representation deals with some simple aspects
%; of vision.
%;
%; agent looks at object.

% event LookAt(agent,object)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.e',15).
% From E: 
% 
% event(lookAt(agent,object)).
events([lookAt/2]).
mpred_prop(lookAt(agent, object), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.e',15).
actions([lookAt/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.e',18).
%; agent sees object.

% fluent See(agent,object)
% From E: 
% 
% fluent(see(agent,object)).
mpred_prop(see(agent, object), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.e',18).
fluents([see/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.e',21).
%; An effect axiom states that if an agent looks at
%; an object, the agent will see the object:
% [agent,object,time]
% Initiates(LookAt(agent,object),
%           See(agent,object),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.e',23).
% From E: 
% 
% initiates_at(
%    lookAt(Agent,Object), 
%    see(Agent,Object), 
%    Time).
lookAt(Agent, Object)initiates see(Agent, Object).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.e',23).

 /*  initiated(happens(lookAt(Agent,Object),
     		  Time_from,
     		  Time_until),
     	  see(Agent,Object),
     	  []).
 */
 %  % =================================.


%; A precondition axiom states that for
%; an agent to look at an object,
%; there must be a location such that
%; the agent is at the location and
%; the object is at the location, or
%; there must be a door such that
%; the agent is near the door,
%; the object is near the door, and
%; the door is open:
%;[agent,object,time]
%;Happens(LookAt(agent,object),time) ->
%;({location}
%; HoldsAt(At(agent,location),time) &
%; HoldsAt(At(object,location),time))|
%;({door}
%; HoldsAt(NearPortal(agent,door),time) &
%; HoldsAt(NearPortal(object,door),time) &
%; HoldsAt(DoorIsOpen(door),time)).
%; An effect axiom states that if an agent
%; looks at an object, the agent will no longer
%; see other objects:
% [agent,object1,object2,time]
% object1!=% object2 ->
% Terminates(LookAt(agent,object1),
%            See(agent,object2),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.e',50).
% From E: 
% 
% '->'(
%    Object1\=Object2, 
%    terminates_at(
%       lookAt(Agent,Object1), 
%       see(Agent,Object2), 
%       Time)).
(   terminates(lookAt(Agent, Object1),
               see(Agent, Object2)at Time)
;   not {dif(Object1, Object2)}
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.e',50).

 /*   (   terminates(lookAt(Agent, Object1),
                       at(see(Agent, Object2), Time))
        ;   not({dif(Object1, Object2)})
        ).
 */
 %  % =================================.


%; Several effect axioms state that if an
%; agent walks through a door, up a staircase, or down a staircase,
%; the agent no longer sees an object:
% [agent,door,object,time]
% Terminates(WalkThroughDoor12(agent,door),
%            See(agent,object),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.e',59).
% From E: 
% 
% terminates_at(
%    walkThroughDoor12(Agent,Door), 
%    see(Agent,Object), 
%    Time).
walkThroughDoor12(Agent, Door)terminates see(Agent, Object).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.e',59).

 /*  terminated(happens(walkThroughDoor12(Agent,Door),
     		   Time_from,
     		   Time_until),
     	   see(Agent,Object),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.e',63).
% [agent,door,object,time]
% Terminates(WalkThroughDoor21(agent,door),
%            See(agent,object),
%            time).
% From E: 
% 
% terminates_at(
%    walkThroughDoor21(Agent,Door), 
%    see(Agent,Object), 
%    Time).
walkThroughDoor21(Agent, Door)terminates see(Agent, Object).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.e',63).

 /*  terminated(happens(walkThroughDoor21(Agent,Door),
     		   Time_from,
     		   Time_until),
     	   see(Agent,Object),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.e',67).
% [agent,door,object,time]
% Terminates(WalkUpStaircase(agent,door),
%            See(agent,object),
%            time).
% From E: 
% 
% terminates_at(
%    walkUpStaircase(Agent,Door), 
%    see(Agent,Object), 
%    Time).
walkUpStaircase(Agent, Door)terminates see(Agent, Object).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.e',67).

 /*  terminated(happens(walkUpStaircase(Agent,Door),
     		   Time_from,
     		   Time_until),
     	   see(Agent,Object),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.e',71).
% [agent,door,object,time]
% Terminates(WalkDownStaircase(agent,door),
%            See(agent,object),
%            time).
% From E: 
% 
% terminates_at(
%    walkDownStaircase(Agent,Door), 
%    see(Agent,Object), 
%    Time).
walkDownStaircase(Agent, Door)terminates see(Agent, Object).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.e',71).

 /*  terminated(happens(walkDownStaircase(Agent,Door),
     		   Time_from,
     		   Time_until),
     	   see(Agent,Object),
     	   []).
 */
 %  % =================================.


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.e',75).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.lps.pl')).
