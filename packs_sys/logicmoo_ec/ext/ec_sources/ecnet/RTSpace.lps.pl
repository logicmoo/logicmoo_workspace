% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RidingInACarriage.e',185).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.lps.pl')).
% Fri, 26 Mar 2021 01:06:08 GMT File: <stream>(0x555566fa2600)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; RTSpace: room-scale topological space
%;
%; We use topological and metric representations of space,
%; at two levels of granularity---room-scale and object-scale.
%; The RTSpace representation deals with topological space at
%; the scale of rooms and outdoor locations.
%; This representation of space consists of locations, which
%; are connected by portals. There are two types of locations:
%; rooms and outside areas (outsides).
%;
%; object is at location.

% fluent At(object,location)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',22).
% From E: 
% 
% fluent(at_loc(object,location)).
mpred_prop(at_loc(object, location), fluent).
fluents([at_loc/2]).

% manualrelease At
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',24).
% From E: 
% 
% ':-'(call_pel_directive(manualrelease(at_loc))).
:- call_pel_directive(manualrelease(at_loc)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',26).
% [object1,location,time]
% (% {object2} PartOf(object1,object2)) ->
% ReleasedAt(At(object1,location),time).
% From E: 
% 
% exists(Object2, 
%    '->'(
%       partOf(Object1,Object2), 
%       released_at(
%          at_loc(Object1,Location), 
%          Time))).
exists(Object2,  (released_at(at_loc(Object1, Location), Time);not partOf(Object1, Object2))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',26).

 /*  exists(Object2,
       (   released_at(at_loc(Object1, Location), Time)
        ;   not(partOf(Object1, Object2))
        )).
 */
 %  % =================================.


%; A state constraint says that an object
%; is at one location at a time:
% [object,location1,location2,time]
% HoldsAt(At(object,location1),time) &
% HoldsAt(At(object,location2),time) ->
% location1=location2.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',32).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          at_loc(Object,Location1), 
%          Time), 
%       holds(
%          at_loc(Object,Location2), 
%          Time)), 
%    Location1=Location2).
(   equals(Location1, Location2)
;   not at_loc(Object, Location1)at Time
;   not at_loc(Object, Location2)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',32).

 /*   (   equals(Location1, Location2)
        ;   at(not(at_loc(Object, Location1)), Time)
        ;   at(not(at_loc(Object, Location2)), Time)
        ).
 */
 %  % =================================.


%; connectivity
%; Side one of portal is location.

% function Side1(portal): location
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',39).
% From E: 
% 
% function(
%    side1(portal), 
%    location).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',39).
function(side1(portal),location).
%; Side two of portal is location.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',42).
% function Side2(portal): location
% From E: 
% 
% function(
%    side2(portal), 
%    location).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',42).
function(side2(portal),location).
%; The building of room is building.

% function BuildingOf(room): building
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',44).
% From E: 
% 
% function(
%    buildingOf(room), 
%    building).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',44).
function(buildingOf(room),building).
%; object is at a location that has portal.

% fluent NearPortal(object,portal)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',47).
% From E: 
% 
% fluent(nearPortal(object,portal)).
mpred_prop(nearPortal(object, portal), fluent).
fluents([nearPortal/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',49).
% noninertial NearPortal
% From E: 
% 
% ':-'(call_pel_directive(noninertial(nearPortal))).
:- call_pel_directive(noninertial(nearPortal)).
%; A state constraint says that an object is near
%; a portal if and only if there is a location such that
%; the object is at the location and one of the sides
%; of the portal is the location:
% [object,portal,time]
% HoldsAt(NearPortal(object,portal),time) <->
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',55).
% {location}% 
%  (Side1(portal)=location|
%   Side2(portal)=location) &
%  HoldsAt(At(object,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',57).
% From E: 
% 
% exists(Location, 
%    <->(
%       holds(
%          nearPortal(Object,Portal), 
%          Time), 
%       ','(
%          ';'(
%             '='(
%                side1(Portal), 
%                Location), 
%             '='(
%                side2(Portal), 
%                Location)), 
%          holds(
%             at_loc(Object,Location), 
%             Time)))).
exists(Location,  (((equals(side1(Portal), Location);equals(side2(Portal), Location)), at_loc(Object, Location)at Time;not nearPortal(Object, Portal)at Time), (nearPortal(Object, Portal)at Time;not equals(side1(Portal), Location), not equals(side2(Portal), Location);not at_loc(Object, Location)at Time))).
 %  exists(Location,  (((equals(side1(Portal), Location);equals(side2(Portal), Location)), at(at_loc(Object, Location), Time);at(not(nearPortal(Object, Portal)), Time)), (at(nearPortal(Object, Portal), Time);not(equals(side1(Portal), Location)), not(equals(side2(Portal), Location));at(not(at_loc(Object, Location)), Time)))).
 %  % =================================.


%; locking and unlocking doors
%; agent unlocks door.

% event DoorUnlock(agent,door)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',64).
% From E: 
% 
% event(doorUnlock(agent,door)).
events([doorUnlock/2]).
mpred_prop(doorUnlock(agent, door), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',64).
actions([doorUnlock/2]).


%; agent locks door.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',67).
% event DoorLock(agent,door)
% From E: 
% 
% event(doorLock(agent,door)).
events([doorLock/2]).
mpred_prop(doorLock(agent, door), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',67).
actions([doorLock/2]).


%; door is unlocked.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',69).
% fluent DoorUnlocked(door)
% From E: 
% 
% fluent(doorUnlocked(door)).
mpred_prop(doorUnlocked(door), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',69).
fluents([doorUnlocked/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',71).
%; A precondition axiom states that
%; for an agent to unlock a door,
%; the agent must be awake,
%; the door must not already be unlocked, and
%; the agent must be near the door:
% [agent,door,time]
% Happens(DoorUnlock(agent,door),time) ->
% HoldsAt(Awake(agent),time) &
% !HoldsAt(DoorUnlocked(door),time) &
% HoldsAt(NearPortal(agent,door),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',76).
% From E: 
% 
% '->'(
%    happens(
%       doorUnlock(Agent,Door), 
%       Time), 
%    ','(
%       holds(
%          awake(Agent), 
%          Time), 
%       ','(
%          holds(
%             not(doorUnlocked(Door)), 
%             Time), 
%          holds(
%             nearPortal(Agent,Door), 
%             Time)))).
(   awake(Agent)at Time,
    not doorUnlocked(Door)at Time,
    nearPortal(Agent, Door)at Time
;   not happens(doorUnlock(Agent, Door), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',76).

 /*  (   at(awake(Agent), Time),
         at(not(doorUnlocked(Door)), Time),
         at(nearPortal(Agent, Door), Time)
     ;   not(happens(doorUnlock(Agent, Door), Time))
     ).
 */
 %  % =================================.


%; An effect axiom states that
%; if an agent unlocks a door,
%; the door will be unlocked:
% [agent,door,time]
% Initiates(DoorUnlock(agent,door),DoorUnlocked(door),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',85).
% From E: 
% 
% initiates_at(
%    doorUnlock(Agent,Door), 
%    doorUnlocked(Door), 
%    Time).
doorUnlock(Agent, Door)initiates doorUnlocked(Door).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',85).

 /*  initiated(happens(doorUnlock(Agent,Door),
     		  Time_from,
     		  Time_until),
     	  doorUnlocked(Door),
     	  []).
 */
 %  % =================================.


%; A precondition axiom states that
%; for an agent to lock a door,
%; the agent must be awake,
%; the door must be unlocked, and
%; the agent must be near the door:
% [agent,door,time]
% Happens(DoorLock(agent,door),time) ->
% HoldsAt(Awake(agent),time) &
% HoldsAt(DoorUnlocked(door),time) &
% HoldsAt(NearPortal(agent,door),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',93).
% From E: 
% 
% '->'(
%    happens(
%       doorLock(Agent,Door), 
%       Time), 
%    ','(
%       holds(
%          awake(Agent), 
%          Time), 
%       ','(
%          holds(
%             doorUnlocked(Door), 
%             Time), 
%          holds(
%             nearPortal(Agent,Door), 
%             Time)))).
(   awake(Agent)at Time,
    doorUnlocked(Door)at Time,
    nearPortal(Agent, Door)at Time
;   not happens(doorLock(Agent, Door), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',93).

 /*  (   at(awake(Agent), Time),
         at(doorUnlocked(Door), Time),
         at(nearPortal(Agent, Door), Time)
     ;   not(happens(doorLock(Agent, Door), Time))
     ).
 */
 %  % =================================.


%; An effect axiom states that
%; if an agent locks a door,
%; the door will no longer be unlocked.
% [agent,door,time]
% Terminates(DoorLock(agent,door),DoorUnlocked(door),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',102).
% From E: 
% 
% terminates_at(
%    doorLock(Agent,Door), 
%    doorUnlocked(Door), 
%    Time).
doorLock(Agent, Door)terminates doorUnlocked(Door).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',102).

 /*  terminated(happens(doorLock(Agent,Door),
     		   Time_from,
     		   Time_until),
     	   doorUnlocked(Door),
     	   []).
 */
 %  % =================================.


%; A state constraint says that if a door is open,
%; it is unlocked:
% [door,time]
% HoldsAt(DoorIsOpen(door),time) -> HoldsAt(DoorUnlocked(door),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',107).
% From E: 
% 
% '->'(
%    holds(
%       doorIsOpen(Door), 
%       Time), 
%    holds(
%       doorUnlocked(Door), 
%       Time)).
(   doorUnlocked(Door)at Time
;   not doorIsOpen(Door)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',107).

 /*   (   at(doorUnlocked(Door), Time)
        ;   at(not(doorIsOpen(Door)), Time)
        ).
 */
 %  % =================================.


%; opening and closing doors
%; agent opens door.

% event DoorOpen(agent,door)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',112).
% From E: 
% 
% event(doorOpen(agent,door)).
events([doorOpen/2]).
mpred_prop(doorOpen(agent, door), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',112).
actions([doorOpen/2]).


%; agent closes door.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',115).
% event DoorClose(agent,door)
% From E: 
% 
% event(doorClose(agent,door)).
events([doorClose/2]).
mpred_prop(doorClose(agent, door), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',115).
actions([doorClose/2]).


%; door is open.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',117).
% fluent DoorIsOpen(door)
% From E: 
% 
% fluent(doorIsOpen(door)).
mpred_prop(doorIsOpen(door), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',117).
fluents([doorIsOpen/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',119).
%; A precondition axiom states that
%; for an agent to open a door,
%; the agent must be awake,
%; the door must not already be open,
%; the door must be unlocked, and
%; the agent must be near the door:
% [agent,door,time]
% Happens(DoorOpen(agent,door),time) ->
% HoldsAt(Awake(agent),time) &
% !HoldsAt(DoorIsOpen(door),time) &
% HoldsAt(DoorUnlocked(door),time) &
% HoldsAt(NearPortal(agent,door),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',125).
% From E: 
% 
% '->'(
%    happens(
%       doorOpen(Agent,Door), 
%       Time), 
%    ','(
%       holds(
%          awake(Agent), 
%          Time), 
%       ','(
%          holds(
%             not(doorIsOpen(Door)), 
%             Time), 
%          ','(
%             holds(
%                doorUnlocked(Door), 
%                Time), 
%             holds(
%                nearPortal(Agent,Door), 
%                Time))))).
(   awake(Agent)at Time,
    not doorIsOpen(Door)at Time,
    doorUnlocked(Door)at Time,
    nearPortal(Agent, Door)at Time
;   not happens(doorOpen(Agent, Door), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',125).

 /*  (   at(awake(Agent), Time),
         at(not(doorIsOpen(Door)), Time),
         at(doorUnlocked(Door), Time),
         at(nearPortal(Agent, Door), Time)
     ;   not(happens(doorOpen(Agent, Door), Time))
     ).
 */
 %  % =================================.


%; An effect axiom states that
%; if an agent opens a door,
%; the door will be open:
% [agent,door,time]
% Initiates(DoorOpen(agent,door),DoorIsOpen(door),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',135).
% From E: 
% 
% initiates_at(
%    doorOpen(Agent,Door), 
%    doorIsOpen(Door), 
%    Time).
doorOpen(Agent, Door)initiates doorIsOpen(Door).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',135).

 /*  initiated(happens(doorOpen(Agent,Door),
     		  Time_from,
     		  Time_until),
     	  doorIsOpen(Door),
     	  []).
 */
 %  % =================================.


%; A precondition axiom states that
%; for an agent to close a door,
%; the agent must be awake,
%; the door must be open,
%; the door must be unlocked, and
%; the agent must be near the door:
% [agent,door,time]
% Happens(DoorClose(agent,door),time) ->
% HoldsAt(Awake(agent),time) &
% HoldsAt(DoorIsOpen(door),time) &
% HoldsAt(DoorUnlocked(door),time) &
% HoldsAt(NearPortal(agent,door),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',144).
% From E: 
% 
% '->'(
%    happens(
%       doorClose(Agent,Door), 
%       Time), 
%    ','(
%       holds(
%          awake(Agent), 
%          Time), 
%       ','(
%          holds(
%             doorIsOpen(Door), 
%             Time), 
%          ','(
%             holds(
%                doorUnlocked(Door), 
%                Time), 
%             holds(
%                nearPortal(Agent,Door), 
%                Time))))).
(   awake(Agent)at Time,
    doorIsOpen(Door)at Time,
    doorUnlocked(Door)at Time,
    nearPortal(Agent, Door)at Time
;   not happens(doorClose(Agent, Door), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',144).

 /*  (   at(awake(Agent), Time),
         at(doorIsOpen(Door), Time),
         at(doorUnlocked(Door), Time),
         at(nearPortal(Agent, Door), Time)
     ;   not(happens(doorClose(Agent, Door), Time))
     ).
 */
 %  % =================================.


%; An effect axiom states that
%; if an agent closes a door,
%; the door will no longer be open:
% [agent,door,time]
% Terminates(DoorClose(agent,door),DoorIsOpen(door),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',154).
% From E: 
% 
% terminates_at(
%    doorClose(Agent,Door), 
%    doorIsOpen(Door), 
%    Time).
doorClose(Agent, Door)terminates doorIsOpen(Door).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',154).

 /*  terminated(happens(doorClose(Agent,Door),
     		   Time_from,
     		   Time_until),
     	   doorIsOpen(Door),
     	   []).
 */
 %  % =================================.


%; passing through doors
%; agent walks through side one of door.

% event WalkThroughDoor12(agent,door)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',159).
% From E: 
% 
% event(walkThroughDoor12(agent,door)).
events([walkThroughDoor12/2]).
mpred_prop(walkThroughDoor12(agent, door), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',159).
actions([walkThroughDoor12/2]).


%; agent walks through side two of door.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',162).
% event WalkThroughDoor21(agent,door)
% From E: 
% 
% event(walkThroughDoor21(agent,door)).
events([walkThroughDoor21/2]).
mpred_prop(walkThroughDoor21(agent, door), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',162).
actions([walkThroughDoor21/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',164).
%; Precondition axioms state that
%; for an agent to walk through a side of a door,
%; the agent must be awake and standing,
%; the door must be open, and
%; the agent must be at the side of the door that
%; the agent walks through:
% [agent,door,time]
% Happens(WalkThroughDoor12(agent,door),time) ->
% HoldsAt(Awake(agent),time) &
% HoldsAt(Standing(agent),time) &
% HoldsAt(DoorIsOpen(door),time) &
% HoldsAt(At(agent,Side1(door)),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',170).
% From E: 
% 
% '->'(
%    happens(
%       walkThroughDoor12(Agent,Door), 
%       Time), 
%    ','(
%       holds(
%          awake(Agent), 
%          Time), 
%       ','(
%          holds(
%             standing(Agent), 
%             Time), 
%          ','(
%             holds(
%                doorIsOpen(Door), 
%                Time), 
%             holds(
%                at_loc(Agent, 
%                   side1(Door)), 
%                Time))))).
(   awake(Agent)at Time,
    standing(Agent)at Time,
    doorIsOpen(Door)at Time,
    at_loc(Agent, side1(Door))at Time
;   not happens(walkThroughDoor12(Agent, Door), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',170).

 /*  (   at(awake(Agent), Time),
         at(standing(Agent), Time),
         at(doorIsOpen(Door), Time),
         at(at_loc(Agent, side1(Door)), Time)
     ;   not(happens(walkThroughDoor12(Agent, Door), Time))
     ).
 */
 %  % =================================.


% [agent,door,time]
% Happens(WalkThroughDoor21(agent,door),time) ->
% HoldsAt(Awake(agent),time) &
% HoldsAt(Standing(agent),time) &
% HoldsAt(DoorIsOpen(door),time) &
% HoldsAt(At(agent,Side2(door)),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',178).
% From E: 
% 
% '->'(
%    happens(
%       walkThroughDoor21(Agent,Door), 
%       Time), 
%    ','(
%       holds(
%          awake(Agent), 
%          Time), 
%       ','(
%          holds(
%             standing(Agent), 
%             Time), 
%          ','(
%             holds(
%                doorIsOpen(Door), 
%                Time), 
%             holds(
%                at_loc(Agent, 
%                   side2(Door)), 
%                Time))))).
(   awake(Agent)at Time,
    standing(Agent)at Time,
    doorIsOpen(Door)at Time,
    at_loc(Agent, side2(Door))at Time
;   not happens(walkThroughDoor21(Agent, Door), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',178).

 /*  (   at(awake(Agent), Time),
         at(standing(Agent), Time),
         at(doorIsOpen(Door), Time),
         at(at_loc(Agent, side2(Door)), Time)
     ;   not(happens(walkThroughDoor21(Agent, Door), Time))
     ).
 */
 %  % =================================.


%; Effect axioms state that
%; if an agent walks through one side of a door,
%; the agent will be at the other side of the door:
% [agent,door,location,time]
% Side2(door)=location ->
% Initiates(WalkThroughDoor12(agent,door),At(agent,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',187).
% From E: 
% 
% '->'(
%    '='(
%       side2(Door), 
%       Location), 
%    initiates_at(
%       walkThroughDoor12(Agent,Door), 
%       at_loc(Agent,Location), 
%       Time)).
(   initiates(walkThroughDoor12(Agent, Door),
              at_loc(Agent, Location)at Time)
;   not equals(side2(Door), Location)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',187).

 /*   (   initiates(walkThroughDoor12(Agent, Door),
                      at(at_loc(Agent, Location), Time))
        ;   not(equals(side2(Door), Location))
        ).
 */
 %  % =================================.


% [agent,door,location,time]
% Side1(door)=location ->
% Initiates(WalkThroughDoor21(agent,door),At(agent,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',192).
% From E: 
% 
% '->'(
%    '='(
%       side1(Door), 
%       Location), 
%    initiates_at(
%       walkThroughDoor21(Agent,Door), 
%       at_loc(Agent,Location), 
%       Time)).
(   initiates(walkThroughDoor21(Agent, Door),
              at_loc(Agent, Location)at Time)
;   not equals(side1(Door), Location)
).

 /*   (   initiates(walkThroughDoor21(Agent, Door),
                      at(at_loc(Agent, Location), Time))
        ;   not(equals(side1(Door), Location))
        ).
 */
 %  % =================================.


% [agent,door,location,time]
% Side1(door)=location ->
% Terminates(WalkThroughDoor12(agent,door),At(agent,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',196).
% From E: 
% 
% '->'(
%    '='(
%       side1(Door), 
%       Location), 
%    terminates_at(
%       walkThroughDoor12(Agent,Door), 
%       at_loc(Agent,Location), 
%       Time)).
(   terminates(walkThroughDoor12(Agent, Door),
               at_loc(Agent, Location)at Time)
;   not equals(side1(Door), Location)
).

 /*   (   terminates(walkThroughDoor12(Agent, Door),
                       at(at_loc(Agent, Location), Time))
        ;   not(equals(side1(Door), Location))
        ).
 */
 %  % =================================.


% [agent,door,location,time]
% Side2(door)=location ->
% Terminates(WalkThroughDoor21(agent,door),At(agent,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',200).
% From E: 
% 
% '->'(
%    '='(
%       side2(Door), 
%       Location), 
%    terminates_at(
%       walkThroughDoor21(Agent,Door), 
%       at_loc(Agent,Location), 
%       Time)).
(   terminates(walkThroughDoor21(Agent, Door),
               at_loc(Agent, Location)at Time)
;   not equals(side2(Door), Location)
).

 /*   (   terminates(walkThroughDoor21(Agent, Door),
                       at(at_loc(Agent, Location), Time))
        ;   not(equals(side2(Door), Location))
        ).
 */
 %  % =================================.


%; walking from one end of a street to another
%; agent walks from the first end of street to the second end.

% event WalkStreet12(agent,street)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',205).
% From E: 
% 
% event(walkStreet12(agent,street)).
events([walkStreet12/2]).
mpred_prop(walkStreet12(agent, street), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',205).
actions([walkStreet12/2]).


%; agent walks from the second end of street to the first end.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',208).
% event WalkStreet21(agent,street)
% From E: 
% 
% event(walkStreet21(agent,street)).
events([walkStreet21/2]).
mpred_prop(walkStreet21(agent, street), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',208).
actions([walkStreet21/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',210).
%; Precondition axioms state that
%; for an agent to walk from one end of a street to another,
%; the agent must be awake,
%; the agent must be standing, and
%; the agent must be at the first end of the street:
% [agent,street,time]
% Happens(WalkStreet12(agent,street),time) ->
% HoldsAt(Awake(agent),time) &
% HoldsAt(Standing(agent),time) &
% HoldsAt(At(agent,Side1(street)),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',215).
% From E: 
% 
% '->'(
%    happens(
%       walkStreet12(Agent,Street), 
%       Time), 
%    ','(
%       holds(
%          awake(Agent), 
%          Time), 
%       ','(
%          holds(
%             standing(Agent), 
%             Time), 
%          holds(
%             at_loc(Agent, 
%                side1(Street)), 
%             Time)))).
(   awake(Agent)at Time,
    standing(Agent)at Time,
    at_loc(Agent, side1(Street))at Time
;   not happens(walkStreet12(Agent, Street), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',215).

 /*  (   at(awake(Agent), Time),
         at(standing(Agent), Time),
         at(at_loc(Agent, side1(Street)), Time)
     ;   not(happens(walkStreet12(Agent, Street), Time))
     ).
 */
 %  % =================================.


% [agent,street,time]
% Happens(WalkStreet21(agent,street),time) ->
% HoldsAt(Awake(agent),time) &
% HoldsAt(Standing(agent),time) &
% HoldsAt(At(agent,Side2(street)),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',222).
% From E: 
% 
% '->'(
%    happens(
%       walkStreet21(Agent,Street), 
%       Time), 
%    ','(
%       holds(
%          awake(Agent), 
%          Time), 
%       ','(
%          holds(
%             standing(Agent), 
%             Time), 
%          holds(
%             at_loc(Agent, 
%                side2(Street)), 
%             Time)))).
(   awake(Agent)at Time,
    standing(Agent)at Time,
    at_loc(Agent, side2(Street))at Time
;   not happens(walkStreet21(Agent, Street), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',222).

 /*  (   at(awake(Agent), Time),
         at(standing(Agent), Time),
         at(at_loc(Agent, side2(Street)), Time)
     ;   not(happens(walkStreet21(Agent, Street), Time))
     ).
 */
 %  % =================================.


%; Effect axioms state that
%; if an agent walks from one end of a street to another,
%; the agent will be at the other end of the street:
% [agent,street,location,time]
% Side2(street)=location ->
% Initiates(WalkStreet12(agent,street),At(agent,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',230).
% From E: 
% 
% '->'(
%    '='(
%       side2(Street), 
%       Location), 
%    initiates_at(
%       walkStreet12(Agent,Street), 
%       at_loc(Agent,Location), 
%       Time)).
(   initiates(walkStreet12(Agent, Street),
              at_loc(Agent, Location)at Time)
;   not equals(side2(Street), Location)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',230).

 /*   (   initiates(walkStreet12(Agent, Street),
                      at(at_loc(Agent, Location), Time))
        ;   not(equals(side2(Street), Location))
        ).
 */
 %  % =================================.


% [agent,street,location,time]
% Side1(street)=location ->
% Initiates(WalkStreet21(agent,street),At(agent,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',235).
% From E: 
% 
% '->'(
%    '='(
%       side1(Street), 
%       Location), 
%    initiates_at(
%       walkStreet21(Agent,Street), 
%       at_loc(Agent,Location), 
%       Time)).
(   initiates(walkStreet21(Agent, Street),
              at_loc(Agent, Location)at Time)
;   not equals(side1(Street), Location)
).

 /*   (   initiates(walkStreet21(Agent, Street),
                      at(at_loc(Agent, Location), Time))
        ;   not(equals(side1(Street), Location))
        ).
 */
 %  % =================================.


% [agent,street,location,time]
% Side1(street)=location ->
% Terminates(WalkStreet12(agent,street),At(agent,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',239).
% From E: 
% 
% '->'(
%    '='(
%       side1(Street), 
%       Location), 
%    terminates_at(
%       walkStreet12(Agent,Street), 
%       at_loc(Agent,Location), 
%       Time)).
(   terminates(walkStreet12(Agent, Street),
               at_loc(Agent, Location)at Time)
;   not equals(side1(Street), Location)
).

 /*   (   terminates(walkStreet12(Agent, Street),
                       at(at_loc(Agent, Location), Time))
        ;   not(equals(side1(Street), Location))
        ).
 */
 %  % =================================.


% [agent,street,location,time]
% Side2(street)=location ->
% Terminates(WalkStreet21(agent,street),At(agent,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',243).
% From E: 
% 
% '->'(
%    '='(
%       side2(Street), 
%       Location), 
%    terminates_at(
%       walkStreet21(Agent,Street), 
%       at_loc(Agent,Location), 
%       Time)).
(   terminates(walkStreet21(Agent, Street),
               at_loc(Agent, Location)at Time)
;   not equals(side2(Street), Location)
).

 /*   (   terminates(walkStreet21(Agent, Street),
                       at(at_loc(Agent, Location), Time))
        ;   not(equals(side2(Street), Location))
        ).
 */
 %  % =================================.


%; floors
%; The floor of room is integer.

% function Floor(room): integer
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',248).
% From E: 
% 
% function(
%    floor(room), 
%    integer).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',248).
function(floor(room),integer).
%; walking up and down staircases
%; agent walks down staircase.

% event WalkDownStaircase(agent,staircase)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',253).
% From E: 
% 
% event(walkDownStaircase(agent,staircase)).
events([walkDownStaircase/2]).
mpred_prop(walkDownStaircase(agent, staircase), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',253).
actions([walkDownStaircase/2]).


%; agent walks up staircase.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',256).
% event WalkUpStaircase(agent,staircase)
% From E: 
% 
% event(walkUpStaircase(agent,staircase)).
events([walkUpStaircase/2]).
mpred_prop(walkUpStaircase(agent, staircase), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',256).
actions([walkUpStaircase/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',258).
%; Precondition axioms state that
%; for an agent to walk down (up) a staircase,
%; the agent must be awake, standing, and
%; at the top (bottom) of the staircase:
% [agent,staircase,time]
% Happens(WalkDownStaircase(agent,staircase),time) ->
% HoldsAt(Awake(agent),time) &
% HoldsAt(Standing(agent),time) &
% HoldsAt(At(agent,Side2(staircase)),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',262).
% From E: 
% 
% '->'(
%    happens(
%       walkDownStaircase(Agent,Staircase), 
%       Time), 
%    ','(
%       holds(
%          awake(Agent), 
%          Time), 
%       ','(
%          holds(
%             standing(Agent), 
%             Time), 
%          holds(
%             at_loc(Agent, 
%                side2(Staircase)), 
%             Time)))).
(   awake(Agent)at Time,
    standing(Agent)at Time,
    at_loc(Agent, side2(Staircase))at Time
;   not happens(walkDownStaircase(Agent, Staircase), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',262).

 /*  (   at(awake(Agent), Time),
         at(standing(Agent), Time),
         at(at_loc(Agent, side2(Staircase)), Time)
     ;   not(happens(walkDownStaircase(Agent, Staircase), Time))
     ).
 */
 %  % =================================.


% [agent,staircase,time]
% Happens(WalkUpStaircase(agent,staircase),time) ->
% HoldsAt(Awake(agent),time) &
% HoldsAt(Standing(agent),time) &
% HoldsAt(At(agent,Side1(staircase)),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',269).
% From E: 
% 
% '->'(
%    happens(
%       walkUpStaircase(Agent,Staircase), 
%       Time), 
%    ','(
%       holds(
%          awake(Agent), 
%          Time), 
%       ','(
%          holds(
%             standing(Agent), 
%             Time), 
%          holds(
%             at_loc(Agent, 
%                side1(Staircase)), 
%             Time)))).
(   awake(Agent)at Time,
    standing(Agent)at Time,
    at_loc(Agent, side1(Staircase))at Time
;   not happens(walkUpStaircase(Agent, Staircase), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',269).

 /*  (   at(awake(Agent), Time),
         at(standing(Agent), Time),
         at(at_loc(Agent, side1(Staircase)), Time)
     ;   not(happens(walkUpStaircase(Agent, Staircase), Time))
     ).
 */
 %  % =================================.


%; Effect axioms state that
%; if an agent walks down (up) a staircase,
%; the agent will be at the bottom (top) of the staircase:
% [agent,staircase,room,time]
% Side1(staircase)=room ->
% Initiates(WalkDownStaircase(agent,staircase),At(agent,room),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',277).
% From E: 
% 
% '->'(
%    '='(
%       side1(Staircase), 
%       Room), 
%    initiates_at(
%       walkDownStaircase(Agent,Staircase), 
%       at_loc(Agent,Room), 
%       Time)).
(   initiates(walkDownStaircase(Agent, Staircase),
              at_loc(Agent, Room)at Time)
;   not equals(side1(Staircase), Room)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',277).

 /*   (   initiates(walkDownStaircase(Agent, Staircase),
                      at(at_loc(Agent, Room), Time))
        ;   not(equals(side1(Staircase), Room))
        ).
 */
 %  % =================================.


% [agent,staircase,room,time]
% Side2(staircase)=room ->
% Terminates(WalkDownStaircase(agent,staircase),At(agent,room),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',282).
% From E: 
% 
% '->'(
%    '='(
%       side2(Staircase), 
%       Room), 
%    terminates_at(
%       walkDownStaircase(Agent,Staircase), 
%       at_loc(Agent,Room), 
%       Time)).
(   terminates(walkDownStaircase(Agent, Staircase),
               at_loc(Agent, Room)at Time)
;   not equals(side2(Staircase), Room)
).

 /*   (   terminates(walkDownStaircase(Agent, Staircase),
                       at(at_loc(Agent, Room), Time))
        ;   not(equals(side2(Staircase), Room))
        ).
 */
 %  % =================================.


% [agent,staircase,room,time]
% Side2(staircase)=room ->
% Initiates(WalkUpStaircase(agent,staircase),At(agent,room),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',286).
% From E: 
% 
% '->'(
%    '='(
%       side2(Staircase), 
%       Room), 
%    initiates_at(
%       walkUpStaircase(Agent,Staircase), 
%       at_loc(Agent,Room), 
%       Time)).
(   initiates(walkUpStaircase(Agent, Staircase),
              at_loc(Agent, Room)at Time)
;   not equals(side2(Staircase), Room)
).

 /*   (   initiates(walkUpStaircase(Agent, Staircase),
                      at(at_loc(Agent, Room), Time))
        ;   not(equals(side2(Staircase), Room))
        ).
 */
 %  % =================================.


% [agent,staircase,room,time]
% Side1(staircase)=room ->
% Terminates(WalkUpStaircase(agent,staircase),At(agent,room),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',290).
% From E: 
% 
% '->'(
%    '='(
%       side1(Staircase), 
%       Room), 
%    terminates_at(
%       walkUpStaircase(Agent,Staircase), 
%       at_loc(Agent,Room), 
%       Time)).
(   terminates(walkUpStaircase(Agent, Staircase),
               at_loc(Agent, Room)at Time)
;   not equals(side1(Staircase), Room)
).

 /*   (   terminates(walkUpStaircase(Agent, Staircase),
                       at(at_loc(Agent, Room), Time))
        ;   not(equals(side1(Staircase), Room))
        ).
 */
 %  % =================================.


%; A state constraint says that if an agent is outside,
%; the agent is dressed.
% [agent,outside,time]
% HoldsAt(At(agent,outside),time) ->
% HoldsAt(Dressed(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',295).
% From E: 
% 
% '->'(
%    holds(
%       at_loc(Agent,Outside), 
%       Time), 
%    holds(
%       dressed(Agent), 
%       Time)).
(   dressed(Agent)at Time
;   not at_loc(Agent, Outside)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',295).

 /*   (   at(dressed(Agent), Time)
        ;   at(not(at_loc(Agent, Outside)), Time)
        ).
 */
 %  % =================================.


%; room looks out onto outside.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',300).
% function LookOutOnto(room): outside
% From E: 
% 
% function(
%    lookOutOnto(room), 
%    outside).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',300).
function(lookOutOnto(room),outside).
%; location1 is adjacent to location2.

% predicate Adjacent(location,location)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',302).
% From E: 
% 
% predicate(adjacent(location,location)).
mpred_prop(adjacent(location, location), predicate).
predicates([adjacent/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',305).
%; A state constraint says that
%; two locations are adjacent if and only if
%; they have a portal in common:
% [location1,location2]
 
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',308).
% Adjacent(location1,location2) <->
% {portal}% 
% (Side1(portal)=location1 &
%  Side2(portal)=location2) |
% (Side2(portal)=location1 &
%  Side1(portal)=location2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',308).
% From E: 
% 
% exists(Portal, 
%    <->(
%       adjacent(Location1,Location2), 
%       ';'(
%          ','(
%             '='(
%                side1(Portal), 
%                Location1), 
%             '='(
%                side2(Portal), 
%                Location2)), 
%          ','(
%             '='(
%                side2(Portal), 
%                Location1), 
%             '='(
%                side1(Portal), 
%                Location2))))).
exists(Portal,  (((equals(side1(Portal), Location1), equals(side2(Portal), Location2);equals(side2(Portal), Location1), equals(side1(Portal), Location2));not adjacent(Location1, Location2)), (adjacent(Location1, Location2);(not equals(side1(Portal), Location1);not equals(side2(Portal), Location2)), (not equals(side2(Portal), Location1);not equals(side1(Portal), Location2))))).
 %  exists(Portal,  (((equals(side1(Portal), Location1), equals(side2(Portal), Location2);equals(side2(Portal), Location1), equals(side1(Portal), Location2));not(adjacent(Location1, Location2))), (adjacent(Location1, Location2);(not(equals(side1(Portal), Location1));not(equals(side2(Portal), Location2))), (not(equals(side2(Portal), Location1));not(equals(side1(Portal), Location2)))))).
 %  % =================================.


%; The ground of outside is ground.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',316).
% function GroundOf(outside): ground
% From E: 
% 
% function(
%    groundOf(outside), 
%    ground).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',316).
function(groundOf(outside),ground).
%; The sky of outside is sky.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',318).
% function SkyOf(outside): sky
% From E: 
% 
% function(
%    skyOf(outside), 
%    sky).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',318).
function(skyOf(outside),sky).
 %; State constraints fix the location of ground and sky:
% [outside,ground,time]
% GroundOf(outside) = ground ->
% HoldsAt(At(ground,outside),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',322).
% From E: 
% 
% '->'(
%    '='(
%       groundOf(Outside), 
%       Ground), 
%    holds(
%       at_loc(Ground,Outside), 
%       Time)).
(   at_loc(Ground, Outside)at Time
;   not equals(groundOf(Outside), Ground)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',322).

 /*   (   at(at_loc(Ground, Outside), Time)
        ;   not(equals(groundOf(Outside), Ground))
        ).
 */
 %  % =================================.


% [outside,sky,time]
% SkyOf(outside) = sky ->
% HoldsAt(At(sky,outside),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',327).
% From E: 
% 
% '->'(
%    '='(
%       skyOf(Outside), 
%       Sky), 
%    holds(
%       at_loc(Sky,Outside), 
%       Time)).
(   at_loc(Sky, Outside)at Time
;   not equals(skyOf(Outside), Sky)
).

 /*   (   at(at_loc(Sky, Outside), Time)
        ;   not(equals(skyOf(Outside), Sky))
        ).
 */
 %  % =================================.


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.e',329).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RTSpace.lps.pl')).
