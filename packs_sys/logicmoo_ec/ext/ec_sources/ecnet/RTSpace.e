;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; RTSpace: room-scale topological space
;
; We use topological and metric representations of space,
; at two levels of granularity---room-scale and object-scale.
; The RTSpace representation deals with topological space at
; the scale of rooms and outdoor locations.
; This representation of space consists of locations, which
; are connected by portals. There are two types of locations:
; rooms and outside areas (outsides).
;

; object is at location.
fluent At(object,location)
manualrelease At

[object1,location,time]
({object2} PartOf(object1,object2)) ->
ReleasedAt(At(object1,location),time).

; A state constraint says that an object
; is at one location at a time:
[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
HoldsAt(At(object,location2),time) ->
location1=location2.

; connectivity

; Side one of portal is location.
function Side1(portal): location
; Side two of portal is location.
function Side2(portal): location

; The building of room is building.
function BuildingOf(room): building

; object is at a location that has portal.
fluent NearPortal(object,portal)
noninertial NearPortal

; A state constraint says that an object is near
; a portal if and only if there is a location such that
; the object is at the location and one of the sides
; of the portal is the location:
[object,portal,time]
HoldsAt(NearPortal(object,portal),time) <->
{location}
 (Side1(portal)=location|
  Side2(portal)=location) &
 HoldsAt(At(object,location),time).

; locking and unlocking doors

; agent unlocks door.
event DoorUnlock(agent,door)
; agent locks door.
event DoorLock(agent,door)
; door is unlocked.
fluent DoorUnlocked(door)

; A precondition axiom states that
; for an agent to unlock a door,
; the agent must be awake,
; the door must not already be unlocked, and
; the agent must be near the door:
[agent,door,time]
Happens(DoorUnlock(agent,door),time) ->
HoldsAt(Awake(agent),time) &
!HoldsAt(DoorUnlocked(door),time) &
HoldsAt(NearPortal(agent,door),time).

; An effect axiom states that
; if an agent unlocks a door,
; the door will be unlocked:
[agent,door,time]
Initiates(DoorUnlock(agent,door),DoorUnlocked(door),time).

; A precondition axiom states that
; for an agent to lock a door,
; the agent must be awake,
; the door must be unlocked, and
; the agent must be near the door:
[agent,door,time]
Happens(DoorLock(agent,door),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(DoorUnlocked(door),time) &
HoldsAt(NearPortal(agent,door),time).

; An effect axiom states that
; if an agent locks a door,
; the door will no longer be unlocked.
[agent,door,time]
Terminates(DoorLock(agent,door),DoorUnlocked(door),time).

; A state constraint says that if a door is open,
; it is unlocked:
[door,time]
HoldsAt(DoorIsOpen(door),time) -> HoldsAt(DoorUnlocked(door),time).

; opening and closing doors

; agent opens door.
event DoorOpen(agent,door)
; agent closes door.
event DoorClose(agent,door)
; door is open.
fluent DoorIsOpen(door)

; A precondition axiom states that
; for an agent to open a door,
; the agent must be awake,
; the door must not already be open,
; the door must be unlocked, and
; the agent must be near the door:
[agent,door,time]
Happens(DoorOpen(agent,door),time) ->
HoldsAt(Awake(agent),time) &
!HoldsAt(DoorIsOpen(door),time) &
HoldsAt(DoorUnlocked(door),time) &
HoldsAt(NearPortal(agent,door),time).

; An effect axiom states that
; if an agent opens a door,
; the door will be open:
[agent,door,time]
Initiates(DoorOpen(agent,door),DoorIsOpen(door),time).

; A precondition axiom states that
; for an agent to close a door,
; the agent must be awake,
; the door must be open,
; the door must be unlocked, and
; the agent must be near the door:
[agent,door,time]
Happens(DoorClose(agent,door),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(DoorIsOpen(door),time) &
HoldsAt(DoorUnlocked(door),time) &
HoldsAt(NearPortal(agent,door),time).

; An effect axiom states that
; if an agent closes a door,
; the door will no longer be open:
[agent,door,time]
Terminates(DoorClose(agent,door),DoorIsOpen(door),time).

; passing through doors

; agent walks through side one of door.
event WalkThroughDoor12(agent,door)
; agent walks through side two of door.
event WalkThroughDoor21(agent,door)

; Precondition axioms state that
; for an agent to walk through a side of a door,
; the agent must be awake and standing,
; the door must be open, and
; the agent must be at the side of the door that
; the agent walks through:
[agent,door,time]
Happens(WalkThroughDoor12(agent,door),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(Standing(agent),time) &
HoldsAt(DoorIsOpen(door),time) &
HoldsAt(At(agent,Side1(door)),time).

[agent,door,time]
Happens(WalkThroughDoor21(agent,door),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(Standing(agent),time) &
HoldsAt(DoorIsOpen(door),time) &
HoldsAt(At(agent,Side2(door)),time).

; Effect axioms state that
; if an agent walks through one side of a door,
; the agent will be at the other side of the door:
[agent,door,location,time]
Side2(door)=location ->
Initiates(WalkThroughDoor12(agent,door),At(agent,location),time).

[agent,door,location,time]
Side1(door)=location ->
Initiates(WalkThroughDoor21(agent,door),At(agent,location),time).

[agent,door,location,time]
Side1(door)=location ->
Terminates(WalkThroughDoor12(agent,door),At(agent,location),time).

[agent,door,location,time]
Side2(door)=location ->
Terminates(WalkThroughDoor21(agent,door),At(agent,location),time).

; walking from one end of a street to another

; agent walks from the first end of street to the second end.
event WalkStreet12(agent,street)
; agent walks from the second end of street to the first end.
event WalkStreet21(agent,street)

; Precondition axioms state that
; for an agent to walk from one end of a street to another,
; the agent must be awake,
; the agent must be standing, and
; the agent must be at the first end of the street:
[agent,street,time]
Happens(WalkStreet12(agent,street),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(Standing(agent),time) &
HoldsAt(At(agent,Side1(street)),time).

[agent,street,time]
Happens(WalkStreet21(agent,street),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(Standing(agent),time) &
HoldsAt(At(agent,Side2(street)),time).

; Effect axioms state that
; if an agent walks from one end of a street to another,
; the agent will be at the other end of the street:
[agent,street,location,time]
Side2(street)=location ->
Initiates(WalkStreet12(agent,street),At(agent,location),time).

[agent,street,location,time]
Side1(street)=location ->
Initiates(WalkStreet21(agent,street),At(agent,location),time).

[agent,street,location,time]
Side1(street)=location ->
Terminates(WalkStreet12(agent,street),At(agent,location),time).

[agent,street,location,time]
Side2(street)=location ->
Terminates(WalkStreet21(agent,street),At(agent,location),time).

; floors

; The floor of room is integer.
function Floor(room): integer

; walking up and down staircases

; agent walks down staircase.
event WalkDownStaircase(agent,staircase)
; agent walks up staircase.
event WalkUpStaircase(agent,staircase)

; Precondition axioms state that
; for an agent to walk down (up) a staircase,
; the agent must be awake, standing, and
; at the top (bottom) of the staircase:
[agent,staircase,time]
Happens(WalkDownStaircase(agent,staircase),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(Standing(agent),time) &
HoldsAt(At(agent,Side2(staircase)),time).

[agent,staircase,time]
Happens(WalkUpStaircase(agent,staircase),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(Standing(agent),time) &
HoldsAt(At(agent,Side1(staircase)),time).

; Effect axioms state that
; if an agent walks down (up) a staircase,
; the agent will be at the bottom (top) of the staircase:
[agent,staircase,room,time]
Side1(staircase)=room ->
Initiates(WalkDownStaircase(agent,staircase),At(agent,room),time).

[agent,staircase,room,time]
Side2(staircase)=room ->
Terminates(WalkDownStaircase(agent,staircase),At(agent,room),time).

[agent,staircase,room,time]
Side2(staircase)=room ->
Initiates(WalkUpStaircase(agent,staircase),At(agent,room),time).

[agent,staircase,room,time]
Side1(staircase)=room ->
Terminates(WalkUpStaircase(agent,staircase),At(agent,room),time).

; A state constraint says that if an agent is outside,
; the agent is dressed.
[agent,outside,time]
HoldsAt(At(agent,outside),time) ->
HoldsAt(Dressed(agent),time).

; room looks out onto outside.
function LookOutOnto(room): outside

; location1 is adjacent to location2.
predicate Adjacent(location,location)

; A state constraint says that
; two locations are adjacent if and only if
; they have a portal in common:
[location1,location2] Adjacent(location1,location2) <->
{portal}
(Side1(portal)=location1 &
 Side2(portal)=location2) |
(Side2(portal)=location1 &
 Side1(portal)=location2).

; The ground of outside is ground.
function GroundOf(outside): ground
; The sky of outside is sky.
function SkyOf(outside): sky
 

; State constraints fix the location of ground and sky:
[outside,ground,time]
GroundOf(outside) = ground ->
HoldsAt(At(ground,outside),time).

[outside,sky,time]
SkyOf(outside) = sky ->
HoldsAt(At(sky,outside),time).

; End of file.
