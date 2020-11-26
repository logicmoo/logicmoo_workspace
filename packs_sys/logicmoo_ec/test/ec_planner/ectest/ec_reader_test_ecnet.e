


; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: foundations/Root.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
sort boolean
sort integer
reified sort predicate
reified sort function

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: foundations/EC.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; Event Calculus (EC)
;
; @incollection{MillerShanahan:2002,
;   author = "Rob Miller and Murray Shanahan",
;   year = "2002",
;   title = "Some alternative formulations of the event calculus",
;   editor = "Antonis C. Kakas and Fariba Sadri",
;   booktitle = "Computational Logic: Logic Programming and Beyond: Essays in Honour of \uppercase{R}obert \uppercase{A}. \uppercase{K}owalski, Part \uppercase{II}",
;   series = "Lecture Notes in Computer Science",
;   volume = "2408",
;   pages = "452--490",
;   address = "Berlin",
;   publisher = "Springer",
; }
;

sort time: integer
sort offset: integer

reified sort fluent
reified sort event

predicate Happens(event,time)
predicate HoldsAt(fluent,time)
predicate ReleasedAt(fluent,time)
predicate Initiates(event,fluent,time)
predicate Terminates(event,fluent,time)
predicate Releases(event,fluent,time)
predicate Trajectory(fluent,time,fluent,offset)

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: foundations/DEC.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; Discrete Event Calculus (DEC)
;
; @article{Mueller:2004a,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Event calculus reasoning through satisfiability",
;   journal = "Journal of Logic and Computation",
;   volume = "14",
;   number = "5",
;   pages = "703--730",
; }
;

sort time: integer
sort offset: integer

reified sort fluent
reified sort event

predicate Happens(event,time)
predicate HoldsAt(fluent,time)
predicate ReleasedAt(fluent,time)

predicate Initiates(event,fluent,time)
predicate Terminates(event,fluent,time)
predicate Releases(event,fluent,time)

[fluent,time]
(HoldsAt(fluent,time) &
 !ReleasedAt(fluent,time+1) &
 !({event} Happens(event,time) & Terminates(event,fluent,time))) ->
HoldsAt(fluent,time+1).

[fluent,time]
(!HoldsAt(fluent,time) &
 !ReleasedAt(fluent,time+1) &
 !({event} Happens(event,time) & Initiates(event,fluent,time))) ->
!HoldsAt(fluent,time+1).

[fluent,time]
(!ReleasedAt(fluent,time) &
 !({event} Happens(event,time) & Releases(event,fluent,time))) ->
!ReleasedAt(fluent,time+1).

[fluent,time]
(ReleasedAt(fluent,time) &
 !({event} Happens(event,time) &
   (Initiates(event,fluent,time) |
    Terminates(event,fluent,time)))) ->
ReleasedAt(fluent,time+1).

[event,fluent,time]
(Happens(event,time) & Initiates(event,fluent,time)) ->
(HoldsAt(fluent,time+1) & !ReleasedAt(fluent,time+1)).

[event,fluent,time]
(Happens(event,time) & Terminates(event,fluent,time)) ->
(!HoldsAt(fluent,time+1) & !ReleasedAt(fluent,time+1)).

[event,fluent,time]
(Happens(event,time) & Releases(event,fluent,time)) ->
ReleasedAt(fluent,time+1).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: foundations/ECCausal.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; Causal Constraints
;
; @inproceedings{Shanahan:1999a,
;   author = "Murray Shanahan",
;   year = "1999",
;   title = "The ramification problem in the event calculus",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}ixteenth \uppercase{I}nternational \uppercase{J}oint \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
;   pages = "140--146",
;   address = "San Mateo, CA",
;   publisher = "Morgan Kaufmann",
; }
;

predicate Started(fluent,time)
predicate Stopped(fluent,time)

[fluent,time]
Started(fluent,time) <->
(HoldsAt(fluent,time) |
 ({event} Happens(event,time) & Initiates(event,fluent,time))).

[fluent,time]
Stopped(fluent,time) <->
(!HoldsAt(fluent,time) |
 ({event} Happens(event,time) & Terminates(event,fluent,time))).

predicate Initiated(fluent,time)
predicate Terminated(fluent,time)

[fluent,time]
Initiated(fluent,time) <->
(Started(fluent,time) &
 !({event} Happens(event,time) & Terminates(event,fluent,time))).

[fluent,time]
Terminated(fluent,time) <->
(Stopped(fluent,time) &
 !({event} Happens(event,time) & Initiates(event,fluent,time))).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: foundations/ECTraj.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; @incollection{MillerShanahan:2002,
;   author = "Rob Miller and Murray Shanahan",
;   year = "2002",
;   title = "Some alternative formulations of the event calculus",
;   editor = "Antonis C. Kakas and Fariba Sadri",
;   booktitle = "Computational Logic: Logic Programming and Beyond: Essays in Honour of \uppercase{R}obert \uppercase{A}. \uppercase{K}owalski, Part \uppercase{II}",
;   series = "Lecture Notes in Computer Science",
;   volume = "2408",
;   pages = "452--490",
;   address = "Berlin",
;   publisher = "Springer",
; }
;

predicate Clipped(time,fluent,time)
predicate Declipped(time,fluent,time)

predicate Trajectory(fluent,time,fluent,offset)
predicate AntiTrajectory(fluent,time,fluent,offset)

[event,fluent,fluent2,offset,time]
Happens(event,time) &
Initiates(event,fluent,time) &
0 < offset &
Trajectory(fluent,time,fluent2,offset) &
!Clipped(time,fluent,time+offset) ->
HoldsAt(fluent2,time+offset).

[event,fluent,fluent2,offset,time]
Happens(event,time) &
Terminates(event,fluent,time) &
0 < offset &
AntiTrajectory(fluent,time,fluent2,offset) &
!Declipped(time,fluent,time+offset) ->
HoldsAt(fluent2,time+offset).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Ontology.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; integer
;

sort diameter: integer

; object

sort object

sort agent: object

sort physobj: object
sort bed: physobj
sort snowflake: physobj
sort sky: physobj

sort stuff: physobj

sort surface: physobj
sort ground: surface

sort snow: stuff
sort ball

sort food: physobj
sort fruit: food
sort orange: fruit
sort salad: food

sort clothing: physobj
sort scarf: clothing
sort hat: clothing

sort vegetablematter: physobj
sort coal: vegetablematter

sort bodypart: physobj
sort hand: bodypart

sort papertowels: physobj
sort device: physobj
sort electronicdevice: device
sort lamp: electronicdevice

sort cat: physobj
sort horse: physobj

sort weapon: physobj
sort gun: weapon
sort bomb: weapon
sort bullet: weapon

; location

sort location
sort room: location, outside: location

; portal

sort portal
sort door: portal, staircase: portal
sort street: portal
sort track: portal

sort building

sort fire: object
sort smoke: physobj

sort furniture: physobj
sort chair: furniture
sort table: furniture

sort bill: physobj
sort ticket: physobj
sort envelope: physobj

sort text: physobj
sort book: text
sort letter: text
sort menu: text

sort paper: physobj

sort content
sort script

sort container: physobj
sort cigarette: physobj
sort ashtray: physobj
sort umbrella: physobj

sort pen: physobj

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/RTSpace.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/OTSpace.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; OTSpace: object-scale topological space
;
; The OTSpace representation deals with topological space at
; the scale of objects such as agents (humans and animals)
; and physical objects.
;

; PartOf

; physobj is a part of object.
predicate PartOf(physobj,object)

; A state constraint says that if a physical object
; is part of an object, the location of the
; physical object is the same as the location of the object:
[physobj,object,location,time]
PartOf(physobj,object) &
HoldsAt(At(object,location),time) ->
HoldsAt(At(physobj,location),time).

; rolling a snowball bigger

; agent rolls stuff1 along stuff2.
event RollAlong(agent,stuff,stuff)
; The diameter of ball is diameter.
fluent Diameter(ball,diameter)

; A state constraint says that a ball has a unique diameter:
[ball,diameter1,diameter2,time]
HoldsAt(Diameter(ball,diameter1),time) &
HoldsAt(Diameter(ball,diameter2),time) ->
diameter1=diameter2.

; Effect axiom state that if an agent rolls some snow along
; some other snow, the diameter of the first snow will increase:
[agent,snow1,snow2,diameter1,diameter2,time]
HoldsAt(Diameter(snow1,diameter1),time) &
diameter2 = diameter1+1 ->
Initiates(RollAlong(agent,snow1,snow2),
          Diameter(snow1,diameter2),
          time).

[agent,snow1,snow2,diameter1,time]
HoldsAt(Diameter(snow1,diameter1),time) ->
Terminates(RollAlong(agent,snow1,snow2),
           Diameter(snow1,diameter1),
           time).

; A precondition axiom states that
; for an agent to roll some snow along some other snow,
; there must be a location such that
; the agent is at the location,
; the first snow is at the location, and
; the second snow is at the location:
;[agent,snow1,snow2,time]
;Happens(RollAlong(agent,snow1,snow2),time) ->
;{location}
;HoldsAt(At(agent,location),time) &
;HoldsAt(At(snow1,location),time) &
;HoldsAt(At(snow2,location),time).

; motion

; object moves (in place).
event Move(object)

; Holding

; agent is holding physobj.
fluent Holding(agent,physobj)
; agent holds or picks up physobj.
event Hold(agent,physobj)
; agent picks up some stuff1 from stuff2.
event HoldSome(agent,stuff,stuff)
; agent releases or lets go of physobj.
event LetGoOf(agent,physobj)

; An effect axiom states that if an agent holds
; a physical object, the agent will be holding the
; physical object:
[agent,physobj,time]
Initiates(Hold(agent,physobj),Holding(agent,physobj),time).

; A precondition axiom states that
; for an agent to hold a physical object,
; there must be a location such that
; the agent is at the location and
; the physical object is at the location:
;[agent,physobj,time]
;Happens(Hold(agent,physobj),time) ->
;{location}
;  HoldsAt(At(agent,location),time) &
;  HoldsAt(At(physobj,location),time).

; An effect axiom states that if an agent
; lets go of a physical object, the agent is no longer holding
; the physical object:
[agent,physobj,time]
Terminates(LetGoOf(agent,physobj),Holding(agent,physobj),time).

; A precondition axiom states that
; for an agent to let go of a physical object,
; the agent must be holding the physical object:
[agent,physobj,time]
Happens(LetGoOf(agent,physobj),time) ->
HoldsAt(Holding(agent,physobj),time).

; A releases axiom states that if an agent holds
; a physical object,
; the physical object's location will be released
; from inertia:
[agent,physobj,location,time]
Releases(Hold(agent,physobj),At(physobj,location),time).

; A state constraint says that if an agent is holding
; a physical object and the agent is at a location,
; the physical object is also at the location:
[agent,physobj,location,time]
HoldsAt(Holding(agent,physobj),time) &
HoldsAt(At(agent,location),time) ->
HoldsAt(At(physobj,location),time).

; A releases axiom states that if an agent holds
; a physical object,
; the locations of the parts of the physical object
; will be released from inertia:
[agent,physobj1,physobj2,location,time]
PartOf(physobj1,physobj2) ->
Releases(Hold(agent,physobj2),At(physobj1,location),time).

; Further, if an agent holds a physical object,
; the locations of the physical objects of which
; the physical object is a part
; will be released from inertia:
[agent,physobj1,physobj2,location,time]
PartOf(physobj1,physobj2) ->
Releases(Hold(agent,physobj1),At(physobj2,location),time).

;[agent,physobj,location1,location2,time]
;(!{object} PartOf(physobj,object)) &
;HoldsAt(At(agent,location1),time) &
;location1 != location2 ->
;Terminates(LetGoOf(agent,physobj),At(physobj,location2),time).

[agent,physobj,location,time]
(!{object} PartOf(physobj,object)) &
HoldsAt(At(agent,location),time) ->
Initiates(LetGoOf(agent,physobj),At(physobj,location),time).

;[agent,physobj1,physobj2,location1,location2,time]
;PartOf(physobj1,physobj2) &
;(!{object} PartOf(physobj2,object)) &
;HoldsAt(At(agent,location1),time) &
;location1 != location2 ->
;Terminates(LetGoOf(agent,physobj1),At(physobj2,location2),time).

[agent,physobj1,physobj2,location,time]
PartOf(physobj1,physobj2) &
(!{object} PartOf(physobj2,object)) &
HoldsAt(At(agent,location),time) ->
Initiates(LetGoOf(agent,physobj1),At(physobj2,location),time).

; An effect axiom states that if an agent is at a location
; and lets go of a physical object, the physical object
; will be at the location:
[agent,physobj,location,time]
HoldsAt(At(agent,location),time) ->
Initiates(LetGoOf(agent,physobj),At(physobj,location),time).

; An effect axiom states that if an agent picks up
; some stuff out of some other stuff, the agent will
; be holding the first stuff:
[agent,stuff1,stuff2,time]
Initiates(HoldSome(agent,stuff1,stuff2),
          Holding(agent,stuff1),
          time).

; A precondition axiom states that
; for an agent to pick up some stuff out of some other stuff,
; the first stuff must be a part of the second stuff and
; there must be a location such that the agent is at the location,
; the first stuff is at the location, and the second stuff is
; at the location:
[agent,stuff1,stuff2,time]
Happens(HoldSome(agent,stuff1,stuff2),time) ->
PartOf(stuff1,stuff2) &
{location}
  HoldsAt(At(agent,location),time) &
  HoldsAt(At(stuff1,location),time) &
  HoldsAt(At(stuff2,location),time).

; A releases axiom states that if an agent picks up some
; stuff out of some other stuff,
; the first stuff's location will be released
; from inertia:
[agent,stuff1,stuff2,location,time]
Releases(HoldSome(agent,stuff1,stuff2),At(stuff1,location),time).

; Inside

; physobj1 is inside physobj2.
fluent Inside(physobj,physobj)
; agent puts physobj1 inside physobj2.
event PutInside(agent,physobj,physobj)
; agent takes physobj1 out of physobj2.
event TakeOutOf(agent,physobj,physobj)

; A state constraint says that a physical object cannot
; be inside itself:
[physobj1,physobj2,time]
HoldsAt(Inside(physobj1,physobj2),time) ->
physobj1!=physobj2.

; A state constraint says that if a physical object is
; inside another physical object, the second physical object
; is not inside the first physical object:
[physobj1,physobj2,time]
HoldsAt(Inside(physobj1,physobj2),time) ->
!HoldsAt(Inside(physobj2,physobj1),time).

; An effect axiom states that if an agent puts a physical
; object inside another physical object, the first
; physical object will be inside the second physical object:
[agent,physobj1,physobj2,time]
Initiates(PutInside(agent,physobj1,physobj2),
          Inside(physobj1,physobj2),time).

; An effect axiom states that if an agent puts a physical
; object inside another physical object, the agent will
; no longer be holding the first physical object:
[agent,physobj1,physobj2,time]
Terminates(PutInside(agent,physobj1,physobj2),
           Holding(agent,physobj1),time).

; A precondition axiom states that
; for an agent to put a physical object inside another
; physical object,
; the agent must be holding the first physical object
; and there must be a location such that
; the agent is at the location and
; the second physical object is at the location:
;[agent,physobj1,physobj2,time]
;Happens(PutInside(agent,physobj1,physobj2),time) ->
;HoldsAt(Holding(agent,physobj1),time) &
;{location}
; HoldsAt(At(agent,location),time) &
; HoldsAt(At(physobj2,location),time).

; An effect axiom states that
; if an agent takes a physical object out of another
; physical object, the first physical object
; will no longer be inside the second physical object:
[agent,physobj1,physobj2,time]
Terminates(TakeOutOf(agent,physobj1,physobj2),
           Inside(physobj1,physobj2),time).

; A precondition axiom states that
; for an agent to take a physical object out of another
; physical object,
; the first physical object must be inside the second physical object
; and there must be a location such that
; the agent is at the location,
; the first physical object is at the location, and
; the second physical object is at the location:
[agent,physobj1,physobj2,time]
Happens(TakeOutOf(agent,physobj1,physobj2),time) ->
HoldsAt(Inside(physobj1,physobj2),time) &
{location}
 HoldsAt(At(agent,location),time) &
 HoldsAt(At(physobj1,location),time) &
 HoldsAt(At(physobj2,location),time).

; A releases axiom states that if an agent puts a physical
; object inside another physical object,
; the first physical object's location will be released
; from inertia:
[agent,physobj1,physobj2,location,time]
Releases(PutInside(agent,physobj1,physobj2),
         At(physobj1,location),time).

; A state constraint says that if a physical object is inside
; another physical object and the second physical object is
; at a location, the first physical object is also at the location:
[physobj1,physobj2,location,time]
HoldsAt(Inside(physobj1,physobj2),time) &
HoldsAt(At(physobj2,location),time) ->
HoldsAt(At(physobj1,location),time).

; An effect axiom states that if an agent takes a physical
; object out of another physical object,
; the agent will be holding the first physical object:
[agent,physobj1,physobj2,time]
Initiates(TakeOutOf(agent,physobj1,physobj2),
          Holding(agent,physobj1),
          time).

; On

; physobj1 is on physobj2.
fluent On(physobj,physobj)

; agent places physobj1 on physobj2.
event PlaceOn(agent,physobj,physobj)
; agent takes physobj1 off of physobj2.
event TakeOffOf(agent,physobj,physobj)

; A state constraint says that a physical object cannot
; be on itself:
[physobj1,physobj2,time]
HoldsAt(On(physobj1,physobj2),time) ->
physobj1!=physobj2.

; A state constraint says that if a physical object is
; on another physical object, the second physical object
; is not on the first physical object:
[physobj1,physobj2,time]
HoldsAt(On(physobj1,physobj2),time) ->
!HoldsAt(On(physobj2,physobj1),time).

; An effect axiom states that if an agent places a physical
; object on another physical object, the first
; physical object will be on the second physical object:
[agent,physobj1,physobj2,time]
Initiates(PlaceOn(agent,physobj1,physobj2),
          On(physobj1,physobj2),time).

; An effect axiom states that if an agent places a physical
; object on another physical object, the agent will
; no longer be holding the first physical object:
[agent,physobj1,physobj2,time]
Terminates(PlaceOn(agent,physobj1,physobj2),
           Holding(agent,physobj1),time).

; A precondition axiom states that
; for an agent to place a physical object on another
; physical object,
; the agent must be holding the first physical object
; and there must be a location such that
; the agent is at the location and
; the second physical object is at the location:
;[agent,physobj1,physobj2,time]
;Happens(PlaceOn(agent,physobj1,physobj2),time) ->
;HoldsAt(Holding(agent,physobj1),time) &
;{location}
; HoldsAt(At(agent,location),time) &
; HoldsAt(At(physobj2,location),time).

; An effect axiom states that
; if an agent takes a physical object off of another
; physical object, the first physical object
; will no longer be on the second physical object:
[agent,physobj1,physobj2,time]
Terminates(TakeOffOf(agent,physobj1,physobj2),
           On(physobj1,physobj2),time).

; An effect axiom states that if an agent takes a physical
; object off of another physical object,
; the agent will be holding the first physical object:
[agent,physobj1,physobj2,time]
Initiates(TakeOffOf(agent,physobj1,physobj2),
          Holding(agent,physobj1),time).

; A precondition axiom states that
; for an agent to take a physical object off of another
; physical object,
; the first physical object must be on the second physical object
; and there must be a location such that
; the agent is at the location and
; the first physical object is at the location:
; the second physical object is at the location:
[agent,physobj1,physobj2,time]
Happens(TakeOffOf(agent,physobj1,physobj2),time) ->
HoldsAt(On(physobj1,physobj2),time) &
{location}
 HoldsAt(At(agent,location),time) &
 HoldsAt(At(physobj1,location),time) &
 HoldsAt(At(physobj2,location),time).

; A releases axiom states that if an agent places a physical
; object on another physical object,
; the first physical object's location will be released
; from inertia:
[agent,physobj1,physobj2,location,time]
Releases(PlaceOn(agent,physobj1,physobj2),
         At(physobj1,location),
         time).

; A state constraint says that if a physical object is on
; another physical object and the second physical object is
; at a location, the first physical object is also at the location:
[physobj1,physobj2,location,time]
HoldsAt(On(physobj1,physobj2),time) &
HoldsAt(At(physobj2,location),time) ->
HoldsAt(At(physobj1,location),time).

fluent Near(agent,object)
event WalkFromTo(agent,object,object)
event WalkFrom(agent,object)
event RunFromTo(agent,object,object)

[agent,object1,object2,time]
Initiates(WalkFromTo(agent,object1,object2),
          Near(agent,object2),
          time).

[agent,object1,object2,time]
Terminates(WalkFromTo(agent,object1,object2),
           Near(agent,object1),
           time).

[agent,object1,object2,time]
Happens(WalkFromTo(agent,object1,object2),time) ->
{location}
HoldsAt(At(agent,location),time) &
HoldsAt(At(object1,location),time) &
HoldsAt(At(object2,location),time).

[agent,object1,object2,time]
Initiates(RunFromTo(agent,object1,object2),
          Near(agent,object2),
          time).

[agent,object1,object2,time]
Terminates(RunFromTo(agent,object1,object2),
           Near(agent,object1),
           time).

[agent,object1,object2,time]
Happens(RunFromTo(agent,object1,object2),time) ->
{location}
HoldsAt(At(agent,location),time) &
HoldsAt(At(object1,location),time) &
HoldsAt(At(object2,location),time).

[agent,object,time]
Terminates(WalkFrom(agent,object),
           Near(agent,object),
           time).

[agent,object,location,door,time]
HoldsAt(Near(agent,object),time) &
HoldsAt(At(agent,location),time) &
HoldsAt(At(object,location),time) &
Side1(door)=location &
Happens(WalkThroughDoor12(agent,door),time) ->
Happens(WalkFrom(agent,object),time).

[agent,object,location,door,time]
HoldsAt(Near(agent,object),time) &
HoldsAt(At(agent,location),time) &
HoldsAt(At(object,location),time) &
Side2(door)=location &
Happens(WalkThroughDoor21(agent,door),time) ->
Happens(WalkFrom(agent,object),time).

[agent,object,room,staircase,time]
HoldsAt(Near(agent,object),time) &
HoldsAt(At(agent,room),time) &
HoldsAt(At(object,room),time) &
Side1(staircase)=room &
Happens(WalkUpStaircase(agent,staircase),time) ->
Happens(WalkFrom(agent,object),time).

[agent,object,room,staircase,time]
HoldsAt(Near(agent,object),time) &
HoldsAt(At(agent,room),time) &
HoldsAt(At(object,room),time) &
Side2(staircase)=room &
Happens(WalkDownStaircase(agent,staircase),time) ->
Happens(WalkFrom(agent,object),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/OMSpace.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; OMSpace: object-scale metric space
;
; The OMSpace representation deals with metric space at
; the scale of objects.
;
; @article{Morgenstern:2001,
;   author = "Morgenstern, Leora",
;   year = "2001",
;   title = "Mid-sized axiomatizations of commonsense problems: A case study in egg cracking",
;   journal = "Studia Logica",
;   volume = "67",
;   pages = "333--384",
; }
;
; @article{Shanahan:2003,
;   author = "Shanahan, Murray",
;   year = "2004",
;   title = "An attempt to formalise a non-trivial benchmark problem in common sense reasoning",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "141--165",
; }
;

sort height: integer
sort distance: integer

; Height

; The height of object is height.
fluent Height(object,height)

; State constraint represent the fact that each
; object has a unique height:
[object,height1,height2,time]
HoldsAt(Height(object,height1),time) &
HoldsAt(Height(object,height2),time) ->
height1=height2.

[object,time]
{height}
HoldsAt(Height(object,height),time).

; falling

; physobj1 is falling from physobj2 to physobj3.
fluent FallingFromTo(physobj,physobj,physobj)
; physobj1 starts falling from physobj2 to physobj3.
event StartFallingFromTo(physobj,physobj,physobj)
; physobj1 collides with physobj2.
event CollideWith(physobj,physobj)

; An effect axiom states that if a first physical object starts
; falling from a second physical object to a third physical
; object, the first physical object will be falling from the
; second physical object to the third physical object:
[physobj1,physobj2,physobj3,time]
Initiates(StartFallingFromTo(physobj1,physobj2,physobj3),
          FallingFromTo(physobj1,physobj2,physobj3),
          time).

; A precondition axiom states that for
; a first physical object to start
; falling from a second physical object to a third physical
; object,
; the height of the first physical object and the
; second physical object must be the same.
[physobj1,physobj2,physobj3,height1,height2,time]
Happens(StartFallingFromTo(physobj1,physobj2,physobj3),time) &
HoldsAt(Height(physobj1,height1),time) &
HoldsAt(Height(physobj2,height2),time) ->
height1=height2.

; A state constraint says that a physical object
; cannot fall from itself, cannot fall to itself,
; and cannot fall from and to the same physical object:
[physobj1,physobj2,physobj3,time]
HoldsAt(FallingFromTo(physobj1,physobj2,physobj3),time) ->
physobj1!=physobj2 &
physobj1!=physobj3 &
physobj2!=physobj3.

; A state constraint says that the sky cannot fall:
[sky,physobj1,physobj2,time]
!HoldsAt(FallingFromTo(sky,physobj1,physobj2),time).

; A releases axiom states that if
; if a first physical object starts
; falling from a second physical object to a third physical
; object, the height of the first physical object
; will be released from inertia:
[physobj1,physobj2,physobj3,height,time]
Releases(StartFallingFromTo(physobj1,physobj2,physobj3),
         Height(physobj1,height),
         time).

; A trajectory axiom states that
; if a first physical object starts falling
; from a second physical object
; to a third physical object
; at a time and
; the first physical object has a height at the time,
; then the first physical object will have a height
; equal to the height minus an offset
; at a time equal to the time plus the offset:
[physobj1,physobj2,physobj3,height1,height2,offset,time]
HoldsAt(Height(physobj1,height1),time) &
height2=height1-offset ->
Trajectory(FallingFromTo(physobj1,physobj2,physobj3),time,
           Height(physobj1,height2),offset).

; A trigger axiom states that
; if a first physical object is falling
; from a second physical object
; to a third physical object and
; the height of the first physical object
; is the same as the height of the third physical object,
; the first physical object collides with the
; third physical object:
[physobj1,physobj2,physobj3,height,time]
HoldsAt(FallingFromTo(physobj1,physobj2,physobj3),time) &
HoldsAt(Height(physobj1,height),time) &
HoldsAt(Height(physobj3,height),time) ->
Happens(CollideWith(physobj1,physobj3),time).

; An effect axiom states that
; if a first physical object is falling
; from a second physical object
; to a third physical object and
; the first physical object collides with
; the third physical object,
; the first physical object will be on the third physical object:
[physobj1,physobj2,physobj3,time]
HoldsAt(FallingFromTo(physobj1,physobj2,physobj3),time) ->
Initiates(CollideWith(physobj1,physobj3),
          On(physobj1,physobj3),
          time).

; An effect axiom states that
; if a physical object collides with another
; physical object,
; the height of the first physical object will
; be the height of the second physical object:
[physobj1,physobj2,height,time]
HoldsAt(Height(physobj2,height),time) ->
Initiates(CollideWith(physobj1,physobj2),
          Height(physobj1,height),
          time).

;[physobj1,physobj2,height1,height2,time]
;HoldsAt(Height(physobj2,height1),time) &
;height1 != height2 ->
;Terminates(CollideWith(physobj1,physobj2),
;           Height(physobj1,height2),
;           time).

; An effect axiom states that
; if a first physical object is falling
; from a second physical object
; to a third physical object and
; the first physical object collides with
; the third physical object,
; the first physical object will no longer be
; falling from the second physical object to the
; third physical object:
[physobj1,physobj2,physobj3,time]
HoldsAt(FallingFromTo(physobj1,physobj2,physobj3),time) ->
Terminates(CollideWith(physobj1,physobj3),
           FallingFromTo(physobj1,physobj2,physobj3),
           time).

; flying

; agent is flying from physobj1 to physobj2.
fluent FlyingFromTo(agent,physobj,physobj)
; agent starts flying from physobj1 to physobj2.
event StartFlyingFromTo(agent,physobj,physobj)
; agent reaches physobj.
event Reach(agent,physobj)

; An effect axiom states that if an agent starts
; flying from a physical object to another physical object,
; the agent will be flying from the first physical object
; to the second physical object:
[agent,physobj1,physobj2,time]
Initiates(StartFlyingFromTo(agent,physobj1,physobj2),
          FlyingFromTo(agent,physobj1,physobj2),
          time).

; A precondition axiom states that for
; an agent to start flying from a physical object to
; another physical object,
; the height of the agent and
; the first physical object must be the same:
[agent,physobj1,physobj2,height1,height2,time]
Happens(StartFlyingFromTo(agent,physobj1,physobj2),time) &
HoldsAt(Height(agent,height1),time) &
HoldsAt(Height(physobj1,height2),time) ->
height1=height2.

; A state constraint says that an agent
; cannot fly from and to the same physical object:
[agent,physobj1,physobj2,time]
HoldsAt(FlyingFromTo(agent,physobj1,physobj2),time) ->
physobj1!=physobj2.

; A releases axiom states that if an agent
; starts flying from a physical object to another
; physical object, the height of the agent will
; be released from inertia:
[agent,physobj1,physobj2,height,time]
Releases(StartFlyingFromTo(agent,physobj1,physobj2),
         Height(agent,height),
         time).

; A trajectory axiom states that
; if an agent starts flying from
; from a physical object
; to another physical object
; at a time and
; the agent has a height at the time,
; then the agent will have a height
; equal to the height plus an offset
; at a time equal to the time plus the offset:
[agent,physobj1,physobj2,height1,height2,offset,time]
HoldsAt(Height(agent,height1),time) &
height2=height1+offset ->
Trajectory(FlyingFromTo(agent,physobj1,physobj2),time,
           Height(agent,height2),offset).

; A trigger axiom states that
; if an agent is flying
; from a physical object
; to another physical object and
; the height of the agent
; is the same as the height of the second physical object,
; the agent reaches the second physical object:
[agent,physobj1,physobj2,height,time]
HoldsAt(FlyingFromTo(agent,physobj1,physobj2),time) &
HoldsAt(Height(agent,height),time) &
HoldsAt(Height(physobj2,height),time) ->
Happens(Reach(agent,physobj2),time).

; An effect axiom states that
; if an agent reaches a physical object,
; the height of the agent will be the
; height of the physical object:
[agent,physobj,height,time]
HoldsAt(Height(physobj,height),time) ->
Initiates(Reach(agent,physobj),Height(agent,height),time).

;[agent,physobj,height1,height2,time]
;HoldsAt(Height(physobj,height1),time) &
;height1!=height2 ->
;Terminates(Reach(agent,physobj),Height(agent,height2),time).

; An effect axiom states that
; if an agent is flying
; from a physical object
; to another physical object and
; the agent reaches the second physical object,
; the agent will no longer be
; flying from the first physical object
; to the second physical object:
[agent,physobj1,physobj2,time]
HoldsAt(FlyingFromTo(agent,physobj1,physobj2),time) ->
Terminates(Reach(agent,physobj2),
           FlyingFromTo(agent,physobj1,physobj2),
           time).

; A releases axiom states that
; if an agent holds a physical object,
; the height of the physical object is released from inertia:
[agent,physobj,height,time]
Releases(Hold(agent,physobj),Height(physobj,height),time).

;[agent,physobj,height1,height2,time]
;(!{object} PartOf(physobj,object)) &
;HoldsAt(Height(physobj,height1),time) &
;height1 != height2 ->
;Terminates(LetGoOf(agent,physobj),Height(physobj,height2),time).

[agent,physobj,height,time]
(!{object} PartOf(physobj,object)) &
HoldsAt(Height(physobj,height),time) ->
Initiates(LetGoOf(agent,physobj),Height(physobj,height),time).

; A state constraint says that
; if an agent is holding a physical object and
; the height of the agent is height,
; the height of the physical object is height:
[agent,physobj,height,time]
HoldsAt(Holding(agent,physobj),time) &
HoldsAt(Height(agent,height),time) ->
HoldsAt(Height(physobj,height),time).

; A state constraint says that if a physical object
; is part of an object,
; the height of the physical object
; is the same as the height of the object:
[physobj,object,height,time]
PartOf(physobj,object) &
HoldsAt(Height(object,height),time) ->
HoldsAt(Height(physobj,height),time).

;event Catch(agent,physobj)
;event HitFromTo(agent,physobj,object,object)
;fluent Distance(physobj,physobj,distance)
;fluent FlyingAcrossFromTo(physobj,object,object)

;[agent,physobj1,physobj2,physobj3,time]
;Initiates(HitFromTo(agent,physobj1,physobj2,physobj3),
;          FlyingAcrossFromTo(physobj1,physobj2,physobj3),
;          time).

;[agent,physobj1,physobj2,physobj3,distance,time]
;Releases(HitFromTo(agent,physobj1,physobj2,physobj3),
;         Distance(physobj1,physobj2,distance),
;         time).

;[agent,physobj1,physobj2,physobj3,distance,time]
;Releases(HitFromTo(agent,physobj1,physobj2,physobj3),
;         Distance(physobj1,physobj3,distance),
;         time).

;[physobj1,physobj2,physobj3,offset,time]
;Trajectory(FlyingAcrossFromTo(physobj1,physobj2,physobj3),time,
;           Distance(physobj1,physobj2,offset),offset).

;[physobj1,physobj2,physobj3,distance1,distance2,offset,time]
;HoldsAt(Distance(physobj2,physobj3,distance1),time) &
;distance2 = distance1 - time ->
;Trajectory(FlyingAcrossFromTo(physobj1,physobj2,physobj3),time,
;           Distance(physobj1,physobj3,distance2),offset).

;[agent,physobj1,physobj2,physobj3,time]
;HoldsAt(FlyingAcrossFromTo(physobj1,physobj2,physobj3),time) ->
;Initiates(Catch(agent,physobj1),
;          Holding(agent,physobj1),
;          time).

;[agent,physobj1,physobj2,physobj3,time]
;HoldsAt(FlyingAcrossFromTo(physobj1,physobj2,physobj3),time) ->
;Terminates(Catch(agent,physobj1),
;           FlyingAcrossFromTo(physobj1,physobj2,physobj3),
;           time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/GSpace.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; GSpace: grid space
;
; @book{Mueller:1998,
;   author = "Erik T. Mueller",
;   year = "1998",
;   title = "Natural Language Processing with \uppercase{T}hought\uppercase{T}reasure",
;   address = "New York",
;   publisher = "Signiform",
; }
;

sort coord: integer
sort grid

; object is at (coord1, coord2) in grid.
fluent GridAt(grid,object,coord,coord)

; agent walks from (coord1, coord2)
; to (coord3, coord4) in grid.
event GridWalk(grid,agent,coord,coord,coord,coord)

; A state constraint says that for a given grid an
; object is at one cell in that grid at a time:
[grid,object,coord1,coord2,coord3,coord4,time]
HoldsAt(GridAt(grid,object,coord1,coord2),time) &
HoldsAt(GridAt(grid,object,coord3,coord4),time) ->
coord1=coord3 & coord2=coord4.

; An effect axiom states that
; if an agent walks from one cell in a grid to another cell,
; the agent will be at second cell:
[grid,agent,coord1,coord2,coord3,coord4,time]
Initiates(GridWalk(grid,agent,coord1,coord2,coord3,coord4),
          GridAt(grid,agent,coord3,coord4),
          time).

; An effect axiom states that
; if an agent walks from one cell in a grid to another cell,
; the agent will no longer be at the first cell:
[grid,agent,coord1,coord2,coord3,coord4,time]
Terminates(GridWalk(grid,agent,coord1,coord2,coord3,coord4),
           GridAt(grid,agent,coord1,coord2),
           time).

; A precondition axiom states that for an agent to walk
; from one cell in a grid to another cell, the agent
; must be at the first cell, the second cell must not
; be occupied, and the first cell must be adjacent to
; the second cell:
[grid,agent,coord1,coord2,coord3,coord4,time]
Happens(GridWalk(grid,agent,coord1,coord2,coord3,coord4),time) ->
HoldsAt(GridAt(grid,agent,coord1,coord2),time) &
(!{object} HoldsAt(GridAt(grid,object,coord3,coord4),time)) &
(coord1=coord3 |
 coord1=coord3+1 |
 coord1=coord3-1) &
(coord2=coord4 |
 coord2=coord4+1 |
 coord2=coord4-1).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/PolySpace.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; @phdthesis{Cassimatis:2002,
;   author = "Nicholas L. Cassimatis",
;   year = "2002",
;   title = "Polyscheme: A Cognitive Architecture for Integrating Multiple Representation and Inference Schemes",
;   address = "Cambridge, MA",
;   school = "Program in Media Arts and Sciences, School of Architecture and Planning, Massachusetts Institute of Technology",
; }
;

; sorts
sort object
sort xcoord: integer
sort ycoord: integer
sort grid
sort shape
sort color

; constants
shape Round,Square
color Red,Green

; predicates, fluents, and events
predicate Equal(object,object)
predicate Shape(object,shape)
predicate Color(object,color)
fluent Location(grid,object,xcoord,ycoord)
event Move(grid,object,xcoord,ycoord,xcoord,ycoord)

; axioms

[object1,object2] Equal(object1,object2) -> Equal(object2,object1).

; objects have unique shape
[object,shape1,shape2]
Shape(object,shape1) & Shape(object,shape2) ->
shape1=shape2.

; objects have unique color
[object,color1,color2]
Color(object,color1) & Color(object,color2) ->
color1=color2.

; if objects are the same, they have the same shape
[object1,object2]
Equal(object1,object2) ->
({shape} Shape(object1,shape) & Shape(object2,shape)).

; if objects are the same, they have the same color
[object1,object2]
Equal(object1,object2) ->
({color} Color(object1,color) & Color(object2,color)).

; if objects are the same, they have the same location
[grid,object1,object2,xcoord1,ycoord1,xcoord2,ycoord2,time]
Equal(object1,object2) ->
(HoldsAt(Location(grid,object1,xcoord1,ycoord1),time) &
 HoldsAt(Location(grid,object2,xcoord2,ycoord2),time) ->
 xcoord1=xcoord2 & ycoord1=ycoord2).

; object in one location at a time
[grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]
HoldsAt(Location(grid,object,xcoord1,ycoord1),time) &
HoldsAt(Location(grid,object,xcoord2,ycoord2),time) ->
xcoord1=xcoord2 & ycoord1=ycoord2.

; objects have locations
[grid,object,time]
({xcoord,ycoord} HoldsAt(Location(grid,object,xcoord,ycoord),time)).

; different objects are not at same location
[grid,object1,object2,xcoord1,ycoord1,time]
HoldsAt(Location(grid,object1,xcoord1,ycoord1),time) &
HoldsAt(Location(grid,object2,xcoord1,ycoord1),time) ->
Equal(object1,object2).

; moving to a location causes an object to be at that location
[grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]
Initiates(Move(grid,object,xcoord1,ycoord1,xcoord2,ycoord2),
          Location(grid,object,xcoord2,ycoord2),
          time).

; moving to a location causes the object no longer to be at its previous
; location
[grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]
Terminates(Move(grid,object,xcoord1,ycoord1,xcoord2,ycoord2),
           Location(grid,object,xcoord1,ycoord1),
           time).

;; allow diagonal movements
;[grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]
;Happens(Move(grid,object,xcoord1,ycoord1,xcoord2,ycoord2),time) ->
;HoldsAt(Location(grid,object,xcoord1,ycoord1),time) &
;(xcoord1=xcoord2 |
; xcoord1=xcoord2+1 |
; xcoord1=xcoord2-1) &
;(ycoord1=ycoord2 |
; ycoord1=ycoord2+1 |
; ycoord1=ycoord2-1).

; only allow right angle movements
[grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]
Happens(Move(grid,object,xcoord1,ycoord1,xcoord2,ycoord2),time) ->
HoldsAt(Location(grid,object,xcoord1,ycoord1),time) &
((xcoord1=xcoord2 & (ycoord1=ycoord2+1 | ycoord1=ycoord2-1)) |
 (ycoord1=ycoord2 & (xcoord1=xcoord2+1 | xcoord1=xcoord2-1))).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/HandTo.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

event HandTo(agent,agent,physobj)

[agent1,agent2,physobj,time]
Initiates(HandTo(agent1,agent2,physobj),
          Holding(agent2,physobj),
          time).

[agent1,agent2,physobj,time]
Terminates(HandTo(agent1,agent2,physobj),
           Holding(agent1,physobj),
           time).

[agent1,agent2,physobj,time]
Happens(HandTo(agent1,agent2,physobj),time) ->
HoldsAt(Holding(agent1,physobj),time).

event ShakeHands(agent,agent)

event WriteOn(agent,paper,pen)



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Container.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;
; Container: container
;

; linkage to OTSpace(M):
[agent,container1,container2,time]
Happens(TakeOutOf(agent,container1,container2),time) ->
HoldsAt(ContainerIsOpen(container2),time).

[agent,container1,container2,time]
Happens(PutInside(agent,container1,container2),time) ->
HoldsAt(ContainerIsOpen(container2),time).

; agent opens container.
event ContainerOpen(agent,container)

; agent closes container.
event ContainerClose(agent,container)

; container is open.
fluent ContainerIsOpen(container)

fluent ContainerClosed(container)
noninertial ContainerClosed

[container,time]
HoldsAt(ContainerClosed(container),time) <->
!HoldsAt(ContainerIsOpen(container),time).

; A precondition axiom states that
; for an agent to open a container,
; the agent must be awake,
; the container must not already be open, and
; the agent must be holding the container.
[agent,container,time]
Happens(ContainerOpen(agent,container),time) ->
HoldsAt(Awake(agent),time) &
!HoldsAt(ContainerIsOpen(container),time) &
HoldsAt(Holding(agent,container),time).

; An effect axiom states that
; if an agent opens a container,
; the container will be open:
[agent,container,time]
Initiates(ContainerOpen(agent,container),ContainerIsOpen(container),time).

; A precondition axiom states that
; for an agent to close a container,
; the agent must be awake,
; the container must be open, and
; the agent must be holding the container.
[agent,container,time]
Happens(ContainerClose(agent,container),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(ContainerIsOpen(container),time) &
HoldsAt(Holding(agent,container),time).

; An effect axiom states that
; if an agent closes a container,
; the container will no longer be open:
[agent,container,time]
Terminates(ContainerClose(agent,container),ContainerIsOpen(container),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/SpeechAct.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; The SpeechAct representation deals with a few speech acts
; \fullcite{Searle:1969}.
;
; @book{Searle:1969,
;   author = "John R. Searle",
;   year = "1969",
;   title = "Speech Acts: An Essay in the Philosophy of Language",
;   address = "Cambridge",
;   publisher = "Cambridge University Press",
; }
;
; We handle
; the illocutionary acts of
; inviting someone into one's house (a form of request) and
; greeting someone,
; and the expressive speech act of crying for joy.
;

; inviting in

; agent1 invites agent2 into room.
event InviteIn(agent,agent,room)
; agent1 is invited into room by agent2.
fluent InvitedIn(agent,room,agent)

; A precondition axiom states that for
; an agent to invite another agent into a room,
; the first agent must be in the room and
; there must be an outside area such that
; the second agent is at the outside area and
; the outside area is adjacent to the room:
[agent1,agent2,room,time]
Happens(InviteIn(agent1,agent2,room),time) ->
HoldsAt(At(agent1,room),time) &
{outside}
HoldsAt(At(agent2,outside),time) &
Adjacent(room,outside).

; An effect axiom states that if
; an agent invites another agent into a room,
; the second agent will be invited into the room by the first agent:
[agent1,agent2,room,time]
Initiates(InviteIn(agent1,agent2,room),
          InvitedIn(agent2,room,agent1),
          time).

; agent intends to walk into room.
event IntendToWalkIn(agent,room)
; agent has the intention to walk into room.
fluent IntentionToWalkIn(agent,room)
; agent acts on the intention to walk into room.
fluent ActOnIntentionToWalkIn(agent,room)
noninertial ActOnIntentionToWalkIn

; A trigger axiom states that
; if an agent is invited into a room by another agent,
; the first agent likes the second agent, and
; the first agent does not already have the intention to
; walk into the room,
; the first agent intends to walk into the room:
[agent1,agent2,room,time]
HoldsAt(InvitedIn(agent1,room,agent2),time) &
HoldsAt(Like(agent1,agent2),time) &
!HoldsAt(IntentionToWalkIn(agent1,room),time) ->
Happens(IntendToWalkIn(agent1,room),time).

; An effect axiom states that
; if an agent intends to walk into a room,
; the agent will have the intention to walk into the room:
[agent,room,time]
Initiates(IntendToWalkIn(agent,room),
          IntentionToWalkIn(agent,room),
          time).

; Two trigger axioms state that
; if an agent has the intention to walk into a room,
; the agent acts on the intention to walk into the room,
; the agent is at a location,
; side one (two) of a door is the room,
; side two (one) of the door is the location,
; agent will walk through side two (one) of the door:
[agent,room,location,door,time]
HoldsAt(IntentionToWalkIn(agent,room),time) &
HoldsAt(ActOnIntentionToWalkIn(agent,room),time) &
HoldsAt(At(agent,location),time) &
Side1(door)=room &
Side2(door)=location ->
Happens(WalkThroughDoor21(agent,door),time).

[agent,room,location,door,time]
HoldsAt(IntentionToWalkIn(agent,room),time) &
HoldsAt(ActOnIntentionToWalkIn(agent,room),time) &
HoldsAt(At(agent,location),time) &
Side2(door)=room &
Side1(door)=location ->
Happens(WalkThroughDoor12(agent,door),time).

; Two effect axioms state that
; if side one (two) of a door is a room and
; an agent walks through side two (one) of the door,
; the agent will no longer have the intention to
; walk into the room:
[agent,room,door,time]
Side1(door)=room ->
Terminates(WalkThroughDoor21(agent,door),
           IntentionToWalkIn(agent,room),
           time).

[agent,room,door,time]
Side2(door)=room ->
Terminates(WalkThroughDoor12(agent,door),
           IntentionToWalkIn(agent,room),
           time).

; agent greets object.
event Greet(agent,object)

event SayPleasedToMeet(agent,agent)

; agent says goodbye to object.
event SayGoodbye(agent,object)

event TalkAbout(agent,content)

event Converse(agent,agent)

[agent1,agent2,time]
Happens(Converse(agent1,agent2),time) ->
{location}
HoldsAt(At(agent1,location),time) &
HoldsAt(At(agent2,location),time).

; A precondition axiom states that for
; an agent to greet an object,
; there must be a location such that
; the agent is at the location and
; the object is at the location:
[agent,object,time]
Happens(Greet(agent,object),time) ->
{location}
HoldsAt(At(agent,location),time) &
HoldsAt(At(object,location),time).

[agent,object,time]
Happens(SayGoodbye(agent,object),time) ->
{location}
HoldsAt(At(agent,location),time) &
HoldsAt(At(object,location),time).

; speech: expression of emotions

; agent cries for joy.
event CryForJoy(agent)

; A precondition axiom states that for
; an agent to cry for joy,
; the agent must be happy:
[agent,time]
Happens(CryForJoy(agent),time) ->
HoldsAt(Happy(agent),time).

event Threaten(agent,agent,weapon)

event ReleaseFromThreat(agent,agent)

fluent ThreatenedBy(agent,agent)

[agent1,agent2,weapon,time]
Happens(Threaten(agent1,agent2,weapon), time) ->
HoldsAt(Holding(agent1,weapon),time) &
{location}
HoldsAt(At(agent1,location),time) &
HoldsAt(At(agent2,location),time).

[agent1,agent2,weapon,time]
Happens(Threaten(agent1,agent2,weapon), time) ->
Happens(BecomeAngryAt(agent2,agent1),time).

[agent1,agent2,weapon,time]
Initiates(Threaten(agent1,agent2,weapon),
          ThreatenedBy(agent2,agent1),
          time).

[agent1,agent2,time]
Terminates(ReleaseFromThreat(agent1,agent2),
           ThreatenedBy(agent2,agent1),
           time).

event Order(agent,agent,physobj)

fluent KnowOrder(agent,agent,physobj)

[agent1,agent2,physobj,time]
Initiates(Order(agent1,agent2,physobj),
          KnowOrder(agent2,agent1,physobj),
          time).

[agent1,agent2,physobj,time]
Happens(Order(agent1,agent2,physobj),time) ->
{location}
HoldsAt(At(agent1,location),time) &
HoldsAt(At(agent2,location),time).

event Request(agent,agent,physobj)

fluent KnowRequest(agent,agent,physobj)

[agent1,agent2,physobj,time]
Initiates(Request(agent1,agent2,physobj),
          KnowRequest(agent2,agent1,physobj),
          time).

[agent1,agent2,physobj,time]
Happens(Request(agent1,agent2,physobj),time) ->
{location}
HoldsAt(At(agent1,location),time) &
HoldsAt(At(agent2,location),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Sleep.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; The Sleep representation deals with the activity of sleeping and
; body posture.
; It is similar to the finite automaton representation of sleep
; used in ThoughtTreasure \fullcite[chap. 7]{Mueller:1998}.
;
; @book{Mueller:1998,
;   author = "Erik T. Mueller",
;   year = "1998",
;   title = "Natural Language Processing with \uppercase{T}hought\uppercase{T}reasure",
;   address = "New York",
;   publisher = "Signiform",
; }
;

; sleep

; agent wakes up.
event WakeUp(agent)

; agent gets tired.
event GetTired(agent)

; agent falls asleep.
event FallAsleep(agent)

; agent is asleep.
fluent Sleep0(agent)
; agent is awake and in bed.
fluent Sleep1(agent)
; agent is awake, out of bed, and undressed.
fluent Sleep2(agent)
; agent is awake and dressed.
fluent Sleep3(agent)
; agent is tired and dressed.
fluent Sleep4(agent)
; agent is tired and undressed.
fluent Sleep5(agent)
; agent is in bed, waiting to fall asleep.
fluent Sleep6(agent)

; At any time, an agent is in one of seven sleep states:
xor Sleep0, Sleep1, Sleep2, Sleep3, Sleep4, Sleep5, Sleep6

; constraints

; agent is asleep.
fluent Asleep(agent)
; agent is awake.
fluent Awake(agent)
noninertial Asleep
noninertial Awake

; Sleep0 indicates that the agent is asleep:
[agent,time] HoldsAt(Asleep(agent),time) <-> HoldsAt(Sleep0(agent),time).

; In all other sleep states, the agent is awake:
[agent,time]
HoldsAt(Awake(agent),time) <->
HoldsAt(Sleep1(agent),time) |
HoldsAt(Sleep2(agent),time) |
HoldsAt(Sleep3(agent),time) |
HoldsAt(Sleep4(agent),time) |
HoldsAt(Sleep5(agent),time) |
HoldsAt(Sleep6(agent),time).

; A number of axioms are used to specify the transitions of
; a finite automaton.
;--

; Waking up causes a transition from Sleep0
; to Sleep1:
[agent,time] Terminates(WakeUp(agent),Sleep0(agent),time).

[agent,time] Initiates(WakeUp(agent),Sleep1(agent),time).

[agent,time] Happens(WakeUp(agent),time) -> HoldsAt(Sleep0(agent),time).

;--

; Getting out of bed causes a transition from Sleep1
; to Sleep2:
[agent,bed,time] Terminates(RiseFrom(agent,bed),Sleep1(agent),time).

[agent,bed,time] Initiates(RiseFrom(agent,bed),Sleep2(agent),time).

[agent,bed,time]
Happens(RiseFrom(agent,bed),time) -> HoldsAt(Sleep1(agent),time).

;--

; Getting dressed causes a transition from Sleep2
; to Sleep3, the normal state of awakeness:
[agent,time] Terminates(GetDressed(agent),Sleep2(agent),time).

[agent,time] Initiates(GetDressed(agent),Sleep3(agent),time).

[agent,time] Happens(GetDressed(agent),time) -> HoldsAt(Sleep2(agent),time).

;--

; Getting tired causes a transition from Sleep3
; to Sleep4:
[agent,time] Terminates(GetTired(agent),Sleep3(agent),time).

[agent,time] Initiates(GetTired(agent),Sleep4(agent),time).

[agent,time] Happens(GetTired(agent),time) -> HoldsAt(Sleep3(agent),time).

;--

; Getting undressed causes a transition from Sleep4
; to Sleep5:
[agent,time] Terminates(GetUndressed(agent),Sleep4(agent),time).

[agent,time] Initiates(GetUndressed(agent),Sleep5(agent),time).

[agent,time] Happens(GetUndressed(agent),time) -> HoldsAt(Sleep4(agent),time).

;--

; Lying on a bed causes a transition from Sleep5
; to Sleep6:
[agent,bed,time] Terminates(LieOn(agent,bed),Sleep5(agent),time).

[agent,bed,time] Initiates(LieOn(agent,bed),Sleep6(agent),time).

[agent,bed,time] Happens(LieOn(agent,bed),time) -> HoldsAt(Sleep5(agent),time).

;--

; Falling asleep causes a transition from Sleep6
; to Sleep0:
[agent,time] Terminates(FallAsleep(agent),Sleep6(agent),time).

[agent,time] Initiates(FallAsleep(agent),Sleep0(agent),time).

[agent,time] Happens(FallAsleep(agent),time) -> HoldsAt(Sleep6(agent),time).

;--

; agent acts on being in state Sleep5.
fluent ActOnSleep5(agent)
noninertial ActOnSleep5

; We reduce the number of models by asserting that
; an agent only acts on being in state Sleep5 while in
; that state:
[agent,time]
!HoldsAt(Sleep5(agent),time) ->
!HoldsAt(ActOnSleep5(agent),time).

; Undressed is like IntentionToPlay
; ActOnSleep5 is like ActOnIntentionToPlay

; A trigger axiom states that if an agent is in state Sleep5,
; the agent acts on this state, the agent is in a room, and
; a bed is at the room, the agent lies on the bed:
[agent,room,bed,time]
HoldsAt(Sleep5(agent),time) &
HoldsAt(ActOnSleep5(agent),time) &
HoldsAt(At(agent,room),time) &
HoldsAt(At(bed,room),time) ->
Happens(LieOn(agent,bed),time).

; A precondition axiom states that for
; an agent to lie on a bed,
; the agent must be in state Sleep5,
; the agent must act on this state, and
; there must be a room such that
; the agent is in the room and the bed is in the room:
[agent,bed,time]
Happens(LieOn(agent,bed),time) ->
HoldsAt(Sleep5(agent),time) &
HoldsAt(ActOnSleep5(agent),time) &
{room}
 HoldsAt(At(agent,room),time) &
 HoldsAt(At(bed,room),time).

; (body) posture

; agent lies on physobj.
event LieOn(agent,physobj)

; agent sits on physobj.
event SitOn(agent,physobj)

[agent,physobj,time]
Happens(SitOn(agent,physobj),time) ->
{location}
 HoldsAt(At(agent,location),time) &
 HoldsAt(At(physobj,location),time).

; agent rises from physobj.
event RiseFrom(agent,physobj)

; agent is lying on physobj.
fluent LyingOn(agent,physobj)
; agent is sitting on physobj.
fluent SittingOn(agent,physobj)
; agent is standing.
fluent Standing(agent)

; agent is lying down.
fluent Lying(agent)
; agent is sitting.
fluent Sitting(agent)
noninertial Lying
noninertial Sitting

; At any time, an agent is either lying, sitting, or standing:
xor Lying, Sitting, Standing

[agent,physobj,time]
HoldsAt(LyingOn(agent,physobj),time) ->
HoldsAt(Lying(agent),time).

[agent,physobj,time]
HoldsAt(SittingOn(agent,physobj),time) ->
HoldsAt(Sitting(agent),time).

; State constraints represent that an agent can lie or sit
; on at most one object at a time:
[agent,physobj1,physobj2,time]
HoldsAt(LyingOn(agent,physobj1),time) &
HoldsAt(LyingOn(agent,physobj2),time) ->
physobj1=physobj2.

[agent,physobj1,physobj2,time]
HoldsAt(SittingOn(agent,physobj1),time) &
HoldsAt(SittingOn(agent,physobj2),time) ->
physobj1=physobj2.

; An effect axiom states that if an agent is standing and
; lies on a physical object, the agent will be lying on
; the physical object:
[agent,physobj,time]
HoldsAt(Standing(agent),time) ->
Initiates(LieOn(agent,physobj),
          LyingOn(agent,physobj),
          time).

; An effect axiom states that if an agent
; lies on a physical object, the agent will no longer
; be standing:
[agent,physobj,time]
Terminates(LieOn(agent,physobj),
           Standing(agent),
           time).

; An effect axiom states that if an agent is standing and
; sits on a physical object, the agent will be sitting on
; the physical object:
[agent,physobj,time]
HoldsAt(Standing(agent),time) ->
Initiates(SitOn(agent,physobj),
          SittingOn(agent,physobj),
          time).

; An effect axiom states that if an agent
; sits on a physical object, the agent will no longer
; be standing:
[agent,physobj,time]
Terminates(SitOn(agent,physobj),
           Standing(agent),
           time).

; An effect axiom states that if an agent
; is sitting or lying on a physical object and
; the agent rises from the physical object,
; the agent will be standing:
[agent,physobj,time]
(HoldsAt(SittingOn(agent,physobj),time) |
 HoldsAt(LyingOn(agent,physobj),time)) ->
Initiates(RiseFrom(agent,physobj),
          Standing(agent),
          time).

; An effect axiom states that if an agent is sitting on
; a physical object and the agent rises from the physical
; object, the agent will no longer be sitting on the
; physical object:
[agent,physobj,time]
HoldsAt(SittingOn(agent,physobj),time) ->
Terminates(RiseFrom(agent,physobj),
           SittingOn(agent,physobj),
           time).

; An effect axiom states that if an agent is lying on
; a physical object and the agent rises from the physical
; object, the agent will no longer be lying on the
; physical object:
[agent,physobj,time]
HoldsAt(LyingOn(agent,physobj),time) ->
Terminates(RiseFrom(agent,physobj),
           LyingOn(agent,physobj),
           time).

; dressing

; agent gets undressed.
event GetDressed(agent)
; agent gets dressed.
event GetUndressed(agent)
; agent is dressed.
fluent Dressed(agent)

; Effect axioms deal with getting dressed and undressed:
[agent,time] Initiates(GetDressed(agent),Dressed(agent),time).
[agent,time] Terminates(GetUndressed(agent),Dressed(agent),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Sleeping.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; @article{Mueller:2004c,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Understanding script-based stories using commonsense reasoning",
;   journal = "Cognitive Systems Research",
;   volume = "5",
;   number = "4",
;   pages = "307--340",
; }
;

option modeldiff on

ignore Love, ThreatenedBy
ignore LookOutOnto, Floor, BuildingOf, SkyOf, GroundOf
ignore Inside, Near
ignore See

ignore ActOnSleep5

option renaming off

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/Cognition.e
load answers/Mueller2003/Sleep.e

door Door1

room Room0

room Room1

Side1(Door1)=Room0.
Side2(Door1)=Room1.

agent Sleeper1

bed Bed1

outside Outside1

; initial state
[agent,object] !HoldsAt(Holding(agent,object),0).
[agent,physobj] !HoldsAt(SittingOn(agent,physobj),0).
[agent,physobj] !HoldsAt(LyingOn(agent,physobj),0).
HoldsAt(Dressed(Sleeper1),0).
HoldsAt(Awake(Sleeper1),0).
HoldsAt(Sleep3(Sleeper1),0).
HoldsAt(Standing(Sleeper1),0).
HoldsAt(DoorUnlocked(Door1),0).
HoldsAt(DoorIsOpen(Door1),0).
HoldsAt(At(Sleeper1,Room0),0).
HoldsAt(At(Bed1,Room1),0).

; narrative
Happens(GetTired(Sleeper1),0).
Happens(WalkThroughDoor12(Sleeper1,Door1),1).
Happens(GetUndressed(Sleeper1),2).
Happens(LieOn(Sleeper1,Bed1),3).
Happens(FallAsleep(Sleeper1),4).
Happens(Dream(Sleeper1),5).
Happens(WakeUp(Sleeper1),6).
Happens(RiseFrom(Sleeper1,Bed1),7).
Happens(GetDressed(Sleeper1),8).
Happens(WalkThroughDoor21(Sleeper1,Door1),9).

range time 0 10
range offset 0 0
range diameter 0 0

completion Happens

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Rest.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; @article{Mueller:InPress,
;   author = "Erik T. Mueller",
;   year = "in press",
;   title = "Modelling space and time in narratives about restaurants",
;   journal = "Literary and Linguistic Computing",
; }
;

option renaming off
option encoding 3

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/MuellerInPress/RepRest.e

door MainEntrance1

; room-scale topological space
outside Street1
room DiningRoom1
door KitchenDoor1
room Kitchen1
Side1(MainEntrance1)=Street1.
Side2(MainEntrance1)=DiningRoom1.
Side1(KitchenDoor1)=DiningRoom1.
Side2(KitchenDoor1)=Kitchen1.

agent Customer1
menu Menu1
chair Chair1
food Food1
HoldsAt(At(Customer1,Street1),0).
HoldsAt(Hungry(Customer1),0).
HoldsAt(At(Chair1,DiningRoom1),0).
HoldsAt(At(Menu1,DiningRoom1),0).
HoldsAt(On(Menu1,Table1),0).
HoldsAt(At(Food1,Kitchen1),0).

waiter Waiter1
cook Cook1

; props
table Table1
bill Bill1

; restaurant
restaurant Restaurant1
CookOf(Restaurant1)=Cook1.
TableOf(Restaurant1)=Table1.
WaiterOf(Restaurant1)=Waiter1.
KitchenDoorOf(Restaurant1)=KitchenDoor1.
BillOf(Restaurant1)=Bill1.

; prune
sort ona, onb
fluent! On(ona,onb)
event! PlaceOn(agent,ona,onb)
event! TakeOffOf(agent,ona,onb)

sort ordera, orderb, orderc
event! Order(ordera,orderb,orderc)
fluent! KnowOrder(orderb,ordera,orderc)

sort requesta, requestb, requestc
event! Request(requesta,requestb,requestc)
fluent! KnowRequest(requestb,requesta,requestc)

sort holda, holdb, holdc
event! TakeOffOf(holda,holdb,holdc)
event! PickUp(holda,holdb)
event! LetGoOf(holda,holdb)
event! Hold(holda,holdb)
fluent! Holding(holda,holdb)

sort sita, sitb
event! LieOn(sita,sitb)
event! SitOn(sita,sitb)
event! RiseFrom(sita,sitb)
fluent! LyingOn(sita,sitb)
fluent! SittingOn(sita,sitb)

sort greeta, greetb
event! Greet(greeta,greetb)

ona! Menu1, Food1, Bill1
onb! Table1
ordera! Customer1, Waiter1
orderb! Waiter1, Cook1
orderc! Food1
requesta! Customer1
requestb! Waiter1
requestc! Bill1
holda! Customer1, Waiter1
holdb! Menu1, Food1, Bill1
holdc! Table1
sita! Customer1
sitb! Chair1
greeta! Customer1, Waiter1
greetb! Customer1, Waiter1

; initial situation
HoldsAt(At(Waiter1,DiningRoom1),0).
HoldsAt(At(Cook1,Kitchen1),0).
HoldsAt(At(Table1,DiningRoom1),0).
!HoldsAt(On(Bill1,Table1),0).
HoldsAt(At(Bill1,DiningRoom1),0).
[agent] HoldsAt(Standing(agent),0).
[agent,object] !HoldsAt(Holding(agent,object),0).
[agent1,agent2,physobj] !HoldsAt(KnowOrder(agent1,agent2,physobj),0).
[agent1,agent2,physobj] !HoldsAt(KnowRequest(agent1,agent2,physobj),0).
HoldsAt(BeWaiter0(Waiter1),0).
HoldsAt(BeCook0(Cook1),0).
[food] !HoldsAt(FoodPrepared(food),0).
!HoldsAt(Hungry(Cook1),0).
!HoldsAt(Hungry(Waiter1),0).

Happens(WalkThroughDoor12(Customer1,MainEntrance1),0).
Happens(Greet(Waiter1,Customer1),1).
Happens(SitOn(Customer1,Chair1),2).
Happens(TakeOffOf(Customer1,Menu1,Table1),3).
Happens(Order(Customer1,Waiter1,Food1),4).
Happens(PlaceOn(Customer1,Menu1,Table1),5).
Happens(Eat(Customer1,Food1),11).
Happens(Request(Customer1,Waiter1,Bill1),12).
Happens(Pay(Customer1,Waiter1),15).
Happens(Tip(Customer1,Waiter1),15).
Happens(RiseFrom(Customer1,Chair1),16).
Happens(SayGoodbye(Customer1,Waiter1),17).
Happens(WalkThroughDoor21(Customer1,MainEntrance1),18).

range time 0 19
range offset 0 0
range diameter 0 0

completion Happens

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/RepRest.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; @article{Mueller:InPress,
;   author = "Erik T. Mueller",
;   year = "in press",
;   title = "Modelling space and time in narratives about restaurants",
;   journal = "Literary and Linguistic Computing",
; }
;

;sort boolean
;sort integer
;reified sort predicate
;reified sort function
;
;sort time: integer
;sort offset: integer
;
;reified sort fluent
;reified sort event
;
;predicate Happens(event,time)
;predicate HoldsAt(fluent,time)
;predicate ReleasedAt(fluent,time)
;predicate Initiates(event,fluent,time)
;predicate Terminates(event,fluent,time)
;predicate Releases(event,fluent,time)
;
;sort diameter: integer
;
;sort object
;
;sort agent: object
;
;sort physobj: object
;sort bed: physobj
;sort snowflake: physobj
;sort sky: physobj
;
;sort stuff: physobj
;
;sort surface: physobj
;sort ground: surface
;
;sort snow: stuff
;sort ball
;
;sort food: physobj
;sort fruit: food
;sort orange: fruit
;sort salad: food
;
;sort clothing: physobj
;sort scarf: clothing
;sort hat: clothing
;
;sort vegetablematter: physobj
;sort coal: vegetablematter
;
;sort bodypart: physobj
;sort hand: bodypart
;
;sort papertowels: physobj
;sort device: physobj
;sort electronicdevice: device
;sort lamp: electronicdevice
;
;sort cat: physobj
;
;sort weapon: physobj
;sort gun: weapon
;sort bomb: weapon
;sort bullet: weapon
;
;sort location
;sort room: location, outside: location
;
;sort portal
;sort door: portal, staircase: portal
;sort street: portal
;
;sort building
;
;sort fire: object
;
;sort furniture: physobj
;sort chair: furniture
;sort table: furniture
;
;sort menu: physobj
;sort bill: physobj
;
;sort script
;
fluent Holding(agent,physobj)
event PickUp(agent,physobj)
event LetGoOf(agent,physobj)

[agent,physobj,time]
Initiates(PickUp(agent,physobj),Holding(agent,physobj),time).

[agent,physobj,time]
Happens(PickUp(agent,physobj),time) ->
{location}
  HoldsAt(At(agent,location),time) &
  HoldsAt(At(physobj,location),time).

[agent,physobj,time]
Terminates(LetGoOf(agent,physobj),Holding(agent,physobj),time).

[agent,physobj,time]
Happens(LetGoOf(agent,physobj),time) ->
HoldsAt(Holding(agent,physobj),time).

[agent,physobj,location,time]
Releases(PickUp(agent,physobj),At(physobj,location),time).

[agent,physobj,location,time]
HoldsAt(Holding(agent,physobj),time) &
HoldsAt(At(agent,location),time) ->
HoldsAt(At(physobj,location),time).

;[agent,physobj,location1,location2,time]
;HoldsAt(At(agent,location1),time) &
;location1!=location2 ->
;Terminates(LetGoOf(agent,physobj),At(physobj,location2),time).

[agent,physobj,location,time]
HoldsAt(At(agent,location),time) ->
Initiates(LetGoOf(agent,physobj),At(physobj,location),time).

fluent On(physobj,physobj)

event PlaceOn(agent,physobj,physobj)

event TakeOffOf(agent,physobj,physobj)

[physobj1,physobj2,time]
HoldsAt(On(physobj1,physobj2),time) ->
physobj1!=physobj2.

[physobj1,physobj2,time]
HoldsAt(On(physobj1,physobj2),time) ->
!HoldsAt(On(physobj2,physobj1),time).

[agent,physobj1,physobj2,time]
Initiates(PlaceOn(agent,physobj1,physobj2),
          On(physobj1,physobj2),time).

[agent,physobj1,physobj2,time]
Terminates(PlaceOn(agent,physobj1,physobj2),
           Holding(agent,physobj1),time).

[agent,physobj1,physobj2,time]
Happens(PlaceOn(agent,physobj1,physobj2),time) ->
HoldsAt(Holding(agent,physobj1),time) &
{location}
 HoldsAt(At(agent,location),time) &
 HoldsAt(At(physobj2,location),time).

[agent,physobj1,physobj2,time]
Terminates(TakeOffOf(agent,physobj1,physobj2),
           On(physobj1,physobj2),time).

[agent,physobj1,physobj2,time]
Initiates(TakeOffOf(agent,physobj1,physobj2),
          Holding(agent,physobj1),time).

[agent,physobj1,physobj2,location,time]
Releases(TakeOffOf(agent,physobj1,physobj2),
         At(physobj1,location),
         time).

[agent,physobj1,physobj2,time]
Happens(TakeOffOf(agent,physobj1,physobj2),time) ->
HoldsAt(On(physobj1,physobj2),time) &
{location}
 HoldsAt(At(agent,location),time) &
 HoldsAt(At(physobj1,location),time) &
 HoldsAt(At(physobj2,location),time).

[agent,physobj1,physobj2,location,time]
Releases(PlaceOn(agent,physobj1,physobj2),
         At(physobj1,location),
         time).

[physobj1,physobj2,location,time]
HoldsAt(On(physobj1,physobj2),time) &
HoldsAt(At(physobj2,location),time) ->
HoldsAt(At(physobj1,location),time).

fluent At(object,location)

[object,time]
{location} HoldsAt(At(object,location),time).

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
HoldsAt(At(object,location2),time) ->
location1=location2.

function Side1(portal): location
function Side2(portal): location

fluent NearPortal(object,portal)
noninertial NearPortal

[object,portal,time]
HoldsAt(NearPortal(object,portal),time) <->
{location}
 (Side1(portal)=location|
  Side2(portal)=location) &
 HoldsAt(At(object,location),time).

event WalkThroughDoor12(agent,door)
event WalkThroughDoor21(agent,door)

[agent,door,time]
Happens(WalkThroughDoor12(agent,door),time) ->
HoldsAt(Standing(agent),time) &
HoldsAt(At(agent,Side1(door)),time).

[agent,door,time]
Happens(WalkThroughDoor21(agent,door),time) ->
HoldsAt(Standing(agent),time) &
HoldsAt(At(agent,Side2(door)),time).

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

fluent Hungry(agent)

fluent Satiated(agent)
noninertial Satiated

[agent,time] HoldsAt(Hungry(agent),time) <-> !HoldsAt(Satiated(agent),time).

event Eat(agent,food)

[agent,food,time]
Happens(Eat(agent,food),time) ->
{location}
HoldsAt(At(agent,location),time) &
HoldsAt(At(food,location),time).

[agent,food,time]
Terminates(Eat(agent,food),Hungry(agent),time).

sort restaurant: script
sort waiter: agent
sort cook: agent

function BillOf(restaurant): bill
function CookOf(restaurant): cook
function TableOf(restaurant): table
function WaiterOf(restaurant): waiter
function KitchenDoorOf(restaurant): door

fluent BeWaiter0(waiter)

fluent BeWaiter1(waiter)

fluent BeWaiter2(waiter)

fluent BeWaiter3(waiter)

fluent BeWaiter4(waiter)

fluent BeWaiter5(waiter)

fluent BeWaiter6(waiter)

fluent BeWaiter7(waiter)

fluent BeWaiter8(waiter)

fluent BeWaiter9(waiter)

xor BeWaiter0, BeWaiter1, BeWaiter2, BeWaiter3, BeWaiter4, BeWaiter5, BeWaiter6, BeWaiter7, BeWaiter8, BeWaiter9

[waiter,agent,time]
HoldsAt(BeWaiter0(waiter),time) ->
Terminates(Greet(waiter,agent),
           BeWaiter0(waiter),
           time).

[waiter,agent,time]
HoldsAt(BeWaiter0(waiter),time) ->
Initiates(Greet(waiter,agent),
          BeWaiter1(waiter),
          time).

[waiter,agent,food,time]
HoldsAt(BeWaiter1(waiter),time) ->
Terminates(Order(agent,waiter,food),
           BeWaiter1(waiter),
           time).

[waiter,agent,food,time]
HoldsAt(BeWaiter1(waiter),time) ->
Initiates(Order(agent,waiter,food),
          BeWaiter2(waiter),
          time).

[restaurant,waiter,time]
WaiterOf(restaurant)=waiter &
HoldsAt(BeWaiter2(waiter),time) ->
Happens(WalkThroughDoor12(waiter,KitchenDoorOf(restaurant)),time).

[restaurant,waiter,door,time]
HoldsAt(BeWaiter2(waiter),time) &
WaiterOf(restaurant)=waiter &
KitchenDoorOf(restaurant)=door ->
Terminates(WalkThroughDoor12(waiter,door),
           BeWaiter2(waiter),
           time).

[restaurant,waiter,door,time]
HoldsAt(BeWaiter2(waiter),time) &
WaiterOf(restaurant)=waiter &
KitchenDoorOf(restaurant)=door ->
Initiates(WalkThroughDoor12(waiter,door),
          BeWaiter3(waiter),
          time).

[restaurant,food,time]
HoldsAt(BeWaiter3(WaiterOf(restaurant)),time) &
({agent} HoldsAt(KnowOrder(WaiterOf(restaurant),agent,food),time)) ->
Happens(Order(WaiterOf(restaurant),CookOf(restaurant),food),time).

[restaurant,waiter,cook,food,time]
WaiterOf(restaurant)=waiter &
CookOf(restaurant)=cook &
HoldsAt(BeWaiter3(waiter),time) ->
Terminates(Order(waiter,cook,food),
           BeWaiter3(waiter),
           time).

[restaurant,waiter,cook,food,time]
WaiterOf(restaurant)=waiter &
CookOf(restaurant)=cook &
HoldsAt(BeWaiter3(waiter),time) ->
Initiates(Order(waiter,cook,food),
          BeWaiter4(waiter),
          time).

[waiter,food,time]
HoldsAt(BeWaiter4(waiter),time) &
({agent} HoldsAt(KnowOrder(waiter,agent,food),time)) &
HoldsAt(FoodPrepared(food),time) ->
Happens(PickUp(waiter,food),time).

[waiter,food,time]
HoldsAt(BeWaiter4(waiter),time) &
({agent} HoldsAt(KnowOrder(waiter,agent,food),time)) ->
Terminates(PickUp(waiter,food),
           BeWaiter4(waiter),
           time).

[waiter,food,time]
HoldsAt(BeWaiter4(waiter),time) &
({agent} HoldsAt(KnowOrder(waiter,agent,food),time)) ->
Initiates(PickUp(waiter,food),
          BeWaiter5(waiter),
          time).

[restaurant,waiter,time]
WaiterOf(restaurant)=waiter &
HoldsAt(BeWaiter5(waiter),time) ->
Happens(WalkThroughDoor21(waiter,KitchenDoorOf(restaurant)),time).

[restaurant,waiter,door,time]
HoldsAt(BeWaiter5(waiter),time) &
WaiterOf(restaurant)=waiter &
KitchenDoorOf(restaurant)=door ->
Terminates(WalkThroughDoor21(waiter,door),
           BeWaiter5(waiter),
           time).

[restaurant,waiter,door,time]
HoldsAt(BeWaiter5(waiter),time) &
WaiterOf(restaurant)=waiter &
KitchenDoorOf(restaurant)=door ->
Initiates(WalkThroughDoor21(waiter,door),
          BeWaiter6(waiter),
          time).

[restaurant,waiter,table,food,time]
WaiterOf(restaurant)=waiter &
TableOf(restaurant)=table &
HoldsAt(BeWaiter6(waiter),time) &
HoldsAt(Holding(waiter,food),time) ->
Happens(PlaceOn(waiter,food,table),time).

[waiter,food,table,time]
HoldsAt(BeWaiter6(waiter),time) ->
Terminates(PlaceOn(waiter,food,table),
           BeWaiter6(waiter),
           time).

[waiter,food,table,time]
HoldsAt(BeWaiter6(waiter),time) ->
Initiates(PlaceOn(waiter,food,table),
          BeWaiter7(waiter),
          time).

[waiter,agent,bill,time]
HoldsAt(BeWaiter7(waiter),time) ->
Terminates(Request(agent,waiter,bill),
           BeWaiter7(waiter),
           time).

[waiter,agent,bill,time]
HoldsAt(BeWaiter7(waiter),time) ->
Initiates(Request(agent,waiter,bill),
          BeWaiter8(waiter),
          time).

[restaurant,waiter,bill,time]
WaiterOf(restaurant)=waiter &
BillOf(restaurant)=bill &
HoldsAt(BeWaiter8(waiter),time) ->
Happens(PickUp(waiter,bill),time).

[waiter,bill,time]
HoldsAt(BeWaiter8(waiter),time) ->
Terminates(PickUp(waiter,bill),
           BeWaiter8(waiter),
           time).

[waiter,bill,time]
HoldsAt(BeWaiter8(waiter),time) ->
Initiates(PickUp(waiter,bill),
          BeWaiter9(waiter),
          time).

[restaurant,waiter,bill,table,time]
WaiterOf(restaurant)=waiter &
BillOf(restaurant)=bill &
TableOf(restaurant)=table &
HoldsAt(BeWaiter9(waiter),time) ->
Happens(PlaceOn(waiter,bill,table),time).

[waiter,bill,table,time]
HoldsAt(BeWaiter9(waiter),time) ->
Terminates(PlaceOn(waiter,bill,table),
           BeWaiter9(waiter),
           time).

[waiter,bill,table,time]
HoldsAt(BeWaiter9(waiter),time) ->
Initiates(PlaceOn(waiter,bill,table),
          BeWaiter0(waiter),
          time).

fluent BeCook0(cook)

fluent BeCook1(cook)

xor BeCook0, BeCook1

[cook,agent,food,time]
HoldsAt(BeCook0(cook),time) ->
Terminates(Order(agent,cook,food),
           BeCook0(cook),
           time).

[cook,agent,food,time]
HoldsAt(BeCook0(cook),time) ->
Initiates(Order(agent,cook,food),
          BeCook1(cook),
          time).

event FoodPrepare(agent,food)

fluent FoodPrepared(food)

[agent,food,time]
Initiates(FoodPrepare(agent,food),
          FoodPrepared(food),
          time).

[agent,food,time]
Happens(FoodPrepare(agent,food),time) ->
{location}
HoldsAt(At(agent,location),time) &
HoldsAt(At(food,location),time).

[cook,agent,food,time]
HoldsAt(BeCook1(cook),time) &
HoldsAt(KnowOrder(cook,agent,food),time) ->
Happens(FoodPrepare(cook,food),time).

[cook,food,time]
HoldsAt(BeCook1(cook),time) ->
Terminates(FoodPrepare(cook,food),
           BeCook1(cook),
           time).

[cook,food,time]
HoldsAt(BeCook1(cook),time) ->
Initiates(FoodPrepare(cook,food),
          BeCook0(cook),
          time).

event Pay(agent,agent)

event Tip(agent,agent)

[agent,physobj,time]
Happens(LieOn(agent,physobj),time) ->
{room}
 HoldsAt(At(agent,room),time) &
 HoldsAt(At(physobj,room),time).

[agent,physobj,time]
Happens(SitOn(agent,physobj),time) ->
{room}
 HoldsAt(At(agent,room),time) &
 HoldsAt(At(physobj,room),time).

event LieOn(agent,physobj)

event SitOn(agent,physobj)

event RiseFrom(agent,physobj)

fluent LyingOn(agent,physobj)
fluent SittingOn(agent,physobj)
fluent Standing(agent)

fluent Lying(agent)
fluent Sitting(agent)
noninertial Lying
noninertial Sitting

xor Lying, Sitting, Standing

[agent,physobj,time]
HoldsAt(LyingOn(agent,physobj),time) ->
HoldsAt(Lying(agent),time).

[agent,physobj,time]
HoldsAt(SittingOn(agent,physobj),time) ->
HoldsAt(Sitting(agent),time).

[agent,physobj1,physobj2,time]
HoldsAt(LyingOn(agent,physobj1),time) &
HoldsAt(LyingOn(agent,physobj2),time) ->
physobj1=physobj2.

[agent,physobj1,physobj2,time]
HoldsAt(SittingOn(agent,physobj1),time) &
HoldsAt(SittingOn(agent,physobj2),time) ->
physobj1=physobj2.

[agent,physobj,time]
HoldsAt(Standing(agent),time) ->
Initiates(LieOn(agent,physobj),
          LyingOn(agent,physobj),
          time).

[agent,physobj,time]
Terminates(LieOn(agent,physobj),
           Standing(agent),
           time).

[agent,physobj,time]
HoldsAt(Standing(agent),time) ->
Initiates(SitOn(agent,physobj),
          SittingOn(agent,physobj),
          time).

[agent,physobj,time]
Terminates(SitOn(agent,physobj),
           Standing(agent),
           time).

[agent,physobj,time]
(HoldsAt(SittingOn(agent,physobj),time) |
 HoldsAt(LyingOn(agent,physobj),time)) ->
Initiates(RiseFrom(agent,physobj),
          Standing(agent),
          time).

[agent,physobj,time]
HoldsAt(LyingOn(agent,physobj),time) ->
Terminates(RiseFrom(agent,physobj),
           LyingOn(agent,physobj),
           time).

[agent,physobj,time]
HoldsAt(SittingOn(agent,physobj),time) ->
Terminates(RiseFrom(agent,physobj),
           SittingOn(agent,physobj),
           time).

event Greet(agent,agent)

event SayGoodbye(agent,agent)

[agent1,agent2,time]
Happens(Greet(agent1,agent2),time) ->
{location}
HoldsAt(At(agent1,location),time) &
HoldsAt(At(agent2,location),time).

[agent1,agent2,time]
Happens(SayGoodbye(agent1,agent2),time) ->
{location}
HoldsAt(At(agent1,location),time) &
HoldsAt(At(agent2,location),time).

event Order(agent,agent,physobj)

fluent KnowOrder(agent,agent,physobj)

[agent1,agent2,physobj,time]
Initiates(Order(agent1,agent2,physobj),
          KnowOrder(agent2,agent1,physobj),
          time).

[agent1,agent2,physobj,time]
Happens(Order(agent1,agent2,physobj),time) ->
{location}
HoldsAt(At(agent1,location),time) &
HoldsAt(At(agent2,location),time).

event Request(agent,agent,physobj)

fluent KnowRequest(agent,agent,physobj)

[agent1,agent2,physobj,time]
Initiates(Request(agent1,agent2,physobj),
          KnowRequest(agent2,agent1,physobj),
          time).

[agent1,agent2,physobj,time]
Happens(Request(agent1,agent2,physobj),time) ->
{location}
HoldsAt(At(agent1,location),time) &
HoldsAt(At(agent2,location),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Diving.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; scuba diving
;

sort object
sort agent: object
sort diver: agent
sort depth: integer
sort boat: object

; reference line, anchor line, shotline, SMB line, ...
sort line: object

sort equipment: object
sort weight: equipment
sort fin: equipment
sort airtank: equipment

; buoyancy compensator (BC)
; buoyancy control device (BCD)
sort computer: equipment
sort bc: equipment

fluent AtDepth(object,depth)

[object,depth1,depth2,time]
HoldsAt(AtDepth(object,depth1),time) &
HoldsAt(AtDepth(object,depth2),time) ->
depth1 = depth2.

event Ascend(diver,depth)

event Descend(diver,depth)

[diver,depth1,depth2,time]
HoldsAt(AtDepth(diver,depth1),time) &
Happens(Descend(diver,depth2),time) ->
depth2>depth1.

[diver,depth1,depth2,time]
HoldsAt(AtDepth(diver,depth1),time) &
Happens(Ascend(diver,depth2),time) ->
depth2<depth1.

[diver,depth,time]
Initiates(Descend(diver,depth),AtDepth(diver,depth),time).

[diver,depth1,depth2,time]
HoldsAt(AtDepth(diver,depth1),time) ->
Terminates(Descend(diver,depth2),AtDepth(diver,depth1),time).

[diver,depth,time]
Initiates(Ascend(diver,depth),AtDepth(diver,depth),time).

[diver,depth1,depth2,time]
HoldsAt(AtDepth(diver,depth1),time) ->
Terminates(Ascend(diver,depth2),AtDepth(diver,depth1),time).

fluent Wearing(diver,equipment)

event PutOn(diver,equipment)

event TakeOff(diver,equipment)

event Lose(diver,equipment)

[diver,equipment,depth,time]
Releases(PutOn(diver,equipment),AtDepth(equipment,depth),time).

[diver,equipment,time]
Releases(PutOn(diver,equipment),UnderWater(equipment),time).

[diver,equipment,time]
Happens(PutOn(diver,equipment),time) ->
!{diver1} HoldsAt(Wearing(diver1,equipment),time).

[diver,depth,equipment,time]
HoldsAt(Wearing(diver,equipment),time) ->
(HoldsAt(AtDepth(diver,depth),time) <->
 HoldsAt(AtDepth(equipment,depth),time)).

[diver,depth,object,time]
HoldsAt(Holding(diver,object),time) ->
(HoldsAt(AtDepth(diver,depth),time) <->
 HoldsAt(AtDepth(object,depth),time)).

[diver,equipment,time]
HoldsAt(Wearing(diver,equipment),time) ->
(HoldsAt(UnderWater(diver),time) <->
 HoldsAt(UnderWater(equipment),time)).

[diver,object,time]
HoldsAt(Holding(diver,object),time) ->
(HoldsAt(UnderWater(diver),time) <->
 HoldsAt(UnderWater(object),time)).

[diver,depth,equipment,time]
HoldsAt(AtDepth(diver,depth),time) &
HoldsAt(Wearing(diver,equipment),time) ->
Initiates(TakeOff(diver,equipment),AtDepth(equipment,depth),time).

[diver,depth,equipment,time]
!HoldsAt(AtDepth(diver,depth),time) &
HoldsAt(Wearing(diver,equipment),time) ->
Terminates(TakeOff(diver,equipment),AtDepth(equipment,depth),time).

[diver,equipment,time]
HoldsAt(UnderWater(diver),time) ->
Initiates(TakeOff(diver,equipment),UnderWater(equipment),time).

[diver,equipment,time]
!HoldsAt(UnderWater(diver),time) ->
Terminates(TakeOff(diver,equipment),UnderWater(equipment),time).

[diver,equipment,depth,time]
HoldsAt(AtDepth(diver,depth),time) &
HoldsAt(Wearing(diver,equipment),time) ->
Initiates(Lose(diver,equipment),AtDepth(equipment,depth),time).

[diver,equipment,depth,time]
!HoldsAt(AtDepth(diver,depth),time) &
HoldsAt(Wearing(diver,equipment),time) ->
Terminates(Lose(diver,equipment),AtDepth(equipment,depth),time).

[diver,equipment,time]
HoldsAt(UnderWater(diver),time) ->
Initiates(Lose(diver,equipment),UnderWater(equipment),time).

[diver,equipment,time]
!HoldsAt(UnderWater(diver),time) ->
Terminates(Lose(diver,equipment),UnderWater(equipment),time).

fluent Holding(diver,object)

[diver1,diver2,time]
HoldsAt(Holding(diver1,diver2),time) ->
!HoldsAt(Holding(diver2,diver1),time).

event Grab(diver,object)

event LetGoOf(diver,object)

[diver,object,time]
Initiates(Grab(diver,object),Holding(diver,object),time).

[diver,object,time]
Terminates(LetGoOf(diver,object),Holding(diver,object),time).

[diver,object,depth,time]
Releases(Grab(diver,object),AtDepth(object,depth),time).

[diver,object,time]
Releases(Grab(diver,object),UnderWater(object),time).

[diver,object,depth,time]
HoldsAt(AtDepth(diver,depth),time) &
HoldsAt(Holding(diver,object),time) ->
Initiates(LetGoOf(diver,object),AtDepth(object,depth),time).

[diver,object,depth,time]
!HoldsAt(AtDepth(diver,depth),time) &
HoldsAt(Holding(diver,object),time) ->
Terminates(LetGoOf(diver,object),AtDepth(object,depth),time).

[diver,object,time]
HoldsAt(UnderWater(diver),time) ->
Initiates(LetGoOf(diver,object),UnderWater(object),time).

[diver,object,time]
!HoldsAt(UnderWater(diver),time) ->
Terminates(LetGoOf(diver,object),UnderWater(object),time).

[diver,equipment,time]
Initiates(PutOn(diver,equipment),Wearing(diver,equipment),time).

[diver,equipment,time]
Happens(PutOn(diver,equipment),time) ->
!HoldsAt(UnderWater(diver),time).

[diver,equipment,time]
Terminates(TakeOff(diver,equipment),Wearing(diver,equipment),time).

[diver,equipment,time]
Terminates(Lose(diver,equipment),Wearing(diver,equipment),time).

fluent Vertical(diver)

fluent HorizontalDown(diver)

fluent Inverted(diver)

fluent HorizontalUp(diver)

xor Vertical, HorizontalDown, Inverted, HorizontalUp

event RotatePitch(diver)

[diver,time]
HoldsAt(Vertical(diver),time) ->
Initiates(RotatePitch(diver),HorizontalDown(diver),time).

[diver,time]
HoldsAt(HorizontalDown(diver),time) ->
Initiates(RotatePitch(diver),Inverted(diver),time).

[diver,time]
HoldsAt(HorizontalDown(diver),time) ->
Terminates(RotatePitch(diver),HorizontalDown(diver),time).

[diver,time]
HoldsAt(Inverted(diver),time) ->
Initiates(RotatePitch(diver),HorizontalUp(diver),time).

[diver,time]
HoldsAt(Inverted(diver),time) ->
Terminates(RotatePitch(diver),Inverted(diver),time).

[diver,time]
HoldsAt(HorizontalUp(diver),time) ->
Initiates(RotatePitch(diver),Vertical(diver),time).

[diver,time]
HoldsAt(HorizontalUp(diver),time) ->
Terminates(RotatePitch(diver),HorizontalUp(diver),time).

event RotateYaw(diver)

; try taking out Holding condition here
[diver,time]
Happens(Ascend1(diver),time) &
!Happens(RapidAscendToSurface(diver),time) &
!({diver1} HoldsAt(Holding(diver,diver1),time)) ->
Happens(RotateYaw(diver),time).

fluent UnderWater(object)

[object,depth,time]
depth>0 &
HoldsAt(AtDepth(object,depth),time) ->
HoldsAt(UnderWater(object),time).

event EnterWater(object)

event Surface(object)

[object,time]
Initiates(EnterWater(object),UnderWater(object),time).

[diver,time]
Happens(EnterWater(diver),time) ->
!{diver1} HoldsAt(Holding(diver1,diver),time).

[object,depth,time]
depth=0 ->
Initiates(EnterWater(object),AtDepth(object,depth),time).

[object,time]
Terminates(Surface(object),UnderWater(object),time).

[diver,time]
Terminates(Surface(diver),PositivelyBuoyant(diver),time).

[diver,time]
Terminates(Surface(diver),NegativelyBuoyant(diver),time).

[diver,time]
Terminates(Surface(diver),NeutrallyBuoyant(diver),time).

[object,depth,time]
Terminates(Surface(object),AtDepth(object,depth),time).

[diver,time] Happens(EnterWater(diver),time) ->
HoldsAt(Vertical(diver),time).

fluent StandingOn(diver,boat)

event StandOn(diver,boat)

[diver,boat,time]
Terminates(EnterWater(diver),StandingOn(diver,boat),time).

[diver,boat,time]
Initiates(StandOn(diver,boat),StandingOn(diver,boat),time).

fluent PositivelyBuoyant(diver)

fluent NeutrallyBuoyant(diver)

fluent NegativelyBuoyant(diver)

mutex PositivelyBuoyant, NeutrallyBuoyant, NegativelyBuoyant

[diver,time]
HoldsAt(PositivelyBuoyant(diver),time) ->
HoldsAt(UnderWater(diver),time).

[diver,time]
HoldsAt(NeutrallyBuoyant(diver),time) ->
HoldsAt(UnderWater(diver),time).

[diver,time]
HoldsAt(NegativelyBuoyant(diver),time) ->
HoldsAt(UnderWater(diver),time).

event PressDeflateButton(diver,bc)

event PressDumpButton(diver,bc)

event PressInflateButton(diver,bc)

[diver,bc,time]
Happens(PressDeflateButton(diver,bc),time) ->
HoldsAt(Vertical(diver),time) &
HoldsAt(UnderWater(bc),time).

[diver,bc,time]
Happens(PressDumpButton(diver,bc),time) ->
HoldsAt(Vertical(diver),time) &
HoldsAt(UnderWater(bc),time).

[diver,bc,time] Happens(PressDumpButton(diver,bc),time) ->
HoldsAt(UncontrolledBuoyancy(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Initiates(PressDeflateButton(diver,bc),NegativelyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Terminates(PressDeflateButton(diver,bc),NeutrallyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Terminates(PressDeflateButton(diver,bc),PositivelyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Initiates(PressDumpButton(diver,bc),NegativelyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Terminates(PressDumpButton(diver,bc),NeutrallyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Terminates(PressDumpButton(diver,bc),PositivelyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Initiates(PressInflateButton(diver,bc),NeutrallyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Terminates(PressInflateButton(diver,bc),PositivelyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Terminates(PressInflateButton(diver,bc),NegativelyBuoyant(diver),time).

[diver,weight,time]
HoldsAt(Wearing(diver,weight),time) ->
Initiates(TakeOff(diver,weight),PositivelyBuoyant(diver),time).

[diver,weight,time]
HoldsAt(Wearing(diver,weight),time) ->
Terminates(TakeOff(diver,weight),NegativelyBuoyant(diver),time).

[diver,weight,time]
HoldsAt(Wearing(diver,weight),time) ->
Terminates(TakeOff(diver,weight),NeutrallyBuoyant(diver),time).

fluent UncontrolledBuoyancy(diver)

event LoseBuoyancyControl(diver)

predicate IsInexperiencedDiver(diver)

[diver,time]
Happens(LoseBuoyancyControl(diver),time) ->
IsInexperiencedDiver(diver).

[diver,time]
Initiates(LoseBuoyancyControl(diver),UncontrolledBuoyancy(diver),time).

[diver,time]
Initiates(LoseBuoyancyControl(diver),PositivelyBuoyant(diver),time).

[diver,time]
Terminates(LoseBuoyancyControl(diver),NegativelyBuoyant(diver),time).

[diver,time]
Terminates(LoseBuoyancyControl(diver),NeutrallyBuoyant(diver),time).

; determining fluent
fluent AscendDescendAmount(diver,depth)
noninertial AscendDescendAmount

[diver,depth1,depth2,time]
HoldsAt(AscendDescendAmount(diver,depth1),time) &
HoldsAt(AscendDescendAmount(diver,depth2),time) ->
depth1=depth2.

[diver,depth,time]
Happens(Descend(diver,depth),time) ->
HoldsAt(NegativelyBuoyant(diver),time) &
({depth1}
 HoldsAt(AscendDescendAmount(diver,depth1),time) &
 HoldsAt(AtDepth(diver,depth-depth1),time)).

event KickUp(diver)

[diver,depth,time]
Happens(Ascend(diver,depth),time) ->
(HoldsAt(PositivelyBuoyant(diver),time) |
 (HoldsAt(NeutrallyBuoyant(diver),time) & Happens(KickUp(diver),time))) &
({depth1}
 HoldsAt(AscendDescendAmount(diver,depth1),time) &
 HoldsAt(AtDepth(diver,depth+depth1),time)).

[diver,time]
Happens(KickUp(diver),time) ->
HoldsAt(Vertical(diver),time).

event SwimAround(diver)

[diver,time]
Happens(SwimAround(diver),time) ->
HoldsAt(HorizontalDown(diver),time).

; signaling

event SignalDescend(diver,diver)

event SignalOutOfTime(diver,diver)

event SignalAscend(diver,diver)

;[diver1,diver2,time]
;Happens(SignalAscend(diver1,diver2),time) ->
;Happens(SignalOutOfTime(diver1,diver2),time-1).

;[diver1,diver2,time]
;Happens(SignalDescend(diver1,diver2),time) ->
;HoldsAt(See(diver1,diver2),time) &
;HoldsAt(See(diver2,diver1),time).

;[diver1,diver2,time]
;Happens(SignalOutOfTime(diver1,diver2),time) ->
;HoldsAt(See(diver1,diver2),time) &
;HoldsAt(See(diver2,diver1),time).

;[diver1,diver2,time]
;Happens(SignalAscend(diver1,diver2),time) ->
;HoldsAt(See(diver1,diver2),time) &
;HoldsAt(See(diver2,diver1),time).

;event LookAt(agent,object)

;fluent See(agent,object)

;[agent,object,time]
;Initiates(LookAt(agent,object),See(agent,object),time).

;[agent,object1,object2,time]
;object1!=object2 ->
;Terminates(LookAt(agent,object1),
;           See(agent,object2),
;           time).

event Descend1(diver)

event Ascend1(diver)

;[diver,object,time]
;Terminates(Descend1(diver),See(diver,object),time).

;[diver,object,time]
;Terminates(Ascend1(diver),See(diver,object),time).

;[diver,object,time]
;Terminates(RotateYaw(diver),See(diver,object),time).

event RapidAscendToSurface(diver)

[diver,time]
Happens(Descend1(diver),time) <->
({depth} Happens(Descend(diver,depth),time)).

[diver,time]
Happens(Ascend1(diver),time) <->
({depth} Happens(Ascend(diver,depth),time)).

[diver,time]
Happens(RapidAscendToSurface(diver),time) ->
Happens(Ascend(diver,0),time).

event AscendLine(diver,line)

[diver,line,time]
Happens(AscendLine(diver,line),time) ->
Happens(Ascend1(diver),time).

fluent Disoriented(diver)

event BecomeDisoriented(diver)

event BecomeReoriented(diver)

[diver,time]
Initiates(BecomeDisoriented(diver),Disoriented(diver),time).

[diver,time]
Terminates(BecomeReoriented(diver),Disoriented(diver),time).

fluent DisturbedSilt()

event DisturbSilt(diver)

[diver,time]
Initiates(DisturbSilt(diver),DisturbedSilt(),time).

[diver,time]
Happens(BecomeDisoriented(diver),time) ->
(!HoldsAt(DisturbedSilt(),time-1) &
 HoldsAt(DisturbedSilt(),time)).

event Panic(diver)

[diver,time] Happens(Panic(diver),time) ->
HoldsAt(Disoriented(diver),time) |
HoldsAt(UncontrolledBuoyancy(diver),time) |
({equipment} Happens(Lose(diver,equipment),time-1)) |
Happens(Vomit(diver),time-1).

event Vomit(diver)

; conditions

fluent Unconscious(diver)

event GoUnconscious(diver)

event RegainConsciousness(diver)

[diver,time]
Initiates(GoUnconscious(diver),Unconscious(diver),time).

[diver,time]
Terminates(RegainConsciousness(diver),Unconscious(diver),time).

[diver,time]
Happens(GoUnconscious(diver),time) ->
Happens(RapidAscendToSurface(diver),time).

fluent HasEarPain(diver)

event StartEarPain(diver)

[diver,time] Initiates(StartEarPain(diver),HasEarPain(diver),time).

fluent HasRupturedEardrum(diver)

event RuptureEardrum(diver)

[diver,time]
Initiates(RuptureEardrum(diver),HasRupturedEardrum(diver),time).
fluent ConditionOK(diver)

fluent HasDecompressionIllness(diver)

event StartDecompressionIllness(diver)

[diver,time]
Initiates(StartDecompressionIllness(diver),
          HasDecompressionIllness(diver),
          time).

fluent SignalingDecompress(computer,diver)

fluent SignalingLowOnAir(computer,airtank,diver)

[computer,airtank,diver,time]
HoldsAt(SignalingLowOnAir(computer,airtank,diver),time) ->
HoldsAt(LowOnAir(airtank),time).

[computer,diver,time]
HoldsAt(SignalingDecompress(computer,diver),time) ->
!{time1} time1<time & Happens(Decompress(diver),time1).

event Decompress(diver)

event EqualizeEars(diver)

[diver,time]
(Happens(Descend1(diver),time) | Happens(Ascend1(diver),time)) &
!Happens(EqualizeEars(diver),time) ->
Happens(StartEarPain(diver),time) &
Happens(RuptureEardrum(diver),time).

[diver,time]
Happens(Ascend1(diver),time) &
!Happens(Decompress(diver),time) ->
Happens(StartDecompressionIllness(diver),time).

[diver1,diver2,time]
HoldsAt(Holding(diver1,diver2),time) &
Happens(Ascend1(diver1),time) &
!Happens(Decompress(diver2),time) ->
Happens(StartDecompressionIllness(diver2),time).

[diver,time]
Happens(Decompress(diver),time) ->
({depth} depth>0 & HoldsAt(AtDepth(diver,depth),time)) &
!HoldsAt(UncontrolledBuoyancy(diver),time).

fluent HasHeadache(diver)

[diver,time]
HoldsAt(ConditionOK(diver),time) ->
!HoldsAt(Unconscious(diver),time) &
!HoldsAt(HasEarPain(diver),time) &
!HoldsAt(HasRupturedEardrum(diver),time) &
!HoldsAt(HasDecompressionIllness(diver),time) &
!HoldsAt(HasHeadache(diver),time).

event BeAirlifted(diver)

event TakeInWater(diver)

fluent LowOnAir(airtank)

event BecomeLowOnAir(airtank)

[airtank,time]
Initiates(BecomeLowOnAir(airtank),LowOnAir(airtank),time).

; initial state
[diver] HoldsAt(ConditionOK(diver),0).
[diver] HoldsAt(Vertical(diver),0).
!HoldsAt(DisturbedSilt(),0).
[diver] !HoldsAt(UncontrolledBuoyancy(diver),0).
[diver] !HoldsAt(Disoriented(diver),0).
[diver] !HoldsAt(PositivelyBuoyant(diver),0) &
        !HoldsAt(NeutrallyBuoyant(diver),0) &
        !HoldsAt(NegativelyBuoyant(diver),0).
[diver,object] !HoldsAt(Wearing(diver,object),0).
[diver,object] !HoldsAt(Holding(diver,object),0).
[diver1,diver2] !HoldsAt(Separated(diver1,diver2),0).
;[agent,object] !HoldsAt(See(agent,object),0).

fluent Separated(diver,diver)

[diver1,diver2,time]
HoldsAt(Separated(diver1,diver2),time) ->
HoldsAt(Separated(diver2,diver1),time).

event BecomeSeparated(diver,diver)

event BeReunitedWith(diver,diver)

[diver1,diver2,time]
Initiates(BecomeSeparated(diver1,diver2),Separated(diver1,diver2),time).

[diver1,diver2,time]
Initiates(BecomeSeparated(diver1,diver2),Separated(diver2,diver1),time).

[diver1,diver2,time]
Terminates(BeReunitedWith(diver1,diver2),Separated(diver1,diver2),time).

[diver1,diver2,time]
Terminates(BeReunitedWith(diver1,diver2),Separated(diver2,diver1),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Dress.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; Dress
; (cf Sleep)
;

event PutOn(agent,clothing)

event TakeOff(agent,clothing)

fluent Wearing(agent,clothing)

[agent,clothing,time]
Initiates(PutOn(agent,clothing),
          Wearing(agent,clothing),
          time).

[agent,clothing,time]
Happens(PutOn(agent,clothing),time) ->
!HoldsAt(Wearing(agent,clothing),time) &
{location} HoldsAt(At(agent,location),time) &
           HoldsAt(At(clothing,location),time).

[agent,clothing,time]
Terminates(TakeOff(agent,clothing),
           Wearing(agent,clothing),
           time).

[agent,clothing,time]
Happens(TakeOff(agent,clothing),time) ->
HoldsAt(Wearing(agent,clothing),time).

[agent,clothing,location,time]
Releases(PutOn(agent,clothing),At(clothing,location),time).

[agent,clothing,location,time]
HoldsAt(Wearing(agent,clothing),time) &
HoldsAt(At(agent,location),time) ->
HoldsAt(At(clothing,location),time).

;[agent,clothing,location1,location2,time]
;HoldsAt(At(agent,location1),time) &
;location1 != location2 ->
;Terminates(TakeOff(agent,clothing),At(clothing,location2),time).

[agent,clothing,location,time]
HoldsAt(At(agent,location),time) ->
Initiates(TakeOff(agent,clothing),At(clothing,location),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/HungerNeed.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; hunger need
;

fluent Hungry(agent)

fluent Satiated(agent)
noninertial Satiated

[agent,time] HoldsAt(Hungry(agent),time) <-> !HoldsAt(Satiated(agent),time).

event Eat(agent,food)

[agent,food,time]
Happens(Eat(agent,food),time) ->
{location}
HoldsAt(At(agent,location),time) &
HoldsAt(At(food,location),time).

[agent,food,time]
Terminates(Eat(agent,food),Hungry(agent),time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/Restaurant.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
sort restaurant: script
sort waiter: agent
sort cook: agent

function BillOf(restaurant): bill
function CookOf(restaurant): cook
function TableOf(restaurant): table
function WaiterOf(restaurant): waiter
function KitchenDoorOf(restaurant): door

; awaiting customer/waiter has set down bill on customer's table
fluent BeWaiter0(waiter)

; awaiting customer order
fluent BeWaiter1(waiter)

; has customer order
fluent BeWaiter2(waiter)

; in kitchen
fluent BeWaiter3(waiter)

; awaiting preparation of order
fluent BeWaiter4(waiter)

; has order
fluent BeWaiter5(waiter)

; back in dining room
fluent BeWaiter6(waiter)

; order delivered to customer (can ask if all is OK)
fluent BeWaiter7(waiter)

; customer has requested bill
fluent BeWaiter8(waiter)

; waiter is holding bill
fluent BeWaiter9(waiter)

xor BeWaiter0, BeWaiter1, BeWaiter2, BeWaiter3, BeWaiter4, BeWaiter5, BeWaiter6, BeWaiter7, BeWaiter8, BeWaiter9

[waiter,agent,time]
HoldsAt(BeWaiter0(waiter),time) ->
Terminates(Greet(waiter,agent),
           BeWaiter0(waiter),
           time).

[waiter,agent,time]
HoldsAt(BeWaiter0(waiter),time) ->
Initiates(Greet(waiter,agent),
          BeWaiter1(waiter),
          time).

[waiter,agent,food,time]
HoldsAt(BeWaiter1(waiter),time) ->
Terminates(Order(agent,waiter,food),
           BeWaiter1(waiter),
           time).

[waiter,agent,food,time]
HoldsAt(BeWaiter1(waiter),time) ->
Initiates(Order(agent,waiter,food),
          BeWaiter2(waiter),
          time).

[restaurant,waiter,time]
WaiterOf(restaurant)=waiter &
HoldsAt(BeWaiter2(waiter),time) ->
Happens(WalkThroughDoor12(waiter,KitchenDoorOf(restaurant)),time).

[restaurant,waiter,door,time]
HoldsAt(BeWaiter2(waiter),time) &
WaiterOf(restaurant)=waiter &
KitchenDoorOf(restaurant)=door ->
Terminates(WalkThroughDoor12(waiter,door),
           BeWaiter2(waiter),
           time).

[restaurant,waiter,door,time]
HoldsAt(BeWaiter2(waiter),time) &
WaiterOf(restaurant)=waiter &
KitchenDoorOf(restaurant)=door ->
Initiates(WalkThroughDoor12(waiter,door),
          BeWaiter3(waiter),
          time).

[restaurant,food,time]
HoldsAt(BeWaiter3(WaiterOf(restaurant)),time) &
({agent} HoldsAt(KnowOrder(WaiterOf(restaurant),agent,food),time)) ->
Happens(Order(WaiterOf(restaurant),CookOf(restaurant),food),time).

[restaurant,waiter,cook,food,time]
WaiterOf(restaurant)=waiter &
CookOf(restaurant)=cook &
HoldsAt(BeWaiter3(waiter),time) ->
Terminates(Order(waiter,cook,food),
           BeWaiter3(waiter),
           time).

[restaurant,waiter,cook,food,time]
WaiterOf(restaurant)=waiter &
CookOf(restaurant)=cook &
HoldsAt(BeWaiter3(waiter),time) ->
Initiates(Order(waiter,cook,food),
          BeWaiter4(waiter),
          time).

[waiter,food,time]
HoldsAt(BeWaiter4(waiter),time) &
({agent} HoldsAt(KnowOrder(waiter,agent,food),time)) &
HoldsAt(FoodPrepared(food),time) ->
Happens(PickUp(waiter,food),time).

[waiter,food,time]
HoldsAt(BeWaiter4(waiter),time) &
({agent} HoldsAt(KnowOrder(waiter,agent,food),time)) ->
Terminates(PickUp(waiter,food),
           BeWaiter4(waiter),
           time).

[waiter,food,time]
HoldsAt(BeWaiter4(waiter),time) &
({agent} HoldsAt(KnowOrder(waiter,agent,food),time)) ->
Initiates(PickUp(waiter,food),
          BeWaiter5(waiter),
          time).

[restaurant,waiter,time]
WaiterOf(restaurant)=waiter &
HoldsAt(BeWaiter5(waiter),time) ->
Happens(WalkThroughDoor21(waiter,KitchenDoorOf(restaurant)),time).

[restaurant,waiter,door,time]
HoldsAt(BeWaiter5(waiter),time) &
WaiterOf(restaurant)=waiter &
KitchenDoorOf(restaurant)=door ->
Terminates(WalkThroughDoor21(waiter,door),
           BeWaiter5(waiter),
           time).

[restaurant,waiter,door,time]
HoldsAt(BeWaiter5(waiter),time) &
WaiterOf(restaurant)=waiter &
KitchenDoorOf(restaurant)=door ->
Initiates(WalkThroughDoor21(waiter,door),
          BeWaiter6(waiter),
          time).

[restaurant,waiter,table,food,time]
WaiterOf(restaurant)=waiter &
TableOf(restaurant)=table &
HoldsAt(BeWaiter6(waiter),time) &
HoldsAt(Holding(waiter,food),time) ->
Happens(PlaceOn(waiter,food,table),time).

[waiter,food,table,time]
HoldsAt(BeWaiter6(waiter),time) ->
Terminates(PlaceOn(waiter,food,table),
           BeWaiter6(waiter),
           time).

[waiter,food,table,time]
HoldsAt(BeWaiter6(waiter),time) ->
Initiates(PlaceOn(waiter,food,table),
          BeWaiter7(waiter),
          time).

[waiter,agent,bill,time]
HoldsAt(BeWaiter7(waiter),time) ->
Terminates(Request(agent,waiter,bill),
           BeWaiter7(waiter),
           time).

[waiter,agent,bill,time]
HoldsAt(BeWaiter7(waiter),time) ->
Initiates(Request(agent,waiter,bill),
          BeWaiter8(waiter),
          time).

[restaurant,waiter,bill,time]
WaiterOf(restaurant)=waiter &
BillOf(restaurant)=bill &
HoldsAt(BeWaiter8(waiter),time) ->
Happens(PickUp(waiter,bill),time).

[waiter,bill,time]
HoldsAt(BeWaiter8(waiter),time) ->
Terminates(PickUp(waiter,bill),
           BeWaiter8(waiter),
           time).

[waiter,bill,time]
HoldsAt(BeWaiter8(waiter),time) ->
Initiates(PickUp(waiter,bill),
          BeWaiter9(waiter),
          time).

[restaurant,waiter,bill,table,time]
WaiterOf(restaurant)=waiter &
BillOf(restaurant)=bill &
TableOf(restaurant)=table &
HoldsAt(BeWaiter9(waiter),time) ->
Happens(PlaceOn(waiter,bill,table),time).

[waiter,bill,table,time]
HoldsAt(BeWaiter9(waiter),time) ->
Terminates(PlaceOn(waiter,bill,table),
           BeWaiter9(waiter),
           time).

[waiter,bill,table,time]
HoldsAt(BeWaiter9(waiter),time) ->
Initiates(PlaceOn(waiter,bill,table),
          BeWaiter0(waiter),
          time).

; awaiting next waiter order
fluent BeCook0(cook)

; waiter order received
fluent BeCook1(cook)

xor BeCook0, BeCook1

[cook,agent,food,time]
HoldsAt(BeCook0(cook),time) ->
Terminates(Order(agent,cook,food),
           BeCook0(cook),
           time).

[cook,agent,food,time]
HoldsAt(BeCook0(cook),time) ->
Initiates(Order(agent,cook,food),
          BeCook1(cook),
          time).

event FoodPrepare(agent,food)

fluent FoodPrepared(food)

[agent,food,time]
Initiates(FoodPrepare(agent,food),
          FoodPrepared(food),
          time).

[agent,food,time]
Happens(FoodPrepare(agent,food),time) ->
{location}
HoldsAt(At(agent,location),time) &
HoldsAt(At(food,location),time).

[cook,agent,food,time]
HoldsAt(BeCook1(cook),time) &
HoldsAt(KnowOrder(cook,agent,food),time) ->
Happens(FoodPrepare(cook,food),time).

[cook,food,time]
HoldsAt(BeCook1(cook),time) ->
Terminates(FoodPrepare(cook,food),
           BeCook1(cook),
           time).

[cook,food,time]
HoldsAt(BeCook1(cook),time) ->
Initiates(FoodPrepare(cook,food),
          BeCook0(cook),
          time).

; End of file.



; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: ecnet/EatingInAHouse.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; @article{Mueller:2004c,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Understanding script-based stories using commonsense reasoning",
;   journal = "Cognitive Systems Research",
;   volume = "5",
;   number = "4",
;   pages = "307--340",
; }
;

option modeldiff on
option encoding 3
option renaming off

ignore Love, ThreatenedBy
ignore LookOutOnto, Floor, BuildingOf, SkyOf, GroundOf
ignore Inside
ignore Near, WalkFrom, WalkFromTo, RunFromTo
ignore BillOf, CookOf, TableOf, WaiterOf, KitchenDoorOf
ignore BeWaiter0, BeWaiter1, BeWaiter2, BeWaiter3, BeWaiter4
ignore BeWaiter5, BeWaiter6, BeWaiter7, BeWaiter8, BeWaiter9
ignore BeCook0, BeCook1
ignore InviteIn, InvitedIn, IntendToWalkIn, IntentionToWalkIn
ignore ActOnIntentionToWalkIn, Greet, SayGoodbye, CryForJoy
ignore Threaten, ReleaseFromThreat, ThreatenedBy
ignore Order, KnowOrder, Request, KnowRequest
ignore PutInside, TakeOutOf
ignore SayPleaseToMeet, Move

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/HungerNeed.e
load answers/Mueller2004c/Restaurant.e
load answers/Mueller2003/Sleep.e
load answers/Mueller2003/SpeechAct.e
load answers/Mueller2004c/Dress.e

room Upstairs1

staircase Staircase1

room Hallway1

Side1(Staircase1)=Hallway1.
Side2(Staircase1)=Upstairs1.

door DiningRoomDoor1

room DiningRoom1

Side1(DiningRoomDoor1)=Hallway1.
Side2(DiningRoomDoor1)=DiningRoom1.

door KitchenDoor1

room Kitchen1

Side1(KitchenDoor1)=DiningRoom1.
Side2(KitchenDoor1)=Kitchen1.

agent Eater1

agent Eater2

clothing Clothing1

clothing Clothing2

chair Chair1

chair Chair2

food Food1

agent Cook1

table Table1

content Content1

content Content2

outside DummyOutside1

; prune
sort ona, onb
fluent! On(ona,onb)
event! PlaceOn(agent,ona,onb)
event! TakeOffOf(agent,ona,onb)

sort ordera, orderb, orderc
event! Order(ordera,orderb,orderc)
fluent! KnowOrder(orderb,ordera,orderc)

sort requesta, requestb, requestc
event! Request(requesta,requestb,requestc)
fluent! KnowRequest(requestb,requesta,requestc)

sort holda, holdb, holdc
event! TakeOffOf(holda,holdb,holdc)
event! PickUp(holda,holdb)
event! LetGoOf(holda,holdb)
event! Hold(holda,holdb)
fluent! Holding(holda,holdb)

sort sita, sitb
event! LieOn(sita,sitb)
event! SitOn(sita,sitb)
event! RiseFrom(sita,sitb)
fluent! LyingOn(sita,sitb)
fluent! SittingOn(sita,sitb)

ona! Food1
onb! Table1
holda! Cook1
holdb! Food1
holdc! Table1
sita! Eater1
sitb! Chair1

; initial situation
[agent] HoldsAt(Dressed(agent),0).
[agent] HoldsAt(Awake(agent),0).
[agent] HoldsAt(Sleep3(agent),0).
[agent] HoldsAt(Standing(agent),0).
[agent] HoldsAt(Standing(agent),0).
[agent,object] !HoldsAt(Holding(agent,object),0).
[food] HoldsAt(At(food,Kitchen1),0).
[food] !HoldsAt(FoodPrepared(food),0).
[agent] HoldsAt(Hungry(agent),0).
[door] HoldsAt(DoorIsOpen(door),0).
[clothing] HoldsAt(At(clothing,Upstairs1),0).
[chair] HoldsAt(At(chair,DiningRoom1),0).
HoldsAt(At(Cook1,Kitchen1),0).
HoldsAt(At(Table1,DiningRoom1),0).
[agent,clothing] !HoldsAt(Wearing(agent,clothing),0).

; narrative
HoldsAt(At(Cook1,Kitchen1),0).
HoldsAt(At(Eater1,Upstairs1),0).
HoldsAt(At(Eater2,Upstairs1),0).
Happens(FoodPrepare(Cook1,Food1),0).
Happens(PutOn(Eater1,Clothing1),1).
Happens(PutOn(Eater2,Clothing2),2).
Happens(WalkDownStaircase(Eater1,Staircase1),3).
Happens(WalkDownStaircase(Eater2,Staircase1),4).
Happens(WalkThroughDoor12(Eater1,DiningRoomDoor1),5).
Happens(WalkThroughDoor12(Eater2,DiningRoomDoor1),6).
Happens(SitOn(Eater1,Chair1),7).
Happens(SitOn(Eater2,Chair2),8).
Happens(PickUp(Cook1, Food1),9).
Happens(WalkThroughDoor21(Cook1, KitchenDoor1),10).
Happens(PlaceOn(Cook1, Food1, Table1),11).
Happens(WalkThroughDoor12(Cook1, KitchenDoor1),12).
Happens(Eat(Eater1,Food1),13).
Happens(Eat(Eater2,Food1),14).
Happens(Converse(Eater1,Eater2),15).
Happens(TalkAbout(Eater1,Content1),16).
Happens(TalkAbout(Eater2,Content2),17).
Happens(RiseFrom(Eater1,Chair1),18).
Happens(RiseFrom(Eater2,Chair2),19).

range time 0 20
range offset 0 0
range diameter 0 0

completion Happens

; End of file.
