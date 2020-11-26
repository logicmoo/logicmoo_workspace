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
