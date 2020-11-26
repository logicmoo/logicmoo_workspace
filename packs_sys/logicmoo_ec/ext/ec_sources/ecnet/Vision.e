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
; The Vision representation deals with some simple aspects
; of vision.
;

; agent looks at object.
event LookAt(agent,object)

; agent sees object.
fluent See(agent,object)

; An effect axiom states that if an agent looks at
; an object, the agent will see the object:
[agent,object,time]
Initiates(LookAt(agent,object),
          See(agent,object),
          time).

; A precondition axiom states that for
; an agent to look at an object,
; there must be a location such that
; the agent is at the location and
; the object is at the location, or
; there must be a door such that
; the agent is near the door,
; the object is near the door, and
; the door is open:
;[agent,object,time]
;Happens(LookAt(agent,object),time) ->
;({location}
; HoldsAt(At(agent,location),time) &
; HoldsAt(At(object,location),time))|
;({door}
; HoldsAt(NearPortal(agent,door),time) &
; HoldsAt(NearPortal(object,door),time) &
; HoldsAt(DoorIsOpen(door),time)).

; An effect axiom states that if an agent
; looks at an object, the agent will no longer
; see other objects:
[agent,object1,object2,time]
object1!=object2 ->
Terminates(LookAt(agent,object1),
           See(agent,object2),
           time).

; Several effect axioms state that if an
; agent walks through a door, up a staircase, or down a staircase,
; the agent no longer sees an object:
[agent,door,object,time]
Terminates(WalkThroughDoor12(agent,door),
           See(agent,object),
           time).
[agent,door,object,time]
Terminates(WalkThroughDoor21(agent,door),
           See(agent,object),
           time).
[agent,door,object,time]
Terminates(WalkUpStaircase(agent,door),
           See(agent,object),
           time).
[agent,door,object,time]
Terminates(WalkDownStaircase(agent,door),
           See(agent,object),
           time).

; End of file.
