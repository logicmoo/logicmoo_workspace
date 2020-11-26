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
