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
; fire
;

; agent sets fire to physobj with burn time offset.
event SetFireTo(agent,physobj,fire,offset)

; An effect axioms states that
; if an agent sets a fire to a physical object with a burn time,
; the physical object will be burning with the fire and burn time:
[agent,physobj,fire,offset,time]
Initiates(SetFireTo(agent,physobj,fire,offset),
          Burning(physobj,fire,offset),
          time).

; agent puts out fire on physobj.
event PutOutFire(agent,physobj,fire)

; An effect axiom states that
; if an agent puts out a fire on a physical object,
; the physical object will no longer be burning:
[agent,physobj,fire,offset,time]
Terminates(PutOutFire(agent,physobj,fire),
           Burning(physobj,fire,offset),
           time).

; A precondition axiom states that
; for an agent to set fire to a physical object,
; there must be a location such that
; the agent is at the location and
; the physical object is at the location:
[agent,fire,physobj,offset,time]
Happens(SetFireTo(agent,physobj,fire,offset),time) ->
{location} HoldsAt(At(agent,location),time) &
           HoldsAt(At(physobj,location),time).

; A precondition axiom states that
; for an agent to put out a fire on a physical object,
; there must be a location such that
; the agent is at the location and
; the physical object is at the location:
[agent,fire,physobj,time]
Happens(PutOutFire(agent,physobj,fire),time) ->
{location} HoldsAt(At(agent,location),time) &
           HoldsAt(At(physobj,location),time).

; physobj is burning with fire and burn time offset.
fluent Burning(physobj,fire,offset)

; A state constraint says that a physical object burning with
; a fire has at most one burn time at a time:
[physobj,fire,offset1,offset2,time]
HoldsAt(Burning(physobj,fire,offset1),time) &
HoldsAt(Burning(physobj,fire,offset2),time) ->
offset1=offset2.

; The burn time of physobj is decremented.
event DecrementBurning(physobj)

; A trigger axiom states that
; if a physical object is burning with a fire and a burn time and
; the burn time is greater than zero,
; the burn time of the physical object is decremented:
[physobj,fire,offset,time]
HoldsAt(Burning(physobj,fire,offset),time) &
(offset > 0) ->
Happens(DecrementBurning(physobj),time).

; An effect axiom states that if a physical object is
; burning with a fire and a burn time, and the burn time of a physical
; object is decremented, the burn time of the physical
; object will be the burn time minus one:
[physobj,fire,offset1,offset2,time]
HoldsAt(Burning(physobj,fire,offset1),time) &
offset2 = offset1-1 ->
Initiates(DecrementBurning(physobj),
          Burning(physobj,fire,offset2),
          time).

; An effect axiom states that if a physical object is
; burning with a fire and a burn time, and the burn time of a physical
; object is decremented, the burn time of the physical
; object will no longer be the burn time:
[physobj,fire,offset,time]
HoldsAt(Burning(physobj,fire,offset),time) ->
Terminates(DecrementBurning(physobj),
           Burning(physobj,fire,offset),
           time).

; A trigger axiom states that
; if a physical object is burning with a fire and a burn time
; that is not equal to zero, the fire will damage the
; physical object:
[physobj,fire,offset,time]
offset!=0 &
HoldsAt(Burning(physobj,fire,offset),time) &
HoldsAt(Intact(physobj),time) ->
Happens(Damage(fire,physobj),time).

; A trigger axiom states that
; if a physical object is burning with a fire and a burn time
; that is equal to zero, the fire will destroy the
; physical object:
[physobj,fire,time]
HoldsAt(Burning(physobj,fire,0),time) &
!HoldsAt(Destroyed(physobj),time) ->
Happens(Destroy(fire,physobj),time).

; An effect axiom states that if a fire destroys a physical
; object, the physical object will no longer be burning:
[physobj,fire,offset,time]
Terminates(Destroy(fire,physobj),
           Burning(physobj,fire,offset),
           time).

; End of file.
