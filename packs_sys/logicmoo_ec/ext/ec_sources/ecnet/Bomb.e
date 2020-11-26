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
; bomb

; agent is nondeterministically killed.
fluent KilledDeterminingFluent(agent)
noninertial KilledDeterminingFluent

; agent is nondeterministically injured.
fluent InjuredDeterminingFluent(agent)
noninertial InjuredDeterminingFluent

; physobj is nondeterministically destroyed.
fluent DestroyedDeterminingFluent(physobj)
noninertial DestroyedDeterminingFluent

; physobj is nondeterministically damaged.
fluent DamagedDeterminingFluent(physobj)
noninertial DamagedDeterminingFluent

; agent activates bomb.
event BombActivate(agent,bomb)

; agent deactivates bomb.
event BombDeactivate(agent,bomb)

; bomb explodes.
event BombExplode(bomb)

; bomb is activated.
fluent BombActivated(bomb)

; The timer value of bomb is offset.
fluent BombTimerValue(bomb,offset)

; The timer value of bomb is decremented.
event BombDecrementTimer(bomb)

; The time delay of bomb is offset.
function BombTimeDelay(bomb): offset

; A state constraint says that a bomb has one timer
; value at a time:
[bomb,offset1,offset2,time]
HoldsAt(BombTimerValue(bomb,offset1),time) &
HoldsAt(BombTimerValue(bomb,offset2),time) ->
offset1=offset2.

; An effect axiom states that if a bomb is intact and
; an agent activates the bomb,
; the bomb will be activated:
[agent,bomb,time]
HoldsAt(Intact(bomb),time) ->
Initiates(BombActivate(agent,bomb),
          BombActivated(bomb),
          time).

; A precondition axiom states that
; for an agent to activate a bomb,
; the agent must be holding the bomb:
[agent,bomb,time]
Happens(BombActivate(agent,bomb),time) ->
HoldsAt(Holding(agent,bomb),time).

; An effect axiom states that if a bomb is intact and
; an agent deactivates the bomb,
; the bomb will no longer be activated:
[agent,bomb,time]
HoldsAt(Intact(bomb),time) ->
Terminates(BombDeactivate(agent,bomb),
           BombActivated(bomb),
           time).

; An axiom states that if a bomb explodes, the
; bomb destroys the bomb:
[bomb,time]
Happens(BombExplode(bomb),time) ->
Happens(Destroy(bomb,bomb),time).

; An effect axiom states that if a bomb explodes,
; the bomb is no longer activated:
[bomb,time]
Terminates(BombExplode(bomb),BombActivated(bomb),time).

; A trigger axiom states that
; if a bomb is activated,
; the timer value of the bomb is a timer value, and
; the timer value is greater than zero,
; the timer value of the bomb will be decremented:
[bomb,offset,time]
HoldsAt(BombActivated(bomb),time) &
HoldsAt(BombTimerValue(bomb,offset),time) &
(offset > 0) ->
Happens(BombDecrementTimer(bomb),time).

; An effect axiom states that
; if the timer value of the bomb is a timer value and
; the timer value of the bomb is decremented,
; the timer value of the bomb will be the timer value minus one:
[bomb,offset1,offset2,time]
HoldsAt(BombTimerValue(bomb,offset1),time) &
offset2 = offset1-1 ->
Initiates(BombDecrementTimer(bomb),
          BombTimerValue(bomb,offset2),
          time).

; An effect axiom states that
; if the timer value of the bomb is a timer value and
; the timer value of the bomb is decremented,
; the timer value of the bomb will no longer be the timer value:
[bomb,offset,time]
HoldsAt(BombTimerValue(bomb,offset),time) ->
Terminates(BombDecrementTimer(bomb),
           BombTimerValue(bomb,offset),
           time).

; An effect axiom states that if a bomb explodes,
; the bomb will no longer be activated:
[bomb,time]
Terminates(BombExplode(bomb),BombActivated(bomb),time).

; A trigger axiom states that if the timer value
; of a bomb is zero, the bomb will explode:
[bomb,time]
HoldsAt(BombTimerValue(bomb,0),time) ->
Happens(BombExplode(bomb),time).

; An axiom states that if an agent is at a location,
; a bomb is at the location,
; the agent is nondeterministically injured, and
; the bomb explodes, then
; the bomb will injure the agent:
[agent,location,bomb,time]
HoldsAt(At(agent,location),time) &
HoldsAt(At(bomb,location),time) &
HoldsAt(InjuredDeterminingFluent(agent),time) &
Happens(BombExplode(bomb),time) ->
Happens(Injure(bomb,agent),time).

; An axiom states that if an agent is at a location,
; a bomb is at the location,
; the agent is nondeterministically killed, and
; the bomb explodes, then
; the bomb will kill the agent:
[agent,location,bomb,time]
HoldsAt(At(agent,location),time) &
HoldsAt(At(bomb,location),time) &
HoldsAt(KilledDeterminingFluent(agent),time) &
Happens(BombExplode(bomb),time) ->
Happens(Kill(bomb,agent),time).

; An axiom states that if an physical object is at a location,
; a bomb is at the location,
; the physical object is nondeterministically damaged, and
; the bomb explodes, then
; the bomb will damage the physical object:
[physobj,location,bomb,time]
HoldsAt(At(physobj,location),time) &
HoldsAt(At(bomb,location),time) &
HoldsAt(DamagedDeterminingFluent(physobj),time) &
Happens(BombExplode(bomb),time) ->
Happens(Damage(bomb,physobj),time).

; An axiom states that if an physical object is at a location,
; a bomb is at the location,
; the physical object is nondeterministically destroyed, and
; the bomb explodes, then
; the bomb will destroy the physical object:
[physobj,location,bomb,time]
HoldsAt(At(physobj,location),time) &
HoldsAt(At(bomb,location),time) &
HoldsAt(DestroyedDeterminingFluent(physobj),time) &
Happens(BombExplode(bomb),time) ->
Happens(Destroy(bomb,physobj),time).

; End of file.
