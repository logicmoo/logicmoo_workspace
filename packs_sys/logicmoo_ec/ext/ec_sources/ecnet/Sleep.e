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
