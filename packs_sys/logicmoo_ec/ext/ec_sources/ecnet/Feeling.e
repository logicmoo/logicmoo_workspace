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
; feeling = emotion, attitude, ...
;
; The Feeling representation includes simple positive, neutral, and
; negative emotions, and positive, neutral, and negative attitudes
; toward objects.
;

; emotions

; agent is happy.
fluent Happy(agent)

; agent is emotionally neutral or calm.
fluent Calm(agent)

; agent is unhappy.
fluent Unhappy(agent)

; At any moment, an agent is in one of three emotional states:
xor Happy, Calm, Unhappy

; agent becomes happy.
event BecomeHappy(agent)

; agent becomes calm.
event BecomeCalm(agent)

; agent becomes unhappy.
event BecomeUnhappy(agent)

; A number of effect and precondition axioms deal with the transitions
; from one emotional state to another:
[agent,time]
Initiates(BecomeHappy(agent),Happy(agent),time).

[agent,time]
HoldsAt(Calm(agent),time) ->
Terminates(BecomeHappy(agent),Calm(agent),time).

[agent,time]
HoldsAt(Unhappy(agent),time) ->
Terminates(BecomeHappy(agent),Unhappy(agent),time).

[agent,time]
Happens(BecomeHappy(agent),time) ->
!HoldsAt(Happy(agent),time).

[agent,time]
Initiates(BecomeCalm(agent),Calm(agent),time).

[agent,time]
HoldsAt(Happy(agent),time) ->
Terminates(BecomeCalm(agent),Happy(agent),time).

[agent,time]
HoldsAt(Unhappy(agent),time) ->
Terminates(BecomeCalm(agent),Unhappy(agent),time).

[agent,time]
Happens(BecomeCalm(agent),time) -> !HoldsAt(Calm(agent),time).

[agent,time]
Initiates(BecomeUnhappy(agent),Unhappy(agent),time).

[agent,time]
HoldsAt(Happy(agent),time) ->
Terminates(BecomeUnhappy(agent),Happy(agent),time).

[agent,time]
HoldsAt(Calm(agent),time) ->
Terminates(BecomeUnhappy(agent),Calm(agent),time).

[agent,time]
Happens(BecomeUnhappy(agent),time) -> !HoldsAt(Unhappy(agent),time).

; anger

fluent AngryAt(agent,agent)

event BecomeAngryAt(agent,agent)

[agent1,agent2,time]
Initiates(BecomeAngryAt(agent1,agent2),AngryAt(agent1,agent2),time).

[agent1,agent2,time]
Terminates(BecomeHappy(agent1),AngryAt(agent1,agent2),time).

[agent1,agent2,time]
Happens(BecomeAngryAt(agent1,agent2),time) ->
Happens(BecomeUnhappy(agent1),time).

; attitudes

; agent likes object.
fluent Like(agent,object)
; agent loves object.
fluent Love(agent,object)
; agent dislikes object.
fluent Dislike(agent,object)

; agent likes snow.
fluent LikeSnow(agent)

; A trigger axiom states that
; if an agent is awake, likes snow, and is in a room that
; looks out onto a location where it is snowing, that agent
; becomes happy:
[agent,room,outside,time]
!HoldsAt(Happy(agent),time) &
HoldsAt(Awake(agent),time) &
HoldsAt(LikeSnow(agent),time) &
HoldsAt(At(agent,room),time) &
LookOutOnto(room)=outside &
HoldsAt(Snowing(outside),time) ->
Happens(BecomeHappy(agent),time).

; We introduced LikeSnow above since Like
; can only be used to represent that an agent likes a
; particular object, not snow in general.

event Smile(agent)

; End of file.
