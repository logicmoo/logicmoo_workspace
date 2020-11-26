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
