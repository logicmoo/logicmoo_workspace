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
option renaming off

ignore Love, ThreatenedBy
ignore LookOutOnto, Floor, BuildingOf, SkyOf, GroundOf
ignore Inside, PutInside, On, PlaceOn
ignore Like, Happy, BecomeAngryAt
ignore Near, WalkFrom, WalkFromTo, RunFromTo
ignore ActOnIntentionToWalkIn, IntentionToWalkIn, InvitedIn, InviteIn
ignore TakeOffOf, TakeOutOf, LetGoOf
ignore Greet, SayGoodbye, Order, KnowOrder

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2004c/HandTo.e
load answers/Mueller2004c/Money.e
load answers/Mueller2003/Sleep.e
load answers/Mueller2003/SpeechAct.e
load answers/Mueller2004c/Vehicle.e

outside NearStation1

street Street1

outside Station1

Side1(Street1)=NearStation1.
Side2(Street1)=Station1.

room WaitingRoom1

door Door1

Side1(Door1)=Station1.
Side2(Door1)=WaitingRoom1.

outside BeforeStation1

track TrackToStation1

Side1(TrackToStation1)=BeforeStation1.
Side2(TrackToStation1)=Station1.

outside BetweenStation1And2

track TrackToBetweenStation1And2

Side1(TrackToBetweenStation1And2)=Station1.
Side2(TrackToBetweenStation1And2)=BetweenStation1And2.

outside Station2
track TrackToStation2

Side1(TrackToStation2)=BetweenStation1And2.
Side2(TrackToStation2)=Station2.

outside NearStation2

street Street2

Side1(Street2)=Station2.
Side2(Street2)=NearStation2.

agent Passenger1

agent Conductor1

chair TrainSeat1

train Train1

chair WaitingRoomSeat1

ticketagent TicketAgent1

ticket Ticket1

; prune
sort requesta, requestb, requestc
event! Request(requesta,requestb,requestc)
fluent! KnowRequest(requestb,requesta,requestc)
requesta! Passenger1
requestb! TicketAgent1
requestc! Ticket1

sort holda, holdb
event! TakeOffOf(holda,holdb,holdc)
event! PickUp(holda,holdb)
event! LetGoOf(holda,holdb)
event! Hold(holda,holdb)
fluent! Holding(holda,holdb)
event! HandTo(holda,holda,holdb)
holda! TicketAgent1, Passenger1, Conductor1
holdb! Ticket1

sort sita, sitb
event! LieOn(sita,sitb)
event! SitOn(sita,sitb)
event! RiseFrom(sita,sitb)
fluent! LyingOn(sita,sitb)
fluent! SittingOn(sita,sitb)
sita! Passenger1
sitb! WaitingRoomSeat1, TrainSeat1

; release
option manualrelease on
[ball, diameter] !ReleasedAt(Diameter(ball, diameter), 0).
[agent, object] !ReleasedAt(Holding(agent, object), 0).
[door] !ReleasedAt(DoorUnlocked(door), 0).
[door] !ReleasedAt(DoorIsOpen(door), 0).
[agent] !ReleasedAt(Sleep0(agent), 0).
[agent] !ReleasedAt(Sleep1(agent), 0).
[agent] !ReleasedAt(Sleep2(agent), 0).
[agent] !ReleasedAt(Sleep3(agent), 0).
[agent] !ReleasedAt(Sleep4(agent), 0).
[agent] !ReleasedAt(Sleep5(agent), 0).
[agent] !ReleasedAt(Sleep6(agent), 0).
[agent, physobj] !ReleasedAt(LyingOn(agent, physobj), 0).
[agent, physobj] !ReleasedAt(SittingOn(agent, physobj), 0).
[agent] !ReleasedAt(Standing(agent), 0).
[agent] !ReleasedAt(Dressed(agent), 0).
[agent1, agent2, physobj] !ReleasedAt(KnowOrder(agent1, agent2, physobj), 0).
[agent1, agent2, physobj] !ReleasedAt(KnowRequest(agent1, agent2, physobj), 0).
[object, vehicleon] !ReleasedAt(OnVehicle(object, vehicleon), 0).
[ticketagent] !ReleasedAt(BeTicketAgent0(ticketagent), 0).
[ticketagent] !ReleasedAt(BeTicketAgent1(ticketagent), 0).
[ticketagent] !ReleasedAt(BeTicketAgent2(ticketagent), 0).
; special cases
[location] ReleasedAt(At(TrainSeat1,location),0).
[location] ReleasedAt(At(Conductor1,location),0).
[object, location]
object!=TrainSeat1 & object!=Conductor1 ->
!ReleasedAt(At(object, location), 0).

; initial state
[agent,object] !HoldsAt(Holding(agent,object),0).
[agent,physobj] !HoldsAt(SittingOn(agent,physobj),0).
[agent,physobj] !HoldsAt(LyingOn(agent,physobj),0).
[physobj1,physobj2]
!HoldsAt(On(physobj1, physobj2),0).
[agent] HoldsAt(Dressed(agent),0).
[agent] HoldsAt(Awake(agent),0).
[agent] HoldsAt(Sleep3(agent),0).
[agent] HoldsAt(Standing(agent),0).
HoldsAt(DoorUnlocked(Door1),0).
HoldsAt(DoorIsOpen(Door1),0).
HoldsAt(At(WaitingRoomSeat1,WaitingRoom1),0).
HoldsAt(At(Passenger1,NearStation1),0).
HoldsAt(At(Train1,BeforeStation1),0).
HoldsAt(OnVehicle(TrainSeat1,Train1),0).
HoldsAt(OnVehicle(Conductor1,Train1),0).
[object]
object!=TrainSeat1 & object!=Conductor1 ->
!HoldsAt(OnVehicle(object,Train1),0).
HoldsAt(At(Ticket1,WaitingRoom1),0).
HoldsAt(At(TicketAgent1,WaitingRoom1),0).
HoldsAt(BeTicketAgent0(TicketAgent1),0).
[agent1,agent2,physobj] !HoldsAt(KnowOrder(agent1,agent2,physobj),0).
[agent1,agent2,physobj] !HoldsAt(KnowRequest(agent1,agent2,physobj),0).

; narrative
Happens(WalkStreet12(Passenger1,Street1),0).
Happens(WalkThroughDoor12(Passenger1,Door1),1).
Happens(Request(Passenger1,TicketAgent1,Ticket1),2).
Happens(Pay(Passenger1,TicketAgent1),3).
;TRIGGERED Happens(PickUp(TicketAgent1,Ticket1),3).
;TRIGGERED Happens(HandTo(TicketAgent1,Passenger1,Ticket1),4).
Happens(SitOn(Passenger1,WaitingRoomSeat1),5).
Happens(RideTrack12(Train1,TrackToStation1),6).
Happens(RiseFrom(Passenger1,WaitingRoomSeat1),7).
Happens(WalkThroughDoor21(Passenger1,Door1),8).
Happens(GetOnVehicle(Passenger1,Train1),9).
Happens(SitOn(Passenger1,TrainSeat1),10).
Happens(RideTrack12(Train1,TrackToBetweenStation1And2),11).
Happens(HandTo(Passenger1,Conductor1,Ticket1),12).
;OR PAY
Happens(RideTrack12(Train1,TrackToStation2),13).
Happens(RiseFrom(Passenger1,TrainSeat1),14).
Happens(GetOffVehicle(Passenger1,Train1),15).
Happens(WalkStreet12(Passenger1,Street2),16).

range time 0 17
range offset 0 0
range diameter 0 0

completion Happens

; End of file.
