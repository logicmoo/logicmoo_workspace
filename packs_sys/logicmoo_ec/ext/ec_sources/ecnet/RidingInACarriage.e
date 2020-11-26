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
ignore Request, KnowRequest
ignore TakeOffOf, PickUp, LetGoOf, Hold, Holding, HandTo, Grab
ignore Move

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

outside NearLocation1

street Street1

outside Location1

Side1(Street1)=NearLocation1.
Side2(Street1)=Location1.

outside BeforeLocation1

street StreetToLocation1

Side1(StreetToLocation1)=BeforeLocation1.
Side2(StreetToLocation1)=Location1.

outside BetweenLocation1And2

street StreetToBetweenLocation1And2

Side1(StreetToBetweenLocation1And2)=Location1.
Side2(StreetToBetweenLocation1And2)=BetweenLocation1And2.

outside Location2
street StreetToLocation2

Side1(StreetToLocation2)=BetweenLocation1And2.
Side2(StreetToLocation2)=Location2.

outside NearLocation2

street Street2

Side1(Street2)=Location2.
Side2(Street2)=NearLocation2.

agent Passenger1

agent Driver1

chair CarriageSeat1

carriage Carriage1

vehicledoor CarriageDoor1

horse Horse1

; prune
sort sita, sitb
event! LieOn(sita,sitb)
event! SitOn(sita,sitb)
event! RiseFrom(sita,sitb)
fluent! LyingOn(sita,sitb)
fluent! SittingOn(sita,sitb)
sita! Passenger1
sitb! CarriageSeat1

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
[agent1, agent2, physobj] !ReleasedAt(KnowRequest(agent1, agent2, physobj), 0).
[horse, street] !ReleasedAt(PointedToward(horse, street), 0).
[horse, vehicle] !ReleasedAt(HitchedTo(horse, vehicle), 0).
[object, vehicleon] !ReleasedAt(OnVehicle(object, vehicleon), 0).
[object, vehiclein] !ReleasedAt(InVehicle(object, vehiclein), 0).
[vehicledoor] !ReleasedAt(VehicleDoorIsOpen(vehicledoor), 0).
[ticketagent] !ReleasedAt(BeTicketAgent0(ticketagent), 0).
[ticketagent] !ReleasedAt(BeTicketAgent1(ticketagent), 0).
[ticketagent] !ReleasedAt(BeTicketAgent2(ticketagent), 0).
; special cases
[location] ReleasedAt(At(CarriageSeat1,location),0).
[location] ReleasedAt(At(Driver1,location),0).
[object, location]
object!=CarriageSeat1 & object!=Driver1 ->
!ReleasedAt(At(object, location), 0).

; initial state
[agent,object] !HoldsAt(Holding(agent,object),0).
[agent,physobj] !HoldsAt(SittingOn(agent,physobj),0).
[agent,physobj] !HoldsAt(LyingOn(agent,physobj),0).
[physobj1,physobj2] !HoldsAt(On(physobj1, physobj2),0).
[agent] HoldsAt(Dressed(agent),0).
[agent] HoldsAt(Awake(agent),0).
[agent] HoldsAt(Sleep3(agent),0).
[agent] HoldsAt(Standing(agent),0).
HoldsAt(At(Passenger1,NearLocation1),0).
HoldsAt(At(Horse1,BeforeLocation1),0).;;;;;
HoldsAt(At(Carriage1,BeforeLocation1),0).
HoldsAt(InVehicle(CarriageSeat1,Carriage1),0).
HoldsAt(InVehicle(Driver1,Carriage1),0).
[object]
object!=CarriageSeat1 & object!=Driver1 ->
!HoldsAt(InVehicle(object,Carriage1),0).
[agent1,agent2,physobj] !HoldsAt(KnowOrder(agent1,agent2,physobj),0).
[agent1,agent2,physobj] !HoldsAt(KnowRequest(agent1,agent2,physobj),0).
!HoldsAt(VehicleDoorIsOpen(CarriageDoor1),0).
HoldsAt(HitchedTo(Horse1,Carriage1),0).
HoldsAt(PointedToward(Horse1,StreetToLocation1),0).

; narrative
Happens(WalkStreet12(Passenger1,Street1),0).
Happens(Lash(Driver1,Horse1),1).
Happens(VehicleDoorOpen(Passenger1,CarriageDoor1),2).
Happens(GetInVehicle(Passenger1,Carriage1),3).
Happens(VehicleDoorClose(Passenger1,CarriageDoor1),4).
Happens(SitOn(Passenger1,CarriageSeat1),5).
Happens(PointToward(Driver1,Horse1,StreetToBetweenLocation1And2),6).
Happens(Lash(Driver1,Horse1),7).
Happens(PointToward(Driver1,Horse1,StreetToLocation2),8).
Happens(Lash(Driver1,Horse1),9).
Happens(RiseFrom(Passenger1,CarriageSeat1),10).
Happens(VehicleDoorOpen(Passenger1,CarriageDoor1),11).
Happens(GetOutOfVehicle(Passenger1,Carriage1),12).
Happens(VehicleDoorClose(Passenger1,CarriageDoor1),13).
Happens(WalkStreet12(Passenger1,Street2),14).

range time 0 15
range offset 0 0
range diameter 0 0

completion Happens

; End of file.
