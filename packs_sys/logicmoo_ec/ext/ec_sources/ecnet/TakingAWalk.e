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
ignore LieOn, SitOn, RiseFrom, LyingOn, SittingOn

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2003/Sleep.e
load answers/Mueller2004c/Dress.e
load answers/Mueller2004c/Rain.e

room Origin1

door OriginDoor1

outside OriginOutside1

street StreetFromOriginToMiddle1

outside Middle1

street StreetFromMiddleToDestination1

outside DestinationOutside1

door DestinationDoor1

room Destination1

Side1(OriginDoor1)=OriginOutside1.
Side2(OriginDoor1)=Origin1.

Side1(StreetFromOriginToMiddle1)=OriginOutside1.
Side2(StreetFromOriginToMiddle1)=Middle1.

Side1(StreetFromMiddleToDestination1)=Middle1.
Side2(StreetFromMiddleToDestination1)=DestinationOutside1.

Side1(DestinationDoor1)=DestinationOutside1.
Side2(DestinationDoor1)=Destination1.

agent Walker1

clothing Clothes1

umbrella Umbrella1

; initial state
[agent,object] !HoldsAt(Holding(agent,object),0).
[agent] HoldsAt(Dressed(agent),0).
[agent] HoldsAt(Awake(agent),0).
[agent] HoldsAt(Sleep3(agent),0).
[agent] HoldsAt(Standing(agent),0).
HoldsAt(DoorUnlocked(OriginDoor1),0).
!HoldsAt(DoorIsOpen(OriginDoor1),0).
HoldsAt(DoorUnlocked(DestinationDoor1),0).
!HoldsAt(DoorIsOpen(DestinationDoor1),0).
HoldsAt(At(Walker1,Origin1),0).
HoldsAt(At(Clothes1,Origin1),0).
HoldsAt(At(Umbrella1,Origin1),0).
[outside,time] HoldsAt(Raining(outside),time).
[object] !HoldsAt(Wet(object),0).

; narrative
Happens(PutOn(Walker1,Clothes1),0).
Happens(PickUp(Walker1,Umbrella1),1).
Happens(DoorOpen(Walker1,OriginDoor1),2).
Happens(WalkThroughDoor21(Walker1,OriginDoor1),3).
Happens(DoorClose(Walker1,OriginDoor1),4).
Happens(WalkStreet12(Walker1,StreetFromOriginToMiddle1),5).
Happens(WalkStreet12(Walker1,StreetFromMiddleToDestination1),6).
Happens(DoorOpen(Walker1,DestinationDoor1),7).
Happens(WalkThroughDoor12(Walker1,DestinationDoor1),8).
Happens(DoorClose(Walker1,DestinationDoor1),9).
Happens(TakeOff(Walker1,Clothes1),10).

range time 0 11
range offset 0 0
range diameter 0 0

completion Happens

; End of file.
