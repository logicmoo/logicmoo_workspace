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
option encoding 3
option renaming off

ignore Love, ThreatenedBy
ignore LookOutOnto, Floor, BuildingOf, SkyOf, GroundOf
ignore Inside
ignore Near, WalkFrom, WalkFromTo, RunFromTo
ignore BillOf, CookOf, TableOf, WaiterOf, KitchenDoorOf
ignore BeWaiter0, BeWaiter1, BeWaiter2, BeWaiter3, BeWaiter4
ignore BeWaiter5, BeWaiter6, BeWaiter7, BeWaiter8, BeWaiter9
ignore BeCook0, BeCook1
ignore InviteIn, InvitedIn, IntendToWalkIn, IntentionToWalkIn
ignore ActOnIntentionToWalkIn, Greet, SayGoodbye, CryForJoy
ignore Threaten, ReleaseFromThreat, ThreatenedBy
ignore Order, KnowOrder, Request, KnowRequest
ignore PutInside, TakeOutOf
ignore SayPleaseToMeet, Move

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/HungerNeed.e
load answers/Mueller2004c/Restaurant.e
load answers/Mueller2003/Sleep.e
load answers/Mueller2003/SpeechAct.e
load answers/Mueller2004c/Dress.e

room Upstairs1

staircase Staircase1

room Hallway1

Side1(Staircase1)=Hallway1.
Side2(Staircase1)=Upstairs1.

door DiningRoomDoor1

room DiningRoom1

Side1(DiningRoomDoor1)=Hallway1.
Side2(DiningRoomDoor1)=DiningRoom1.

door KitchenDoor1

room Kitchen1

Side1(KitchenDoor1)=DiningRoom1.
Side2(KitchenDoor1)=Kitchen1.

agent Eater1

agent Eater2

clothing Clothing1

clothing Clothing2

chair Chair1

chair Chair2

food Food1

agent Cook1

table Table1

content Content1

content Content2

outside DummyOutside1

; prune
sort ona, onb
fluent! On(ona,onb)
event! PlaceOn(agent,ona,onb)
event! TakeOffOf(agent,ona,onb)

sort ordera, orderb, orderc
event! Order(ordera,orderb,orderc)
fluent! KnowOrder(orderb,ordera,orderc)

sort requesta, requestb, requestc
event! Request(requesta,requestb,requestc)
fluent! KnowRequest(requestb,requesta,requestc)

sort holda, holdb, holdc
event! TakeOffOf(holda,holdb,holdc)
event! PickUp(holda,holdb)
event! LetGoOf(holda,holdb)
event! Hold(holda,holdb)
fluent! Holding(holda,holdb)

sort sita, sitb
event! LieOn(sita,sitb)
event! SitOn(sita,sitb)
event! RiseFrom(sita,sitb)
fluent! LyingOn(sita,sitb)
fluent! SittingOn(sita,sitb)

ona! Food1
onb! Table1
holda! Cook1
holdb! Food1
holdc! Table1
sita! Eater1
sitb! Chair1

; initial situation
[agent] HoldsAt(Dressed(agent),0).
[agent] HoldsAt(Awake(agent),0).
[agent] HoldsAt(Sleep3(agent),0).
[agent] HoldsAt(Standing(agent),0).
[agent] HoldsAt(Standing(agent),0).
[agent,object] !HoldsAt(Holding(agent,object),0).
[food] HoldsAt(At(food,Kitchen1),0).
[food] !HoldsAt(FoodPrepared(food),0).
[agent] HoldsAt(Hungry(agent),0).
[door] HoldsAt(DoorIsOpen(door),0).
[clothing] HoldsAt(At(clothing,Upstairs1),0).
[chair] HoldsAt(At(chair,DiningRoom1),0).
HoldsAt(At(Cook1,Kitchen1),0).
HoldsAt(At(Table1,DiningRoom1),0).
[agent,clothing] !HoldsAt(Wearing(agent,clothing),0).

; narrative
HoldsAt(At(Cook1,Kitchen1),0).
HoldsAt(At(Eater1,Upstairs1),0).
HoldsAt(At(Eater2,Upstairs1),0).
Happens(FoodPrepare(Cook1,Food1),0).
Happens(PutOn(Eater1,Clothing1),1).
Happens(PutOn(Eater2,Clothing2),2).
Happens(WalkDownStaircase(Eater1,Staircase1),3).
Happens(WalkDownStaircase(Eater2,Staircase1),4).
Happens(WalkThroughDoor12(Eater1,DiningRoomDoor1),5).
Happens(WalkThroughDoor12(Eater2,DiningRoomDoor1),6).
Happens(SitOn(Eater1,Chair1),7).
Happens(SitOn(Eater2,Chair2),8).
Happens(PickUp(Cook1, Food1),9).
Happens(WalkThroughDoor21(Cook1, KitchenDoor1),10).
Happens(PlaceOn(Cook1, Food1, Table1),11).
Happens(WalkThroughDoor12(Cook1, KitchenDoor1),12).
Happens(Eat(Eater1,Food1),13).
Happens(Eat(Eater2,Food1),14).
Happens(Converse(Eater1,Eater2),15).
Happens(TalkAbout(Eater1,Content1),16).
Happens(TalkAbout(Eater2,Content2),17).
Happens(RiseFrom(Eater1,Chair1),18).
Happens(RiseFrom(Eater2,Chair2),19).

range time 0 20
range offset 0 0
range diameter 0 0

completion Happens

; End of file.
