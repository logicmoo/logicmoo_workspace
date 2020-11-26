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
; @article{Mueller:InPress,
;   author = "Erik T. Mueller",
;   year = "in press",
;   title = "Modelling space and time in narratives about restaurants",
;   journal = "Literary and Linguistic Computing",
; }
;

option renaming off
option encoding 3

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/MuellerInPress/RepRest.e

door MainEntrance1

; room-scale topological space
outside Street1
room DiningRoom1
door KitchenDoor1
room Kitchen1
Side1(MainEntrance1)=Street1.
Side2(MainEntrance1)=DiningRoom1.
Side1(KitchenDoor1)=DiningRoom1.
Side2(KitchenDoor1)=Kitchen1.

agent Customer1
menu Menu1
chair Chair1
food Food1
HoldsAt(At(Customer1,Street1),0).
HoldsAt(Hungry(Customer1),0).
HoldsAt(At(Chair1,DiningRoom1),0).
HoldsAt(At(Menu1,DiningRoom1),0).
HoldsAt(On(Menu1,Table1),0).
HoldsAt(At(Food1,Kitchen1),0).

waiter Waiter1
cook Cook1

; props
table Table1
bill Bill1

; restaurant
restaurant Restaurant1
CookOf(Restaurant1)=Cook1.
TableOf(Restaurant1)=Table1.
WaiterOf(Restaurant1)=Waiter1.
KitchenDoorOf(Restaurant1)=KitchenDoor1.
BillOf(Restaurant1)=Bill1.

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

sort greeta, greetb
event! Greet(greeta,greetb)

ona! Menu1, Food1, Bill1
onb! Table1
ordera! Customer1, Waiter1
orderb! Waiter1, Cook1
orderc! Food1
requesta! Customer1
requestb! Waiter1
requestc! Bill1
holda! Customer1, Waiter1
holdb! Menu1, Food1, Bill1
holdc! Table1
sita! Customer1
sitb! Chair1
greeta! Customer1, Waiter1
greetb! Customer1, Waiter1

; initial situation
HoldsAt(At(Waiter1,DiningRoom1),0).
HoldsAt(At(Cook1,Kitchen1),0).
HoldsAt(At(Table1,DiningRoom1),0).
!HoldsAt(On(Bill1,Table1),0).
HoldsAt(At(Bill1,DiningRoom1),0).
[agent] HoldsAt(Standing(agent),0).
[agent,object] !HoldsAt(Holding(agent,object),0).
[agent1,agent2,physobj] !HoldsAt(KnowOrder(agent1,agent2,physobj),0).
[agent1,agent2,physobj] !HoldsAt(KnowRequest(agent1,agent2,physobj),0).
HoldsAt(BeWaiter0(Waiter1),0).
HoldsAt(BeCook0(Cook1),0).
[food] !HoldsAt(FoodPrepared(food),0).
!HoldsAt(Hungry(Cook1),0).
!HoldsAt(Hungry(Waiter1),0).

Happens(WalkThroughDoor12(Customer1,MainEntrance1),0).
Happens(Greet(Waiter1,Customer1),1).
Happens(SitOn(Customer1,Chair1),2).
Happens(TakeOffOf(Customer1,Menu1,Table1),3).
Happens(Order(Customer1,Waiter1,Food1),4).
Happens(PlaceOn(Customer1,Menu1,Table1),5).
Happens(Eat(Customer1,Food1),11).
Happens(Request(Customer1,Waiter1,Bill1),12).
Happens(Pay(Customer1,Waiter1),15).
Happens(Tip(Customer1,Waiter1),15).
Happens(RiseFrom(Customer1,Chair1),16).
Happens(SayGoodbye(Customer1,Waiter1),17).
Happens(WalkThroughDoor21(Customer1,MainEntrance1),18).

range time 0 19
range offset 0 0
range diameter 0 0

completion Happens

; End of file.
