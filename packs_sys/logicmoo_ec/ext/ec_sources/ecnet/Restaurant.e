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
sort restaurant: script
sort waiter: agent
sort cook: agent

function BillOf(restaurant): bill
function CookOf(restaurant): cook
function TableOf(restaurant): table
function WaiterOf(restaurant): waiter
function KitchenDoorOf(restaurant): door

; awaiting customer/waiter has set down bill on customer's table
fluent BeWaiter0(waiter)

; awaiting customer order
fluent BeWaiter1(waiter)

; has customer order
fluent BeWaiter2(waiter)

; in kitchen
fluent BeWaiter3(waiter)

; awaiting preparation of order
fluent BeWaiter4(waiter)

; has order
fluent BeWaiter5(waiter)

; back in dining room
fluent BeWaiter6(waiter)

; order delivered to customer (can ask if all is OK)
fluent BeWaiter7(waiter)

; customer has requested bill
fluent BeWaiter8(waiter)

; waiter is holding bill
fluent BeWaiter9(waiter)

xor BeWaiter0, BeWaiter1, BeWaiter2, BeWaiter3, BeWaiter4, BeWaiter5, BeWaiter6, BeWaiter7, BeWaiter8, BeWaiter9

[waiter,agent,time]
HoldsAt(BeWaiter0(waiter),time) ->
Terminates(Greet(waiter,agent),
           BeWaiter0(waiter),
           time).

[waiter,agent,time]
HoldsAt(BeWaiter0(waiter),time) ->
Initiates(Greet(waiter,agent),
          BeWaiter1(waiter),
          time).

[waiter,agent,food,time]
HoldsAt(BeWaiter1(waiter),time) ->
Terminates(Order(agent,waiter,food),
           BeWaiter1(waiter),
           time).

[waiter,agent,food,time]
HoldsAt(BeWaiter1(waiter),time) ->
Initiates(Order(agent,waiter,food),
          BeWaiter2(waiter),
          time).

[restaurant,waiter,time]
WaiterOf(restaurant)=waiter &
HoldsAt(BeWaiter2(waiter),time) ->
Happens(WalkThroughDoor12(waiter,KitchenDoorOf(restaurant)),time).

[restaurant,waiter,door,time]
HoldsAt(BeWaiter2(waiter),time) &
WaiterOf(restaurant)=waiter &
KitchenDoorOf(restaurant)=door ->
Terminates(WalkThroughDoor12(waiter,door),
           BeWaiter2(waiter),
           time).

[restaurant,waiter,door,time]
HoldsAt(BeWaiter2(waiter),time) &
WaiterOf(restaurant)=waiter &
KitchenDoorOf(restaurant)=door ->
Initiates(WalkThroughDoor12(waiter,door),
          BeWaiter3(waiter),
          time).

[restaurant,food,time]
HoldsAt(BeWaiter3(WaiterOf(restaurant)),time) &
({agent} HoldsAt(KnowOrder(WaiterOf(restaurant),agent,food),time)) ->
Happens(Order(WaiterOf(restaurant),CookOf(restaurant),food),time).

[restaurant,waiter,cook,food,time]
WaiterOf(restaurant)=waiter &
CookOf(restaurant)=cook &
HoldsAt(BeWaiter3(waiter),time) ->
Terminates(Order(waiter,cook,food),
           BeWaiter3(waiter),
           time).

[restaurant,waiter,cook,food,time]
WaiterOf(restaurant)=waiter &
CookOf(restaurant)=cook &
HoldsAt(BeWaiter3(waiter),time) ->
Initiates(Order(waiter,cook,food),
          BeWaiter4(waiter),
          time).

[waiter,food,time]
HoldsAt(BeWaiter4(waiter),time) &
({agent} HoldsAt(KnowOrder(waiter,agent,food),time)) &
HoldsAt(FoodPrepared(food),time) ->
Happens(PickUp(waiter,food),time).

[waiter,food,time]
HoldsAt(BeWaiter4(waiter),time) &
({agent} HoldsAt(KnowOrder(waiter,agent,food),time)) ->
Terminates(PickUp(waiter,food),
           BeWaiter4(waiter),
           time).

[waiter,food,time]
HoldsAt(BeWaiter4(waiter),time) &
({agent} HoldsAt(KnowOrder(waiter,agent,food),time)) ->
Initiates(PickUp(waiter,food),
          BeWaiter5(waiter),
          time).

[restaurant,waiter,time]
WaiterOf(restaurant)=waiter &
HoldsAt(BeWaiter5(waiter),time) ->
Happens(WalkThroughDoor21(waiter,KitchenDoorOf(restaurant)),time).

[restaurant,waiter,door,time]
HoldsAt(BeWaiter5(waiter),time) &
WaiterOf(restaurant)=waiter &
KitchenDoorOf(restaurant)=door ->
Terminates(WalkThroughDoor21(waiter,door),
           BeWaiter5(waiter),
           time).

[restaurant,waiter,door,time]
HoldsAt(BeWaiter5(waiter),time) &
WaiterOf(restaurant)=waiter &
KitchenDoorOf(restaurant)=door ->
Initiates(WalkThroughDoor21(waiter,door),
          BeWaiter6(waiter),
          time).

[restaurant,waiter,table,food,time]
WaiterOf(restaurant)=waiter &
TableOf(restaurant)=table &
HoldsAt(BeWaiter6(waiter),time) &
HoldsAt(Holding(waiter,food),time) ->
Happens(PlaceOn(waiter,food,table),time).

[waiter,food,table,time]
HoldsAt(BeWaiter6(waiter),time) ->
Terminates(PlaceOn(waiter,food,table),
           BeWaiter6(waiter),
           time).

[waiter,food,table,time]
HoldsAt(BeWaiter6(waiter),time) ->
Initiates(PlaceOn(waiter,food,table),
          BeWaiter7(waiter),
          time).

[waiter,agent,bill,time]
HoldsAt(BeWaiter7(waiter),time) ->
Terminates(Request(agent,waiter,bill),
           BeWaiter7(waiter),
           time).

[waiter,agent,bill,time]
HoldsAt(BeWaiter7(waiter),time) ->
Initiates(Request(agent,waiter,bill),
          BeWaiter8(waiter),
          time).

[restaurant,waiter,bill,time]
WaiterOf(restaurant)=waiter &
BillOf(restaurant)=bill &
HoldsAt(BeWaiter8(waiter),time) ->
Happens(PickUp(waiter,bill),time).

[waiter,bill,time]
HoldsAt(BeWaiter8(waiter),time) ->
Terminates(PickUp(waiter,bill),
           BeWaiter8(waiter),
           time).

[waiter,bill,time]
HoldsAt(BeWaiter8(waiter),time) ->
Initiates(PickUp(waiter,bill),
          BeWaiter9(waiter),
          time).

[restaurant,waiter,bill,table,time]
WaiterOf(restaurant)=waiter &
BillOf(restaurant)=bill &
TableOf(restaurant)=table &
HoldsAt(BeWaiter9(waiter),time) ->
Happens(PlaceOn(waiter,bill,table),time).

[waiter,bill,table,time]
HoldsAt(BeWaiter9(waiter),time) ->
Terminates(PlaceOn(waiter,bill,table),
           BeWaiter9(waiter),
           time).

[waiter,bill,table,time]
HoldsAt(BeWaiter9(waiter),time) ->
Initiates(PlaceOn(waiter,bill,table),
          BeWaiter0(waiter),
          time).

; awaiting next waiter order
fluent BeCook0(cook)

; waiter order received
fluent BeCook1(cook)

xor BeCook0, BeCook1

[cook,agent,food,time]
HoldsAt(BeCook0(cook),time) ->
Terminates(Order(agent,cook,food),
           BeCook0(cook),
           time).

[cook,agent,food,time]
HoldsAt(BeCook0(cook),time) ->
Initiates(Order(agent,cook,food),
          BeCook1(cook),
          time).

event FoodPrepare(agent,food)

fluent FoodPrepared(food)

[agent,food,time]
Initiates(FoodPrepare(agent,food),
          FoodPrepared(food),
          time).

[agent,food,time]
Happens(FoodPrepare(agent,food),time) ->
{location}
HoldsAt(At(agent,location),time) &
HoldsAt(At(food,location),time).

[cook,agent,food,time]
HoldsAt(BeCook1(cook),time) &
HoldsAt(KnowOrder(cook,agent,food),time) ->
Happens(FoodPrepare(cook,food),time).

[cook,food,time]
HoldsAt(BeCook1(cook),time) ->
Terminates(FoodPrepare(cook,food),
           BeCook1(cook),
           time).

[cook,food,time]
HoldsAt(BeCook1(cook),time) ->
Initiates(FoodPrepare(cook,food),
          BeCook0(cook),
          time).

; End of file.
