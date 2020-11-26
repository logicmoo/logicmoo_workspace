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

;sort boolean
;sort integer
;reified sort predicate
;reified sort function
;
;sort time: integer
;sort offset: integer
;
;reified sort fluent
;reified sort event
;
;predicate Happens(event,time)
;predicate HoldsAt(fluent,time)
;predicate ReleasedAt(fluent,time)
;predicate Initiates(event,fluent,time)
;predicate Terminates(event,fluent,time)
;predicate Releases(event,fluent,time)
;
;sort diameter: integer
;
;sort object
;
;sort agent: object
;
;sort physobj: object
;sort bed: physobj
;sort snowflake: physobj
;sort sky: physobj
;
;sort stuff: physobj
;
;sort surface: physobj
;sort ground: surface
;
;sort snow: stuff
;sort ball
;
;sort food: physobj
;sort fruit: food
;sort orange: fruit
;sort salad: food
;
;sort clothing: physobj
;sort scarf: clothing
;sort hat: clothing
;
;sort vegetablematter: physobj
;sort coal: vegetablematter
;
;sort bodypart: physobj
;sort hand: bodypart
;
;sort papertowels: physobj
;sort device: physobj
;sort electronicdevice: device
;sort lamp: electronicdevice
;
;sort cat: physobj
;
;sort weapon: physobj
;sort gun: weapon
;sort bomb: weapon
;sort bullet: weapon
;
;sort location
;sort room: location, outside: location
;
;sort portal
;sort door: portal, staircase: portal
;sort street: portal
;
;sort building
;
;sort fire: object
;
;sort furniture: physobj
;sort chair: furniture
;sort table: furniture
;
;sort menu: physobj
;sort bill: physobj
;
;sort script
;
fluent Holding(agent,physobj)
event PickUp(agent,physobj)
event LetGoOf(agent,physobj)

[agent,physobj,time]
Initiates(PickUp(agent,physobj),Holding(agent,physobj),time).

[agent,physobj,time]
Happens(PickUp(agent,physobj),time) ->
{location}
  HoldsAt(At(agent,location),time) &
  HoldsAt(At(physobj,location),time).

[agent,physobj,time]
Terminates(LetGoOf(agent,physobj),Holding(agent,physobj),time).

[agent,physobj,time]
Happens(LetGoOf(agent,physobj),time) ->
HoldsAt(Holding(agent,physobj),time).

[agent,physobj,location,time]
Releases(PickUp(agent,physobj),At(physobj,location),time).

[agent,physobj,location,time]
HoldsAt(Holding(agent,physobj),time) &
HoldsAt(At(agent,location),time) ->
HoldsAt(At(physobj,location),time).

;[agent,physobj,location1,location2,time]
;HoldsAt(At(agent,location1),time) &
;location1!=location2 ->
;Terminates(LetGoOf(agent,physobj),At(physobj,location2),time).

[agent,physobj,location,time]
HoldsAt(At(agent,location),time) ->
Initiates(LetGoOf(agent,physobj),At(physobj,location),time).

fluent On(physobj,physobj)

event PlaceOn(agent,physobj,physobj)

event TakeOffOf(agent,physobj,physobj)

[physobj1,physobj2,time]
HoldsAt(On(physobj1,physobj2),time) ->
physobj1!=physobj2.

[physobj1,physobj2,time]
HoldsAt(On(physobj1,physobj2),time) ->
!HoldsAt(On(physobj2,physobj1),time).

[agent,physobj1,physobj2,time]
Initiates(PlaceOn(agent,physobj1,physobj2),
          On(physobj1,physobj2),time).

[agent,physobj1,physobj2,time]
Terminates(PlaceOn(agent,physobj1,physobj2),
           Holding(agent,physobj1),time).

[agent,physobj1,physobj2,time]
Happens(PlaceOn(agent,physobj1,physobj2),time) ->
HoldsAt(Holding(agent,physobj1),time) &
{location}
 HoldsAt(At(agent,location),time) &
 HoldsAt(At(physobj2,location),time).

[agent,physobj1,physobj2,time]
Terminates(TakeOffOf(agent,physobj1,physobj2),
           On(physobj1,physobj2),time).

[agent,physobj1,physobj2,time]
Initiates(TakeOffOf(agent,physobj1,physobj2),
          Holding(agent,physobj1),time).

[agent,physobj1,physobj2,location,time]
Releases(TakeOffOf(agent,physobj1,physobj2),
         At(physobj1,location),
         time).

[agent,physobj1,physobj2,time]
Happens(TakeOffOf(agent,physobj1,physobj2),time) ->
HoldsAt(On(physobj1,physobj2),time) &
{location}
 HoldsAt(At(agent,location),time) &
 HoldsAt(At(physobj1,location),time) &
 HoldsAt(At(physobj2,location),time).

[agent,physobj1,physobj2,location,time]
Releases(PlaceOn(agent,physobj1,physobj2),
         At(physobj1,location),
         time).

[physobj1,physobj2,location,time]
HoldsAt(On(physobj1,physobj2),time) &
HoldsAt(At(physobj2,location),time) ->
HoldsAt(At(physobj1,location),time).

fluent At(object,location)

[object,time]
{location} HoldsAt(At(object,location),time).

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
HoldsAt(At(object,location2),time) ->
location1=location2.

function Side1(portal): location
function Side2(portal): location

fluent NearPortal(object,portal)
noninertial NearPortal

[object,portal,time]
HoldsAt(NearPortal(object,portal),time) <->
{location}
 (Side1(portal)=location|
  Side2(portal)=location) &
 HoldsAt(At(object,location),time).

event WalkThroughDoor12(agent,door)
event WalkThroughDoor21(agent,door)

[agent,door,time]
Happens(WalkThroughDoor12(agent,door),time) ->
HoldsAt(Standing(agent),time) &
HoldsAt(At(agent,Side1(door)),time).

[agent,door,time]
Happens(WalkThroughDoor21(agent,door),time) ->
HoldsAt(Standing(agent),time) &
HoldsAt(At(agent,Side2(door)),time).

[agent,door,location,time]
Side2(door)=location ->
Initiates(WalkThroughDoor12(agent,door),At(agent,location),time).

[agent,door,location,time]
Side1(door)=location ->
Initiates(WalkThroughDoor21(agent,door),At(agent,location),time).

[agent,door,location,time]
Side1(door)=location ->
Terminates(WalkThroughDoor12(agent,door),At(agent,location),time).

[agent,door,location,time]
Side2(door)=location ->
Terminates(WalkThroughDoor21(agent,door),At(agent,location),time).

fluent Hungry(agent)

fluent Satiated(agent)
noninertial Satiated

[agent,time] HoldsAt(Hungry(agent),time) <-> !HoldsAt(Satiated(agent),time).

event Eat(agent,food)

[agent,food,time]
Happens(Eat(agent,food),time) ->
{location}
HoldsAt(At(agent,location),time) &
HoldsAt(At(food,location),time).

[agent,food,time]
Terminates(Eat(agent,food),Hungry(agent),time).

sort restaurant: script
sort waiter: agent
sort cook: agent

function BillOf(restaurant): bill
function CookOf(restaurant): cook
function TableOf(restaurant): table
function WaiterOf(restaurant): waiter
function KitchenDoorOf(restaurant): door

fluent BeWaiter0(waiter)

fluent BeWaiter1(waiter)

fluent BeWaiter2(waiter)

fluent BeWaiter3(waiter)

fluent BeWaiter4(waiter)

fluent BeWaiter5(waiter)

fluent BeWaiter6(waiter)

fluent BeWaiter7(waiter)

fluent BeWaiter8(waiter)

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

fluent BeCook0(cook)

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

event Pay(agent,agent)

event Tip(agent,agent)

[agent,physobj,time]
Happens(LieOn(agent,physobj),time) ->
{room}
 HoldsAt(At(agent,room),time) &
 HoldsAt(At(physobj,room),time).

[agent,physobj,time]
Happens(SitOn(agent,physobj),time) ->
{room}
 HoldsAt(At(agent,room),time) &
 HoldsAt(At(physobj,room),time).

event LieOn(agent,physobj)

event SitOn(agent,physobj)

event RiseFrom(agent,physobj)

fluent LyingOn(agent,physobj)
fluent SittingOn(agent,physobj)
fluent Standing(agent)

fluent Lying(agent)
fluent Sitting(agent)
noninertial Lying
noninertial Sitting

xor Lying, Sitting, Standing

[agent,physobj,time]
HoldsAt(LyingOn(agent,physobj),time) ->
HoldsAt(Lying(agent),time).

[agent,physobj,time]
HoldsAt(SittingOn(agent,physobj),time) ->
HoldsAt(Sitting(agent),time).

[agent,physobj1,physobj2,time]
HoldsAt(LyingOn(agent,physobj1),time) &
HoldsAt(LyingOn(agent,physobj2),time) ->
physobj1=physobj2.

[agent,physobj1,physobj2,time]
HoldsAt(SittingOn(agent,physobj1),time) &
HoldsAt(SittingOn(agent,physobj2),time) ->
physobj1=physobj2.

[agent,physobj,time]
HoldsAt(Standing(agent),time) ->
Initiates(LieOn(agent,physobj),
          LyingOn(agent,physobj),
          time).

[agent,physobj,time]
Terminates(LieOn(agent,physobj),
           Standing(agent),
           time).

[agent,physobj,time]
HoldsAt(Standing(agent),time) ->
Initiates(SitOn(agent,physobj),
          SittingOn(agent,physobj),
          time).

[agent,physobj,time]
Terminates(SitOn(agent,physobj),
           Standing(agent),
           time).

[agent,physobj,time]
(HoldsAt(SittingOn(agent,physobj),time) |
 HoldsAt(LyingOn(agent,physobj),time)) ->
Initiates(RiseFrom(agent,physobj),
          Standing(agent),
          time).

[agent,physobj,time]
HoldsAt(LyingOn(agent,physobj),time) ->
Terminates(RiseFrom(agent,physobj),
           LyingOn(agent,physobj),
           time).

[agent,physobj,time]
HoldsAt(SittingOn(agent,physobj),time) ->
Terminates(RiseFrom(agent,physobj),
           SittingOn(agent,physobj),
           time).

event Greet(agent,agent)

event SayGoodbye(agent,agent)

[agent1,agent2,time]
Happens(Greet(agent1,agent2),time) ->
{location}
HoldsAt(At(agent1,location),time) &
HoldsAt(At(agent2,location),time).

[agent1,agent2,time]
Happens(SayGoodbye(agent1,agent2),time) ->
{location}
HoldsAt(At(agent1,location),time) &
HoldsAt(At(agent2,location),time).

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
