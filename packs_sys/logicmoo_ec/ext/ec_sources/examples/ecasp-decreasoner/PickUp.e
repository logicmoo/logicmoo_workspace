load foundations/Root.e
load foundations/DEC.e

sort object
sort agent: object
sort physobj: object
sort location

agent James
physobj Coin
location L1,L2,L3,L4


fluent At(object,location)
fluent Holding(agent,physobj)
event PickUp(agent,physobj)
event SetDown(agent,physobj)
event Move(agent,location,location)

range time 0 9
range offset 1 1

; state constraints

[agent,location,physobj,time]
(HoldsAt(At(agent,location),time) &
HoldsAt(Holding(agent,physobj),time) ->
HoldsAt(At(physobj,location),time)).

[object,location1,location2,time]
(HoldsAt(At(object,location1),time) &
HoldsAt(At(object,location2),time) ->
location1=location2).

; effect axioms

[agent,location1,location2,time]
(Initiates(Move(agent,location1,location2),At(agent,location2),time)).

[agent,location1,location2,time]
(Terminates(Move(agent,location1,location2),At(agent,location1),time)).

[agent,physobj,time]
(Initiates(PickUp(agent,physobj),Holding(agent,physobj),time)).

[agent,physobj,time]
(Terminates(SetDown(agent,physobj),Holding(agent,physobj),time)).

; preconditions

[agent,location1,location2,time]
(Happens(Move(agent,location1,location2),time) ->
HoldsAt(At(agent,location1),time)).

[agent,physobj,time]
(Happens(PickUp(agent,physobj),time) ->
{location} (HoldsAt(At(agent,location),time) &
           HoldsAt(At(physobj,location),time))).

; releases

[agent,physobj,location,time]
(Releases(PickUp(agent,physobj),At(physobj,location),time)).

[agent,physobj,location,time]
(HoldsAt(At(agent,location),time) ->
Initiates(SetDown(agent,physobj),At(physobj,location),time)).


!HoldsAt(Holding(James,Coin),0).
HoldsAt(At(Coin,L4),0).
HoldsAt(At(James,L1),0).
Happens(Move(James,L1,L2),0).
Happens(Move(James,L2,L3),1).
Happens(Move(James,L3,L4),2).
Happens(PickUp(James,Coin),3).
Happens(Move(James,L4,L3),4).
Happens(Move(James,L3,L2),5).
Happens(SetDown(James,Coin),6).
Happens(Move(James,L2,L3),7).
Happens(Move(James,L3,L4),8).


completion Happens

; End of file.
