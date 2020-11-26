#domain time(Time), object(Object), agent(Agent), physobj(Physobj), location(Location;Location2;Location1).

time(0..maxstep).
offset(1..1).

agent(james).
physobj(coin).
location(l1).
location(l2).
location(l3).
location(l4).

object(V) :- agent(V).object(V) :- physobj(V).
fluent(at(Object,Location)).
fluent(holding(Agent,Physobj)).

event(pickUp(Agent,Physobj)).
event(setDown(Agent,Physobj)).
event(move(Agent,Location,Location2)).


holdsAt(at(Physobj,Location),Time) :- holdsAt(at(Agent,Location),Time), holdsAt(holding(Agent,Physobj),Time).

:- holdsAt(at(Object,Location1),Time), holdsAt(at(Object,Location2),Time), Location1!=Location2.

initiates(move(Agent,Location1,Location2),at(Agent,Location2),Time).

terminates(move(Agent,Location1,Location2),at(Agent,Location1),Time).

initiates(pickUp(Agent,Physobj),holding(Agent,Physobj),Time).

terminates(setDown(Agent,Physobj),holding(Agent,Physobj),Time).

holdsAt(at(Agent,Location1),Time) :- happens(move(Agent,Location1,Location2),Time).

newPre1(Agent,Physobj,Time) :- holdsAt(at(Agent,Location),Time), holdsAt(at(Physobj,Location),Time).

:- happens(pickUp(Agent,Physobj),Time),not newPre1(Agent,Physobj,Time).

releases(pickUp(Agent,Physobj),at(Physobj,Location),Time).

initiates(setDown(Agent,Physobj),at(Physobj,Location),Time) :- holdsAt(at(Agent,Location),Time).

:- holdsAt(holding(james,coin),0).

holdsAt(at(coin,l4),0).

holdsAt(at(james,l1),0).

happens(move(james,l1,l2),0).

happens(move(james,l2,l3),1).

happens(move(james,l3,l4),2).

happens(pickUp(james,coin),3).

happens(move(james,l4,l3),4).

happens(move(james,l3,l2),5).

happens(setDown(james,coin),6).

happens(move(james,l2,l3),7).

happens(move(james,l3,l4),8).

:- releasedAt(F,0), fluent(F).

hide.

show holdsAt(F,T), releasedAt(F,T), happens(E,T).
