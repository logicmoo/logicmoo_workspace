#domain time(Time), offset(Offset), object(Object), agent(Agent), height(Height;Height1;Height2).

time(0..25).
height(0..18).
offset(1..18).

object(apple).
agent(nathan).


fluent(falling(Object)).
fluent(height(Object,Height)).

event(drop(Agent,Object)).
event(hitGround(Object)).



initiates(drop(Agent,Object),falling(Object),Time).

terminates(hitGround(Object),falling(Object),Time).

happens(hitGround(Object),Time) :- holdsAt(falling(Object),Time), holdsAt(height(Object,0),Time).

happens(drop(nathan,apple),0).

:- holdsAt(height(Object,Height1),Time), holdsAt(height(Object,Height2),Time), Height1!=Height2.

trajectory(falling(Object),Time,height(Object,Height2),Offset) :- holdsAt(height(Object,Height1),Time), Height2==Height1-Offset.

antiTrajectory(falling(Object),Time,height(Object,Height),Offset) :- holdsAt(height(Object,Height),Time).

:- holdsAt(falling(apple),0).

holdsAt(height(apple,18),0).

:- releasedAt(falling(Object),0).

releasedAt(height(Object,Height),0).

hide.

show holdsAt(F,T), happens(E,T), happens3(E,T,T2).
