#domain time(Time), offset(Offset), balloon(Balloon), agent(Agent), height(Height;Height1;Height2).

time(0..40).
height(0..20).
offset(1..20).

balloon(balloon).
agent(nathan).


fluent(heaterOn(Balloon)).
fluent(height(Balloon,Height)).

event(turnOnHeater(Agent,Balloon)).
event(turnOffHeater(Agent,Balloon)).



initiates(turnOnHeater(Agent,Balloon),heaterOn(Balloon),Time).

terminates(turnOffHeater(Agent,Balloon),heaterOn(Balloon),Time).

happens(turnOnHeater(nathan,balloon),0).

happens(turnOffHeater(nathan,balloon),20).

:- holdsAt(height(Balloon,Height1),Time), holdsAt(height(Balloon,Height2),Time), Height1!=Height2.

trajectory(heaterOn(Balloon),Time,height(Balloon,Height2),Offset) :- holdsAt(height(Balloon,Height1),Time), Height2==Height1+Offset.

antiTrajectory(heaterOn(Balloon),Time,height(Balloon,Height2),Offset) :- holdsAt(height(Balloon,Height1),Time), Height2==Height1-Offset.

holdsAt(height(balloon,0),0).

:- holdsAt(heaterOn(balloon),0).

releasedAt(height(Balloon,Height),Time).

:- releasedAt(heaterOn(Balloon),0).

hide.

show holdsAt(F,T), happens(E,T), happens3(E,T,T2).
