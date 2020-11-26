#domain time(Time), agent(Agent).

time(0..maxstep).
offset(1..1).

agent(john).


fluent(awake(Agent)).

event(wakeUp(Agent)).
event(fallAsleep(Agent)).


initiates(wakeUp(Agent),awake(Agent),Time).

terminates(fallAsleep(Agent),awake(Agent),Time).

holdsAt(awake(john),0).

happens(fallAsleep(john),1).

happens(wakeUp(john),2).

:- releasedAt(F,0), fluent(F).

hide.

show holdsAt(F,T), releasedAt(F,T), happens(E,T).
