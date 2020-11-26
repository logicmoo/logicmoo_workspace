#domain time(Time), agent(Agent).

time(0..maxstep).
offset(1..1).

agent(james).


fluent(awake(Agent)).

event(wakeUp(Agent)).


initiates(wakeUp(Agent),awake(Agent),Time).

:- happens(wakeUp(Agent),Time), holdsAt(awake(Agent),Time).

happens(wakeUp(james),0).

holdsAt(awake(james),1).

:- releasedAt(F,0), fluent(F).

hide.

show holdsAt(F,T), releasedAt(F,T), happens(E,T).
