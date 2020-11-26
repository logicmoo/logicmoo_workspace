load foundations/Root.e
load foundations/EC.e

sort agent
agent James

fluent Awake(agent)
event WakeUp(agent)

range time 0 1
range offset 1 1


[agent,time](Initiates(WakeUp(agent),Awake(agent),time)).
[agent,time](Happens(WakeUp(agent),time)->
!HoldsAt(Awake(agent),time)).

Happens(WakeUp(James),0).
HoldsAt(Awake(James),1).


completion Happens
