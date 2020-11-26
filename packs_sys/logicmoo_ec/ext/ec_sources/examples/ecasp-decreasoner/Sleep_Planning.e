load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
event WakeUp(agent)

range time 0 1
range offset 1 1


[agent,time](Initiates(WakeUp(agent),Awake(agent),time)).

agent James

!HoldsAt(Awake(James),0).
HoldsAt(Awake(James),1).
