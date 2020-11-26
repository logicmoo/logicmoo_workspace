load foundations/Root.e
load foundations/DEC.e

sort agent
agent John

fluent Awake(agent)
event WakeUp(agent) 
event FallAsleep(agent)

range time 0 5
range offset 1 1


[agent, time]
(Initiates(WakeUp(agent), Awake(agent), time)).

[agent, time]
(Terminates(FallAsleep(agent), Awake(agent), time)).


HoldsAt(Awake(John), 0).
Happens(FallAsleep(John), 1).
Happens(WakeUp(John), 2).

completion Happens


; End of File
