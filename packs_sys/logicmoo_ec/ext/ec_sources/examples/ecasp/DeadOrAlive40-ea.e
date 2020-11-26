; DeadOrAlive40-ea.e

load foundations/Root.e
load foundations/EC.e

event Load()
event Shoot()
event Sneeze()
fluent Loaded()
fluent Alive()
fluent Dead()

[time] 
(Initiates(Load(),Loaded(),time)).

[time] 
(HoldsAt(Loaded(),time) -> Terminates(Shoot(),Alive(),time)).

[time]
(Terminates(Shoot(),Loaded(),time)).

[time] 
(HoldsAt(Dead(),time) <-> !HoldsAt(Alive(),time)).


[time]
(ReleasedAt(Dead(),time)).

!ReleasedAt(Loaded(),0).
!ReleasedAt(Alive(),0).


HoldsAt(Alive(),0).
!HoldsAt(Loaded(),0).
Happens(Load(),10).
Happens(Sneeze(),20).
Happens(Shoot(),35).

completion Happens

range time 0 40
range offset 1 1

; End of file.
