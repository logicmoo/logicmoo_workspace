; DeadOrAlive3-ea.e

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
Happens(Load(),0).
Happens(Sneeze(),1).
Happens(Shoot(),2).

completion Happens

range time 0 3
range offset 1 1

; End of file.
