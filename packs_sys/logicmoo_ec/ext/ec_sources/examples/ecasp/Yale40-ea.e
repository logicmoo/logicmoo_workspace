; Yale40-ea.e
option showpred off

load foundations/Root.e
load foundations/EC.e

event Load()
event Shoot()
event Sneeze()
fluent Loaded()
fluent Alive()

[time](Initiates(Load(),Loaded(),time)).
[time](HoldsAt(Loaded(),time) -> Terminates(Shoot(),Alive(),time)).
[time](Terminates(Shoot(),Loaded(),time)).


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
