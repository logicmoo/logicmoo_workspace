; RussianTurkey4-ea.e

load foundations/Root.e
load foundations/EC.e

event Load()
event Shoot()
event Spin()
fluent Loaded()
fluent Alive()

[time](Initiates(Load(),Loaded(),time)).
[time](HoldsAt(Loaded(),time) -> Terminates(Shoot(),Alive(),time)).
[time](Releases(Spin(),Loaded(),time)).
[time](Terminates(Shoot(),Loaded(),time)).

!ReleasedAt(Loaded(), 0).
!ReleasedAt(Alive(), 0).

HoldsAt(Alive(),0).
!HoldsAt(Loaded(),0).
Happens(Load(),1).
Happens(Spin(),2).
Happens(Shoot(),3).

completion Happens

range time 0 4
range offset 1 1

; End of file.
