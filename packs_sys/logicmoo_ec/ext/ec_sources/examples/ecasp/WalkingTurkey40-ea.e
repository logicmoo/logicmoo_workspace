; WalkingTurkey40-ea.e
load foundations/Root.e
load foundations/EC.e

fluent Loaded()
fluent Alive()
fluent Walking()
event Load()
event Shoot()

[time]
(Initiates(Load(), Loaded(), time)).

[time]
(HoldsAt(Loaded(),time) ->
 Terminates(Shoot(), Alive(), time)).

; Effect Constraint
[time]
(Terminates(Shoot(), Alive(), time) ->
 Terminates(Shoot(), Walking(), time)).

!ReleasedAt(Loaded(),0).
!ReleasedAt(Alive(),0).
!ReleasedAt(Walking(),0).

HoldsAt(Alive(), 0).
HoldsAt(Loaded(), 0).
HoldsAt(Walking(), 0).

Happens(Shoot(), 25).

!HoldsAt(Walking(),37).

completion Happens

range time 0 40
range offset 1 1

; End of file
