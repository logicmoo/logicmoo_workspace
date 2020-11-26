; StolenCar40-ea.e

load foundations/Root.e
load foundations/EC.e

event Park()
event Steal()
fluent CarParked()

[time](Initiates(Park(),CarParked(),time)).
[time](Terminates(Steal(),CarParked(),time)).


!ReleasedAt(CarParked(),0).

!HoldsAt(CarParked(),0).
Happens(Park(),20).
; ABDUCED Happens(Steal(), 1).
!HoldsAt(CarParked(),40).


; Action Precondition axiom added
[time](Happens(Steal(),time) -> HoldsAt(CarParked(),time)).

; Event Occurrence Constraints added
[time1, time2]
(Happens(Steal(), time1) & time1<time2 ->
!Happens(Park(), time2)).

range time 0 40
range offset 1 1

; End of file.
