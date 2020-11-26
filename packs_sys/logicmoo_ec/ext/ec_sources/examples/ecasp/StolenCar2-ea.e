; StolenCar2-ea.e

load foundations/Root.e
load foundations/EC.e

event Park()
event Steal()
fluent CarParked()

[time](Initiates(Park(),CarParked(),time)).
[time](Terminates(Steal(),CarParked(),time)).


!ReleasedAt(CarParked(),0).

!HoldsAt(CarParked(),0).
Happens(Park(),0).
; ABDUCED Happens(Steal(), 1).
!HoldsAt(CarParked(),2).

range time 0 2
range offset 1 1

; End of file.
