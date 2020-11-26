; StuffyRoom40-ea.e

load foundations/Root.e
load foundations/EC.e

event Close1()
event Close2()
event Start()
fluent Blocked1()
fluent Blocked2()
fluent Stuffy()

[time](Initiates(Close1(),Blocked1(),time)).
[time](Initiates(Close2(),Blocked2(),time)).

[time]
(HoldsAt(Stuffy(),time) <->
HoldsAt(Blocked1(),time)&HoldsAt(Blocked2(),time)).

[time](Initiates(Start(),Blocked1(),time)).
[time](Terminates(Start(),Blocked2(),time)).


;noninertial Stuffy
[time](ReleasedAt(Stuffy(),time)).

!ReleasedAt(Blocked1(),0).
!ReleasedAt(Blocked2(),0).


!HoldsAt(Blocked1(),0).
!HoldsAt(Blocked2(),0).
Happens(Start(),10).
Happens(Close2(),20).

completion Happens

range time 0 40
range offset 1 1

; End of file.
