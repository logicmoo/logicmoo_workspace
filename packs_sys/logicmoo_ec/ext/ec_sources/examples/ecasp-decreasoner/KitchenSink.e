load foundations/Root.e
load foundations/EC.e

sort height: integer

event TapOn()
event TapOff()
event Overflow()

fluent Filling()
fluent HasHeight(height)
fluent Spilling()

range time 0 25
range height 0 20
range offset 1 20


[time]
(Initiates(TapOn(), Filling(), time)).

[time]
(Terminates(TapOff(), Filling(), time)).

[height, time]
(Releases(TapOn(), HasHeight(height), time)).

[height, time]
(HoldsAt(HasHeight(height), time) ->
Initiates(TapOff(), HasHeight(height), time)).

[time]
(Terminates(Overflow(), Filling(), time)).

[height, time]
(HoldsAt(HasHeight(height), time) ->
Initiates(Overflow(), HasHeight(height), time)).

[time]
(Initiates(Overflow(), Spilling(), time)).

[height1, height2, offset, time]
(HoldsAt(HasHeight(height1), time) &
height2 = (height1 + offset) ->
Trajectory(Filling(), time, HasHeight(height2), offset)).

[height1, height2, time]
(HoldsAt(HasHeight(height1), time) &
HoldsAt(HasHeight(height2), time) ->
height1=height2).

[time]
(HoldsAt(HasHeight(10), time) &
HoldsAt(Filling(), time) ->
Happens(Overflow(), time)).


HoldsAt(HasHeight(0), 0).
!HoldsAt(Filling(), 0).
!HoldsAt(Spilling(), 0).

Happens(TapOn(), 5).

completion Happens

; End of File
