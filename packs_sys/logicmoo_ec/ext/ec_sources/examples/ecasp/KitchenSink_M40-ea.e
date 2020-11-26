; KitchenSink_M40-ea.e
option trajectory on

load foundations/Root.e
load foundations/EC.e

sort height: integer

event TapOn()
event TapOff()
event Overflow()
fluent Filling()
fluent Height(height)
fluent Spilling()

[time] Initiates(TapOn(),Filling(),time).

[time] Terminates(TapOff(),Filling(),time).

; should probably add:
;[time] Terminates(TapOff(),Spilling(),time).

[height,time] Releases(TapOn(),Height(height),time).

[height,time]
(HoldsAt(Height(height),time) ->
Initiates(TapOff(),Height(height),time)).

[time] Terminates(Overflow(),Filling(),time).

[height,time]
(HoldsAt(Height(height),time) ->
Initiates(Overflow(),Height(height),time)).

[time] Initiates(Overflow(),Spilling(),time).

[height1,height2,offset,time]
(HoldsAt(Height(height1),time) &
height2 = height1 + offset ->
Trajectory(Filling(),time,Height(height2),offset)).

[height1,height2,time]
(HoldsAt(Height(height1),time) &
HoldsAt(Height(height2),time) ->
height1 = height2).

[time] (HoldsAt(Height(25),time) & HoldsAt(Filling(),time) ->
Happens(Overflow(),time)).

HoldsAt(Height(0),0).
!HoldsAt(Filling(),0).
!HoldsAt(Spilling(),0).
Happens(TapOn(),10). 

!ReleasedAt(Filling(),0).
[height]!ReleasedAt(Height(height),0).
!ReleasedAt(Spilling(),0).

completion Happens

range time 0 40 
range height 0 25
range offset 1 25

; End of file.
