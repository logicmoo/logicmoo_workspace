load foundations/Root.e
load foundations/EC.e

sort dir
dir Left, Right

fluent Oscillating()
fluent At(dir)

event Hold()
event Swing()


range time 0 5
range offset 1 3


; State Constraints:
[dir1, dir2, time]
(HoldsAt(At(dir1), time) &
 HoldsAt(At(dir2), time) ->
 dir1=dir2).


; Effect Axioms:
[time]
(Initiates(Swing(), Oscillating(), time)).

[time, dir]
(Releases(Swing(), At(dir), time)).

[time,dir]
(HoldsAt(At(dir), time) ->
 Initiates(Hold(), At(dir), time)).

[time]
(Terminates(Hold(), Oscillating(), time)).


[time, offset]
(HoldsAt(At(Left), time) & (offset % 2!=0) ->
 Trajectory(Oscillating(),time,At(Right),offset)).

[time, offset]
(HoldsAt(At(Right), time) & (offset % 2!=0) ->
 Trajectory(Oscillating(),time,At(Left),offset)).

[time, offset]
(HoldsAt(At(Left), time) & (offset % 2=0) ->
 Trajectory(Oscillating(),time,At(Left),offset)).

[time, offset]
(HoldsAt(At(Right), time) & (offset % 2=0) ->
 Trajectory(Oscillating(),time,At(Right),offset)).


; Observation
!HoldsAt(Oscillating(),0).
HoldsAt(At(Left),0).

; Event Occurrence
Happens(Swing(), 0).
Happens(Hold(), 3).




completion Happens

; end of file
