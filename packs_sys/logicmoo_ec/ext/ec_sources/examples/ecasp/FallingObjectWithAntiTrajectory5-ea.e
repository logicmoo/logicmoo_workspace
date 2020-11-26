; FallingObjectWithAntiTrajectory5-ea.e
option encoding 3
option trajectory on

load foundations/Root.e
load foundations/EC.e


sort object
sort agent
sort height: integer

agent Nathan
object Apple

fluent Falling(object)
fluent Height(object,height)
;noninertial Height

event Drop(agent,object)
event HitGround(object)

; Sigma

[agent,object,time]
Initiates(Drop(agent,object),Falling(object),time).

[object,time]
Terminates(HitGround(object),Falling(object),time).

; Delta
[object,time]
(HoldsAt(Falling(object),time) &
HoldsAt(Height(object,0),time) ->
Happens(HitGround(object),time)).

Happens(Drop(Nathan,Apple),0).

; Psi

[object,height1,height2,time]
(HoldsAt(Height(object,height1),time) &
HoldsAt(Height(object,height2),time) ->
height1=height2).

; Pi

[object,height1,height2,offset,time]
(HoldsAt(Height(object,height1),time) &
height2 = (height1 - offset) ->
Trajectory(Falling(object),time,Height(object,height2),offset)).

[object,height,offset,time]
(HoldsAt(Height(object,height),time) ->
AntiTrajectory(Falling(object),time,Height(object,height),offset)).

; Gamma

!HoldsAt(Falling(Apple),0).
HoldsAt(Height(Apple,3),0).

[object]
!ReleasedAt(Falling(object),0).

;noninertial Height
[object,height]
ReleasedAt(Height(object,height),0).

completion Happens
;completion Delta Happens

range time 0 5
range height 0 3
range offset 1 3


; End of file.
