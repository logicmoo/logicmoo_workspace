; HotAirBalloon3-ea.e
option encoding 3
option trajectory on

load foundations/Root.e
load foundations/EC.e

sort balloon
sort agent
sort height: integer

agent Nathan
balloon Balloon

fluent HeaterOn(balloon)
fluent Height(balloon,height)
;noninertial Height

event TurnOnHeater(agent,balloon)
event TurnOffHeater(agent,balloon)

; Sigma

[agent,balloon,time]
Initiates(TurnOnHeater(agent,balloon),HeaterOn(balloon),time).

[agent,balloon,time]
Terminates(TurnOffHeater(agent,balloon),HeaterOn(balloon),time).

; Delta

Happens(TurnOnHeater(Nathan,Balloon),0).
Happens(TurnOffHeater(Nathan,Balloon),2).


; Psi

[balloon,height1,height2,time]
(HoldsAt(Height(balloon,height1),time) &
HoldsAt(Height(balloon,height2),time) ->
height1=height2).

; Pi

[balloon,height1,height2,offset,time]
(HoldsAt(Height(balloon,height1),time) &
height2 = (height1 + offset) ->
Trajectory(HeaterOn(balloon),time,Height(balloon,height2),offset)).

[balloon,height1,height2,offset,time]
(HoldsAt(Height(balloon,height1),time) &
height2 = (height1 - offset) ->
AntiTrajectory(HeaterOn(balloon),time,Height(balloon,height2),offset)).

; Gamma

HoldsAt(Height(Balloon,0),0).

; added:
!HoldsAt(HeaterOn(Balloon),0).

;noninertial Height
[balloon,height,time]
ReleasedAt(Height(balloon,height), time).

[balloon]!ReleasedAt(HeaterOn(balloon),0).

completion Happens

range time 0 3
range height 0 2
range offset 1 2

; End of file.
