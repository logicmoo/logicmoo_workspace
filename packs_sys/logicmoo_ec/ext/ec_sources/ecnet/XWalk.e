;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; XWalk: WALK x-schema representation of walking
;
; @phdthesis{Narayanan:1997,
;   author = "Srinivas S. Narayanan",
;   year = "1997",
;   title = "Knowledge-based Action Representations for Metaphor and Aspect (\uppercase{KARMA})",
;   address = "Berkeley, CA",
;   school = "University of California, Berkeley",
; }
;

option trajectory on

sort xschema

; parameters

predicate XWalkAgent(xschema,agent)
function XWalkRate(xschema): offset ; step duration
function XWalkSize(xschema): offset ; step size

; TTL input lines

fluent XWalkEnabled(xschema)
fluent XWalkGroundStable(xschema)
fluent XWalkPosture(xschema)
fluent XWalkFootingOK(xschema)
noninertial XWalkEnabled, XWalkGroundStable, XWalkPosture, XWalkFootingOK

; fluents

fluent XWalkDistance(xschema,distance)

[xschema,distance1,distance2,time]
HoldsAt(XWalkDistance(xschema,distance1),time) &
HoldsAt(XWalkDistance(xschema,distance2),time) ->
distance1=distance2.

; logic gate behavior

fluent XWalkVision(xschema)
fluent XWalkVisionOK(xschema)
fluent XWalkAtDestination(xschema)
fluent XWalkDone(xschema)
noninertial XWalkVision, XWalkVisionOK, XWalkAtDestination, XWalkDone

[xschema,time]
HoldsAt(XWalkGroundStable(xschema),time) <->
HoldsAt(XWalkVision(xschema),time).

[xschema,time]
HoldsAt(XWalkEnabled(xschema),time) &
HoldsAt(XWalkVision(xschema),time) &
HoldsAt(XWalkPosture(xschema),time) <->
HoldsAt(XWalkVisionOK(xschema),time).

[xschema,time]
HoldsAt(XWalkDistance(xschema,0),time) <->
HoldsAt(XWalkAtDestination(xschema),time).

[xschema,time]
HoldsAt(XWalkAtDestination(xschema),time) <->
HoldsAt(XWalkDone(xschema),time).

; durative events

; distance is the goal
fluent XWalkStepping(xschema,distance) 
event XWalkSteppingOn(xschema)
event XWalkSteppingOff(xschema)

[xschema,distance1,distance2,time]
HoldsAt(XWalkDistance(xschema,distance1),time) &
distance2 = distance1 - XWalkSize(xschema) ->
Initiates(XWalkSteppingOn(xschema),XWalkStepping(xschema,distance2),time).

[xschema,distance,time]
Terminates(XWalkSteppingOff(xschema),XWalkStepping(xschema,distance),time).

[xschema,distance,time]
Releases(XWalkSteppingOn(xschema),XWalkDistance(xschema,distance),time).

[xschema,distance1,distance2,time]
HoldsAt(XWalkDistance(xschema,distance1),time) &
distance1 != distance2 ->
Terminates(XWalkSteppingOff(xschema),XWalkDistance(xschema,distance2),time).

[xschema,distance,time]
HoldsAt(XWalkDistance(xschema,distance),time) ->
Initiates(XWalkSteppingOff(xschema),XWalkDistance(xschema,distance),time).

[xschema,distance01,distance02,distance03,offset,time]
HoldsAt(XWalkDistance(xschema,distance01),time) &
(distance03=(distance01-(offset*(XWalkSize(xschema)/XWalkRate(xschema))))) ->
Trajectory(XWalkStepping(xschema,distance02),
           time,
           XWalkDistance(xschema,distance03),
           offset).

[xschema,distance,time]
HoldsAt(XWalkStepping(xschema,distance),time) &
HoldsAt(XWalkDistance(xschema,distance),time) ->
Happens(XWalkSteppingOff(xschema),time).

; punctual events

event XWalkTestFooting(xschema)
event XWalkMoveFoot(xschema)

[xschema,time]
Happens(XWalkTestFooting(xschema),time) &
!HoldsAt(XWalkFootingOK(xschema),time) ->
Happens(XWalkMoveFoot(xschema),time+1).

[xschema,time]
Happens(XWalkMoveFoot(xschema),time) ->
Happens(XWalkReadyOn(xschema),time+1).

; Petri net behavior

fluent XWalkReady(xschema)
event XWalkReadyOn(xschema)
event XWalkReadyOff(xschema)

[xschema,time]
HoldsAt(XWalkEnabled(xschema),time) &
HoldsAt(XWalkVision(xschema),time) &
HoldsAt(XWalkPosture(xschema),time) &
!({distance} HoldsAt(XWalkStepping(xschema,distance),time)) & ; !!! pulse
!HoldsAt(XWalkReady(xschema),time) ->
Happens(XWalkReadyOn(xschema),time).

[xschema,time]
Initiates(XWalkReadyOn(xschema),XWalkReady(xschema),time).

[xschema,time]
Terminates(XWalkReadyOff(xschema),XWalkReady(xschema),time).

; bypass_ok
[xschema,time]
!({distance} HoldsAt(XWalkStepping(xschema,distance),time)) &
HoldsAt(XWalkVisionOK(xschema),time) &
HoldsAt(XWalkReady(xschema),time) ->
Happens(XWalkSteppingOn(xschema),time) &
Happens(XWalkReadyOff(xschema),time).

; !bypass_ok
[xschema,time]
!({distance} HoldsAt(XWalkStepping(xschema,distance),time)) &
!HoldsAt(XWalkVisionOK(xschema),time) &
HoldsAt(XWalkReady(xschema),time) ->
Happens(XWalkTestFooting(xschema),time) &
Happens(XWalkReadyOff(xschema),time).

[xschema,distance,time]
HoldsAt(XWalkStepping(xschema,distance),time) &
HoldsAt(XWalkDistance(xschema,distance),time) &
(distance > 0) ->
Happens(XWalkReadyOn(xschema),time).

; End of file.
