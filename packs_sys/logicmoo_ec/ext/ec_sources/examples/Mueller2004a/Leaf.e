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
; @article{Mueller:2004a,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Event calculus reasoning through satisfiability",
;   journal = "Journal of Logic and Computation",
;   volume = "14",
;   number = "5",
;   pages = "703--730",
; }
;

option trajectory on

load foundations/Root.e
load foundations/EC.e

sort object
sort height: integer

fluent Height(object,height)
fluent Falling(object)
event StartFalling(object)
event HitsGround(object)

[object,height1,height2,time]
HoldsAt(Height(object,height1),time) &
HoldsAt(Height(object,height2),time) ->
height1=height2.

[object,time]
Initiates(StartFalling(object),Falling(object),time).

[object,height,time]
Releases(StartFalling(object),Height(object,height),time).

[object,height1,height2,offset,time]
HoldsAt(Height(object,height1),time) &
height2=height1-offset*offset ->
Trajectory(Falling(object),time,Height(object,height2),offset).

[object,time]
HoldsAt(Falling(object),time) &
HoldsAt(Height(object,0),time) ->
Happens(HitsGround(object),time).

;[object,height1,height2,time]
;HoldsAt(Height(object,height1),time) &
;height1 != height2 ->
;Terminates(HitsGround(object),Height(object,height2),time).

[object,height,time]
HoldsAt(Height(object,height),time) ->
Initiates(HitsGround(object),Height(object,height),time).

[object,time]
Terminates(HitsGround(object),Falling(object),time).

object Leaf

!HoldsAt(Falling(Leaf),0).
HoldsAt(Height(Leaf,9),0).
Happens(StartFalling(Leaf),0).

completion Happens

range time 0 4
range offset 1 9
range height 0 9

; End of file.
