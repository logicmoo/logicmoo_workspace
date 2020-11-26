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
; @inproceedings{Mueller:2004b,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
;   editor = "Valerie Barr and Zdravko Markov",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
;   pages = "147--152",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
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
height2=height1-offset ->
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
HoldsAt(Height(Leaf,4),0).
Happens(StartFalling(Leaf),2).

completion Happens

range time 0 7
range offset 1 4
range height 0 4

; End of file.
