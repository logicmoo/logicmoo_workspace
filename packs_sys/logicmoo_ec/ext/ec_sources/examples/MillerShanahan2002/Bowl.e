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
; \fullciteA[p. 461]{MillerShanahan:2002}
;
; @incollection{MillerShanahan:2002,
;   author = "Rob Miller and Murray Shanahan",
;   year = "2002",
;   title = "Some alternative formulations of the event calculus",
;   editor = "Antonis C. Kakas and Fariba Sadri",
;   booktitle = "Computational Logic: Logic Programming and Beyond: Essays in Honour of \uppercase{R}obert \uppercase{A}. \uppercase{K}owalski, Part \uppercase{II}",
;   series = "Lecture Notes in Computer Science",
;   volume = "2408",
;   pages = "452--490",
;   address = "Berlin",
;   publisher = "Springer",
; }
;

load foundations/Root.e
load foundations/EC.e

event LiftLeft()
event LiftRight()
fluent Spilt()
fluent Raised()

[time]
!Happens(LiftRight(), time) ->
Initiates(LiftLeft(), Spilt(), time).

[time]
!Happens(LiftLeft(), time) ->
Initiates(LiftRight(), Spilt(), time).

[time]
Happens(LiftLeft(), time) ->
Initiates(LiftRight(), Raised(), time).

!HoldsAt(Spilt(), 0).
!HoldsAt(Raised(), 0).
Happens(LiftLeft(), 2).
Happens(LiftRight(), 2).

completion Happens

range time 0 3
range offset 1 1

; End of file.
