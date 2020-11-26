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
; \fullciteA[pp. 302--304]{Shanahan:1997}
;
; @book{Shanahan:1997,
;   author = "Murray Shanahan",
;   year = "1997",
;   title = "Solving the Frame Problem",
;   address = "Cambridge, MA",
;   publisher = "MIT Press",
; }
;
; deduction
;
; modifications from Shanahan's formulation:
; reformulated using the method of \fullciteA[pp. 460--461]{MillerShanahan:2002}
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
; added:
; !HoldsAt(Forwards(), 0).
; !HoldsAt(Backwards(), 0).
; !HoldsAt(Spinning(), 0).
;

load foundations/Root.e
load foundations/EC.e

event Push()
event Pull()
fluent Forwards()
fluent Backwards()
fluent Spinning()

[time]
!Happens(Pull(), time) ->
Initiates(Push(), Forwards(), time).

[time]
!Happens(Pull(), time) ->
Terminates(Push(), Backwards(), time).

[time]
!Happens(Push(), time) ->
Initiates(Pull(), Backwards(), time).

[time]
!Happens(Push(), time) ->
Terminates(Pull(), Forwards(), time).

[time]
Happens(Push(), time) ->
Initiates(Pull(), Spinning(), time).

[time]
Happens(Push(), time) ->
Terminates(Pull(), Forwards(), time).

[time]
Happens(Push(), time) ->
Terminates(Pull(), Backwards(), time).

[time]
!Happens(Pull(), time) ->
Terminates(Push(), Spinning(), time).

[time]
!Happens(Push(), time) ->
Terminates(Pull(), Spinning(), time).

!HoldsAt(Forwards(), 0).
!HoldsAt(Backwards(), 0).
!HoldsAt(Spinning(), 0).

Happens(Push(), 5).
Happens(Pull(), 5).
Happens(Pull(), 10).
Happens(Push(), 10).

completion Happens

range time 0 12
range offset 1 1

; End of file.
