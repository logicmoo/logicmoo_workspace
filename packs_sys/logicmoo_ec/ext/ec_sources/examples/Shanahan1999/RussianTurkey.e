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
; @book{Sandewall:1994,
;   author = "Sandewall, Erik",
;   year = "1994",
;   title = "Features and Fluents: The Representation of Knowledge about Dynamical Systems",
;   volume = "I",
;   address = "Oxford",
;   publisher = "Oxford University Press",
; }
;
; @incollection{Shanahan:1999,
;   author = "Shanahan, Murray",
;   year = "1999",
;   title = "The Event Calculus explained",
;   editor = "Michael J. Wooldridge and Manuela M. Veloso",
;   booktitle = "Artificial Intelligence Today: Recent Trends and Developments",
;   series = "Lecture Notes in Computer Science",
;   volume = "1600",
;   pages = "409--430",
;   address = "Berlin",
;   publisher = "Springer",
; }
;
; model finding
;
; modifications from Shanahan's formulation:
; InitiallyP -> HoldsAt
; added [time] Terminates(Shoot(),Loaded(),time).
; added !HoldsAt(Loaded(),0) to prune models
; timestamps
;

load foundations/Root.e
load foundations/EC.e

event Load()
event Shoot()
event Spin()
fluent Loaded()
fluent Alive()

[time] Initiates(Load(),Loaded(),time).
[time] HoldsAt(Loaded(),time) -> Terminates(Shoot(),Alive(),time).
[time] Releases(Spin(),Loaded(),time).
[time] Terminates(Shoot(),Loaded(),time).

HoldsAt(Alive(),0).
!HoldsAt(Loaded(),0).
Happens(Load(),1).
Happens(Spin(),2).
Happens(Shoot(),3).

completion Happens

range time 0 4
range offset 1 1

; End of file.
