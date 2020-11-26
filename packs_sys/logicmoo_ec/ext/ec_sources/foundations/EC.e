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
; Event Calculus (EC)
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

sort time: integer
sort offset: integer

reified sort fluent
reified sort event

predicate Happens(event,time)
predicate HoldsAt(fluent,time)
predicate ReleasedAt(fluent,time)
predicate Initiates(event,fluent,time)
predicate Terminates(event,fluent,time)
predicate Releases(event,fluent,time)
predicate Trajectory(fluent,time,fluent,offset)

; End of file.
