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
; deduction
;
; modifications from Shanahan's formulation:
; InitiallyN -> !HoldsAt
; InitiallyP -> HoldsAt
; timestamps
;

load foundations/Root.e
load foundations/EC.e

sort person
event Feed(person)
event Clothe(person)
fluent Happy(person)
fluent Hungry(person)
fluent Cold(person)
noninertial Happy

[person,time]
HoldsAt(Happy(person),time) <->
!HoldsAt(Hungry(person),time) &
!HoldsAt(Cold(person),time).

[person,time]
Terminates(Feed(person),Hungry(person),time).

[person,time]
Terminates(Clothe(person),Cold(person),time).

person Fred

HoldsAt(Hungry(Fred),0).
!HoldsAt(Cold(Fred),0).
Happens(Feed(Fred),1).

completion Happens

range time 0 2
range offset 1 1

; End of file.
