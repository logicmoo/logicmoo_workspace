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
; @article{GinsbergSmith:1988a,
;   author = "Matthew L. Ginsberg and David E. Smith",
;   year = "1988",
;   title = "Reasoning about action \uppercase{I}: \uppercase{A} possible worlds approach",
;   journal = "Artificial Intelligence",
;   volume = "35",
;   number = "2",
;   pages = "165--195",
; }
;
; \fullciteA[pp. 288--289]{Shanahan:1997}
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
; timestamps
; added:
; !HoldsAt(Blocked1(),0).
; !HoldsAt(Blocked2(),0).
;

load foundations/Root.e
load foundations/EC.e

event Close1()
event Close2()
event Start()
fluent Blocked1()
fluent Blocked2()
fluent Stuffy()
noninertial Stuffy

[time] Initiates(Close1(),Blocked1(),time).
[time] Initiates(Close2(),Blocked2(),time).

[time]
HoldsAt(Stuffy(),time) <->
HoldsAt(Blocked1(),time)&HoldsAt(Blocked2(),time).

[time] Initiates(Start(),Blocked1(),time).
[time] Terminates(Start(),Blocked2(),time).

!HoldsAt(Blocked1(),0).
!HoldsAt(Blocked2(),0).
Happens(Start(),0).
Happens(Close2(),1).

completion Happens

range time 0 2
range offset 1 1

; End of file.
