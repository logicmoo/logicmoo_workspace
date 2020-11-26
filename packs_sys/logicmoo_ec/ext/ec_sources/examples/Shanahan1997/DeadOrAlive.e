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
; \fullciteA[p. 324]{Shanahan:1997}
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
; InitiallyP -> HoldsAt
; timestamps
; added [time] Terminates(Shoot(),Loaded(),time).
;

load foundations/Root.e
load foundations/EC.e

event Load()
event Shoot()
event Sneeze()
fluent Loaded()
fluent Alive()
fluent Dead()
noninertial Dead

[time] Initiates(Load(),Loaded(),time).
[time] HoldsAt(Loaded(),time) -> Terminates(Shoot(),Alive(),time).
[time] Terminates(Shoot(),Loaded(),time).
[time] HoldsAt(Dead(),time) <-> !HoldsAt(Alive(),time).

HoldsAt(Alive(),0).
!HoldsAt(Loaded(),0).
Happens(Load(),0).
Happens(Sneeze(),1).
Happens(Shoot(),2).

completion Happens

range time 0 3
range offset 1 1

; End of file.
