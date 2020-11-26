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
; @article{MillerShanahan:1999,
;   author = "Rob Miller and Murray Shanahan",
;   year = "1999",
;   title = "The event calculus in classical logic---\uppercase{A}lternative axiomatisations",
;   journal = "Link{\"{o}}ping Electronic Articles in Computer and Information Science",
;   volume = "4",
;   number = "016",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option encoding 3
option trajectory on

load foundations/Root.e
load foundations/EC.e

sort balloon
sort agent
sort height: integer

agent Nathan
balloon Balloon

fluent HeaterOn(balloon)
fluent Height(balloon,height)
noninertial Height

event TurnOnHeater(agent,balloon)
event TurnOffHeater(agent,balloon)

; Sigma

[agent,balloon,time]
Initiates(TurnOnHeater(agent,balloon),HeaterOn(balloon),time).

[agent,balloon,time]
Terminates(TurnOffHeater(agent,balloon),HeaterOn(balloon),time).

; Delta

Delta: Happens(TurnOnHeater(Nathan,Balloon),0).
Delta: Happens(TurnOffHeater(Nathan,Balloon),2).

; Psi

[balloon,height1,height2,time]
HoldsAt(Height(balloon,height1),time) &
HoldsAt(Height(balloon,height2),time) ->
height1=height2.

; Pi

[balloon,height1,height2,offset,time]
HoldsAt(Height(balloon,height1),time) &
height2 = (height1 + offset) ->
Trajectory(HeaterOn(balloon),time,Height(balloon,height2),offset).

[balloon,height1,height2,offset,time]
HoldsAt(Height(balloon,height1),time) &
height2 = (height1 - offset) ->
AntiTrajectory(HeaterOn(balloon),time,Height(balloon,height2),offset).

; Gamma

HoldsAt(Height(Balloon,0),0).

; added:
!HoldsAt(HeaterOn(Balloon),0).

completion Delta Happens

range time 0 3
range height 0 2
range offset 1 2

; End of file.
