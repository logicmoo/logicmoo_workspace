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
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent
sort location

agent James
location Bookstore

fluent Tired(agent)

event Go(agent,location)
event Run(agent,location)
event Drive(agent,location)

[agent,location,time]
Happens(Go(agent,location),time) ->
Happens(Run(agent,location),time) | Happens(Drive(agent,location),time).

xor Run, Drive

[agent,location,time] Initiates(Run(agent,location),Tired(agent),time).

!HoldsAt(Tired(James),0).
Happens(Go(James,Bookstore),0).
HoldsAt(Tired(James),1).

range time 0 1
range offset 1 1

; End of file.
