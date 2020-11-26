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
; @article{Thielscher:1997,
;   author = "Michael Thielscher",
;   year = "1997",
;   title = "Ramification and causality",
;   journal = "Artificial Intelligence",
;   volume = "89",
;   pages = "317--364",
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

load foundations/Root.e
load foundations/EC.e
load foundations/ECCausal.e

sort switch
sort relay
sort light

switch S1, S2, S3
relay R
light L

event Light(light)
event Close(switch)
event Open(switch)
event Activate(relay)

fluent Lit(light)
fluent Closed(switch)
fluent Activated(relay)

[time]
Stopped(Lit(L),time) &
Initiated(Closed(S1),time) &
Initiated(Closed(S2),time) ->
Happens(Light(L),time).

[time]
Started(Closed(S2),time) &
Initiated(Activated(R),time) ->
Happens(Open(S2),time).

[time]
Stopped(Activated(R),time) &
Initiated(Closed(S1),time) &
Initiated(Closed(S3),time) ->
Happens(Activate(R),time).

[switch,time] Initiates(Close(switch),Closed(switch),time).
[switch,time] Terminates(Open(switch),Closed(switch),time).
[relay,time] Initiates(Activate(relay),Activated(relay),time).
[light,time] Initiates(Light(light),Lit(light),time).

!HoldsAt(Closed(S1),0).
HoldsAt(Closed(S2),0).
HoldsAt(Closed(S3),0).
!HoldsAt(Activated(R),0).
!HoldsAt(Lit(L),0).

Happens(Close(S1),0).

completion Happens

range time 0 1
range offset 1 1

; End of file.
