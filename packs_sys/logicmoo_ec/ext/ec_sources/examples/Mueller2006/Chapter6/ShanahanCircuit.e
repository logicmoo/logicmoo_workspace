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
; @inproceedings{Shanahan:1999a,
;   author = "Murray Shanahan",
;   year = "1999",
;   title = "The ramification problem in the event calculus",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}ixteenth \uppercase{I}nternational \uppercase{J}oint \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
;   pages = "140--146",
;   address = "San Mateo, CA",
;   publisher = "Morgan Kaufmann",
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

sort switch
sort relay
sort light

switch S1, S2, S3
relay R
light L

event Light(light)
event Unlight(light)
event Close(switch)
event Open(switch)
event Activate(relay)
event Deactivate(relay)

fluent Lit(light)
fluent Closed(switch)
fluent Activated(relay)

[time]
!HoldsAt(Lit(L),time) &
HoldsAt(Closed(S1),time) &
HoldsAt(Closed(S2),time) ->
Happens(Light(L),time).

[time]
HoldsAt(Lit(L),time) &
(!HoldsAt(Closed(S1),time) | !HoldsAt(Closed(S2),time)) ->
Happens(Unlight(L),time).

[time]
HoldsAt(Closed(S2),time) &
HoldsAt(Activated(R),time) ->
Happens(Open(S2),time).

[time]
!HoldsAt(Activated(R),time) &
HoldsAt(Closed(S1),time) &
HoldsAt(Closed(S2),time) &
HoldsAt(Closed(S3),time) ->
Happens(Activate(R),time).

[time]
HoldsAt(Activated(R),time) &
(!HoldsAt(Closed(S1),time) |
 !HoldsAt(Closed(S2),time) |
 !HoldsAt(Closed(S3),time)) ->
Happens(Deactivate(R),time).

[switch,time] Initiates(Close(switch),Closed(switch),time).
[switch,time] Terminates(Open(switch),Closed(switch),time).
[relay,time] Initiates(Activate(relay),Activated(relay),time).
[relay,time] Terminates(Deactivate(relay),Activated(relay),time).
[light,time] Initiates(Light(light),Lit(light),time).
[light,time] Terminates(Unlight(light),Lit(light),time).

!HoldsAt(Closed(S1),0).
HoldsAt(Closed(S2),0).
HoldsAt(Closed(S3),0).
!HoldsAt(Activated(R),0).
!HoldsAt(Lit(L),0).

Happens(Close(S1),0).

completion Happens

range time 0 4
range offset 1 1

; End of file.
