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
; @incollection{CicekliYildirim:2000,
;   author = "Nihan Kesim Cicekli and Yakup Yildirim",
;   year = "2000",
;   title = "Formalizing workflows using the event calculus",
;   editor = "Mohamed T. Ibrahim and Josef K{\"{u}}ng and Norman Revell",
;   booktitle = "Database and Expert Systems Applications",
;   series = "Lecture Notes in Computer Science",
;   volume = "1873",
;   pages = "222--231",
;   address = "Berlin",
;   publisher = "Springer",
; }
;
; @unpublished{WFMC:1999,
;   author = "{Workflow Management Coalition}",
;   year = "1999",
;   title = "\uppercase{W}orkflow \uppercase{M}anagement \uppercase{C}oalition Terminology \& Glossary",
;   howpublished = "Document Number WFMC-TC-1011, Document Status -- Issue 3.0, Workflow Management Coalition, Winchester, UK",
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

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort activity
sort condition
activity A, B, C1, C2, C3, D, E1, E2, E3, F, G
condition E1C, E2C, E3C, FC

fluent Active(activity)
fluent Completed(activity)
fluent Condition(condition)
noninertial Condition

event Start(activity)
event End(activity)

; Sigma

[activity,time]
Initiates(Start(activity),Active(activity),time).

[activity,time]
Terminates(Start(activity),Completed(activity),time).

[activity,time]
Initiates(End(activity),Completed(activity),time).

[activity,time]
Terminates(End(activity),Active(activity),time).

; Delta

; A; B
Delta: [time]
!HoldsAt(Active(B),time) &
!HoldsAt(Completed(A),time-1) &
HoldsAt(Completed(A),time) ->
Happens(Start(B),time).

; B; AND-split C1, C2, C3
Delta: [time]
!HoldsAt(Active(C1),time) &
!HoldsAt(Completed(B),time-1) &
HoldsAt(Completed(B),time) ->
Happens(Start(C1),time).

Delta: [time]
!HoldsAt(Active(C2),time) &
!HoldsAt(Completed(B),time-1) &
HoldsAt(Completed(B),time) ->
Happens(Start(C2),time).

Delta: [time]
!HoldsAt(Active(C3),time) &
!HoldsAt(Completed(B),time-1) &
HoldsAt(Completed(B),time) ->
Happens(Start(C3),time).

; AND-join C1, C2, C3; D
Delta: [time]
!HoldsAt(Active(D),time) &
((!HoldsAt(Completed(C1),time-1) & HoldsAt(Completed(C1),time))|
 (!HoldsAt(Completed(C2),time-1) & HoldsAt(Completed(C2),time))|
 (!HoldsAt(Completed(C3),time-1) & HoldsAt(Completed(C3),time))) &
HoldsAt(Completed(C1),time) &
HoldsAt(Completed(C2),time) &
HoldsAt(Completed(C3),time) ->
Happens(Start(D),time).

; D; XOR-split E1, E2, E3
Delta: [time]
!HoldsAt(Active(E1),time) &
!HoldsAt(Completed(D),time-1) &
HoldsAt(Completed(D),time) &
HoldsAt(Condition(E1C),time) ->
Happens(Start(E1),time).

Delta: [time]
!HoldsAt(Active(E2),time) &
!HoldsAt(Completed(D),time-1) &
HoldsAt(Completed(D),time) &
HoldsAt(Condition(E2C),time) ->
Happens(Start(E2),time).

Delta: [time]
!HoldsAt(Active(E3),time) &
!HoldsAt(Completed(D),time-1) &
HoldsAt(Completed(D),time) &
HoldsAt(Condition(E3C),time) ->
Happens(Start(E3),time).

; XOR-join E1, E2, E3; F
Delta: [time]
!HoldsAt(Active(F),time) &
((!HoldsAt(Completed(E1),time-1) & HoldsAt(Completed(E1),time))|
 (!HoldsAt(Completed(E2),time-1) & HoldsAt(Completed(E2),time))|
 (!HoldsAt(Completed(E3),time-1) & HoldsAt(Completed(E3),time))) ->
Happens(Start(F),time).

; while (FC) F; G
Delta: [time]
!HoldsAt(Active(F),time) &
!HoldsAt(Completed(F),time-1) &
HoldsAt(Completed(F),time) &
HoldsAt(Condition(FC),time) ->
Happens(Start(F),time).

Delta: [time]
!HoldsAt(Active(G),time) &
!HoldsAt(Completed(F),time-1) &
HoldsAt(Completed(F),time) &
!HoldsAt(Condition(FC),time) ->
Happens(Start(G),time).

Delta: Happens(Start(A),0).
Delta: Happens(End(A),1).
Delta: Happens(End(B),3).
Delta: Happens(End(C1),5).
Delta: Happens(End(C2),6).
Delta: Happens(End(C3),7).
Delta: Happens(End(D),9).
Delta: Happens(End(E2),11).
Delta: Happens(End(F),13).
Delta: Happens(End(F),15).

; Gamma

[activity] !HoldsAt(Active(activity),0).
[activity] !HoldsAt(Completed(activity),0).
[time] time=14 <-> HoldsAt(Condition(FC),time).
[time] !HoldsAt(Condition(E1C),time).
[time] time=10 <-> HoldsAt(Condition(E2C),time).
[time] !HoldsAt(Condition(E3C),time).

completion Delta Happens

range time 0 18
range offset 1 1

; End of file.
