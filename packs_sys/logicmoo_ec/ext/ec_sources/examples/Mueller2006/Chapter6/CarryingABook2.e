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
; Example: Carrying a Book (Release Axioms and State Constraints)
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

sort object
sort agent: object
sort room

object Book
agent Nathan
room LivingRoom, Kitchen

event LetGoOf(agent,object)
event PickUp(agent,object)
event Walk(agent,room,room)

fluent InRoom(object,room)
fluent Holding(agent,object)

; Sigma

[agent,room1,room2,time]
Initiates(Walk(agent,room1,room2),InRoom(agent,room2),time).

[agent,room1,room2,time]
room1!=room2 ->
Terminates(Walk(agent,room1,room2),InRoom(agent,room1),time).

[agent,object,room,time]
HoldsAt(InRoom(agent,room),time) &
HoldsAt(InRoom(object,room),time) ->
Initiates(PickUp(agent,object),Holding(agent,object),time).

[agent,object,time]
HoldsAt(Holding(agent,object),time) ->
Terminates(LetGoOf(agent,object),Holding(agent,object),time).

[agent,object,room,time]
Releases(PickUp(agent,object),InRoom(object,room),time).

[agent,object,room,time]
HoldsAt(InRoom(agent,room),time) ->
Initiates(LetGoOf(agent,object),InRoom(object,room),time).

; Delta

Happens(PickUp(Nathan,Book),0).
Happens(Walk(Nathan,LivingRoom,Kitchen),1).

; Psi

[object,room1,room2,time]
HoldsAt(InRoom(object,room1),time) &
HoldsAt(InRoom(object,room2),time) ->
room1=room2.

[agent,object,room,time]
HoldsAt(Holding(agent,object),time) &
HoldsAt(InRoom(agent,room),time) ->
HoldsAt(InRoom(object,room),time).

; Gamma

HoldsAt(InRoom(Nathan,LivingRoom),0).
HoldsAt(InRoom(Book,LivingRoom),0).

; added:
!HoldsAt(Holding(Nathan,Book),0).
[agent,time] !HoldsAt(Holding(agent,agent),time).

completion Happens

range time 0 2
range offset 1 1

; End of file.
