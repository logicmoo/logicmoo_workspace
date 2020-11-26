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

sort object
sort agent: object
sort physobj: object
sort room: object

fluent IN(object,object)
fluent INROOM(object,room)
noninertial INROOM

event MOVE(agent,object,object,object)

agent Lisa
physobj Box, Newspaper
room Kitchen, LivingRoom

; Sigma

; RS10
[agent,physobj1,physobj2,room,time]
HoldsAt(IN(agent,room),time) &
HoldsAt(IN(physobj1,room),time) &
HoldsAt(INROOM(physobj2,room),time) ->
Initiates(MOVE(agent,physobj1,room,physobj2),IN(physobj1,physobj2),time).

; RS11
[agent,physobj1,physobj2,room,time]
HoldsAt(IN(agent,room),time) &
HoldsAt(IN(physobj1,room),time) &
HoldsAt(INROOM(physobj2,room),time) ->
Terminates(MOVE(agent,physobj1,room,physobj2),IN(physobj1,room),time).

; RS12
[agent,physobj1,physobj2,room,time]
HoldsAt(IN(agent,room),time) ->
Initiates(MOVE(agent,physobj1,physobj2,room),IN(physobj1,room),time).

; RS13
[agent,physobj1,physobj2,room,time]
HoldsAt(IN(agent,room),time) ->
Terminates(MOVE(agent,physobj1,physobj2,room),IN(physobj1,physobj2),time).

; RS14
[agent,room1,room2,time]
HoldsAt(IN(agent,room1),time) ->
Initiates(MOVE(agent,agent,room1,room2),IN(agent,room2),time).

; RS15
[agent,room1,room2,time]
HoldsAt(IN(agent,room1),time) ->
Terminates(MOVE(agent,agent,room1,room2),IN(agent,room1),time).

; RS16
[agent,physobj,room,time]
HoldsAt(IN(agent,room),time) &
HoldsAt(IN(physobj,room),time) ->
Initiates(MOVE(agent,physobj,room,agent),IN(physobj,agent),time).

; RS17
[agent,physobj,room,time]
HoldsAt(IN(agent,room),time) &
HoldsAt(IN(physobj,room),time) ->
Terminates(MOVE(agent,physobj,room,agent),IN(physobj,room),time).

; RS18
[agent,physobj,room,time]
HoldsAt(IN(physobj,agent),time) &
HoldsAt(IN(agent,room),time) ->
Initiates(MOVE(agent,physobj,agent,room),IN(physobj,room),time).

; RS19
[agent,physobj,room,time]
HoldsAt(IN(physobj,agent),time) &
HoldsAt(IN(agent,room),time) ->
Terminates(MOVE(agent,physobj,agent,room),IN(physobj,agent),time).

; Delta

Happens(MOVE(Lisa,Newspaper,LivingRoom,Box),0).
Happens(MOVE(Lisa,Box,LivingRoom,Lisa),1).
Happens(MOVE(Lisa,Lisa,LivingRoom,Kitchen),2).
Happens(MOVE(Lisa,Box,Lisa,Kitchen),3).
Happens(MOVE(Lisa,Lisa,Kitchen,LivingRoom),4).

; Psi

; RS1
[object,time] !HoldsAt(IN(object,object),time).

; RS2
[object1,object2,time]
HoldsAt(IN(object1,object2),time) ->
!HoldsAt(IN(object2,object1),time).

; RS3
[object1,object2,object3,time]
HoldsAt(IN(object1,object2),time) &
HoldsAt(IN(object2,object3),time) ->
!HoldsAt(IN(object1,object3),time).

; RS4
[object,object1,object2,time]
HoldsAt(IN(object,object1),time) &
HoldsAt(IN(object,object2),time) ->
object1=object2.

; RS7
[object,room,time]
HoldsAt(IN(object,room),time) ->
HoldsAt(INROOM(object,room),time).

; RS8
[object1,object2,room,time]
HoldsAt(IN(object1,object2),time) &
HoldsAt(INROOM(object2,room),time) ->
HoldsAt(INROOM(object1,room),time).

; RS9
[object,room1,room2,time]
HoldsAt(INROOM(object,room1),time) &
HoldsAt(INROOM(object,room2),time) ->
room1=room2.

; Gamma

HoldsAt(IN(Lisa,LivingRoom),0).
HoldsAt(IN(Newspaper,LivingRoom),0).
HoldsAt(IN(Box,LivingRoom),0).

; added:
[room1,room2,time] !HoldsAt(INROOM(room1,room2),time).
[room,object,time] !HoldsAt(IN(room,object),time).

; entailed:
; HoldsAt(IN(Lisa,LivingRoom),5).
; HoldsAt(IN(Box,Kitchen),5).
; HoldsAt(INROOM(Newspaper,Kitchen),5).

completion Happens

range time 0 5
range offset 1 1

; End of file.
