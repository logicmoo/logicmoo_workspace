

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FILE: examples/Mueller2006/Chapter10/MovingNewspaperAndBox.e
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available in
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
include foundations/Root.e
include foundations/EC.e

sort object
sort agent: object
sort physobj: object
sort room: object

fluent directlyIn(object,object)
fluent inRoom(object,room)
noninertial inRoom

;; executable(move(agent,object,object,object))

agent Lisa
physobj Box, Newspaper
room Kitchen, LivingRoom

; Sigma

; RS10
[agent,physobj1,physobj2,room,time]
HoldsAt(directlyIn(agent,room),time) &
HoldsAt(directlyIn(physobj1,room),time) &
HoldsAt(inRoom(physobj2,room),time) ->
Initiates(move(agent,physobj1,room,physobj2),directlyIn(physobj1,physobj2),time).

; RS11
[agent,physobj1,physobj2,room,time]
HoldsAt(directlyIn(agent,room),time) &
HoldsAt(directlyIn(physobj1,room),time) &
HoldsAt(inRoom(physobj2,room),time) ->
Terminates(move(agent,physobj1,room,physobj2),directlyIn(physobj1,room),time).

; RS12
[agent,physobj1,physobj2,room,time]
HoldsAt(directlyIn(agent,room),time) ->
Initiates(move(agent,physobj1,physobj2,room),directlyIn(physobj1,room),time).

; RS13
[agent,physobj1,physobj2,room,time]
HoldsAt(directlyIn(agent,room),time) ->
Terminates(move(agent,physobj1,physobj2,room),directlyIn(physobj1,physobj2),time).

; RS14
[agent,room1,room2,time]
HoldsAt(directlyIn(agent,room1),time) ->
Initiates(move(agent,agent,room1,room2),directlyIn(agent,room2),time).

; RS15
[agent,room1,room2,time]
HoldsAt(directlyIn(agent,room1),time) ->
Terminates(move(agent,agent,room1,room2),directlyIn(agent,room1),time).

; RS16
[agent,physobj,room,time]
HoldsAt(directlyIn(agent,room),time) &
HoldsAt(directlyIn(physobj,room),time) ->
Initiates(move(agent,physobj,room,agent),directlyIn(physobj,agent),time).

; RS17
[agent,physobj,room,time]
HoldsAt(directlyIn(agent,room),time) &
HoldsAt(directlyIn(physobj,room),time) ->
Terminates(move(agent,physobj,room,agent),directlyIn(physobj,room),time).

; RS18
[agent,physobj,room,time]
HoldsAt(directlyIn(physobj,agent),time) &
HoldsAt(directlyIn(agent,room),time) ->
Initiates(move(agent,physobj,agent,room),directlyIn(physobj,room),time).

; RS19
[agent,physobj,room,time]
HoldsAt(directlyIn(physobj,agent),time) &
HoldsAt(directlyIn(agent,room),time) ->
Terminates(move(agent,physobj,agent,room),directlyIn(physobj,agent),time).

; Delta

Happens(move(Lisa,Newspaper,LivingRoom,Box),0).
Happens(move(Lisa,Box,LivingRoom,Lisa),1).
Happens(move(Lisa,Lisa,LivingRoom,Kitchen),2).
Happens(move(Lisa,Box,Lisa,Kitchen),3).
Happens(move(Lisa,Lisa,Kitchen,LivingRoom),4).

; Psi

; RS1
[object,time] !HoldsAt(directlyIn(object,object),time).

; RS2
[object1,object2,time]
HoldsAt(directlyIn(object1,object2),time) ->
!HoldsAt(directlyIn(object2,object1),time).

; RS3
[object1,object2,object3,time]
HoldsAt(directlyIn(object1,object2),time) &
HoldsAt(directlyIn(object2,object3),time) ->
!HoldsAt(directlyIn(object1,object3),time).

; RS4
[object,object1,object2,time]
HoldsAt(directlyIn(object,object1),time) &
HoldsAt(directlyIn(object,object2),time) ->
object1=object2.

; RS7
[object,room,time]
HoldsAt(directlyIn(object,room),time) ->
HoldsAt(inRoom(object,room),time).

; RS8
[object1,object2,room,time]
HoldsAt(directlyIn(object1,object2),time) &
HoldsAt(inRoom(object2,room),time) ->
HoldsAt(inRoom(object1,room),time).

; RS9
[object,room1,room2,time]
HoldsAt(inRoom(object,room1),time) &
HoldsAt(inRoom(object,room2),time) ->
room1=room2.

; Gamma

HoldsAt(directlyIn(Lisa,LivingRoom),0).
HoldsAt(directlyIn(Newspaper,LivingRoom),0).
HoldsAt(directlyIn(Box,LivingRoom),0).

; added:                                                 
; DMILES REMOVED [room1,room2,time] !HoldsAt(inRoom(room1,room2),time).
; DMILES REMOVED [room,object,time] !HoldsAt(directlyIn(room,object),time).

; entailed:
HoldsAt(directlyIn(Lisa,LivingRoom),5).
HoldsAt(directlyIn(Box,Kitchen),5).
HoldsAt(inRoom(Newspaper,Kitchen),5).

completion Happens

range time 0 5
range offset 1 1

; End of file.


