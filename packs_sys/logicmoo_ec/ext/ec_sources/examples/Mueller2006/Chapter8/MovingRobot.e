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
; @inproceedings{Shanahan:1996,
;   author = "Murray Shanahan",
;   year = "1996",
;   title = "Robotics and the common sense informatic situation",
;   editor = "Wolfgang Wahlster",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{T}welfth \uppercase{E}uropean \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
;   pages = "684--688",
;   address = "Chichester, UK",
;   publisher = "John Wiley",
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

option renaming off

load foundations/Root.e
load foundations/EC.e

sort coord: integer

sort direction: integer
; 0 -> 0, 1 -> 90, 2 -> 180, 3 -> 370

sort robot

robot Robot1

function Sin(direction): coord
function Cos(direction): coord

Sin(0)=0.
Sin(1)=1.
Sin(2)=2.
Sin(3)=3.

Cos(0)=1.
Cos(1)=2.
Cos(2)=3.
Cos(3)=4.

fluent Direction(robot,direction)
fluent Location(robot,coord,coord)

event MoveLeftWheel(robot)
event MoveRightWheel(robot)

; Sigma

[robot,direction1,direction2,time]
!Happens(MoveRightWheel(robot),time) &
HoldsAt(Direction(robot,direction1),time) &
direction2 = (direction1-1)->
Initiates(MoveLeftWheel(robot),Direction(robot,direction2),time).

[robot,direction,time]
!Happens(MoveRightWheel(robot),time) &
HoldsAt(Direction(robot,direction),time) ->
Terminates(MoveLeftWheel(robot),Direction(robot,direction),time).

[robot,direction1,direction2,time]
!Happens(MoveLeftWheel(robot),time) &
HoldsAt(Direction(robot,direction1),time) &
direction2 = (direction1+1)->
Initiates(MoveRightWheel(robot),Direction(robot,direction2),time).

[robot,direction,time]
!Happens(MoveLeftWheel(robot),time) &
HoldsAt(Direction(robot,direction),time) ->
Terminates(MoveRightWheel(robot),Direction(robot,direction),time).

[robot,direction,coord1,coord2,coord3,coord4,time]
Happens(MoveLeftWheel(robot),time) &
HoldsAt(Location(robot,coord1,coord2),time) &
HoldsAt(Direction(robot,direction),time) &
coord3 = coord1+Cos(direction) &
coord4 = coord2+Sin(direction) ->
Initiates(MoveRightWheel(robot),
          Location(robot,coord3,coord4),
          time).

[robot,coord1,coord2,time]
Happens(MoveLeftWheel(robot),time) &
HoldsAt(Location(robot,coord1,coord2),time) ->
; FIX: Direction not needed!!
; HoldsAt(Direction(robot,direction),time) ->
Terminates(MoveRightWheel(robot),Location(robot,coord1,coord2),time).

; Delta

Happens(MoveRightWheel(Robot1),0).
Happens(MoveLeftWheel(Robot1),1).
Happens(MoveRightWheel(Robot1),1).

; Psi


[robot,coord1,coord2,coord3,coord4,time]
HoldsAt(Location(robot,coord1,coord2),time) &
HoldsAt(Location(robot,coord3,coord4),time) ->
coord1=coord3 &
coord2=coord4.

[robot,direction1,direction2,time]
HoldsAt(Direction(robot,direction1),time) &
HoldsAt(Direction(robot,direction2),time) ->
direction1=direction2.

; Gamma

HoldsAt(Location(Robot1,0,0),0).
HoldsAt(Direction(Robot1,0),0).

completion Happens

range time 0 3
range coord 0 3
range direction 0 3
range offset 1 1

; End of file.
