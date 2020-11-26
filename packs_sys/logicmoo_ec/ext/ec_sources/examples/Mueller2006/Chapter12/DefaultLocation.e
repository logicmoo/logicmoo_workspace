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
sort device: object
sort tv: device
sort room

agent Nathan
tv TV
room LivingRoom, Kitchen

event TurnOn(agent,device)
event Walk(agent,room,room)

fluent InRoom(object,room)
fluent On(device)
fluent PluggedIn(device)
fluent BrokenSwitch(device)

predicate Ab1(device,time)
predicate Ab2(room,time)

; Sigma

[agent,room1,room2,time]
Initiates(Walk(agent,room1,room2),InRoom(agent,room2),time).

[agent,room1,room2,time]
room1!=room2 ->
Terminates(Walk(agent,room1,room2),InRoom(agent,room1),time).

[agent,device,time]
!Ab1(device,time) ->
Initiates(TurnOn(agent,device),On(device),time).

; Delta

[agent,room1,room2,time]
Happens(Walk(agent,room1,room2),time) ->
room1!=room2 &
HoldsAt(InRoom(agent,room1),time).

[agent,device,time]
Happens(TurnOn(agent,device),time) ->
{room} HoldsAt(InRoom(agent,room),time) &
       HoldsAt(InRoom(device,room),time).

[event1,event2,time]
Happens(event1,time) &
Happens(event2,time) ->
event1=event2.

; Theta

Theta: [device,time] HoldsAt(BrokenSwitch(device),time) -> Ab1(device,time).
Theta: [device,time] !HoldsAt(PluggedIn(device),time) -> Ab1(device,time).

; Psi

[object,room1,room2,time]
HoldsAt(InRoom(object,room1),time) &
HoldsAt(InRoom(object,room2),time) ->
room1=room2.

; Gamma

[tv] !HoldsAt(On(tv),0).
[tv] !HoldsAt(BrokenSwitch(tv),0).
[tv] HoldsAt(PluggedIn(tv),0).

HoldsAt(InRoom(Nathan,Kitchen),0).

[time]
!Ab2(LivingRoom,time) ->
{tv} HoldsAt(InRoom(tv,LivingRoom),time).

; goal

{tv} Happens(TurnOn(Nathan,tv),1).

; for two TVs:
;[tv,time] !HoldsAt(InRoom(tv,Kitchen),time).
;[tv,time] {room} HoldsAt(InRoom(tv,room),time).

completion Theta Ab1
completion Theta Ab2

range time 0 2
range offset 1 1

; End of file.
