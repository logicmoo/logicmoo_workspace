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
; @article{Akman:2004,
;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
;   year = "2004",
;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "105--140",
; }
;

sort position: integer
sort location
sort cage: location
sort gate
sort animal
sort elephant: animal
sort horse: animal
sort dog: animal
sort human: animal
sort species

function Loc(position): location
function Side1(gate): position
function Side2(gate): position
function Species(animal): species

predicate Accessible(position,position,time)
predicate Adult(animal)
predicate Large(animal)
predicate LargeSpecies(species)
predicate Neighbor(position,position)
predicate Sides(position,position,gate)

event Close(human,gate)
event GetOff(human,animal)
event Mount(human,animal)
event Move(animal,position)
event Open(human,gate)
event ThrowOff(animal,human)

fluent AbnormalEncroachment(human)
noninertial AbnormalEncroachment
fluent DoneBy(event,animal)
noninertial DoneBy
fluent Mounted(human,animal)
fluent MountFails(human)
noninertial MountFails
fluent Moves(animal)
noninertial Moves
fluent Opened(gate)
fluent Pos(animal,position)
fluent PosDeterminingFluent(human,position)
noninertial PosDeterminingFluent
fluent ThrowOffFails(animal,human)
noninertial ThrowOffFails

species HumanSpecies, ElephantSpecies, HorseSpecies, DogSpecies
location Outside

LargeSpecies(HumanSpecies).
LargeSpecies(ElephantSpecies).
LargeSpecies(HorseSpecies).
!LargeSpecies(DogSpecies).

[event,animal,time]
HoldsAt(DoneBy(event,animal),time) <->
(Happens(event,time) &
 (({gate} event=Close(animal,gate)) |
  ({animal1} event=GetOff(animal,animal1))|
  ({animal1} event=Mount(animal,animal1))|
  ({position} event=Move(animal,position))|
  ({gate} event=Open(animal,gate)) |
  ({human1} event=ThrowOff(animal,human1)))).

[event1,event2,animal,time]
HoldsAt(DoneBy(event1,animal),time) &
HoldsAt(DoneBy(event2,animal),time) ->
event1=event2.

[animal] Large(animal) <-> (Adult(animal) & LargeSpecies(Species(animal))).

[position] {position1} position1!=position & Neighbor(position,position1).

[position] !Neighbor(position,position).

[position1,position2]
Neighbor(position1,position2) ->
Neighbor(position2,position1).

[cage] cage!=Outside.

[position1,position2,gate]
Sides(position1,position2,gate) <->
((Side1(gate)=position1 &
  Side2(gate)=position2) |
 (Side2(gate)=position1 &
  Side1(gate)=position2)).

[gate] Loc(Side1(gate))!=Loc(Side2(gate)).

[position1,position2,gate1,gate2]
Sides(position1,position2,gate1) &
Sides(position1,position2,gate2) ->
gate1=gate2.

[position1,position2,gate]
Sides(position1,position2,gate) ->
Neighbor(position1,position2).

[position1,position2]
Loc(position1) != Loc(position2) &
Neighbor(position1,position2) ->
{gate} Sides(position1,position2,gate).

[animal,position1,position2,time]
HoldsAt(Pos(animal,position1),time) &
HoldsAt(Pos(animal,position2),time) ->
position1=position2.

[animal,time]
{position} HoldsAt(Pos(animal,position),time).

[animal1,animal2,position,time]
(animal1!=animal2 &
 Large(animal1) &
 Large(animal2) &
 HoldsAt(Pos(animal1,position),time) &
 HoldsAt(Pos(animal2,position),time)) ->
(({human} human=animal1 & HoldsAt(Mounted(human,animal2),time)) |
 ({human} human=animal2 & HoldsAt(Mounted(human,animal1),time))).

[human,position1,position2,time]
HoldsAt(PosDeterminingFluent(human,position1),time) &
HoldsAt(PosDeterminingFluent(human,position2),time) ->
position1=position2.

[animal,position,time]
Initiates(Move(animal,position),Pos(animal,position),time).

[animal,position1,position2,time]
HoldsAt(Pos(animal,position1),time) ->
Terminates(Move(animal,position2),Pos(animal,position1),time).

[animal,position,time]
Happens(Move(animal,position),time) ->
!HoldsAt(Pos(animal,position),time).

[human,position,time]
Happens(Move(human,position),time) ->
!{animal} HoldsAt(Mounted(human,animal),time).

[human,gate,time]
Initiates(Open(human,gate),Opened(gate),time).

[human,gate,time]
Happens(Open(human,gate),time) ->
!HoldsAt(Opened(gate),time) &
(!{animal} HoldsAt(Mounted(human,animal),time)) &
({position}
 (Side1(gate)=position | Side2(gate)=position) &
 HoldsAt(Pos(human,position),time)).

[human,gate,time]
Terminates(Close(human,gate),Opened(gate),time).

[human,gate,time]
Happens(Close(human,gate),time) ->
HoldsAt(Opened(gate),time) &
(!{animal} HoldsAt(Mounted(human,animal),time)) &
{position}
(Side1(gate)=position | Side2(gate)=position) &
HoldsAt(Pos(human,position),time).

[human,animal,position,time]
HoldsAt(Mounted(human,animal),time) &
HoldsAt(Pos(animal,position),time) ->
HoldsAt(Pos(human,position),time).

[animal,time]
HoldsAt(Moves(animal),time) <->
({position}
 HoldsAt(Pos(animal,position),time) &
 !HoldsAt(Pos(animal,position),time+1)).

[human,time]
HoldsAt(MountFails(human),time) <->
({animal}
  Happens(Mount(human,animal),time) &
  HoldsAt(Moves(animal),time)).

[human,animal,position,time]
!HoldsAt(Moves(animal),time) ->
Releases(Mount(human,animal),Pos(human,position),time).

[human,animal,time]
!HoldsAt(Moves(animal),time) ->
Initiates(Mount(human,animal),Mounted(human,animal),time).

[human,animal,position,time]
HoldsAt(Pos(animal,position),time) &
HoldsAt(Moves(animal),time) ->
Initiates(Mount(human,animal),Pos(human,position),time).

[human,animal,position,time]
HoldsAt(Pos(human,position),time) &
HoldsAt(Moves(animal),time) ->
Terminates(Mount(human,animal),Pos(human,position),time).

[human,animal,time]
Happens(Mount(human,animal),time) ->
Large(animal).

[human,animal,time]
HoldsAt(Mounted(human,animal),time) ->
Large(animal).

[human1,human2,time]
Happens(Mount(human1,human2),time) ->
!Large(human1).

[human1,human2,time]
HoldsAt(Mounted(human1,human2),time) ->
!Large(human1).

[human,animal,time]
Happens(Mount(human,animal),time) ->
!{human1} human1!=human & HoldsAt(Mounted(human1,animal),time).

[human1,human2,animal,time]
HoldsAt(Mounted(human1,animal),time) &
HoldsAt(Mounted(human2,animal),time) ->
human1=human2.

[human,animal,time]
Happens(Mount(human,animal),time) ->
!{human1} human1!=human & HoldsAt(Mounted(human1,human),time).

[human1,human2,time]
Happens(Mount(human1,human2),time) ->
{animal} HoldsAt(Mounted(human2,animal),time).

[human1,human2,time]
HoldsAt(Mounted(human1,human2),time) ->
!{animal} HoldsAt(Mounted(human2,animal),time).

[human,animal,time]
Happens(Mount(human,animal),time) ->
!{animal1} HoldsAt(Mounted(human,animal1),time).

[human,animal,time]
!HoldsAt(Moves(animal),time) ->
Terminates(GetOff(human,animal),Mounted(human,animal),time).

[human,animal,position,time]
!HoldsAt(Moves(animal),time) &
HoldsAt(PosDeterminingFluent(human,position),time) ->
Initiates(GetOff(human,animal),Pos(human,position),time).

[human,animal,position,time]
!HoldsAt(Moves(animal),time) &
HoldsAt(Pos(human,position),time) ->
Terminates(GetOff(human,animal),Pos(human,position),time).

[human,animal,position1,position2,time]
!HoldsAt(Moves(animal),time) &
HoldsAt(Pos(human,position1),time) &
position1!=position2 ->
Terminates(GetOff(human,animal),Pos(human,position2),time).

[human,animal,time]
Happens(GetOff(human,animal),time) ->
HoldsAt(Mounted(human,animal),time).

[animal1,human,time]
HoldsAt(ThrowOffFails(animal1,human),time) <->
({position,animal2}
 animal2!=human &
 HoldsAt(PosDeterminingFluent(human,position),time) &
 Large(animal2) &
 HoldsAt(Pos(animal2,position),time+1)).

[animal,human,position,time]
HoldsAt(PosDeterminingFluent(human,position),time) &
!HoldsAt(ThrowOffFails(animal,human),time) ->
Initiates(ThrowOff(animal,human),Pos(human,position),time).

[animal,human,position,time]
HoldsAt(Pos(human,position),time) &
!HoldsAt(ThrowOffFails(animal,human),time) ->
Terminates(ThrowOff(animal,human),Pos(human,position),time).

[animal,human,position1,position2,time]
!HoldsAt(ThrowOffFails(animal,human),time) &
HoldsAt(Pos(human,position1),time) &
!HoldsAt(PosDeterminingFluent(human,position2),time) &
position1!=position2 ->
Terminates(ThrowOff(animal,human),Pos(human,position2),time).

[human,time]
(!{animal} Happens(ThrowOff(animal,human),time) |
           Happens(GetOff(human,animal),time)) ->
HoldsAt(PosDeterminingFluent(human,1),time).

[human,position,animal1,animal2,time]
HoldsAt(PosDeterminingFluent(human,position),time) &
HoldsAt(ThrowOffFails(animal1,human),time) &
HoldsAt(Pos(animal2,position),time) ->
Initiates(ThrowOff(animal1,human),Mounted(human,animal2),time).

[human,animal,time]
!HoldsAt(ThrowOffFails(animal,human),time) ->
Terminates(ThrowOff(animal,human),Mounted(human,animal),time).

[animal,human,time]
Happens(ThrowOff(animal,human),time) ->
HoldsAt(Mounted(human,animal),time).

[animal,human,time]
Happens(ThrowOff(animal,human),time) ->
!Happens(GetOff(human,animal),time).

[animal,human,time]
Happens(GetOff(human,animal),time) ->
!Happens(ThrowOff(animal,human),time).

[position1,position2,time]
Accessible(position1,position2,time) <->
(Neighbor(position1,position2) &
 !{gate} Sides(position1,position2,gate) &
         !HoldsAt(Opened(gate),time)).

[animal,position1,position2,time]
(position1!=position2 &
 HoldsAt(Pos(animal,position1),time) &
 HoldsAt(Pos(animal,position2),time+1)) ->
Accessible(position1,position2,time).

[human,time]
HoldsAt(AbnormalEncroachment(human),time) <->
(HoldsAt(MountFails(human),time) |
 ({position,animal1,animal2}
   HoldsAt(PosDeterminingFluent(human,position),time) &
   !HoldsAt(ThrowOffFails(animal2,human),time) &
   Happens(ThrowOff(animal2,human),time) &
   animal1!=human &
   Large(animal1) &
   HoldsAt(Pos(animal1,position),time) &
   !HoldsAt(Pos(animal1,position),time+1))).

[animal1,animal2,position,time]
HoldsAt(Pos(animal1,position),time) &
!HoldsAt(Pos(animal1,position),time+1) &
!HoldsAt(Pos(animal2,position),time) &
HoldsAt(Pos(animal2,position),time+1) ->
(!Large(animal1) |
 !Large(animal2) |
 ({human} human=animal2 & HoldsAt(AbnormalEncroachment(human),time))).

[animal1,animal2,position1,position2,time]
animal1!=animal2 &
Large(animal1) & Large(animal2) &
HoldsAt(Pos(animal1,position1),time) &
HoldsAt(Pos(animal1,position2),time+1) &
HoldsAt(Pos(animal2,position1),time) &
HoldsAt(Pos(animal2,position2),time+1) ->
!{gate} Sides(position1,position2,gate).

[animal1,animal2,position1,position2,time]
animal1!=animal2 &
Large(animal1) & Large(animal2) &
HoldsAt(Pos(animal1,position1),time) &
HoldsAt(Pos(animal1,position2),time+1) &
HoldsAt(Pos(animal2,position2),time) &
HoldsAt(Pos(animal2,position1),time+1) ->
!{gate} Sides(position1,position2,gate).

[gate,position1,position2,time]
HoldsAt(Opened(gate),time) &
!HoldsAt(Opened(gate),time+1) &
Sides(position1,position2,gate) ->
!{animal}
HoldsAt(Pos(animal,position1),time) &
HoldsAt(Pos(animal,position2),time+1).

gate GateAO
cage CageA

Loc(1)=CageA.
Loc(2)=CageA.
Loc(3)=CageA.
Loc(4)=CageA.
Loc(5)=Outside.
Loc(6)=Outside.
Loc(7)=Outside.
Loc(8)=Outside.

[position1,position2]
Neighbor(position1,position2) <->
((position1=1 & position2=2) |
 (position1=1 & position2=3) |
 (position1=1 & position2=4) |
 (position1=2 & position2=3) |
 (position1=2 & position2=4) |
 (position1=3 & position2=4) |
 (position1=5 & position2=6) |
 (position1=5 & position2=7) |
 (position1=5 & position2=8) |
 (position1=6 & position2=7) |
 (position1=6 & position2=8) |
 (position1=7 & position2=8) |
 (position2=1 & position1=2) |
 (position2=1 & position1=3) |
 (position2=1 & position1=4) |
 (position2=2 & position1=3) |
 (position2=2 & position1=4) |
 (position2=3 & position1=4) |
 (position2=5 & position1=6) |
 (position2=5 & position1=7) |
 (position2=5 & position1=8) |
 (position2=6 & position1=7) |
 (position2=6 & position1=8) |
 (position2=7 & position1=8) |
 (position1=4 & position2=7) |
 (position2=4 & position1=7)).

Side1(GateAO)=4.
Side2(GateAO)=7.

; End of file.
