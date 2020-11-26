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
; DEV-MUC3-0060
; Arson
;
; @article{Mueller:2004c,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Understanding script-based stories using commonsense reasoning",
;   journal = "Cognitive Systems Research",
;   volume = "5",
;   number = "4",
;   pages = "307--340",
; }
;

ignore SkyOf, GroundOf, Near, Inside, WalkFromTo, RunFromTo
ignore RollAlong, Diameter, Move, HoldSome, PutInside
ignore TakeOutOf, On, DoorUnlock, DoorLock, WalkThroughDoor12
ignore WalkThroughDoor21, WalkDownStaircase, WalkUpStaircase

ignore ThreatenedBy

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2004c/Fire.e
load answers/Mueller2004c/Condition.e
load answers/Mueller2003/Sleep.e

fire Fire1
HoldsAt(At(Fire1,Outside1),0).

agent Perp1

physobj PhysTarget1
HoldsAt(Intact(PhysTarget1),0).
HoldsAt(At(PhysTarget1,Outside1),0).

; room-scale topological space
outside Outside1
outside Outside2
room Inside1
door Door1
building Building1
street Street1
Side1(Door1)=Outside1.
Side2(Door1)=Inside1.
LookOutOnto(Inside1)=Outside1.
Floor(Inside1)=1.
BuildingOf(Inside1)=Building1.
Side1(Street1)=Outside1.
Side2(Street1)=Outside2.

HoldsAt(Alive(Perp1),0).
HoldsAt(Awake(Perp1),0).
HoldsAt(Standing(Perp1),0).
HoldsAt(Sleep2(Perp1),0).
!HoldsAt(Injured(Perp1),0).
[object] !HoldsAt(Holding(Perp1,object),0).
HoldsAt(At(Perp1,Outside2),0).
!HoldsAt(DoorIsOpen(Door1),0).
HoldsAt(DoorUnlocked(Door1),0).
[physobj,fire,offset] !HoldsAt(Burning(physobj,fire,offset),0).

; narrative
Happens(WalkStreet21(Perp1,Street1),0).
Happens(SetFireTo(Perp1,PhysTarget1,Fire1,3),1).
Happens(WalkStreet12(Perp1,Street1),2).

range time 0 6
range offset 0 3
range diameter 0 0

completion Happens

; End of file.
