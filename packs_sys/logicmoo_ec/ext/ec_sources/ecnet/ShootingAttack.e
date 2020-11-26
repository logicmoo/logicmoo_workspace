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
; DEV-MUC3-0147
; ShootingAttack
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

ignore SkyOf, GroundOf, Near, WalkFromTo, RunFromTo
ignore RollAlong, Diameter, Move, HoldSome
ignore TakeOutOf, On, DoorUnlock, DoorLock, WalkThroughDoor12
ignore WalkThroughDoor21, WalkDownStaircase, WalkUpStaircase

ignore Love, ThreatenedBy

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2004c/Condition.e
load answers/Mueller2004c/Gun.e
load answers/Mueller2003/Sleep.e

gun Gun1
bullet Bullet1
HoldsAt(Intact(Gun1),0).
HoldsAt(Intact(Bullet1),0).

agent Perp1

physobj PhysTarget1
HoldsAt(Intact(PhysTarget1),0).
HoldsAt(At(PhysTarget1,Outside1),0).

; prune
sort shoota, shootb, shooto, shooth, shootp
event! Shoot(shoota,shootb,shooto)
event! ShootInjure(shoota,shootb,shooth)
event! ShootKill(shoota,shootb,shooth)
event! ShootDamage(shoota,shootb,shootp)
event! ShootDestroy(shoota,shootb,shootp)
shoota! Perp1
shootb! Gun1
shooto! PhysTarget1
shootp! PhysTarget1

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
HoldsAt(At(Gun1,Outside2),0).
HoldsAt(At(Perp1,Outside2),0).
HoldsAt(At(Bullet1,Outside2),0).
HoldsAt(DoorIsOpen(Door1),0).
HoldsAt(DoorUnlocked(Door1),0).
[physobj1,physobj2] !HoldsAt(Inside(physobj1,physobj2),0).

; narrative
Happens(PickUp(Perp1,Gun1),0).
Happens(PickUp(Perp1,Bullet1),1).
Happens(PutInside(Perp1,Bullet1,Gun1),2).
Happens(WalkStreet21(Perp1,Street1),3).
Happens(Shoot(Perp1,Gun1,PhysTarget1),4).
Happens(ShootDestroy(Perp1,Gun1,PhysTarget1),4).
Happens(WalkStreet12(Perp1,Street1),5).

range time 0 6
range offset 0 3
range diameter 0 0

completion Happens

; End of file.
