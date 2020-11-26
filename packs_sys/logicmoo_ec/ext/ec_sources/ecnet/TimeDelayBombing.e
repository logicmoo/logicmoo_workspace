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
; DEV-MUC3-0008
; TimeDelayBombing
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
load answers/Mueller2004c/Condition.e
load answers/Mueller2003/Sleep.e
load answers/Mueller2004c/Bomb.e

bomb Bomb1
BombTimeDelay(Bomb1)=3.
[time] !HoldsAt(DestroyedDeterminingFluent(Bomb1),time).
[time] !HoldsAt(DamagedDeterminingFluent(Bomb1),time).

agent Perp1
[time] !HoldsAt(InjuredDeterminingFluent(Perp1),time).
[time] !HoldsAt(KilledDeterminingFluent(Perp1),time).

agent HumanTarget1
HoldsAt(Alive(HumanTarget1),0).
HoldsAt(Awake(HumanTarget1),0).
HoldsAt(Standing(HumanTarget1),0).
HoldsAt(Sleep2(HumanTarget1),0).
!HoldsAt(Injured(HumanTarget1),0).
[object] !HoldsAt(Holding(HumanTarget1,object),0).
HoldsAt(At(HumanTarget1,Outside1),0).

[time] HoldsAt(KilledDeterminingFluent(HumanTarget1),time).
[time] !HoldsAt(InjuredDeterminingFluent(HumanTarget1),time).

agent HumanTarget2
HoldsAt(Alive(HumanTarget2),0).
HoldsAt(Awake(HumanTarget2),0).
HoldsAt(Standing(HumanTarget2),0).
HoldsAt(Sleep2(HumanTarget2),0).
!HoldsAt(Injured(HumanTarget2),0).
[object] !HoldsAt(Holding(HumanTarget2,object),0).
HoldsAt(At(HumanTarget2,Outside1),0).

[time] HoldsAt(KilledDeterminingFluent(HumanTarget2),time).
[time] !HoldsAt(InjuredDeterminingFluent(HumanTarget2),time).

agent HumanTarget3
HoldsAt(Alive(HumanTarget3),0).
HoldsAt(Awake(HumanTarget3),0).
HoldsAt(Standing(HumanTarget3),0).
HoldsAt(Sleep2(HumanTarget3),0).
!HoldsAt(Injured(HumanTarget3),0).
[object] !HoldsAt(Holding(HumanTarget3,object),0).
HoldsAt(At(HumanTarget3,Outside1),0).

[time] HoldsAt(KilledDeterminingFluent(HumanTarget3),time).
[time] !HoldsAt(InjuredDeterminingFluent(HumanTarget3),time).

agent HumanTarget4
HoldsAt(Alive(HumanTarget4),0).
HoldsAt(Awake(HumanTarget4),0).
HoldsAt(Standing(HumanTarget4),0).
HoldsAt(Sleep2(HumanTarget4),0).
!HoldsAt(Injured(HumanTarget4),0).
[object] !HoldsAt(Holding(HumanTarget4,object),0).
HoldsAt(At(HumanTarget4,Outside1),0).

[time] HoldsAt(KilledDeterminingFluent(HumanTarget4),time).
[time] !HoldsAt(InjuredDeterminingFluent(HumanTarget4),time).

physobj PhysTarget1
HoldsAt(Intact(PhysTarget1),0).
HoldsAt(At(PhysTarget1,Outside1),0).

[time] HoldsAt(DestroyedDeterminingFluent(PhysTarget1),time).
[time] !HoldsAt(DamagedDeterminingFluent(PhysTarget1),time).

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
HoldsAt(At(Bomb1,Outside2),0).
HoldsAt(At(Perp1,Outside2),0).
HoldsAt(Intact(Bomb1),0).
!HoldsAt(BombActivated(Bomb1),0).
!HoldsAt(DoorIsOpen(Door1),0).
HoldsAt(DoorUnlocked(Door1),0).
HoldsAt(BombTimerValue(Bomb1, BombTimeDelay(Bomb1)),0).

; narrative
Happens(PickUp(Perp1,Bomb1),0).
Happens(WalkStreet21(Perp1,Street1),1).
Happens(BombActivate(Perp1,Bomb1),2).
Happens(LetGoOf(Perp1,Bomb1),3).
Happens(WalkStreet12(Perp1,Street1),4).

range time 0 7
range offset 0 3
range diameter 0 0

completion Happens

; End of file.
