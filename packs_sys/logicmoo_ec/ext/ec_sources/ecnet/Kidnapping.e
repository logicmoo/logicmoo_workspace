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
; Kidnapping
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
ignore On, DoorUnlock, DoorLock
ignore WalkDownStaircase, WalkUpStaircase

ignore Request, KnowRequest, Order, KnowOrder, SayGoodbye
ignore IntentionToWalkIn, InvitedIn
ignore Snowing
ignore Like, Dislike, LikeSnow

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2003/Feeling.e
load answers/Mueller2004c/Condition.e
load answers/Mueller2004c/Gun.e
load answers/Mueller2003/Sleep.e
load answers/Mueller2003/SpeechAct.e

gun Gun1
bullet Bullet1
HoldsAt(Intact(Gun1),0).
HoldsAt(Intact(Bullet1),0).

agent Perp1

agent HumanTarget1
HoldsAt(Calm(HumanTarget1),0).
HoldsAt(Alive(HumanTarget1),0).
HoldsAt(Awake(HumanTarget1),0).
HoldsAt(Standing(HumanTarget1),0).
HoldsAt(Sleep2(HumanTarget1),0).
!HoldsAt(Injured(HumanTarget1),0).
[object] !HoldsAt(Holding(HumanTarget1,object),0).
HoldsAt(At(HumanTarget1,Outside1),0).
[physobj1,physobj2] !HoldsAt(Inside(physobj1,physobj2),0).

; prune
sort shoota, shootb, shooto, shooth, shootp
event! Shoot(shoota,shootb,shooto)
event! ShootInjure(shoota,shootb,shooth)
event! ShootKill(shoota,shootb,shooth)
event! ShootDamage(shoota,shootb,shootp)
event! ShootDestroy(shoota,shootb,shootp)
shoota! Perp1
shootb! Gun1
shooto! HumanTarget1
shooth! HumanTarget1

; room-scale topological space
outside Outside1
outside Outside2
room Inside1
door Door1
building Building1
street Street1
Side1(Door1)=Outside2.
Side2(Door1)=Inside1.
LookOutOnto(Inside1)=Outside1.
Floor(Inside1)=1.
BuildingOf(Inside1)=Building1.
Side1(Street1)=Outside1.
Side2(Street1)=Outside2.

HoldsAt(Calm(Perp1),0).
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
[agent1,agent2] !HoldsAt(ThreatenedBy(agent1,agent2),0).
[agent1,agent2] !HoldsAt(AngryAt(agent1,agent2),0).
[physobj1,physobj2] !HoldsAt(Inside(physobj1,physobj2),0).
[agent,object] !HoldsAt(Love(agent,object),0).

; narrative
Happens(PickUp(Perp1,Gun1),0).
Happens(PickUp(Perp1,Bullet1),1).
Happens(PutInside(Perp1,Bullet1,Gun1),2).
Happens(WalkStreet21(Perp1,Street1),3).
Happens(Threaten(Perp1,HumanTarget1,Gun1),4).
Happens(Grab(Perp1,HumanTarget1),5).
Happens(WalkStreet12(Perp1,Street1),6).
Happens(WalkThroughDoor12(Perp1,Door1),7).
Happens(LetGoOf(Perp1,HumanTarget1),8).
Happens(Shoot(Perp1,Gun1,HumanTarget1),9).
Happens(ShootKill(Perp1,Gun1,HumanTarget1),9).

range time 0 10
range offset 0 3
range diameter 0 0

completion Happens

; End of file.
