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

option modeldiff on

ignore Love, ThreatenedBy
ignore LookOutOnto, Floor, BuildingOf, SkyOf, GroundOf
ignore Near, WalkFrom, WalkFromTo, RunFromTo
ignore Side1, Side2

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/Container.e
load answers/Mueller2003/Sleep.e
load answers/Mueller2004c/SmallFire.e
load answers/Mueller2004c/Smoke.e

location Location1

portal DummyPortal1

agent Smoker1

cigarette Cigarette1

container Package1

physobj Surface1

physobj LightingDevice1

ashtray AshTray1

physobj Trash1

smoke Smoke1

; prune
sort ona, onb
fluent! On(ona,onb)
event! PlaceOn(agent,ona,onb)
event! TakeOffOf(agent,ona,onb)
ona! LightingDevice1, Package1, Cigarette1
onb! Surface1, AshTray1

sort insidea, insideb
fluent! Inside(insidea,insideb)
event! PutInside(agent,insidea,insideb)
event! TakeOutOf(agent,insidea,insideb)
insidea! Cigarette1
insideb! Package1, Trash1

sort lighta, lightb, lightc
event! LightWith(lighta,lightb,lightc)
lighta! Smoker1
lightb! Cigarette1
lightc! LightingDevice1

; initial state
[agent,object] !HoldsAt(Holding(agent,object),0).
[agent,physobj] !HoldsAt(SittingOn(agent,physobj),0).
[agent,physobj] !HoldsAt(LyingOn(agent,physobj),0).
HoldsAt(On(Package1,Surface1),0).
[physobj1,physobj2]
!(physobj1=Package1 & physobj2=Surface1) ->
!HoldsAt(On(physobj1, physobj2),0).
HoldsAt(Dressed(Smoker1),0).
HoldsAt(Awake(Smoker1),0).
HoldsAt(Sleep3(Smoker1),0).
HoldsAt(Standing(Smoker1),0).
HoldsAt(CraveNicotine(Smoker1),0).
HoldsAt(ContainerClosed(Package1),0).
[physobj] !HoldsAt(IsBurning(physobj),0).
HoldsAt(Inside(Cigarette1,Package1),0).
[physobj1,physobj2]
!(physobj1=Cigarette1 & physobj2=Package1) ->
!HoldsAt(Inside(physobj1, physobj2),0).

; narrative
Happens(TakeOffOf(Smoker1,Package1,Surface1),0).
Happens(ContainerOpen(Smoker1,Package1),1).
Happens(TakeOutOf(Smoker1,Cigarette1,Package1),2).
Happens(PickUp(Smoker1,LightingDevice1),3).
Happens(Light(Smoker1,LightingDevice1),4).
Happens(LightWith(Smoker1,Cigarette1,LightingDevice1),5).
Happens(BlowOut(Smoker1,LightingDevice1),6).
Happens(PlaceOn(Smoker1,LightingDevice1,Surface1),7).
Happens(PlaceOn(Smoker1,Package1,Surface1),8).
Happens(Puff(Smoker1,Cigarette1),9).
Happens(BlowOutSmoke(Smoker1,Smoke1),10).
Happens(PlaceOn(Smoker1,Cigarette1,AshTray1),11).
Happens(TakeOffOf(Smoker1,Cigarette1,AshTray1),12).
Happens(Puff(Smoker1,Cigarette1),13).
Happens(PutOut(Smoker1,Cigarette1),14).
Happens(PutInside(Smoker1,Cigarette1,Trash1),15).

range time 0 16
range offset 0 0
range diameter 0 0

completion Happens

; End of file.
