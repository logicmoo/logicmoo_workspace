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
load answers/Mueller2004c/Cognition.e
load answers/Mueller2003/Sleep.e
load answers/Mueller2003/Vision.e
load answers/Mueller2004c/HandTo.e

location Location1

portal DummyPortal1

agent Recipient1

letter Letter1

container Envelope1

physobj Surface1

chair Chair1

content Content1

agent Carrier1

; prune
sort ona, onb
fluent! On(ona,onb)
event! PlaceOn(agent,ona,onb)
event! TakeOffOf(agent,ona,onb)
ona! Envelope1, Letter1
onb! Surface1

sort insidea, insideb
fluent! Inside(insidea,insideb)
event! PutInside(agent,insidea,insideb)
event! TakeOutOf(agent,insidea,insideb)
insidea! Letter1
insideb! Envelope1

; initial state
[agent,physobj] !HoldsAt(SittingOn(agent,physobj),0).
[agent,physobj] !HoldsAt(LyingOn(agent,physobj),0).
HoldsAt(Dressed(Recipient1),0).
HoldsAt(Dressed(Carrier1),0).
HoldsAt(Awake(Recipient1),0).
HoldsAt(Awake(Carrier1),0).
HoldsAt(Sleep3(Recipient1),0).
HoldsAt(Sleep3(Carrier1),0).
HoldsAt(Standing(Recipient1),0).
HoldsAt(Standing(Carrier1),0).
HoldsAt(ContainerClosed(Envelope1),0).
HoldsAt(Inside(Letter1,Envelope1),0).
[physobj1,physobj2]
!(physobj1=Letter1 & physobj2=Envelope1) ->
!HoldsAt(Inside(physobj1, physobj2),0).
[agent,object] !HoldsAt(See(agent,object),0).
[agent,object]
!(agent=Carrier1 & object=Envelope1) ->
!HoldsAt(Holding(agent,object),0).
HoldsAt(Holding(Carrier1,Envelope1),0).
[physobj1,physobj2] !HoldsAt(On(physobj1, physobj2),0).

; narrative
Happens(PlaceOn(Carrier1,Envelope1,Surface1),0).
Happens(TakeOffOf(Recipient1,Envelope1,Surface1),1).
;Happens(HandTo(Carrier1,Recipient1,Envelope1),0).
Happens(SitOn(Recipient1,Chair1),2).
Happens(ContainerOpen(Recipient1,Envelope1),3).
Happens(TakeOutOf(Recipient1,Letter1,Envelope1),4).
Happens(LookAt(Recipient1,Letter1),5).
Happens(Read(Recipient1,Letter1,Content1),6).
Happens(ThinkAbout(Recipient1,Content1),7).
Happens(Understand(Recipient1,Content1),8).
Happens(PutInside(Recipient1,Letter1,Envelope1),9).
Happens(RiseFrom(Recipient1,Chair1),10).
Happens(PlaceOn(Recipient1,Envelope1,Surface1),11).

range time 0 12
range offset 0 0
range diameter 0 0

completion Happens

; End of file.
