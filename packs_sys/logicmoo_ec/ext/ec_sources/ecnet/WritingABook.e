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
ignore Inside
ignore Near, WalkFrom, WalkFromTo, RunFromTo
ignore See

option renaming off

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/HandTo.e
load answers/Mueller2003/Sleep.e
load answers/Mueller2004c/Cognition.e

door Door1

room Room0

room Room1

Side1(Door1)=Room0.
Side2(Door1)=Room1.

agent Writer1

paper Paper1

pen Pen1

chair Chair1

physobj Desk1

; initial state
[agent,object] !HoldsAt(Holding(agent,object),0).
[agent,physobj] !HoldsAt(SittingOn(agent,physobj),0).
[agent,physobj] !HoldsAt(LyingOn(agent,physobj),0).
[physobj1,physobj2]
!(physobj1=Pen1 & physobj2=Desk1) &
!(physobj1=Paper1 & physobj2=Desk1) ->
!HoldsAt(On(physobj1, physobj2),0).
HoldsAt(On(Paper1,Desk1),0).
HoldsAt(On(Pen1,Desk1),0).
HoldsAt(Dressed(Writer1),0).
HoldsAt(Awake(Writer1),0).
HoldsAt(Sleep3(Writer1),0).
HoldsAt(Standing(Writer1),0).
HoldsAt(DoorUnlocked(Door1),0).
HoldsAt(DoorIsOpen(Door1),0).
HoldsAt(At(Writer1,Room0),0).
HoldsAt(At(Chair1,Room1),0).
HoldsAt(At(Desk1,Room1),0).

; narrative
Happens(WalkThroughDoor12(Writer1,Door1),0).
Happens(SitOn(Writer1,Chair1),1).
Happens(TakeOffOf(Writer1,Pen1,Desk1),2).
Happens(Think(Writer1),3).
Happens(WriteOn(Writer1,Paper1,Pen1),4).
Happens(WriteOn(Writer1,Paper1,Pen1),5).
Happens(PlaceOn(Writer1,Pen1,Desk1),6).
Happens(RiseFrom(Writer1,Chair1),7).
Happens(WalkThroughDoor21(Writer1,Door1),8).

range time 0 9
range offset 0 0
range diameter 0 0

completion Happens

; End of file.
