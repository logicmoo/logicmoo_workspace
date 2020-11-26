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

option renaming off

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/Book.e
load answers/Mueller2004c/Cognition.e
load answers/Mueller2003/Sleep.e
load answers/Mueller2003/Vision.e

door Door1

room Room0

room Room1

Side1(Door1)=Room0.
Side2(Door1)=Room1.

agent Reader1

book Book1

chair Chair1

physobj BookSupport1

content Content1

; initial state
[agent,object] !HoldsAt(Holding(agent,object),0).
[agent,physobj] !HoldsAt(SittingOn(agent,physobj),0).
[agent,physobj] !HoldsAt(LyingOn(agent,physobj),0).
!{page} HoldsAt(BookIsOpenTo(Book1,page),0).
[physobj1,physobj2]
!(physobj1=Book1 & physobj2=BookSupport1) ->
!HoldsAt(On(physobj1, physobj2),0).
HoldsAt(Dressed(Reader1),0).
HoldsAt(Awake(Reader1),0).
HoldsAt(Sleep3(Reader1),0).
HoldsAt(Standing(Reader1),0).
HoldsAt(DoorUnlocked(Door1),0).
HoldsAt(DoorIsOpen(Door1),0).
HoldsAt(At(Reader1,Room0),0).
HoldsAt(At(Chair1,Room1),0).
HoldsAt(At(Book1,Room1),0).
HoldsAt(On(Book1,BookSupport1),0).
[object] !HoldsAt(See(Reader1,object),0).

; narrative
Happens(WalkThroughDoor12(Reader1,Door1),0).
Happens(TakeOffOf(Reader1,Book1,BookSupport1),1).
Happens(SitOn(Reader1,Chair1),2).
Happens(BookOpenTo(Reader1,Book1,1),3).
Happens(LookAt(Reader1,Book1),4).
Happens(Read(Reader1,Book1,Content1),5).
Happens(ThinkAbout(Reader1,Content1),6).
Happens(Understand(Reader1,Content1),7).
Happens(BookTurnPageTo(Reader1,Book1,2),8).
Happens(BookTurnPageTo(Reader1,Book1,3),9).
Happens(BookClose(Reader1,Book1),10).
Happens(RiseFrom(Reader1,Chair1),11).
Happens(PlaceOn(Reader1,Book1,BookSupport1),12).
Happens(WalkThroughDoor21(Reader1,Door1),13).

range time 0 14
range page 1 3
range offset 0 0
range diameter 0 0

completion Happens

; End of file.
