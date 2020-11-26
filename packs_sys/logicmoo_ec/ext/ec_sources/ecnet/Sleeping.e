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
ignore Inside, Near
ignore See

ignore ActOnSleep5

option renaming off

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2004c/OTSpaceM.e
load answers/Mueller2004c/Cognition.e
load answers/Mueller2003/Sleep.e

door Door1

room Room0

room Room1

Side1(Door1)=Room0.
Side2(Door1)=Room1.

agent Sleeper1

bed Bed1

outside Outside1

; initial state
[agent,object] !HoldsAt(Holding(agent,object),0).
[agent,physobj] !HoldsAt(SittingOn(agent,physobj),0).
[agent,physobj] !HoldsAt(LyingOn(agent,physobj),0).
HoldsAt(Dressed(Sleeper1),0).
HoldsAt(Awake(Sleeper1),0).
HoldsAt(Sleep3(Sleeper1),0).
HoldsAt(Standing(Sleeper1),0).
HoldsAt(DoorUnlocked(Door1),0).
HoldsAt(DoorIsOpen(Door1),0).
HoldsAt(At(Sleeper1,Room0),0).
HoldsAt(At(Bed1,Room1),0).

; narrative
Happens(GetTired(Sleeper1),0).
Happens(WalkThroughDoor12(Sleeper1,Door1),1).
Happens(GetUndressed(Sleeper1),2).
Happens(LieOn(Sleeper1,Bed1),3).
Happens(FallAsleep(Sleeper1),4).
Happens(Dream(Sleeper1),5).
Happens(WakeUp(Sleeper1),6).
Happens(RiseFrom(Sleeper1,Bed1),7).
Happens(GetDressed(Sleeper1),8).
Happens(WalkThroughDoor21(Sleeper1,Door1),9).

range time 0 10
range offset 0 0
range diameter 0 0

completion Happens

; End of file.
