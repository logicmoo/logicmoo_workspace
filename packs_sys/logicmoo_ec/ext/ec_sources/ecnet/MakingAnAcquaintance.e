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

ignore LookOutOnto, Floor, BuildingOf, SkyOf, GroundOf
ignore Near, WalkFrom, WalkFromTo, RunFromTo
ignore Side1, Side2
ignore FriendOf, NeutralOf, EnemyOf,
ignore BecomeFriends, BecomeNeutral, BecomeEnemies
ignore Happy, Calm, Unhappy
ignore BecomeHappy, BecomeCalm, BecomeUnhappy
ignore AngryAt, BecomeAngryAt
ignore Like, Love, Dislike, LikeSnow
ignore HandTo
ignore InviteIn, InvitedIn, IntendToWalkIn, IntentionToWalkIn
ignore ActOnIntentionToWalkIn, Greet, SayGoodbye, CryForJoy
ignore Threaten, ReleaseFromThreat, ThreatenedBy
ignore Order, KnowOrder, Request, KnowRequest

load foundations/Root.e
load foundations/EC.e
load answers/Mueller2003/Ontology.e
load answers/Mueller2003/Feeling.e
load answers/Mueller2004c/HandTo.e
load answers/Mueller2004c/RTSpaceM.e
load answers/Mueller2003/Sleep.e
load answers/Mueller2003/SpeechAct.e
load answers/Mueller2004c/IPRel.e

location Location1

portal DummyPortal1

agent Introducer1

agent Introducee1

agent Introduced1

; initial state
[agent,physobj] !HoldsAt(SittingOn(agent,physobj),0).
[agent,physobj] !HoldsAt(LyingOn(agent,physobj),0).
[agent] HoldsAt(Dressed(agent),0).
[agent] HoldsAt(Awake(agent),0).
[agent] HoldsAt(Sleep3(agent),0).
[agent] HoldsAt(Standing(agent),0).
HoldsAt(AcquaintanceOf(Introducer1,Introducee1),0).
HoldsAt(AcquaintanceOf(Introducer1,Introduced1),0).
!HoldsAt(AcquaintanceOf(Introducee1,Introduced1),0).

; narrative
;Happens(IntroduceMutual(Introducer1,Introducee1,Introduced1),0).
Happens(Introduce(Introducee1,Introduced1),0).
;Happens(Introduce(Introduced1,Introducee1),0).
;Happens(Smile(Introducer1),1).
Happens(Smile(Introducee1),1).
Happens(Smile(Introduced1),2).
Happens(SayPleasedToMeet(Introducee1,Introduced1),3).
Happens(SayPleasedToMeet(Introduced1,Introducee1),4).
Happens(ShakeHands(Introducee1,Introduced1),5).

range time 0 6
range offset 0 0
range diameter 0 0

completion Happens

; End of file.
