% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Kidnapping.e',127).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.lps.pl')).
% Fri, 26 Mar 2021 01:06:01 GMT File: <stream>(0x555567a69700)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; @article{Mueller:2004c,
%;   author = "Erik T. Mueller",
%;   year = "2004",
%;   title = "Understanding script-based stories using commonsense reasoning",
%;   journal = "Cognitive Systems Research",
%;   volume = "5",
%;   number = "4",
%;   pages = "307--340",
%; }
%;

% option modeldiff on
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',22).
% From E: 
% 
% ':-'(call_pel_directive(option(modeldiff,on))).
:- call_pel_directive(option(modeldiff, on)).

% ignore LookOutOnto, Floor, BuildingOf, SkyOf, GroundOf
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(lookOutOnto))).
:- call_pel_directive(ignore(lookOutOnto)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(floor))).
:- call_pel_directive(ignore(floor)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(buildingOf))).
:- call_pel_directive(ignore(buildingOf)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(skyOf))).
:- call_pel_directive(ignore(skyOf)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(groundOf))).
:- call_pel_directive(ignore(groundOf)).

% ignore Near, WalkFrom, WalkFromTo, RunFromTo
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(near))).
:- call_pel_directive(ignore(near)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(walkFrom))).
:- call_pel_directive(ignore(walkFrom)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(walkFromTo))).
:- call_pel_directive(ignore(walkFromTo)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(runFromTo))).
:- call_pel_directive(ignore(runFromTo)).

% ignore Side1, Side2
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',26).
% From E: 
% 
% ':-'(call_pel_directive(ignore(side1))).
:- call_pel_directive(ignore(side1)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(side2))).
:- call_pel_directive(ignore(side2)).

% ignore FriendOf, NeutralOf, EnemyOf,
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',26).
% From E: 
% 
% ':-'(call_pel_directive(ignore(friendOf))).
:- call_pel_directive(ignore(friendOf)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(neutralOf))).
:- call_pel_directive(ignore(neutralOf)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',26).
% From E: 
% 
% ':-'(call_pel_directive(ignore(enemyOf))).
:- call_pel_directive(ignore(enemyOf)).

% ignore BecomeFriends, BecomeNeutral, BecomeEnemies
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',28).
% From E: 
% 
% ':-'(call_pel_directive(ignore(becomeFriends))).
:- call_pel_directive(ignore(becomeFriends)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(becomeNeutral))).
:- call_pel_directive(ignore(becomeNeutral)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',28).
% From E: 
% 
% ':-'(call_pel_directive(ignore(becomeEnemies))).
:- call_pel_directive(ignore(becomeEnemies)).

% ignore Happy, Calm, Unhappy
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',28).
% From E: 
% 
% ':-'(call_pel_directive(ignore(happy))).
:- call_pel_directive(ignore(happy)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(calm))).
:- call_pel_directive(ignore(calm)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',28).
% From E: 
% 
% ':-'(call_pel_directive(ignore(unhappy))).
:- call_pel_directive(ignore(unhappy)).

% ignore BecomeHappy, BecomeCalm, BecomeUnhappy
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',30).
% From E: 
% 
% ':-'(call_pel_directive(ignore(becomeHappy))).
:- call_pel_directive(ignore(becomeHappy)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(becomeCalm))).
:- call_pel_directive(ignore(becomeCalm)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',30).
% From E: 
% 
% ':-'(call_pel_directive(ignore(becomeUnhappy))).
:- call_pel_directive(ignore(becomeUnhappy)).

% ignore AngryAt, BecomeAngryAt
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',30).
% From E: 
% 
% ':-'(call_pel_directive(ignore(angryAt))).
:- call_pel_directive(ignore(angryAt)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(becomeAngryAt))).
:- call_pel_directive(ignore(becomeAngryAt)).

% ignore Like, Love, Dislike, LikeSnow
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',32).
% From E: 
% 
% ':-'(call_pel_directive(ignore(like))).
:- call_pel_directive(ignore(like)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(love))).
:- call_pel_directive(ignore(love)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',32).
% From E: 
% 
% ':-'(call_pel_directive(ignore(dislike))).
:- call_pel_directive(ignore(dislike)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(likeSnow))).
:- call_pel_directive(ignore(likeSnow)).

% ignore HandTo
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',32).
% From E: 
% 
% ':-'(call_pel_directive(ignore(handTo))).
:- call_pel_directive(ignore(handTo)).

% ignore InviteIn, InvitedIn, IntendToWalkIn, IntentionToWalkIn
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',34).
% From E: 
% 
% ':-'(call_pel_directive(ignore(inviteIn))).
:- call_pel_directive(ignore(inviteIn)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(invitedIn))).
:- call_pel_directive(ignore(invitedIn)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',34).
% From E: 
% 
% ':-'(call_pel_directive(ignore(intendToWalkIn))).
:- call_pel_directive(ignore(intendToWalkIn)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(intentionToWalkIn))).
:- call_pel_directive(ignore(intentionToWalkIn)).

% ignore ActOnIntentionToWalkIn, Greet, SayGoodbye, CryForJoy
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',34).
% From E: 
% 
% ':-'(call_pel_directive(ignore(actOnIntentionToWalkIn))).
:- call_pel_directive(ignore(actOnIntentionToWalkIn)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(greet))).
:- call_pel_directive(ignore(greet)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',34).
% From E: 
% 
% ':-'(call_pel_directive(ignore(sayGoodbye))).
:- call_pel_directive(ignore(sayGoodbye)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(cryForJoy))).
:- call_pel_directive(ignore(cryForJoy)).

% ignore Threaten, ReleaseFromThreat, ThreatenedBy
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',36).
% From E: 
% 
% ':-'(call_pel_directive(ignore(threaten))).
:- call_pel_directive(ignore(threaten)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(releaseFromThreat))).
:- call_pel_directive(ignore(releaseFromThreat)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',36).
% From E: 
% 
% ':-'(call_pel_directive(ignore(threatenedBy))).
:- call_pel_directive(ignore(threatenedBy)).

% ignore Order, KnowOrder, Request, KnowRequest
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',36).
% From E: 
% 
% ':-'(call_pel_directive(ignore(order))).
:- call_pel_directive(ignore(order)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(knowOrder))).
:- call_pel_directive(ignore(knowOrder)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',36).
% From E: 
% 
% ':-'(call_pel_directive(ignore(request))).
:- call_pel_directive(ignore(request)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(knowRequest))).
:- call_pel_directive(ignore(knowRequest)).

% load foundations/Root.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',39).
% From E: 
% 
% ':-'(call_pel_directive(load('foundations/Root.e'))).
:- call_pel_directive(load('foundations/Root.e')).

% load foundations/EC.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',39).
% From E: 
% 
% ':-'(call_pel_directive(load('foundations/EC.e'))).
:- call_pel_directive(load('foundations/EC.e')).

% load answers/Mueller2003/Ontology.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',41).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2003/Ontology.e'))).
:- call_pel_directive(load('answers/Mueller2003/Ontology.e')).

% load answers/Mueller2003/Feeling.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',41).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2003/Feeling.e'))).
:- call_pel_directive(load('answers/Mueller2003/Feeling.e')).

% load answers/Mueller2004c/HandTo.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',43).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/HandTo.e'))).
:- call_pel_directive(load('answers/Mueller2004c/HandTo.e')).

% load answers/Mueller2004c/RTSpaceM.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',43).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/RTSpaceM.e'))).
:- call_pel_directive(load('answers/Mueller2004c/RTSpaceM.e')).

% load answers/Mueller2003/Sleep.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',45).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2003/Sleep.e'))).
:- call_pel_directive(load('answers/Mueller2003/Sleep.e')).

% load answers/Mueller2003/SpeechAct.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',45).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2003/SpeechAct.e'))).
:- call_pel_directive(load('answers/Mueller2003/SpeechAct.e')).

% load answers/Mueller2004c/IPRel.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',47).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/IPRel.e'))).
:- call_pel_directive(load('answers/Mueller2004c/IPRel.e')).

% location Location1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',49).
% From E: 
% 
% t(location,location1).
isa(location1, location).

% portal DummyPortal1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',51).
% From E: 
% 
% t(portal,dummyPortal1).
isa(dummyPortal1, portal).

% agent Introducer1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',53).
% From E: 
% 
% t(agent,introducer1).
isa(introducer1, agent).

% agent Introducee1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',55).
% From E: 
% 
% t(agent,introducee1).
isa(introducee1, agent).

% agent Introduced1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',57).
% From E: 
% 
% t(agent,introduced1).
isa(introduced1, agent).
%; initial state
% [agent,physobj]
 % !HoldsAt(SittingOn(agent,physobj),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',59).
% From E: 
% 
% holds(
%    not(sittingOn(Agent,Physobj)), 0).
initially not sittingOn(Agent, Physobj).
 %  initial_state([not(sittingOn(Agent,Physobj))]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',61).
% [agent,physobj]
 % !HoldsAt(LyingOn(agent,physobj),0).
% From E: 
% 
% holds(
%    not(lyingOn(Agent,Physobj)), 0).
initially not lyingOn(Agent, Physobj).
 %  initial_state([not(lyingOn(Agent,Physobj))]).
 %  % =================================.


% [agent]
 % HoldsAt(Dressed(agent),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',61).
% From E: 
% 
% holds(
%    dressed(Agent), 0).
initially dressed(Agent).
 %  initial_state([dressed(Agent)]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',63).
% [agent]
 % HoldsAt(Awake(agent),0).
% From E: 
% 
% holds(
%    awake(Agent), 0).
initially awake(Agent).
 %  initial_state([awake(Agent)]).
 %  % =================================.


% [agent]
 % HoldsAt(Sleep3(agent),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',63).
% From E: 
% 
% holds(
%    sleep3(Agent), 0).
initially sleep3(Agent).
 %  initial_state([sleep3(Agent)]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',65).
% [agent]
 % HoldsAt(Standing(agent),0).
% From E: 
% 
% holds(
%    standing(Agent), 0).
initially standing(Agent).
 %  initial_state([standing(Agent)]).
 %  % =================================.


% HoldsAt(AcquaintanceOf(Introducer1,Introducee1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',65).
% From E: 
% 
% holds(
%    acquaintanceOf(introducer1,introducee1), 0).
initially acquaintanceOf(introducer1, introducee1).
 %  initial_state([acquaintanceOf(introducer1,introducee1)]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',67).
% HoldsAt(AcquaintanceOf(Introducer1,Introduced1),0).
% From E: 
% 
% holds(
%    acquaintanceOf(introducer1,introduced1), 0).
initially acquaintanceOf(introducer1, introduced1).
 %  initial_state([acquaintanceOf(introducer1,introduced1)]).
 %  % =================================.


% !HoldsAt(AcquaintanceOf(Introducee1,Introduced1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',67).
% From E: 
% 
% holds(
%    not(acquaintanceOf(introducee1,introduced1)), 0).
initially not acquaintanceOf(introducee1, introduced1).
 %  initial_state([not(acquaintanceOf(introducee1,introduced1))]).
 %  % =================================.


%; narrative
%;Happens(IntroduceMutual(Introducer1,Introducee1,Introduced1),0).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',72).
% Happens(Introduce(Introducee1,Introduced1),0).
% From E: 
% 
% happens(
%    introduce(introducee1,introduced1), 0).
observe introduce(introducee1, introduced1)at 0.
 %  observe([introduce(introducee1,introduced1)],0).
 %  % =================================.


%;Happens(Introduce(Introduced1,Introducee1),0).
%;Happens(Smile(Introducer1),1).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',75).
% Happens(Smile(Introducee1),1).
% From E: 
% 
% happens(
%    smile(introducee1), 1).
observe smile(introducee1)at 1.
 %  observe([smile(introducee1)],1).
 %  % =================================.


% Happens(Smile(Introduced1),2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',75).
% From E: 
% 
% happens(
%    smile(introduced1), 2).
observe smile(introduced1)at 2.
 %  observe([smile(introduced1)],2).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',77).
% Happens(SayPleasedToMeet(Introducee1,Introduced1),3).
% From E: 
% 
% happens(
%    sayPleasedToMeet(introducee1,introduced1), 3).
observe sayPleasedToMeet(introducee1, introduced1)at 3.
 %  observe([sayPleasedToMeet(introducee1,introduced1)],3).
 %  % =================================.


% Happens(SayPleasedToMeet(Introduced1,Introducee1),4).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',77).
% From E: 
% 
% happens(
%    sayPleasedToMeet(introduced1,introducee1), 4).
observe sayPleasedToMeet(introduced1, introducee1)at 4.
 %  observe([sayPleasedToMeet(introduced1,introducee1)],4).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',79).
% Happens(ShakeHands(Introducee1,Introduced1),5).
% From E: 
% 
% happens(
%    shakeHands(introducee1,introduced1), 5).
observe shakeHands(introducee1, introduced1)at 5.
 %  observe([shakeHands(introducee1,introduced1)],5).
 %  % =================================.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',81).
% range time 0 6
% From E: 
% 
% ':-'(call_pel_directive(range(time,0,6))).
:- call_pel_directive(range(time, 0, 6)).

% range offset 0 0
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',81).
% From E: 
% 
% ':-'(call_pel_directive(range(offset,0,0))).
:- call_pel_directive(range(offset, 0, 0)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',83).
% range diameter 0 0
% From E: 
% 
% ':-'(call_pel_directive(range(diameter,0,0))).
:- call_pel_directive(range(diameter, 0, 0)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',85).
% completion Happens
% From E: 
% 
% ':-'(call_pel_directive(completion(happens))).
:- call_pel_directive(completion(happens)).
%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.e',87).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/MakingAnAcquaintance.lps.pl')).
