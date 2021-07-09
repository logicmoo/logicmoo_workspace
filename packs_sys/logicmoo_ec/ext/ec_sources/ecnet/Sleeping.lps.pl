% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',321).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.lps.pl')).
% Fri, 26 Mar 2021 01:06:09 GMT File: <stream>(0x555567c95900)%;
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
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',22).
% From E: 
% 
% ':-'(call_pel_directive(option(modeldiff,on))).
:- call_pel_directive(option(modeldiff, on)).

% ignore Love, ThreatenedBy
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(love))).
:- call_pel_directive(ignore(love)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(threatenedBy))).
:- call_pel_directive(ignore(threatenedBy)).

% ignore LookOutOnto, Floor, BuildingOf, SkyOf, GroundOf
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(lookOutOnto))).
:- call_pel_directive(ignore(lookOutOnto)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(floor))).
:- call_pel_directive(ignore(floor)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(buildingOf))).
:- call_pel_directive(ignore(buildingOf)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(skyOf))).
:- call_pel_directive(ignore(skyOf)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(groundOf))).
:- call_pel_directive(ignore(groundOf)).

% ignore Inside, Near
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',26).
% From E: 
% 
% ':-'(call_pel_directive(ignore(inside))).
:- call_pel_directive(ignore(inside)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(near))).
:- call_pel_directive(ignore(near)).

% ignore See
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',26).
% From E: 
% 
% ':-'(call_pel_directive(ignore(see))).
:- call_pel_directive(ignore(see)).

% ignore ActOnSleep5
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',29).
% From E: 
% 
% ':-'(call_pel_directive(ignore(actOnSleep5))).
:- call_pel_directive(ignore(actOnSleep5)).

% option renaming off
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',31).
% From E: 
% 
% ':-'(call_pel_directive(option(renaming,off))).
:- call_pel_directive(option(renaming, off)).

% load foundations/Root.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',33).
% From E: 
% 
% ':-'(call_pel_directive(load('foundations/Root.e'))).
:- call_pel_directive(load('foundations/Root.e')).

% load foundations/EC.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',33).
% From E: 
% 
% ':-'(call_pel_directive(load('foundations/EC.e'))).
:- call_pel_directive(load('foundations/EC.e')).

% load answers/Mueller2003/Ontology.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',35).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2003/Ontology.e'))).
:- call_pel_directive(load('answers/Mueller2003/Ontology.e')).

% load answers/Mueller2004c/RTSpaceM.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',35).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/RTSpaceM.e'))).
:- call_pel_directive(load('answers/Mueller2004c/RTSpaceM.e')).

% load answers/Mueller2004c/OTSpaceM.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',37).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/OTSpaceM.e'))).
:- call_pel_directive(load('answers/Mueller2004c/OTSpaceM.e')).

% load answers/Mueller2004c/Cognition.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',37).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/Cognition.e'))).
:- call_pel_directive(load('answers/Mueller2004c/Cognition.e')).

% load answers/Mueller2003/Sleep.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',39).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2003/Sleep.e'))).
:- call_pel_directive(load('answers/Mueller2003/Sleep.e')).

% door Door1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',41).
% From E: 
% 
% t(door,door1).
isa(door1, door).

% room Room0
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',43).
% From E: 
% 
% t(room,room0).
isa(room0, room).

% room Room1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',45).
% From E: 
% 
% t(room,room1).
isa(room1, room).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',47).
% Side1(Door1)=Room0.
% From E: 
% 
% '='(
%    side1(door1), 
%    room0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',47).
side1(door1,room0).


% Side2(Door1)=Room1.
% From E: 
% 
% '='(
%    side2(door1), 
%    room1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',47).
side2(door1,room1).

% agent Sleeper1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',49).
% From E: 
% 
% t(agent,sleeper1).
isa(sleeper1, agent).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',52).
% bed Bed1
% From E: 
% 
% t(bed,bed1).
isa(bed1, bed).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',54).
% outside Outside1
% From E: 
% 
% t(outside,outside1).
isa(outside1, outside).
%; initial state
% [agent,object]
 % !HoldsAt(Holding(agent,object),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',56).
% From E: 
% 
% holds(
%    not(holding(Agent,Object)), 0).
initially not holding(Agent, Object).
 %  initial_state([not(holding(Agent,Object))]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',58).
% [agent,physobj]
 % !HoldsAt(SittingOn(agent,physobj),0).
% From E: 
% 
% holds(
%    not(sittingOn(Agent,Physobj)), 0).
initially not sittingOn(Agent, Physobj).
 %  initial_state([not(sittingOn(Agent,Physobj))]).
 %  % =================================.


% [agent,physobj]
 % !HoldsAt(LyingOn(agent,physobj),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',58).
% From E: 
% 
% holds(
%    not(lyingOn(Agent,Physobj)), 0).
initially not lyingOn(Agent, Physobj).
 %  initial_state([not(lyingOn(Agent,Physobj))]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',60).
% HoldsAt(Dressed(Sleeper1),0).
% From E: 
% 
% holds(
%    dressed(sleeper1), 0).
initially dressed(sleeper1).
 %  initial_state([dressed(sleeper1)]).
 %  % =================================.


% HoldsAt(Awake(Sleeper1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',60).
% From E: 
% 
% holds(
%    awake(sleeper1), 0).
initially awake(sleeper1).
 %  initial_state([awake(sleeper1)]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',62).
% HoldsAt(Sleep3(Sleeper1),0).
% From E: 
% 
% holds(
%    sleep3(sleeper1), 0).
initially sleep3(sleeper1).
 %  initial_state([sleep3(sleeper1)]).
 %  % =================================.


% HoldsAt(Standing(Sleeper1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',62).
% From E: 
% 
% holds(
%    standing(sleeper1), 0).
initially standing(sleeper1).
 %  initial_state([standing(sleeper1)]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',64).
% HoldsAt(DoorUnlocked(Door1),0).
% From E: 
% 
% holds(
%    doorUnlocked(door1), 0).
initially doorUnlocked(door1).
 %  initial_state([doorUnlocked(door1)]).
 %  % =================================.


% HoldsAt(DoorIsOpen(Door1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',64).
% From E: 
% 
% holds(
%    doorIsOpen(door1), 0).
initially doorIsOpen(door1).
 %  initial_state([doorIsOpen(door1)]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',66).
% HoldsAt(At(Sleeper1,Room0),0).
% From E: 
% 
% holds(
%    at_loc(sleeper1,room0), 0).
initially at_loc(sleeper1, room0).
 %  initial_state([at_loc(sleeper1,room0)]).
 %  % =================================.


% HoldsAt(At(Bed1,Room1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',66).
% From E: 
% 
% holds(
%    at_loc(bed1,room1), 0).
initially at_loc(bed1, room1).
 %  initial_state([at_loc(bed1,room1)]).
 %  % =================================.


%; narrative


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',70).
% Happens(GetTired(Sleeper1),0).
% From E: 
% 
% happens(
%    getTired(sleeper1), 0).
observe getTired(sleeper1)at 0.
 %  observe([getTired(sleeper1)],0).
 %  % =================================.


% Happens(WalkThroughDoor12(Sleeper1,Door1),1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',70).
% From E: 
% 
% happens(
%    walkThroughDoor12(sleeper1,door1), 1).
observe walkThroughDoor12(sleeper1, door1)at 1.
 %  observe([walkThroughDoor12(sleeper1,door1)],1).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',72).
% Happens(GetUndressed(Sleeper1),2).
% From E: 
% 
% happens(
%    getUndressed(sleeper1), 2).
observe getUndressed(sleeper1)at 2.
 %  observe([getUndressed(sleeper1)],2).
 %  % =================================.


% Happens(LieOn(Sleeper1,Bed1),3).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',72).
% From E: 
% 
% happens(
%    lieOn(sleeper1,bed1), 3).
observe lieOn(sleeper1, bed1)at 3.
 %  observe([lieOn(sleeper1,bed1)],3).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',74).
% Happens(FallAsleep(Sleeper1),4).
% From E: 
% 
% happens(
%    fallAsleep(sleeper1), 4).
observe fallAsleep(sleeper1)at 4.
 %  observe([fallAsleep(sleeper1)],4).
 %  % =================================.


% Happens(Dream(Sleeper1),5).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',74).
% From E: 
% 
% happens(
%    dream(sleeper1), 5).
observe dream(sleeper1)at 5.
 %  observe([dream(sleeper1)],5).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',76).
% Happens(WakeUp(Sleeper1),6).
% From E: 
% 
% happens(
%    wakeUp(sleeper1), 6).
observe wakeUp(sleeper1)at 6.
 %  observe([wakeUp(sleeper1)],6).
 %  % =================================.


% Happens(RiseFrom(Sleeper1,Bed1),7).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',76).
% From E: 
% 
% happens(
%    riseFrom(sleeper1,bed1), 7).
observe riseFrom(sleeper1, bed1)at 7.
 %  observe([riseFrom(sleeper1,bed1)],7).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',78).
% Happens(GetDressed(Sleeper1),8).
% From E: 
% 
% happens(
%    getDressed(sleeper1), 8).
observe getDressed(sleeper1)at 8.
 %  observe([getDressed(sleeper1)],8).
 %  % =================================.


% Happens(WalkThroughDoor21(Sleeper1,Door1),9).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',78).
% From E: 
% 
% happens(
%    walkThroughDoor21(sleeper1,door1), 9).
observe walkThroughDoor21(sleeper1, door1)at 9.
 %  observe([walkThroughDoor21(sleeper1,door1)],9).
 %  % =================================.

% range time 0 10
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',80).
% From E: 
% 
% ':-'(call_pel_directive(range(time,0,10))).
:- call_pel_directive(range(time, 0, 10)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',82).
% range offset 0 0
% From E: 
% 
% ':-'(call_pel_directive(range(offset,0,0))).
:- call_pel_directive(range(offset, 0, 0)).

% range diameter 0 0
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',82).
% From E: 
% 
% ':-'(call_pel_directive(range(diameter,0,0))).
:- call_pel_directive(range(diameter, 0, 0)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',85).
% completion Happens
% From E: 
% 
% ':-'(call_pel_directive(completion(happens))).
:- call_pel_directive(completion(happens)).
%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.e',87).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleeping.lps.pl')).
