% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vision.e',75).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.lps.pl')).
% Fri, 26 Mar 2021 01:06:14 GMT File: <stream>(0x555567a68b00)%;
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
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',22).
% From E: 
% 
% ':-'(call_pel_directive(option(modeldiff,on))).
:- call_pel_directive(option(modeldiff, on)).

% ignore Love, ThreatenedBy
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(love))).
:- call_pel_directive(ignore(love)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(threatenedBy))).
:- call_pel_directive(ignore(threatenedBy)).

% ignore LookOutOnto, Floor, BuildingOf, SkyOf, GroundOf
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(lookOutOnto))).
:- call_pel_directive(ignore(lookOutOnto)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(floor))).
:- call_pel_directive(ignore(floor)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(buildingOf))).
:- call_pel_directive(ignore(buildingOf)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(skyOf))).
:- call_pel_directive(ignore(skyOf)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(groundOf))).
:- call_pel_directive(ignore(groundOf)).

% ignore Inside
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',26).
% From E: 
% 
% ':-'(call_pel_directive(ignore(inside))).
:- call_pel_directive(ignore(inside)).

% ignore Near, WalkFrom, WalkFromTo, RunFromTo
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',26).
% From E: 
% 
% ':-'(call_pel_directive(ignore(near))).
:- call_pel_directive(ignore(near)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(walkFrom))).
:- call_pel_directive(ignore(walkFrom)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',26).
% From E: 
% 
% ':-'(call_pel_directive(ignore(walkFromTo))).
:- call_pel_directive(ignore(walkFromTo)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(runFromTo))).
:- call_pel_directive(ignore(runFromTo)).

% ignore See
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',28).
% From E: 
% 
% ':-'(call_pel_directive(ignore(see))).
:- call_pel_directive(ignore(see)).

% option renaming off
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',30).
% From E: 
% 
% ':-'(call_pel_directive(option(renaming,off))).
:- call_pel_directive(option(renaming, off)).

% load foundations/Root.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',32).
% From E: 
% 
% ':-'(call_pel_directive(load('foundations/Root.e'))).
:- call_pel_directive(load('foundations/Root.e')).

% load foundations/EC.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',32).
% From E: 
% 
% ':-'(call_pel_directive(load('foundations/EC.e'))).
:- call_pel_directive(load('foundations/EC.e')).

% load answers/Mueller2003/Ontology.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',34).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2003/Ontology.e'))).
:- call_pel_directive(load('answers/Mueller2003/Ontology.e')).

% load answers/Mueller2004c/RTSpaceM.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',34).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/RTSpaceM.e'))).
:- call_pel_directive(load('answers/Mueller2004c/RTSpaceM.e')).

% load answers/Mueller2004c/OTSpaceM.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',36).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/OTSpaceM.e'))).
:- call_pel_directive(load('answers/Mueller2004c/OTSpaceM.e')).

% load answers/Mueller2004c/HandTo.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',36).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/HandTo.e'))).
:- call_pel_directive(load('answers/Mueller2004c/HandTo.e')).

% load answers/Mueller2003/Sleep.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',38).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2003/Sleep.e'))).
:- call_pel_directive(load('answers/Mueller2003/Sleep.e')).

% load answers/Mueller2004c/Cognition.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',38).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/Cognition.e'))).
:- call_pel_directive(load('answers/Mueller2004c/Cognition.e')).

% door Door1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',41).
% From E: 
% 
% t(door,door1).
isa(door1, door).

% room Room0
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',43).
% From E: 
% 
% t(room,room0).
isa(room0, room).

% room Room1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',45).
% From E: 
% 
% t(room,room1).
isa(room1, room).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',47).
% Side1(Door1)=Room0.
% From E: 
% 
% '='(
%    side1(door1), 
%    room0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',47).
side1(door1,room0).


% Side2(Door1)=Room1.
% From E: 
% 
% '='(
%    side2(door1), 
%    room1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',47).
side2(door1,room1).

% agent Writer1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',49).
% From E: 
% 
% t(agent,writer1).
isa(writer1, agent).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',52).
% paper Paper1
% From E: 
% 
% t(paper,paper1).
isa(paper1, paper).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',54).
% pen Pen1
% From E: 
% 
% t(pen,pen1).
isa(pen1, pen).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',56).
% chair Chair1
% From E: 
% 
% t(chair,chair1).
isa(chair1, chair).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',58).
% physobj Desk1
% From E: 
% 
% t(physobj,desk1).
isa(desk1, physobj).
%; initial state
% [agent,object]
 % !HoldsAt(Holding(agent,object),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',60).
% From E: 
% 
% holds(
%    not(holding(Agent,Object)), 0).
initially not holding(Agent, Object).
 %  initial_state([not(holding(Agent,Object))]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',62).
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
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',62).
% From E: 
% 
% holds(
%    not(lyingOn(Agent,Physobj)), 0).
initially not lyingOn(Agent, Physobj).
 %  initial_state([not(lyingOn(Agent,Physobj))]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',64).
% [physobj1,physobj2]
% !(physobj1=Pen1 & physobj2=Desk1) &
% !(physobj1=Paper1 & physobj2=Desk1) ->
% !HoldsAt(On(physobj1, physobj2),0).
% From E: 
% 
% '->'(
%    ','(
%       not(','(
%              Physobj1=pen1, 
%              Physobj2=desk1)), 
%       not(','(
%              Physobj1=paper1, 
%              Physobj2=desk1))), 
%    holds(
%       not(on(Physobj1,Physobj2)), 0)).
on(Physobj1, Physobj2)at 0 if equals(Physobj1, pen1), equals(Physobj2, desk1);equals(Physobj1, paper1), equals(Physobj2, desk1).
 %  l_int(holds(on(Physobj1, Physobj2), 0), [(equals(Physobj1, pen1), equals(Physobj2, desk1);equals(Physobj1, paper1), equals(Physobj2, desk1))]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',68).
% HoldsAt(On(Paper1,Desk1),0).
% From E: 
% 
% holds(
%    on(paper1,desk1), 0).
initially on(paper1, desk1).
 %  initial_state([on(paper1,desk1)]).
 %  % =================================.


% HoldsAt(On(Pen1,Desk1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',68).
% From E: 
% 
% holds(
%    on(pen1,desk1), 0).
initially on(pen1, desk1).
 %  initial_state([on(pen1,desk1)]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',70).
% HoldsAt(Dressed(Writer1),0).
% From E: 
% 
% holds(
%    dressed(writer1), 0).
initially dressed(writer1).
 %  initial_state([dressed(writer1)]).
 %  % =================================.


% HoldsAt(Awake(Writer1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',70).
% From E: 
% 
% holds(
%    awake(writer1), 0).
initially awake(writer1).
 %  initial_state([awake(writer1)]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',72).
% HoldsAt(Sleep3(Writer1),0).
% From E: 
% 
% holds(
%    sleep3(writer1), 0).
initially sleep3(writer1).
 %  initial_state([sleep3(writer1)]).
 %  % =================================.


% HoldsAt(Standing(Writer1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',72).
% From E: 
% 
% holds(
%    standing(writer1), 0).
initially standing(writer1).
 %  initial_state([standing(writer1)]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',74).
% HoldsAt(DoorUnlocked(Door1),0).
% From E: 
% 
% holds(
%    doorUnlocked(door1), 0).
initially doorUnlocked(door1).
 %  initial_state([doorUnlocked(door1)]).
 %  % =================================.


% HoldsAt(DoorIsOpen(Door1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',74).
% From E: 
% 
% holds(
%    doorIsOpen(door1), 0).
initially doorIsOpen(door1).
 %  initial_state([doorIsOpen(door1)]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',76).
% HoldsAt(At(Writer1,Room0),0).
% From E: 
% 
% holds(
%    at_loc(writer1,room0), 0).
initially at_loc(writer1, room0).
 %  initial_state([at_loc(writer1,room0)]).
 %  % =================================.


% HoldsAt(At(Chair1,Room1),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',76).
% From E: 
% 
% holds(
%    at_loc(chair1,room1), 0).
initially at_loc(chair1, room1).
 %  initial_state([at_loc(chair1,room1)]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',78).
% HoldsAt(At(Desk1,Room1),0).
% From E: 
% 
% holds(
%    at_loc(desk1,room1), 0).
initially at_loc(desk1, room1).
 %  initial_state([at_loc(desk1,room1)]).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',80).
%; narrative


% Happens(WalkThroughDoor12(Writer1,Door1),0).
% From E: 
% 
% happens(
%    walkThroughDoor12(writer1,door1), 0).
observe walkThroughDoor12(writer1, door1)at 0.
 %  observe([walkThroughDoor12(writer1,door1)],0).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',82).
% Happens(SitOn(Writer1,Chair1),1).
% From E: 
% 
% happens(
%    sitOn(writer1,chair1), 1).
observe sitOn(writer1, chair1)at 1.
 %  observe([sitOn(writer1,chair1)],1).
 %  % =================================.


% Happens(TakeOffOf(Writer1,Pen1,Desk1),2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',82).
% From E: 
% 
% happens(
%    takeOffOf(writer1,pen1,desk1), 2).
observe takeOffOf(writer1, pen1, desk1)at 2.
 %  observe([takeOffOf(writer1,pen1,desk1)],2).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',84).
% Happens(Think(Writer1),3).
% From E: 
% 
% happens(
%    think(writer1), 3).
observe think(writer1)at 3.
 %  observe([think(writer1)],3).
 %  % =================================.


% Happens(WriteOn(Writer1,Paper1,Pen1),4).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',84).
% From E: 
% 
% happens(
%    writeOn(writer1,paper1,pen1), 4).
observe writeOn(writer1, paper1, pen1)at 4.
 %  observe([writeOn(writer1,paper1,pen1)],4).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',86).
% Happens(WriteOn(Writer1,Paper1,Pen1),5).
% From E: 
% 
% happens(
%    writeOn(writer1,paper1,pen1), 5).
observe writeOn(writer1, paper1, pen1)at 5.
 %  observe([writeOn(writer1,paper1,pen1)],5).
 %  % =================================.


% Happens(PlaceOn(Writer1,Pen1,Desk1),6).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',86).
% From E: 
% 
% happens(
%    placeOn(writer1,pen1,desk1), 6).
observe placeOn(writer1, pen1, desk1)at 6.
 %  observe([placeOn(writer1,pen1,desk1)],6).
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',88).
% Happens(RiseFrom(Writer1,Chair1),7).
% From E: 
% 
% happens(
%    riseFrom(writer1,chair1), 7).
observe riseFrom(writer1, chair1)at 7.
 %  observe([riseFrom(writer1,chair1)],7).
 %  % =================================.


% Happens(WalkThroughDoor21(Writer1,Door1),8).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',88).
% From E: 
% 
% happens(
%    walkThroughDoor21(writer1,door1), 8).
observe walkThroughDoor21(writer1, door1)at 8.
 %  observe([walkThroughDoor21(writer1,door1)],8).
 %  % =================================.

% range time 0 9
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',90).
% From E: 
% 
% ':-'(call_pel_directive(range(time,0,9))).
:- call_pel_directive(range(time, 0, 9)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',92).
% range offset 0 0
% From E: 
% 
% ':-'(call_pel_directive(range(offset,0,0))).
:- call_pel_directive(range(offset, 0, 0)).

% range diameter 0 0
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',92).
% From E: 
% 
% ':-'(call_pel_directive(range(diameter,0,0))).
:- call_pel_directive(range(diameter, 0, 0)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',95).
% completion Happens
% From E: 
% 
% ':-'(call_pel_directive(completion(happens))).
:- call_pel_directive(completion(happens)).
%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',97).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.lps.pl')).
