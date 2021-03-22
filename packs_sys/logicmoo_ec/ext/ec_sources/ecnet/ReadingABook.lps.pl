% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReaderTest.e',20).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.lps.pl')).
% Sun, 21 Mar 2021 23:28:13 GMT File: <stream>(0x555567722300)


%;
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

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',22).
% option modeldiff on
% From E: 
% 
% ':-'(call_pel_directive(option(modeldiff,on))).
:- call_pel_directive(option(modeldiff, on)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',24).
% ignore Love, ThreatenedBy
% From E: 
% 
% ':-'(call_pel_directive(ignore(love))).
:- call_pel_directive(ignore(love)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(threatenedBy))).
:- call_pel_directive(ignore(threatenedBy)).

% ignore LookOutOnto, Floor, BuildingOf, SkyOf, GroundOf
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(lookOutOnto))).
:- call_pel_directive(ignore(lookOutOnto)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(floor))).
:- call_pel_directive(ignore(floor)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(buildingOf))).
:- call_pel_directive(ignore(buildingOf)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(skyOf))).
:- call_pel_directive(ignore(skyOf)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',24).
% From E: 
% 
% ':-'(call_pel_directive(ignore(groundOf))).
:- call_pel_directive(ignore(groundOf)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',26).
% ignore Inside
% From E: 
% 
% ':-'(call_pel_directive(ignore(inside))).
:- call_pel_directive(ignore(inside)).

% ignore Near, WalkFrom, WalkFromTo, RunFromTo
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',26).
% From E: 
% 
% ':-'(call_pel_directive(ignore(near))).
:- call_pel_directive(ignore(near)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(walkFrom))).
:- call_pel_directive(ignore(walkFrom)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',26).
% From E: 
% 
% ':-'(call_pel_directive(ignore(walkFromTo))).
:- call_pel_directive(ignore(walkFromTo)).
% From E: 
% 
% ':-'(call_pel_directive(ignore(runFromTo))).
:- call_pel_directive(ignore(runFromTo)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',29).
% option renaming off
% From E: 
% 
% ':-'(call_pel_directive(option(renaming,off))).
:- call_pel_directive(option(renaming, off)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',31).
% load foundations/Root.e
% From E: 
% 
% ':-'(call_pel_directive(load('foundations/Root.e'))).
:- call_pel_directive(load('foundations/Root.e')).

% load foundations/EC.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',31).
% From E: 
% 
% ':-'(call_pel_directive(load('foundations/EC.e'))).
:- call_pel_directive(load('foundations/EC.e')).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',33).
% load answers/Mueller2003/Ontology.e
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2003/Ontology.e'))).
:- call_pel_directive(load('answers/Mueller2003/Ontology.e')).

% load answers/Mueller2004c/RTSpaceM.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',33).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/RTSpaceM.e'))).
:- call_pel_directive(load('answers/Mueller2004c/RTSpaceM.e')).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',35).
% load answers/Mueller2004c/OTSpaceM.e
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/OTSpaceM.e'))).
:- call_pel_directive(load('answers/Mueller2004c/OTSpaceM.e')).

% load answers/Mueller2004c/Book.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',35).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/Book.e'))).
:- call_pel_directive(load('answers/Mueller2004c/Book.e')).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',37).
% load answers/Mueller2004c/Cognition.e
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2004c/Cognition.e'))).
:- call_pel_directive(load('answers/Mueller2004c/Cognition.e')).

% load answers/Mueller2003/Sleep.e
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',37).
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2003/Sleep.e'))).
:- call_pel_directive(load('answers/Mueller2003/Sleep.e')).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',39).
% load answers/Mueller2003/Vision.e
% From E: 
% 
% ':-'(call_pel_directive(load('answers/Mueller2003/Vision.e'))).
:- call_pel_directive(load('answers/Mueller2003/Vision.e')).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',41).
% door Door1
% From E: 
% 
% t(door,door1).
door(door1).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',43).
% room Room0
% From E: 
% 
% t(room,room0).
room(room0).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',45).
% room Room1
% From E: 
% 
% t(room,room1).
room(room1).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',47).
% Side1(Door1)=Room0.
% From E: 
% 
% '='(
%    side1(door1), 
%    room0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',47).
side1(door1,room0).


% Side2(Door1)=Room1.
% From E: 
% 
% '='(
%    side2(door1), 
%    room1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',47).
side2(door1,room1).

% agent Reader1
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',49).
% From E: 
% 
% t(agent,reader1).
agent(reader1).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',52).
% book Book1
% From E: 
% 
% t(book,book1).
book(book1).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',54).
% chair Chair1
% From E: 
% 
% t(chair,chair1).
chair(chair1).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',56).
% physobj BookSupport1
% From E: 
% 
% t(physobj,bookSupport1).
physobj(bookSupport1).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',58).
% content Content1
% From E: 
% 
% t(content,content1).
content(content1).
%; initial state
% [agent,object]
 % !HoldsAt(Holding(agent,object),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',60).
% From E: 
% 
% holds(
%    not(holding(Agent,Object)), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',60).
initially(not(holding(Agent,Object))).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',62).
% [agent,physobj]
 % !HoldsAt(SittingOn(agent,physobj),0).
% From E: 
% 
% holds(
%    not(sittingOn(Agent,Physobj)), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',62).
initially(not(sittingOn(Agent,Physobj))).


% [agent,physobj]
 % !HoldsAt(LyingOn(agent,physobj),0).
% From E: 
% 
% holds(
%    not(lyingOn(Agent,Physobj)), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',62).
initially(not(lyingOn(Agent,Physobj))).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',64).
% !{page}% HoldsAt(BookIsOpenTo(Book1,page),0).
% From E: 
% 
% not(thereExists(Page, 
%        holds(
%           bookIsOpenTo(book1,Page), 0))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',64).
not(thereExists(Page,initially(bookIsOpenTo(book1,Page)))).


% [physobj1,physobj2]
% !(physobj1=Book1 & physobj2=BookSupport1) ->
% !HoldsAt(On(physobj1, physobj2),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',66).
% From E: 
% 
% '->'(
%    not(','(
%           Physobj1=book1, 
%           Physobj2=bookSupport1)), 
%    holds(
%       not(on(Physobj1,Physobj2)), 0)).
if(initially(on(Physobj1, Physobj2)),  (equals(Physobj1, book1), equals(Physobj2, bookSupport1))).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',68).
% HoldsAt(Dressed(Reader1),0).
% From E: 
% 
% holds(
%    dressed(reader1), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',68).
initially(dressed(reader1)).


% HoldsAt(Awake(Reader1),0).
% From E: 
% 
% holds(
%    awake(reader1), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',68).
initially(awake(reader1)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',70).
% HoldsAt(Sleep3(Reader1),0).
% From E: 
% 
% holds(
%    sleep3(reader1), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',70).
initially(sleep3(reader1)).


% HoldsAt(Standing(Reader1),0).
% From E: 
% 
% holds(
%    standing(reader1), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',70).
initially(standing(reader1)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',72).
% HoldsAt(DoorUnlocked(Door1),0).
% From E: 
% 
% holds(
%    doorUnlocked(door1), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',72).
initially(doorUnlocked(door1)).


% HoldsAt(DoorIsOpen(Door1),0).
% From E: 
% 
% holds(
%    doorIsOpen(door1), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',72).
initially(doorIsOpen(door1)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',74).
% HoldsAt(At(Reader1,Room0),0).
% From E: 
% 
% holds(
%    at_loc(reader1,room0), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',74).
initially(at_loc(reader1,room0)).


% HoldsAt(At(Chair1,Room1),0).
% From E: 
% 
% holds(
%    at_loc(chair1,room1), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',74).
initially(at_loc(chair1,room1)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',76).
% HoldsAt(At(Book1,Room1),0).
% From E: 
% 
% holds(
%    at_loc(book1,room1), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',76).
initially(at_loc(book1,room1)).


% HoldsAt(On(Book1,BookSupport1),0).
% From E: 
% 
% holds(
%    on(book1,bookSupport1), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',76).
initially(on(book1,bookSupport1)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',78).
% [object]
 % !HoldsAt(See(Reader1,object),0).
% From E: 
% 
% holds(
%    not(see(reader1,Object)), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',78).
initially(not(see(reader1,Object))).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',80).
%; narrative


% Happens(WalkThroughDoor12(Reader1,Door1),0).
% From E: 
% 
% happens(
%    walkThroughDoor12(reader1,door1), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',80).
observe(at(walkThroughDoor12(reader1,door1),0)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',82).
% Happens(TakeOffOf(Reader1,Book1,BookSupport1),1).
% From E: 
% 
% happens(
%    takeOffOf(reader1,book1,bookSupport1), 1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',82).
observe(at(takeOffOf(reader1,book1,bookSupport1),1)).


% Happens(SitOn(Reader1,Chair1),2).
% From E: 
% 
% happens(
%    sitOn(reader1,chair1), 2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',82).
observe(at(sitOn(reader1,chair1),2)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',84).
% Happens(BookOpenTo(Reader1,Book1,1),3).
% From E: 
% 
% happens(
%    bookOpenTo(reader1,book1,1), 3).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',84).
observe(at(bookOpenTo(reader1,book1,1),3)).


% Happens(LookAt(Reader1,Book1),4).
% From E: 
% 
% happens(
%    lookAt(reader1,book1), 4).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',84).
observe(at(lookAt(reader1,book1),4)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',86).
% Happens(Read(Reader1,Book1,Content1),5).
% From E: 
% 
% happens(
%    read(reader1,book1,content1), 5).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',86).
observe(at(read(reader1,book1,content1),5)).


% Happens(ThinkAbout(Reader1,Content1),6).
% From E: 
% 
% happens(
%    thinkAbout(reader1,content1), 6).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',86).
observe(at(thinkAbout(reader1,content1),6)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',88).
% Happens(Understand(Reader1,Content1),7).
% From E: 
% 
% happens(
%    understand(reader1,content1), 7).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',88).
observe(at(understand(reader1,content1),7)).


% Happens(BookTurnPageTo(Reader1,Book1,2),8).
% From E: 
% 
% happens(
%    bookTurnPageTo(reader1,book1,2), 8).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',88).
observe(at(bookTurnPageTo(reader1,book1,2),8)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',90).
% Happens(BookTurnPageTo(Reader1,Book1,3),9).
% From E: 
% 
% happens(
%    bookTurnPageTo(reader1,book1,3), 9).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',90).
observe(at(bookTurnPageTo(reader1,book1,3),9)).


% Happens(BookClose(Reader1,Book1),10).
% From E: 
% 
% happens(
%    bookClose(reader1,book1), 10).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',90).
observe(at(bookClose(reader1,book1),10)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',92).
% Happens(RiseFrom(Reader1,Chair1),11).
% From E: 
% 
% happens(
%    riseFrom(reader1,chair1), 11).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',92).
observe(at(riseFrom(reader1,chair1),11)).


% Happens(PlaceOn(Reader1,Book1,BookSupport1),12).
% From E: 
% 
% happens(
%    placeOn(reader1,book1,bookSupport1), 12).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',92).
observe(at(placeOn(reader1,book1,bookSupport1),12)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',94).
% Happens(WalkThroughDoor21(Reader1,Door1),13).
% From E: 
% 
% happens(
%    walkThroughDoor21(reader1,door1), 13).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',94).
observe(at(walkThroughDoor21(reader1,door1),13)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',96).
% range time 0 14
% From E: 
% 
% ':-'(call_pel_directive(range(time,0,14))).
:- call_pel_directive(range(time, 0, 14)).

% range page 1 3
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',96).
% From E: 
% 
% ':-'(call_pel_directive(range(page,1,3))).
:- call_pel_directive(range(page, 1, 3)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',98).
% range offset 0 0
% From E: 
% 
% ':-'(call_pel_directive(range(offset,0,0))).
:- call_pel_directive(range(offset, 0, 0)).

% range diameter 0 0
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',98).
% From E: 
% 
% ':-'(call_pel_directive(range(diameter,0,0))).
:- call_pel_directive(range(diameter, 0, 0)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',101).
% completion Happens
% From E: 
% 
% ':-'(call_pel_directive(completion(happens))).
:- call_pel_directive(completion(happens)).
%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.e',103).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingABook.lps.pl')).
