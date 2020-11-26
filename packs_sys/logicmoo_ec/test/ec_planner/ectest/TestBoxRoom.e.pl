:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/Story1.e',47).
:- call_pel_directive(translate(unskipped,
                                '/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(ecalc).
:- call_pel_directive(translate(begining,
                                '/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e.pl')).
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: examples/Mueller2006/Chapter10/MovingNewspaperAndBox.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available in
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; @book{Mueller:2006,
%;   author = "Erik T. Mueller",
%;   year = "2006",
%;   title = "Commonsense Reasoning",
%;   address = "San Francisco",
%;   publisher = "Morgan Kaufmann/Elsevier",
%; }
%;

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',24).
% include foundations/Root.e
:- call_pel_directive(include('foundations/Root.e')).

% include foundations/EC.e
:- call_pel_directive(include('foundations/EC.e')).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',27).
% sort object
sort(object).

% sort agent: object
subsort(agent,object).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',29).
% sort physobj: object
subsort(physobj,object).

% sort room: object
subsort(room,object).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',32).
% fluent directlyIn(object,object)
fluent(directlyIn(object,object)).

% fluent inRoom(object,room)
fluent(inRoom(object,room)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',34).
% noninertial inRoom
:- call_pel_directive(noninertial(inRoom)).
%;; executable(move(agent,object,object,object))

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',38).
% agent Lisa
t(agent,lisa).

% physobj Box, Newspaper
t(physobj,box).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',38).
t(physobj,newspaper).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',40).
% room Kitchen, LivingRoom
t(room,kitchen).
t(room,livingRoom).
%; Sigma
%; RS10
% [agent,physobj1,physobj2,room,time]
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',46).
% HoldsAt(directlyIn(agent,room),time) &
% HoldsAt(directlyIn(physobj1,room),time) &
% HoldsAt(inRoom(physobj2,room),time) ->
% Initiates(move(agent,physobj1,room,physobj2),directlyIn(physobj1,physobj2),time).
holds_at(directlyIn(Agent, Room), Time), holds_at(directlyIn(Physobj1, Room), Time), holds_at(inRoom(Physobj2, Room), Time) ->
    initiates_at(move(Agent, Physobj1, Room, Physobj2),
                 directlyIn(Physobj1, Physobj2),
                 Time).


%; RS11
% [agent,physobj1,physobj2,room,time]
% HoldsAt(directlyIn(agent,room),time) &
% HoldsAt(directlyIn(physobj1,room),time) &
% HoldsAt(inRoom(physobj2,room),time) ->
% Terminates(move(agent,physobj1,room,physobj2),directlyIn(physobj1,room),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',52).
holds_at(directlyIn(Agent, Room), Time), holds_at(directlyIn(Physobj1, Room), Time), holds_at(inRoom(Physobj2, Room), Time) ->
    terminates_at(move(Agent, Physobj1, Room, Physobj2),
                  directlyIn(Physobj1, Room),
                  Time).


%; RS12
% [agent,physobj1,physobj2,room,time]
% HoldsAt(directlyIn(agent,room),time) ->
% Initiates(move(agent,physobj1,physobj2,room),directlyIn(physobj1,room),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',59).
holds_at(directlyIn(Agent, Room), Time) ->
    initiates_at(move(Agent, Physobj1, Physobj2, Room),
                 directlyIn(Physobj1, Room),
                 Time).


%; RS13
% [agent,physobj1,physobj2,room,time]
% HoldsAt(directlyIn(agent,room),time) ->
% Terminates(move(agent,physobj1,physobj2,room),directlyIn(physobj1,physobj2),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',64).
holds_at(directlyIn(Agent, Room), Time) ->
    terminates_at(move(Agent, Physobj1, Physobj2, Room),
                  directlyIn(Physobj1, Physobj2),
                  Time).


%; RS14
% [agent,room1,room2,time]
% HoldsAt(directlyIn(agent,room1),time) ->
% Initiates(move(agent,agent,room1,room2),directlyIn(agent,room2),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',69).
holds_at(directlyIn(Agent, Room1), Time) ->
    initiates_at(move(Agent, Agent, Room1, Room2),
                 directlyIn(Agent, Room2),
                 Time).


%; RS15
% [agent,room1,room2,time]
% HoldsAt(directlyIn(agent,room1),time) ->
% Terminates(move(agent,agent,room1,room2),directlyIn(agent,room1),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',74).
holds_at(directlyIn(Agent, Room1), Time) ->
    terminates_at(move(Agent, Agent, Room1, Room2),
                  directlyIn(Agent, Room1),
                  Time).


%; RS16
% [agent,physobj,room,time]
% HoldsAt(directlyIn(agent,room),time) &
% HoldsAt(directlyIn(physobj,room),time) ->
% Initiates(move(agent,physobj,room,agent),directlyIn(physobj,agent),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',79).
holds_at(directlyIn(Agent, Room), Time), holds_at(directlyIn(Physobj, Room), Time) ->
    initiates_at(move(Agent, Physobj, Room, Agent),
                 directlyIn(Physobj, Agent),
                 Time).


%; RS17
% [agent,physobj,room,time]
% HoldsAt(directlyIn(agent,room),time) &
% HoldsAt(directlyIn(physobj,room),time) ->
% Terminates(move(agent,physobj,room,agent),directlyIn(physobj,room),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',85).
holds_at(directlyIn(Agent, Room), Time), holds_at(directlyIn(Physobj, Room), Time) ->
    terminates_at(move(Agent, Physobj, Room, Agent),
                  directlyIn(Physobj, Room),
                  Time).


%; RS18
% [agent,physobj,room,time]
% HoldsAt(directlyIn(physobj,agent),time) &
% HoldsAt(directlyIn(agent,room),time) ->
% Initiates(move(agent,physobj,agent,room),directlyIn(physobj,room),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',91).
holds_at(directlyIn(Physobj, Agent), Time), holds_at(directlyIn(Agent, Room), Time) ->
    initiates_at(move(Agent, Physobj, Agent, Room),
                 directlyIn(Physobj, Room),
                 Time).


%; RS19
% [agent,physobj,room,time]
% HoldsAt(directlyIn(physobj,agent),time) &
% HoldsAt(directlyIn(agent,room),time) ->
% Terminates(move(agent,physobj,agent,room),directlyIn(physobj,agent),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',97).
holds_at(directlyIn(Physobj, Agent), Time), holds_at(directlyIn(Agent, Room), Time) ->
    terminates_at(move(Agent, Physobj, Agent, Room),
                  directlyIn(Physobj, Agent),
                  Time).


%; Delta


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',104).
% Happens(move(Lisa,Newspaper,LivingRoom,Box),0).
happens_at(move(lisa,newspaper,livingRoom,box),0).


% Happens(move(Lisa,Box,LivingRoom,Lisa),1).
happens_at(move(lisa,box,livingRoom,lisa),1).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',106).
% Happens(move(Lisa,Lisa,LivingRoom,Kitchen),2).
happens_at(move(lisa,lisa,livingRoom,kitchen),2).


% Happens(move(Lisa,Box,Lisa,Kitchen),3).
happens_at(move(lisa,box,lisa,kitchen),3).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',108).
% Happens(move(Lisa,Lisa,Kitchen,LivingRoom),4).
happens_at(move(lisa,lisa,kitchen,livingRoom),4).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',110).
%; Psi
%; RS1
% [object,time]
 % !HoldsAt(directlyIn(object,object),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',112).
holds_at(not(directlyIn(Object,Object)),Time).


%; RS2
% [object1,object2,time]
% HoldsAt(directlyIn(object1,object2),time) ->
% !HoldsAt(directlyIn(object2,object1),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',116).
holds_at(directlyIn(Object1, Object2), Time) ->
    holds_at(not(directlyIn(Object2, Object1)), Time).


%; RS3
% [object1,object2,object3,time]
% HoldsAt(directlyIn(object1,object2),time) &
% HoldsAt(directlyIn(object2,object3),time) ->
% !HoldsAt(directlyIn(object1,object3),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',121).
holds_at(directlyIn(Object1, Object2), Time), holds_at(directlyIn(Object2, Object3), Time) ->
    holds_at(not(directlyIn(Object1, Object3)), Time).


%; RS4
% [object,object1,object2,time]
% HoldsAt(directlyIn(object,object1),time) &
% HoldsAt(directlyIn(object,object2),time) ->
% object1=object2.
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',127).
holds_at(directlyIn(Object, Object1), Time), holds_at(directlyIn(Object, Object2), Time) ->
    Object1=Object2.


%; RS7
% [object,room,time]
% HoldsAt(directlyIn(object,room),time) ->
% HoldsAt(inRoom(object,room),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',133).
holds_at(directlyIn(Object, Room), Time) ->
    holds_at(inRoom(Object, Room), Time).


%; RS8
% [object1,object2,room,time]
% HoldsAt(directlyIn(object1,object2),time) &
% HoldsAt(inRoom(object2,room),time) ->
% HoldsAt(inRoom(object1,room),time).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',138).
holds_at(directlyIn(Object1, Object2), Time), holds_at(inRoom(Object2, Room), Time) ->
    holds_at(inRoom(Object1, Room), Time).


%; RS9
% [object,room1,room2,time]
% HoldsAt(inRoom(object,room1),time) &
% HoldsAt(inRoom(object,room2),time) ->
% room1=room2.
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',144).
holds_at(inRoom(Object, Room1), Time), holds_at(inRoom(Object, Room2), Time) ->
    Room1=Room2.


%; Gamma


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',151).
% HoldsAt(directlyIn(Lisa,LivingRoom),0).
holds_at(directlyIn(lisa,livingRoom),0).


% HoldsAt(directlyIn(Newspaper,LivingRoom),0).
holds_at(directlyIn(newspaper,livingRoom),0).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',153).
% HoldsAt(directlyIn(Box,LivingRoom),0).
holds_at(directlyIn(box,livingRoom),0).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',155).
%; added:                                                 
%; DMILES REMOVED [room1,room2,time] !HoldsAt(inRoom(room1,room2),time).
%; DMILES REMOVED [room,object,time] !HoldsAt(directlyIn(room,object),time).
%; entailed:


% HoldsAt(directlyIn(Lisa,LivingRoom),5).
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',159).
holds_at(directlyIn(lisa,livingRoom),5).


:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',161).
% HoldsAt(directlyIn(Box,Kitchen),5).
holds_at(directlyIn(box,kitchen),5).


% HoldsAt(inRoom(Newspaper,Kitchen),5).
holds_at(inRoom(newspaper,kitchen),5).

% completion Happens
:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',163).
:- call_pel_directive(completion(happens_at)).

:-was_s_l('/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',166).
% range time 0 5
:- call_pel_directive(range(time, 0, 5)).

% range offset 1 1
:- call_pel_directive(range(offset, 1, 1)).
%; End of file.
:- call_pel_directive(translate(ending,
                                '/pack/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e.pl')).
