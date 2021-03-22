% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',43).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.lps.pl')).
% Sun, 21 Mar 2021 23:28:07 GMT File: <stream>(0x555567d02f00)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; scuba diving
%;

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',14).
% sort object
% From E: 
% 
% sort(object).
sort(object).

% sort agent: object
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',14).
% From E: 
% 
% subsort(agent,object).
subsort(agent, object).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',16).
% sort diver: agent
% From E: 
% 
% subsort(diver,agent).
subsort(diver, agent).

% sort depth: integer
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',16).
% From E: 
% 
% subsort(depth,integer).
subsort(depth, integer).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',18).
% sort boat: object
% From E: 
% 
% subsort(boat,object).
subsort(boat, object).
%; reference line, anchor line, shotline, SMB line, ...

% sort line: object
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',20).
% From E: 
% 
% subsort(line,object).
subsort(line, object).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',23).
% sort equipment: object
% From E: 
% 
% subsort(equipment,object).
subsort(equipment, object).

% sort weight: equipment
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',23).
% From E: 
% 
% subsort(weight,equipment).
subsort(weight, equipment).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',25).
% sort fin: equipment
% From E: 
% 
% subsort(fin,equipment).
subsort(fin, equipment).

% sort airtank: equipment
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',25).
% From E: 
% 
% subsort(airtank,equipment).
subsort(airtank, equipment).
%; buoyancy compensator (BC)
%; buoyancy control device (BCD)

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',30).
% sort computer: equipment
% From E: 
% 
% subsort(computer,equipment).
subsort(computer, equipment).

% sort bc: equipment
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',30).
% From E: 
% 
% subsort(bc,equipment).
subsort(bc, equipment).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',33).
% fluent AtDepth(object,depth)
% From E: 
% 
% fluent(atDepth(object,depth)).
mpred_prop(atDepth(object, depth), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',33).
fluents([atDepth/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',35).
% [object,depth1,depth2,time]
% HoldsAt(AtDepth(object,depth1),time) &
% HoldsAt(AtDepth(object,depth2),time) ->
% depth1 = depth2.
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          atDepth(Object,Depth1), 
%          Time), 
%       holds(
%          atDepth(Object,Depth2), 
%          Time)), 
%    Depth1=Depth2).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',35).
 if(not(equals(Depth1, Depth2)),
       (not(atDepth(Object, Depth1));not(atDepth(Object, Depth2)))).

% event Ascend(diver,depth)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',39).
% From E: 
% 
% event(ascend(diver,depth)).
events([ascend/2]).
mpred_prop(ascend(diver, depth), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',39).
actions([ascend/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',42).
% event Descend(diver,depth)
% From E: 
% 
% event(descend(diver,depth)).
events([descend/2]).
mpred_prop(descend(diver, depth), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',42).
actions([descend/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',44).
% [diver,depth1,depth2,time]
% HoldsAt(AtDepth(diver,depth1),time) &
% Happens(Descend(diver,depth2),time) ->
% depth2>depth1.
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          atDepth(Diver,Depth1), 
%          Time), 
%       happens(
%          descend(Diver,Depth2), 
%          Time)), 
%    Depth2>Depth1).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',44).
 if(not(comparison(Depth2, Depth1, >)),
       (not(atDepth(Diver, Depth1));not(descend(Diver, Depth2)))).


% [diver,depth1,depth2,time]
% HoldsAt(AtDepth(diver,depth1),time) &
% Happens(Ascend(diver,depth2),time) ->
% depth2<depth1.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',50).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          atDepth(Diver,Depth1), 
%          Time), 
%       happens(
%          ascend(Diver,Depth2), 
%          Time)), 
%    Depth2<Depth1).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',50).
 if(not(comparison(Depth2, Depth1, <)),
       (not(atDepth(Diver, Depth1));not(ascend(Diver, Depth2)))).


% [diver,depth,time]
% Initiates(Descend(diver,depth),AtDepth(diver,depth),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',55).
% From E: 
% 
% initiates_at(
%    descend(Diver,Depth), 
%    atDepth(Diver,Depth), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',55).
initiates(descend(Diver,Depth),
	  atDepth(Diver,Depth)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',57).
% [diver,depth1,depth2,time]
% HoldsAt(AtDepth(diver,depth1),time) ->
% Terminates(Descend(diver,depth2),AtDepth(diver,depth1),time).
% From E: 
% 
% '->'(
%    holds(
%       atDepth(Diver,Depth1), 
%       Time), 
%    terminates_at(
%       descend(Diver,Depth2), 
%       atDepth(Diver,Depth1), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',57).
if(not(terminates(descend(Diver,Depth2),
		  at(atDepth(Diver,Depth1),Time))),
   not(holds(atDepth(Diver,Depth1),Time))).


% [diver,depth,time]
% Initiates(Ascend(diver,depth),AtDepth(diver,depth),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',62).
% From E: 
% 
% initiates_at(
%    ascend(Diver,Depth), 
%    atDepth(Diver,Depth), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',62).
initiates(ascend(Diver,Depth),
	  atDepth(Diver,Depth)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',64).
% [diver,depth1,depth2,time]
% HoldsAt(AtDepth(diver,depth1),time) ->
% Terminates(Ascend(diver,depth2),AtDepth(diver,depth1),time).
% From E: 
% 
% '->'(
%    holds(
%       atDepth(Diver,Depth1), 
%       Time), 
%    terminates_at(
%       ascend(Diver,Depth2), 
%       atDepth(Diver,Depth1), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',64).
if(not(terminates(ascend(Diver,Depth2),
		  at(atDepth(Diver,Depth1),Time))),
   not(holds(atDepth(Diver,Depth1),Time))).

% fluent Wearing(diver,equipment)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',67).
% From E: 
% 
% fluent(wearing(diver,equipment)).
mpred_prop(wearing(diver, equipment), fluent).
fluents([wearing/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',70).
% event PutOn(diver,equipment)
% From E: 
% 
% event(putOn(diver,equipment)).
events([putOn/2]).
mpred_prop(putOn(diver, equipment), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',70).
actions([putOn/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',72).
% event TakeOff(diver,equipment)
% From E: 
% 
% event(takeOff(diver,equipment)).
events([takeOff/2]).
mpred_prop(takeOff(diver, equipment), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',72).
actions([takeOff/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',74).
% event Lose(diver,equipment)
% From E: 
% 
% event(lose(diver,equipment)).
events([lose/2]).
mpred_prop(lose(diver, equipment), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',74).
actions([lose/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',76).
% [diver,equipment,depth,time]
% Releases(PutOn(diver,equipment),AtDepth(equipment,depth),time).
% From E: 
% 
% releases_at(
%    putOn(Diver,Equipment), 
%    atDepth(Equipment,Depth), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',76).
releases(putOn(Diver,Equipment),
	 atDepth(Equipment,Depth)).


% [diver,equipment,time]
% Releases(PutOn(diver,equipment),UnderWater(equipment),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',80).
% From E: 
% 
% releases_at(
%    putOn(Diver,Equipment), 
%    underWater(Equipment), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',80).
releases(putOn(Diver,Equipment),
	 underWater(Equipment)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',82).
% [diver,equipment,time]
% Happens(PutOn(diver,equipment),time) ->
% !{diver1} HoldsAt(Wearing(diver1,equipment),time).
% From E: 
% 
% '->'(
%    happens(
%       putOn(Diver,Equipment), 
%       Time), 
%    not(thereExists(Diver1, 
%           holds(
%              wearing(Diver1,Equipment), 
%              Time)))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',82).
if(thereExists(Diver1,wearing(Diver1,Equipment)),
   not(putOn(Diver,Equipment))).


% [diver,depth,equipment,time]
% HoldsAt(Wearing(diver,equipment),time) ->
% (HoldsAt(AtDepth(diver,depth),time) <->
%  HoldsAt(AtDepth(equipment,depth),time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',87).
% From E: 
% 
% '->'(
%    holds(
%       wearing(Diver,Equipment), 
%       Time), 
%    <->(
%       holds(
%          atDepth(Diver,Depth), 
%          Time), 
%       holds(
%          atDepth(Equipment,Depth), 
%          Time))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',87).
if((not(atDepth(Equipment, Depth)), atDepth(Diver, Depth);not(atDepth(Diver, Depth)), atDepth(Equipment, Depth)), not(wearing(Diver, Equipment))).


% [diver,depth,object,time]
% HoldsAt(Holding(diver,object),time) ->
% (HoldsAt(AtDepth(diver,depth),time) <->
%  HoldsAt(AtDepth(object,depth),time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',92).
% From E: 
% 
% '->'(
%    holds(
%       holding(Diver,Object), 
%       Time), 
%    <->(
%       holds(
%          atDepth(Diver,Depth), 
%          Time), 
%       holds(
%          atDepth(Object,Depth), 
%          Time))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',92).
if((not(atDepth(Object, Depth)), atDepth(Diver, Depth);not(atDepth(Diver, Depth)), atDepth(Object, Depth)), not(holding(Diver, Object))).


% [diver,equipment,time]
% HoldsAt(Wearing(diver,equipment),time) ->
% (HoldsAt(UnderWater(diver),time) <->
%  HoldsAt(UnderWater(equipment),time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',97).
% From E: 
% 
% '->'(
%    holds(
%       wearing(Diver,Equipment), 
%       Time), 
%    <->(
%       holds(
%          underWater(Diver), 
%          Time), 
%       holds(
%          underWater(Equipment), 
%          Time))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',97).
if((not(underWater(Equipment)), underWater(Diver);not(underWater(Diver)), underWater(Equipment)), not(wearing(Diver, Equipment))).


% [diver,object,time]
% HoldsAt(Holding(diver,object),time) ->
% (HoldsAt(UnderWater(diver),time) <->
%  HoldsAt(UnderWater(object),time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',102).
% From E: 
% 
% '->'(
%    holds(
%       holding(Diver,Object), 
%       Time), 
%    <->(
%       holds(
%          underWater(Diver), 
%          Time), 
%       holds(
%          underWater(Object), 
%          Time))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',102).
if((not(underWater(Object)), underWater(Diver);not(underWater(Diver)), underWater(Object)), not(holding(Diver, Object))).


% [diver,depth,equipment,time]
% HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Wearing(diver,equipment),time) ->
% Initiates(TakeOff(diver,equipment),AtDepth(equipment,depth),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',107).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          atDepth(Diver,Depth), 
%          Time), 
%       holds(
%          wearing(Diver,Equipment), 
%          Time)), 
%    initiates_at(
%       takeOff(Diver,Equipment), 
%       atDepth(Equipment,Depth), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',107).
 if(not(initiates(takeOff(Diver, Equipment),
                    at(atDepth(Equipment, Depth), Time))),
       (not(holds(atDepth(Diver, Depth), Time));not(holds(wearing(Diver, Equipment), Time)))).


% [diver,depth,equipment,time]
% !HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Wearing(diver,equipment),time) ->
% Terminates(TakeOff(diver,equipment),AtDepth(equipment,depth),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',112).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          not(atDepth(Diver,Depth)), 
%          Time), 
%       holds(
%          wearing(Diver,Equipment), 
%          Time)), 
%    terminates_at(
%       takeOff(Diver,Equipment), 
%       atDepth(Equipment,Depth), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',112).
 if(not(terminates(takeOff(Diver, Equipment),
                     at(atDepth(Equipment, Depth), Time))),
       (holds(atDepth(Diver, Depth), Time);not(holds(wearing(Diver, Equipment), Time)))).


% [diver,equipment,time]
% HoldsAt(UnderWater(diver),time) ->
% Initiates(TakeOff(diver,equipment),UnderWater(equipment),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',117).
% From E: 
% 
% '->'(
%    holds(
%       underWater(Diver), 
%       Time), 
%    initiates_at(
%       takeOff(Diver,Equipment), 
%       underWater(Equipment), 
%       Time)).
 %   [Time].
if(not(initiates(takeOff(Diver,Equipment),
		 at(underWater(Equipment),Time))),
   not(holds(underWater(Diver),Time))).


% [diver,equipment,time]
% !HoldsAt(UnderWater(diver),time) ->
% Terminates(TakeOff(diver,equipment),UnderWater(equipment),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',121).
% From E: 
% 
% '->'(
%    holds(
%       not(underWater(Diver)), 
%       Time), 
%    terminates_at(
%       takeOff(Diver,Equipment), 
%       underWater(Equipment), 
%       Time)).
 %   [Time].
if(not(terminates(takeOff(Diver,Equipment),
		  at(underWater(Equipment),Time))),
   holds(underWater(Diver),Time)).


% [diver,equipment,depth,time]
% HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Wearing(diver,equipment),time) ->
% Initiates(Lose(diver,equipment),AtDepth(equipment,depth),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',125).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          atDepth(Diver,Depth), 
%          Time), 
%       holds(
%          wearing(Diver,Equipment), 
%          Time)), 
%    initiates_at(
%       lose(Diver,Equipment), 
%       atDepth(Equipment,Depth), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',125).
 if(not(initiates(lose(Diver, Equipment),
                    at(atDepth(Equipment, Depth), Time))),
       (not(holds(atDepth(Diver, Depth), Time));not(holds(wearing(Diver, Equipment), Time)))).


% [diver,equipment,depth,time]
% !HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Wearing(diver,equipment),time) ->
% Terminates(Lose(diver,equipment),AtDepth(equipment,depth),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',130).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          not(atDepth(Diver,Depth)), 
%          Time), 
%       holds(
%          wearing(Diver,Equipment), 
%          Time)), 
%    terminates_at(
%       lose(Diver,Equipment), 
%       atDepth(Equipment,Depth), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',130).
 if(not(terminates(lose(Diver, Equipment),
                     at(atDepth(Equipment, Depth), Time))),
       (holds(atDepth(Diver, Depth), Time);not(holds(wearing(Diver, Equipment), Time)))).


% [diver,equipment,time]
% HoldsAt(UnderWater(diver),time) ->
% Initiates(Lose(diver,equipment),UnderWater(equipment),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',135).
% From E: 
% 
% '->'(
%    holds(
%       underWater(Diver), 
%       Time), 
%    initiates_at(
%       lose(Diver,Equipment), 
%       underWater(Equipment), 
%       Time)).
 %   [Time].
if(not(initiates(lose(Diver,Equipment),
		 at(underWater(Equipment),Time))),
   not(holds(underWater(Diver),Time))).


% [diver,equipment,time]
% !HoldsAt(UnderWater(diver),time) ->
% Terminates(Lose(diver,equipment),UnderWater(equipment),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',139).
% From E: 
% 
% '->'(
%    holds(
%       not(underWater(Diver)), 
%       Time), 
%    terminates_at(
%       lose(Diver,Equipment), 
%       underWater(Equipment), 
%       Time)).
 %   [Time].
if(not(terminates(lose(Diver,Equipment),
		  at(underWater(Equipment),Time))),
   holds(underWater(Diver),Time)).

% fluent Holding(diver,object)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',141).
% From E: 
% 
% fluent(holding(diver,object)).
mpred_prop(holding(diver, object), fluent).
fluents([holding/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',144).
% [diver1,diver2,time]
% HoldsAt(Holding(diver1,diver2),time) ->
% !HoldsAt(Holding(diver2,diver1),time).
% From E: 
% 
% '->'(
%    holds(
%       holding(Diver1,Diver2), 
%       Time), 
%    holds(
%       not(holding(Diver2,Diver1)), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',144).
if(holding(Diver2,Diver1),
   not(holding(Diver1,Diver2))).

% event Grab(diver,object)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',147).
% From E: 
% 
% event(grab(diver,object)).
events([grab/2]).
mpred_prop(grab(diver, object), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',147).
actions([grab/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',150).
% event LetGoOf(diver,object)
% From E: 
% 
% event(letGoOf(diver,object)).
events([letGoOf/2]).
mpred_prop(letGoOf(diver, object), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',150).
actions([letGoOf/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',152).
% [diver,object,time]
% Initiates(Grab(diver,object),Holding(diver,object),time).
% From E: 
% 
% initiates_at(
%    grab(Diver,Object), 
%    holding(Diver,Object), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',152).
initiates(grab(Diver,Object),
	  holding(Diver,Object)).


% [diver,object,time]
% Terminates(LetGoOf(diver,object),Holding(diver,object),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',156).
% From E: 
% 
% terminates_at(
%    letGoOf(Diver,Object), 
%    holding(Diver,Object), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',156).
terminates(letGoOf(Diver,Object),
	   holding(Diver,Object)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',158).
% [diver,object,depth,time]
% Releases(Grab(diver,object),AtDepth(object,depth),time).
% From E: 
% 
% releases_at(
%    grab(Diver,Object), 
%    atDepth(Object,Depth), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',158).
releases(grab(Diver,Object),
	 atDepth(Object,Depth)).


% [diver,object,time]
% Releases(Grab(diver,object),UnderWater(object),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',162).
% From E: 
% 
% releases_at(
%    grab(Diver,Object), 
%    underWater(Object), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',162).
releases(grab(Diver,Object),underWater(Object)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',164).
% [diver,object,depth,time]
% HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Holding(diver,object),time) ->
% Initiates(LetGoOf(diver,object),AtDepth(object,depth),time).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          atDepth(Diver,Depth), 
%          Time), 
%       holds(
%          holding(Diver,Object), 
%          Time)), 
%    initiates_at(
%       letGoOf(Diver,Object), 
%       atDepth(Object,Depth), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',164).
 if(not(initiates(letGoOf(Diver, Object),
                    at(atDepth(Object, Depth), Time))),
       (not(holds(atDepth(Diver, Depth), Time));not(holds(holding(Diver, Object), Time)))).


% [diver,object,depth,time]
% !HoldsAt(AtDepth(diver,depth),time) &
% HoldsAt(Holding(diver,object),time) ->
% Terminates(LetGoOf(diver,object),AtDepth(object,depth),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',170).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          not(atDepth(Diver,Depth)), 
%          Time), 
%       holds(
%          holding(Diver,Object), 
%          Time)), 
%    terminates_at(
%       letGoOf(Diver,Object), 
%       atDepth(Object,Depth), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',170).
 if(not(terminates(letGoOf(Diver, Object),
                     at(atDepth(Object, Depth), Time))),
       (holds(atDepth(Diver, Depth), Time);not(holds(holding(Diver, Object), Time)))).


% [diver,object,time]
% HoldsAt(UnderWater(diver),time) ->
% Initiates(LetGoOf(diver,object),UnderWater(object),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',175).
% From E: 
% 
% '->'(
%    holds(
%       underWater(Diver), 
%       Time), 
%    initiates_at(
%       letGoOf(Diver,Object), 
%       underWater(Object), 
%       Time)).
 %   [Time].
if(not(initiates(letGoOf(Diver,Object),
		 at(underWater(Object),Time))),
   not(holds(underWater(Diver),Time))).


% [diver,object,time]
% !HoldsAt(UnderWater(diver),time) ->
% Terminates(LetGoOf(diver,object),UnderWater(object),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',179).
% From E: 
% 
% '->'(
%    holds(
%       not(underWater(Diver)), 
%       Time), 
%    terminates_at(
%       letGoOf(Diver,Object), 
%       underWater(Object), 
%       Time)).
 %   [Time].
if(not(terminates(letGoOf(Diver,Object),
		  at(underWater(Object),Time))),
   holds(underWater(Diver),Time)).


% [diver,equipment,time]
% Initiates(PutOn(diver,equipment),Wearing(diver,equipment),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',183).
% From E: 
% 
% initiates_at(
%    putOn(Diver,Equipment), 
%    wearing(Diver,Equipment), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',183).
initiates(putOn(Diver,Equipment),
	  wearing(Diver,Equipment)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',185).
% [diver,equipment,time]
% Happens(PutOn(diver,equipment),time) ->
% !HoldsAt(UnderWater(diver),time).
% From E: 
% 
% '->'(
%    happens(
%       putOn(Diver,Equipment), 
%       Time), 
%    holds(
%       not(underWater(Diver)), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',185).
if(underWater(Diver),not(putOn(Diver,Equipment))).


% [diver,equipment,time]
% Terminates(TakeOff(diver,equipment),Wearing(diver,equipment),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',190).
% From E: 
% 
% terminates_at(
%    takeOff(Diver,Equipment), 
%    wearing(Diver,Equipment), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',190).
terminates(takeOff(Diver,Equipment),
	   wearing(Diver,Equipment)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',192).
% [diver,equipment,time]
% Terminates(Lose(diver,equipment),Wearing(diver,equipment),time).
% From E: 
% 
% terminates_at(
%    lose(Diver,Equipment), 
%    wearing(Diver,Equipment), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',192).
terminates(lose(Diver,Equipment),
	   wearing(Diver,Equipment)).

% fluent Vertical(diver)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',194).
% From E: 
% 
% fluent(vertical(diver)).
mpred_prop(vertical(diver), fluent).
fluents([vertical/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',197).
% fluent HorizontalDown(diver)
% From E: 
% 
% fluent(horizontalDown(diver)).
mpred_prop(horizontalDown(diver), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',197).
fluents([horizontalDown/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',199).
% fluent Inverted(diver)
% From E: 
% 
% fluent(inverted(diver)).
mpred_prop(inverted(diver), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',199).
fluents([inverted/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',201).
% fluent HorizontalUp(diver)
% From E: 
% 
% fluent(horizontalUp(diver)).
mpred_prop(horizontalUp(diver), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',201).
fluents([horizontalUp/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',203).
% xor Vertical, HorizontalDown, Inverted, HorizontalUp
% From E: 
% 
% xor([vertical,horizontalDown,inverted,horizontalUp]).
xor([vertical,horizontalDown,inverted,horizontalUp]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',205).
% event RotatePitch(diver)
% From E: 
% 
% event(rotatePitch(diver)).
events([rotatePitch/1]).
mpred_prop(rotatePitch(diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',205).
actions([rotatePitch/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',207).
% [diver,time]
% HoldsAt(Vertical(diver),time) ->
% Initiates(RotatePitch(diver),HorizontalDown(diver),time).
% From E: 
% 
% '->'(
%    holds(
%       vertical(Diver), 
%       Time), 
%    initiates_at(
%       rotatePitch(Diver), 
%       horizontalDown(Diver), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',207).
if(not(initiates(rotatePitch(Diver),
		 at(horizontalDown(Diver),Time))),
   not(holds(vertical(Diver),Time))).


% [diver,time]
% HoldsAt(HorizontalDown(diver),time) ->
% Initiates(RotatePitch(diver),Inverted(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',212).
% From E: 
% 
% '->'(
%    holds(
%       horizontalDown(Diver), 
%       Time), 
%    initiates_at(
%       rotatePitch(Diver), 
%       inverted(Diver), 
%       Time)).
 %   [Time].
if(not(initiates(rotatePitch(Diver),
		 at(inverted(Diver),Time))),
   not(holds(horizontalDown(Diver),Time))).


% [diver,time]
% HoldsAt(HorizontalDown(diver),time) ->
% Terminates(RotatePitch(diver),HorizontalDown(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',216).
% From E: 
% 
% '->'(
%    holds(
%       horizontalDown(Diver), 
%       Time), 
%    terminates_at(
%       rotatePitch(Diver), 
%       horizontalDown(Diver), 
%       Time)).
 %   [Time].
if(not(terminates(rotatePitch(Diver),
		  at(horizontalDown(Diver),Time))),
   not(holds(horizontalDown(Diver),Time))).


% [diver,time]
% HoldsAt(Inverted(diver),time) ->
% Initiates(RotatePitch(diver),HorizontalUp(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',220).
% From E: 
% 
% '->'(
%    holds(
%       inverted(Diver), 
%       Time), 
%    initiates_at(
%       rotatePitch(Diver), 
%       horizontalUp(Diver), 
%       Time)).
 %   [Time].
if(not(initiates(rotatePitch(Diver),
		 at(horizontalUp(Diver),Time))),
   not(holds(inverted(Diver),Time))).


% [diver,time]
% HoldsAt(Inverted(diver),time) ->
% Terminates(RotatePitch(diver),Inverted(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',224).
% From E: 
% 
% '->'(
%    holds(
%       inverted(Diver), 
%       Time), 
%    terminates_at(
%       rotatePitch(Diver), 
%       inverted(Diver), 
%       Time)).
 %   [Time].
if(not(terminates(rotatePitch(Diver),
		  at(inverted(Diver),Time))),
   not(holds(inverted(Diver),Time))).


% [diver,time]
% HoldsAt(HorizontalUp(diver),time) ->
% Initiates(RotatePitch(diver),Vertical(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',228).
% From E: 
% 
% '->'(
%    holds(
%       horizontalUp(Diver), 
%       Time), 
%    initiates_at(
%       rotatePitch(Diver), 
%       vertical(Diver), 
%       Time)).
 %   [Time].
if(not(initiates(rotatePitch(Diver),
		 at(vertical(Diver),Time))),
   not(holds(horizontalUp(Diver),Time))).


% [diver,time]
% HoldsAt(HorizontalUp(diver),time) ->
% Terminates(RotatePitch(diver),HorizontalUp(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',232).
% From E: 
% 
% '->'(
%    holds(
%       horizontalUp(Diver), 
%       Time), 
%    terminates_at(
%       rotatePitch(Diver), 
%       horizontalUp(Diver), 
%       Time)).
 %   [Time].
if(not(terminates(rotatePitch(Diver),
		  at(horizontalUp(Diver),Time))),
   not(holds(horizontalUp(Diver),Time))).

% event RotateYaw(diver)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',234).
% From E: 
% 
% event(rotateYaw(diver)).
events([rotateYaw/1]).
mpred_prop(rotateYaw(diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',234).
actions([rotateYaw/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',237).
%; try taking out Holding condition here
% [diver,time]
% Happens(Ascend1(diver),time) &
% !Happens(RapidAscendToSurface(diver),time) &
% !({diver1} HoldsAt(Holding(diver,diver1),time)) ->
% Happens(RotateYaw(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',239).
% From E: 
% 
% '->'(
%    ','(
%       happens(
%          ascend1(Diver), 
%          Time), 
%       ','(
%          not(happens(
%                 rapidAscendToSurface(Diver), 
%                 Time)), 
%          not(thereExists(Diver1, 
%                 holds(
%                    holding(Diver,Diver1), 
%                    Time))))), 
%    happens(
%       rotateYaw(Diver), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',239).
 if(not(rotateYaw(Diver)),
       (not(ascend1(Diver));rapidAscendToSurface(Diver);thereExists(Diver1, holding(Diver, Diver1)))).

% fluent UnderWater(object)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',243).
% From E: 
% 
% fluent(underWater(object)).
mpred_prop(underWater(object), fluent).
fluents([underWater/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',246).
% [object,depth,time]
% depth>% 0 &
% HoldsAt(AtDepth(object,depth),time) ->
% HoldsAt(UnderWater(object),time).
% From E: 
% 
% '->'(
%    ','(
%       Depth>0, 
%       holds(
%          atDepth(Object,Depth), 
%          Time)), 
%    holds(
%       underWater(Object), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',246).
 if(not(underWater(Object)),
       (not(comparison(Depth, 0, >));not(atDepth(Object, Depth)))).

% event EnterWater(object)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',250).
% From E: 
% 
% event(enterWater(object)).
mpred_prop(enterWater(object), event).
events([enterWater/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',253).
% event Surface(object)
% From E: 
% 
% event(surface(object)).
mpred_prop(surface(object), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',253).
events([surface/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',255).
% [object,time]
% Initiates(EnterWater(object),UnderWater(object),time).
% From E: 
% 
% initiates_at(
%    enterWater(Object), 
%    underWater(Object), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',255).
initiates(enterWater(Object),underWater(Object)).


% [diver,time]
% Happens(EnterWater(diver),time) ->
% !{diver1} HoldsAt(Holding(diver1,diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',259).
% From E: 
% 
% '->'(
%    happens(
%       enterWater(Diver), 
%       Time), 
%    not(thereExists(Diver1, 
%           holds(
%              holding(Diver1,Diver), 
%              Time)))).
 %   [Time].
if(thereExists(Diver1,holding(Diver1,Diver)),
   not(enterWater(Diver))).


% [object,depth,time]
% depth=% 0 ->
% Initiates(EnterWater(object),AtDepth(object,depth),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',263).
% From E: 
% 
% '->'(
%    Depth=0, 
%    initiates_at(
%       enterWater(Object), 
%       atDepth(Object,Depth), 
%       Time)).
 %   [Time].
if(not(initiates(enterWater(Object),
		 at(atDepth(Object,Depth),Time))),
   not(equals(Depth,0))).


% [object,time]
% Terminates(Surface(object),UnderWater(object),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',267).
% From E: 
% 
% terminates_at(
%    surface(Object), 
%    underWater(Object), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',267).
terminates(surface(Object),underWater(Object)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',269).
% [diver,time]
% Terminates(Surface(diver),PositivelyBuoyant(diver),time).
% From E: 
% 
% terminates_at(
%    surface(Diver), 
%    positivelyBuoyant(Diver), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',269).
terminates(surface(Diver),positivelyBuoyant(Diver)).


% [diver,time]
% Terminates(Surface(diver),NegativelyBuoyant(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',273).
% From E: 
% 
% terminates_at(
%    surface(Diver), 
%    negativelyBuoyant(Diver), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',273).
terminates(surface(Diver),negativelyBuoyant(Diver)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',275).
% [diver,time]
% Terminates(Surface(diver),NeutrallyBuoyant(diver),time).
% From E: 
% 
% terminates_at(
%    surface(Diver), 
%    neutrallyBuoyant(Diver), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',275).
terminates(surface(Diver),neutrallyBuoyant(Diver)).


% [object,depth,time]
% Terminates(Surface(object),AtDepth(object,depth),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',279).
% From E: 
% 
% terminates_at(
%    surface(Object), 
%    atDepth(Object,Depth), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',279).
terminates(surface(Object),atDepth(Object,Depth)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',281).
% [diver,time]
 % Happens(EnterWater(diver),time) ->
% HoldsAt(Vertical(diver),time).
% From E: 
% 
% '->'(
%    happens(
%       enterWater(Diver), 
%       Time), 
%    holds(
%       vertical(Diver), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',281).
if(not(vertical(Diver)),not(enterWater(Diver))).

% fluent StandingOn(diver,boat)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',283).
% From E: 
% 
% fluent(standingOn(diver,boat)).
mpred_prop(standingOn(diver, boat), fluent).
fluents([standingOn/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',286).
% event StandOn(diver,boat)
% From E: 
% 
% event(standOn(diver,boat)).
events([standOn/2]).
mpred_prop(standOn(diver, boat), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',286).
actions([standOn/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',288).
% [diver,boat,time]
% Terminates(EnterWater(diver),StandingOn(diver,boat),time).
% From E: 
% 
% terminates_at(
%    enterWater(Diver), 
%    standingOn(Diver,Boat), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',288).
terminates(enterWater(Diver),standingOn(Diver,Boat)).


% [diver,boat,time]
% Initiates(StandOn(diver,boat),StandingOn(diver,boat),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',292).
% From E: 
% 
% initiates_at(
%    standOn(Diver,Boat), 
%    standingOn(Diver,Boat), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',292).
initiates(standOn(Diver,Boat),
	  standingOn(Diver,Boat)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',294).
% fluent PositivelyBuoyant(diver)
% From E: 
% 
% fluent(positivelyBuoyant(diver)).
mpred_prop(positivelyBuoyant(diver), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',294).
fluents([positivelyBuoyant/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',296).
% fluent NeutrallyBuoyant(diver)
% From E: 
% 
% fluent(neutrallyBuoyant(diver)).
mpred_prop(neutrallyBuoyant(diver), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',296).
fluents([neutrallyBuoyant/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',298).
% fluent NegativelyBuoyant(diver)
% From E: 
% 
% fluent(negativelyBuoyant(diver)).
mpred_prop(negativelyBuoyant(diver), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',298).
fluents([negativelyBuoyant/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',300).
% mutex PositivelyBuoyant, NeutrallyBuoyant, NegativelyBuoyant
% From E: 
% 
% ':-'(call_pel_directive(mutex(positivelyBuoyant))).
:- call_pel_directive(mutex(positivelyBuoyant)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',300).
% From E: 
% 
% ':-'(call_pel_directive(mutex(neutrallyBuoyant))).
:- call_pel_directive(mutex(neutrallyBuoyant)).
% From E: 
% 
% ':-'(call_pel_directive(mutex(negativelyBuoyant))).
:- call_pel_directive(mutex(negativelyBuoyant)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',302).
% [diver,time]
% HoldsAt(PositivelyBuoyant(diver),time) ->
% HoldsAt(UnderWater(diver),time).
% From E: 
% 
% '->'(
%    holds(
%       positivelyBuoyant(Diver), 
%       Time), 
%    holds(
%       underWater(Diver), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',302).
if(not(underWater(Diver)),not(positivelyBuoyant(Diver))).


% [diver,time]
% HoldsAt(NeutrallyBuoyant(diver),time) ->
% HoldsAt(UnderWater(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',307).
% From E: 
% 
% '->'(
%    holds(
%       neutrallyBuoyant(Diver), 
%       Time), 
%    holds(
%       underWater(Diver), 
%       Time)).
 %   [Time].
if(not(underWater(Diver)),not(neutrallyBuoyant(Diver))).


% [diver,time]
% HoldsAt(NegativelyBuoyant(diver),time) ->
% HoldsAt(UnderWater(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',311).
% From E: 
% 
% '->'(
%    holds(
%       negativelyBuoyant(Diver), 
%       Time), 
%    holds(
%       underWater(Diver), 
%       Time)).
 %   [Time].
if(not(underWater(Diver)),not(negativelyBuoyant(Diver))).

% event PressDeflateButton(diver,bc)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',313).
% From E: 
% 
% event(pressDeflateButton(diver,bc)).
events([pressDeflateButton/2]).
mpred_prop(pressDeflateButton(diver, bc), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',313).
actions([pressDeflateButton/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',316).
% event PressDumpButton(diver,bc)
% From E: 
% 
% event(pressDumpButton(diver,bc)).
events([pressDumpButton/2]).
mpred_prop(pressDumpButton(diver, bc), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',316).
actions([pressDumpButton/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',318).
% event PressInflateButton(diver,bc)
% From E: 
% 
% event(pressInflateButton(diver,bc)).
events([pressInflateButton/2]).
mpred_prop(pressInflateButton(diver, bc), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',318).
actions([pressInflateButton/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',320).
% [diver,bc,time]
% Happens(PressDeflateButton(diver,bc),time) ->
% HoldsAt(Vertical(diver),time) &
% HoldsAt(UnderWater(bc),time).
% From E: 
% 
% '->'(
%    happens(
%       pressDeflateButton(Diver,Bc), 
%       Time), 
%    ','(
%       holds(
%          vertical(Diver), 
%          Time), 
%       holds(
%          underWater(Bc), 
%          Time))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',320).
 if((not(vertical(Diver));not(underWater(Bc))),
      not(pressDeflateButton(Diver, Bc))).


% [diver,bc,time]
% Happens(PressDumpButton(diver,bc),time) ->
% HoldsAt(Vertical(diver),time) &
% HoldsAt(UnderWater(bc),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',326).
% From E: 
% 
% '->'(
%    happens(
%       pressDumpButton(Diver,Bc), 
%       Time), 
%    ','(
%       holds(
%          vertical(Diver), 
%          Time), 
%       holds(
%          underWater(Bc), 
%          Time))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',326).
 if((not(vertical(Diver));not(underWater(Bc))),
      not(pressDumpButton(Diver, Bc))).


% [diver,bc,time]
 % Happens(PressDumpButton(diver,bc),time) ->
% HoldsAt(UncontrolledBuoyancy(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',329).
% From E: 
% 
% '->'(
%    happens(
%       pressDumpButton(Diver,Bc), 
%       Time), 
%    holds(
%       uncontrolledBuoyancy(Diver), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',329).
if(not(uncontrolledBuoyancy(Diver)),
   not(pressDumpButton(Diver,Bc))).


% [diver,bc,time]
% HoldsAt(Wearing(diver,bc),time) ->
% Initiates(PressDeflateButton(diver,bc),NegativelyBuoyant(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',334).
% From E: 
% 
% '->'(
%    holds(
%       wearing(Diver,Bc), 
%       Time), 
%    initiates_at(
%       pressDeflateButton(Diver,Bc), 
%       negativelyBuoyant(Diver), 
%       Time)).
 %   [Time].
if(not(initiates(pressDeflateButton(Diver,Bc),
		 at(negativelyBuoyant(Diver),Time))),
   not(holds(wearing(Diver,Bc),Time))).


% [diver,bc,time]
% HoldsAt(Wearing(diver,bc),time) ->
% Terminates(PressDeflateButton(diver,bc),NeutrallyBuoyant(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',338).
% From E: 
% 
% '->'(
%    holds(
%       wearing(Diver,Bc), 
%       Time), 
%    terminates_at(
%       pressDeflateButton(Diver,Bc), 
%       neutrallyBuoyant(Diver), 
%       Time)).
 %   [Time].
if(not(terminates(pressDeflateButton(Diver,Bc),
		  at(neutrallyBuoyant(Diver),Time))),
   not(holds(wearing(Diver,Bc),Time))).


% [diver,bc,time]
% HoldsAt(Wearing(diver,bc),time) ->
% Terminates(PressDeflateButton(diver,bc),PositivelyBuoyant(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',342).
% From E: 
% 
% '->'(
%    holds(
%       wearing(Diver,Bc), 
%       Time), 
%    terminates_at(
%       pressDeflateButton(Diver,Bc), 
%       positivelyBuoyant(Diver), 
%       Time)).
 %   [Time].
if(not(terminates(pressDeflateButton(Diver,Bc),
		  at(positivelyBuoyant(Diver),Time))),
   not(holds(wearing(Diver,Bc),Time))).


% [diver,bc,time]
% HoldsAt(Wearing(diver,bc),time) ->
% Initiates(PressDumpButton(diver,bc),NegativelyBuoyant(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',346).
% From E: 
% 
% '->'(
%    holds(
%       wearing(Diver,Bc), 
%       Time), 
%    initiates_at(
%       pressDumpButton(Diver,Bc), 
%       negativelyBuoyant(Diver), 
%       Time)).
 %   [Time].
if(not(initiates(pressDumpButton(Diver,Bc),
		 at(negativelyBuoyant(Diver),Time))),
   not(holds(wearing(Diver,Bc),Time))).


% [diver,bc,time]
% HoldsAt(Wearing(diver,bc),time) ->
% Terminates(PressDumpButton(diver,bc),NeutrallyBuoyant(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',350).
% From E: 
% 
% '->'(
%    holds(
%       wearing(Diver,Bc), 
%       Time), 
%    terminates_at(
%       pressDumpButton(Diver,Bc), 
%       neutrallyBuoyant(Diver), 
%       Time)).
 %   [Time].
if(not(terminates(pressDumpButton(Diver,Bc),
		  at(neutrallyBuoyant(Diver),Time))),
   not(holds(wearing(Diver,Bc),Time))).


% [diver,bc,time]
% HoldsAt(Wearing(diver,bc),time) ->
% Terminates(PressDumpButton(diver,bc),PositivelyBuoyant(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',354).
% From E: 
% 
% '->'(
%    holds(
%       wearing(Diver,Bc), 
%       Time), 
%    terminates_at(
%       pressDumpButton(Diver,Bc), 
%       positivelyBuoyant(Diver), 
%       Time)).
 %   [Time].
if(not(terminates(pressDumpButton(Diver,Bc),
		  at(positivelyBuoyant(Diver),Time))),
   not(holds(wearing(Diver,Bc),Time))).


% [diver,bc,time]
% HoldsAt(Wearing(diver,bc),time) ->
% Initiates(PressInflateButton(diver,bc),NeutrallyBuoyant(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',358).
% From E: 
% 
% '->'(
%    holds(
%       wearing(Diver,Bc), 
%       Time), 
%    initiates_at(
%       pressInflateButton(Diver,Bc), 
%       neutrallyBuoyant(Diver), 
%       Time)).
 %   [Time].
if(not(initiates(pressInflateButton(Diver,Bc),
		 at(neutrallyBuoyant(Diver),Time))),
   not(holds(wearing(Diver,Bc),Time))).


% [diver,bc,time]
% HoldsAt(Wearing(diver,bc),time) ->
% Terminates(PressInflateButton(diver,bc),PositivelyBuoyant(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',362).
% From E: 
% 
% '->'(
%    holds(
%       wearing(Diver,Bc), 
%       Time), 
%    terminates_at(
%       pressInflateButton(Diver,Bc), 
%       positivelyBuoyant(Diver), 
%       Time)).
 %   [Time].
if(not(terminates(pressInflateButton(Diver,Bc),
		  at(positivelyBuoyant(Diver),Time))),
   not(holds(wearing(Diver,Bc),Time))).


% [diver,bc,time]
% HoldsAt(Wearing(diver,bc),time) ->
% Terminates(PressInflateButton(diver,bc),NegativelyBuoyant(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',366).
% From E: 
% 
% '->'(
%    holds(
%       wearing(Diver,Bc), 
%       Time), 
%    terminates_at(
%       pressInflateButton(Diver,Bc), 
%       negativelyBuoyant(Diver), 
%       Time)).
 %   [Time].
if(not(terminates(pressInflateButton(Diver,Bc),
		  at(negativelyBuoyant(Diver),Time))),
   not(holds(wearing(Diver,Bc),Time))).


% [diver,weight,time]
% HoldsAt(Wearing(diver,weight),time) ->
% Initiates(TakeOff(diver,weight),PositivelyBuoyant(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',370).
% From E: 
% 
% '->'(
%    holds(
%       wearing(Diver,Weight), 
%       Time), 
%    initiates_at(
%       takeOff(Diver,Weight), 
%       positivelyBuoyant(Diver), 
%       Time)).
 %   [Time].
if(not(initiates(takeOff(Diver,Weight),
		 at(positivelyBuoyant(Diver),Time))),
   not(holds(wearing(Diver,Weight),Time))).


% [diver,weight,time]
% HoldsAt(Wearing(diver,weight),time) ->
% Terminates(TakeOff(diver,weight),NegativelyBuoyant(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',374).
% From E: 
% 
% '->'(
%    holds(
%       wearing(Diver,Weight), 
%       Time), 
%    terminates_at(
%       takeOff(Diver,Weight), 
%       negativelyBuoyant(Diver), 
%       Time)).
 %   [Time].
if(not(terminates(takeOff(Diver,Weight),
		  at(negativelyBuoyant(Diver),Time))),
   not(holds(wearing(Diver,Weight),Time))).


% [diver,weight,time]
% HoldsAt(Wearing(diver,weight),time) ->
% Terminates(TakeOff(diver,weight),NeutrallyBuoyant(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',378).
% From E: 
% 
% '->'(
%    holds(
%       wearing(Diver,Weight), 
%       Time), 
%    terminates_at(
%       takeOff(Diver,Weight), 
%       neutrallyBuoyant(Diver), 
%       Time)).
 %   [Time].
if(not(terminates(takeOff(Diver,Weight),
		  at(neutrallyBuoyant(Diver),Time))),
   not(holds(wearing(Diver,Weight),Time))).

% fluent UncontrolledBuoyancy(diver)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',380).
% From E: 
% 
% fluent(uncontrolledBuoyancy(diver)).
mpred_prop(uncontrolledBuoyancy(diver), fluent).
fluents([uncontrolledBuoyancy/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',383).
% event LoseBuoyancyControl(diver)
% From E: 
% 
% event(loseBuoyancyControl(diver)).
events([loseBuoyancyControl/1]).
mpred_prop(loseBuoyancyControl(diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',383).
actions([loseBuoyancyControl/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',385).
% predicate IsInexperiencedDiver(diver)
% From E: 
% 
% predicate(isInexperiencedDiver(diver)).
mpred_prop(isInexperiencedDiver(diver), predicate).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',385).
predicates([isInexperiencedDiver/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',387).
% [diver,time]
% Happens(LoseBuoyancyControl(diver),time) ->
% IsInexperiencedDiver(diver).
% From E: 
% 
% '->'(
%    happens(
%       loseBuoyancyControl(Diver), 
%       Time), 
%    isInexperiencedDiver(Diver)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',387).
if(not(isInexperiencedDiver(Diver)),
   not(loseBuoyancyControl(Diver))).


% [diver,time]
% Initiates(LoseBuoyancyControl(diver),UncontrolledBuoyancy(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',392).
% From E: 
% 
% initiates_at(
%    loseBuoyancyControl(Diver), 
%    uncontrolledBuoyancy(Diver), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',392).
initiates(loseBuoyancyControl(Diver),
	  uncontrolledBuoyancy(Diver)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',394).
% [diver,time]
% Initiates(LoseBuoyancyControl(diver),PositivelyBuoyant(diver),time).
% From E: 
% 
% initiates_at(
%    loseBuoyancyControl(Diver), 
%    positivelyBuoyant(Diver), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',394).
initiates(loseBuoyancyControl(Diver),
	  positivelyBuoyant(Diver)).


% [diver,time]
% Terminates(LoseBuoyancyControl(diver),NegativelyBuoyant(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',398).
% From E: 
% 
% terminates_at(
%    loseBuoyancyControl(Diver), 
%    negativelyBuoyant(Diver), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',398).
terminates(loseBuoyancyControl(Diver),
	   negativelyBuoyant(Diver)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',400).
% [diver,time]
% Terminates(LoseBuoyancyControl(diver),NeutrallyBuoyant(diver),time).
% From E: 
% 
% terminates_at(
%    loseBuoyancyControl(Diver), 
%    neutrallyBuoyant(Diver), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',400).
terminates(loseBuoyancyControl(Diver),
	   neutrallyBuoyant(Diver)).


%; determining fluent

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',404).
% fluent AscendDescendAmount(diver,depth)
% From E: 
% 
% fluent(ascendDescendAmount(diver,depth)).
mpred_prop(ascendDescendAmount(diver, depth), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',404).
fluents([ascendDescendAmount/2]).

% noninertial AscendDescendAmount
% From E: 
% 
% ':-'(call_pel_directive(noninertial(ascendDescendAmount))).
:- call_pel_directive(noninertial(ascendDescendAmount)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',407).
% [diver,depth1,depth2,time]
% HoldsAt(AscendDescendAmount(diver,depth1),time) &
% HoldsAt(AscendDescendAmount(diver,depth2),time) ->
% depth1=depth2.
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          ascendDescendAmount(Diver,Depth1), 
%          Time), 
%       holds(
%          ascendDescendAmount(Diver,Depth2), 
%          Time)), 
%    Depth1=Depth2).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',407).
 if(not(equals(Depth1, Depth2)),
       (not(ascendDescendAmount(Diver, Depth1));not(ascendDescendAmount(Diver, Depth2)))).


% [diver,depth,time]
% Happens(Descend(diver,depth),time) ->
% HoldsAt(NegativelyBuoyant(diver),time) &
% ({depth1}
%  HoldsAt(AscendDescendAmount(diver,depth1),time) &
%  HoldsAt(AtDepth(diver,depth-depth1),time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',413).
% From E: 
% 
% '->'(
%    happens(
%       descend(Diver,Depth), 
%       Time), 
%    ','(
%       holds(
%          negativelyBuoyant(Diver), 
%          Time), 
%       thereExists(Depth1, 
%          ','(
%             holds(
%                ascendDescendAmount(Diver,Depth1), 
%                Time), 
%             holds(
%                atDepth(Diver,Depth-Depth1), 
%                Time))))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',413).
if((not(negativelyBuoyant(Diver));not(thereExists(Depth1,  (ascendDescendAmount(Diver, Depth1), atDepth(Diver, Depth-Depth1))))), not(descend(Diver, Depth))).

% event KickUp(diver)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',418).
% From E: 
% 
% event(kickUp(diver)).
events([kickUp/1]).
mpred_prop(kickUp(diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',418).
actions([kickUp/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',421).
% [diver,depth,time]
% Happens(Ascend(diver,depth),time) ->
% (HoldsAt(PositivelyBuoyant(diver),time) |
%  (HoldsAt(NeutrallyBuoyant(diver),time) & Happens(KickUp(diver),time))) &
% ({depth1}
%  HoldsAt(AscendDescendAmount(diver,depth1),time) &
%  HoldsAt(AtDepth(diver,depth+depth1),time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',421).
% From E: 
% 
% '->'(
%    happens(
%       ascend(Diver,Depth), 
%       Time), 
%    ','(
%       ';'(
%          holds(
%             positivelyBuoyant(Diver), 
%             Time), 
%          ','(
%             holds(
%                neutrallyBuoyant(Diver), 
%                Time), 
%             happens(
%                kickUp(Diver), 
%                Time))), 
%       thereExists(Depth1, 
%          ','(
%             holds(
%                ascendDescendAmount(Diver,Depth1), 
%                Time), 
%             holds(
%                atDepth(Diver,Depth+Depth1), 
%                Time))))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',421).
if((not(positivelyBuoyant(Diver)), (not(neutrallyBuoyant(Diver));not(kickUp(Diver)));not(thereExists(Depth1,  (ascendDescendAmount(Diver, Depth1), atDepth(Diver, Depth+Depth1))))), not(ascend(Diver, Depth))).


% [diver,time]
% Happens(KickUp(diver),time) ->
% HoldsAt(Vertical(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',430).
% From E: 
% 
% '->'(
%    happens(
%       kickUp(Diver), 
%       Time), 
%    holds(
%       vertical(Diver), 
%       Time)).
 %   [Time].
if(not(vertical(Diver)),not(kickUp(Diver))).

% event SwimAround(diver)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',432).
% From E: 
% 
% event(swimAround(diver)).
events([swimAround/1]).
mpred_prop(swimAround(diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',432).
actions([swimAround/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',435).
% [diver,time]
% Happens(SwimAround(diver),time) ->
% HoldsAt(HorizontalDown(diver),time).
% From E: 
% 
% '->'(
%    happens(
%       swimAround(Diver), 
%       Time), 
%    holds(
%       horizontalDown(Diver), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',435).
if(not(horizontalDown(Diver)),not(swimAround(Diver))).


%; signaling

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',441).
% event SignalDescend(diver,diver)
% From E: 
% 
% event(signalDescend(diver,diver)).
events([signalDescend/2]).
mpred_prop(signalDescend(diver, diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',441).
actions([signalDescend/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',443).
% event SignalOutOfTime(diver,diver)
% From E: 
% 
% event(signalOutOfTime(diver,diver)).
events([signalOutOfTime/2]).
mpred_prop(signalOutOfTime(diver, diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',443).
actions([signalOutOfTime/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',445).
% event SignalAscend(diver,diver)
% From E: 
% 
% event(signalAscend(diver,diver)).
events([signalAscend/2]).
mpred_prop(signalAscend(diver, diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',445).
actions([signalAscend/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',447).
%;[diver1,diver2,time]
%;Happens(SignalAscend(diver1,diver2),time) ->
%;Happens(SignalOutOfTime(diver1,diver2),time-1).
%;[diver1,diver2,time]
%;Happens(SignalDescend(diver1,diver2),time) ->
%;HoldsAt(See(diver1,diver2),time) &
%;HoldsAt(See(diver2,diver1),time).
%;[diver1,diver2,time]
%;Happens(SignalOutOfTime(diver1,diver2),time) ->
%;HoldsAt(See(diver1,diver2),time) &
%;HoldsAt(See(diver2,diver1),time).
%;[diver1,diver2,time]
%;Happens(SignalAscend(diver1,diver2),time) ->
%;HoldsAt(See(diver1,diver2),time) &
%;HoldsAt(See(diver2,diver1),time).
%;event LookAt(agent,object)
%;fluent See(agent,object)
%;[agent,object,time]
%;Initiates(LookAt(agent,object),See(agent,object),time).
%;[agent,object1,object2,time]
%;object1!=object2 ->
%;Terminates(LookAt(agent,object1),
%;           See(agent,object2),
%;           time).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',479).
% event Descend1(diver)
% From E: 
% 
% event(descend1(diver)).
events([descend1/1]).
mpred_prop(descend1(diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',479).
actions([descend1/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',481).
% event Ascend1(diver)
% From E: 
% 
% event(ascend1(diver)).
events([ascend1/1]).
mpred_prop(ascend1(diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',481).
actions([ascend1/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',483).
%;[diver,object,time]
%;Terminates(Descend1(diver),See(diver,object),time).
%;[diver,object,time]
%;Terminates(Ascend1(diver),See(diver,object),time).
%;[diver,object,time]
%;Terminates(RotateYaw(diver),See(diver,object),time).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',492).
% event RapidAscendToSurface(diver)
% From E: 
% 
% event(rapidAscendToSurface(diver)).
events([rapidAscendToSurface/1]).
mpred_prop(rapidAscendToSurface(diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',492).
actions([rapidAscendToSurface/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',494).
% [diver,time]
% Happens(Descend1(diver),time) <->
% ({depth} Happens(Descend(diver,depth),time)).
% From E: 
% 
% <->(
%    happens(
%       descend1(Diver), 
%       Time), 
%    thereExists(Depth, 
%       happens(
%          descend(Diver,Depth), 
%          Time))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',494).
if(not(thereExists(Depth, descend(Diver, Depth))), not(descend1(Diver))),
if(not(descend1(Diver)), not(thereExists(Depth, descend(Diver, Depth)))).


% [diver,time]
% Happens(Ascend1(diver),time) <->
% ({depth} Happens(Ascend(diver,depth),time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',499).
% From E: 
% 
% <->(
%    happens(
%       ascend1(Diver), 
%       Time), 
%    thereExists(Depth, 
%       happens(
%          ascend(Diver,Depth), 
%          Time))).
 %   [Time].
if(not(thereExists(Depth, ascend(Diver, Depth))), not(ascend1(Diver))),
if(not(ascend1(Diver)), not(thereExists(Depth, ascend(Diver, Depth)))).


% [diver,time]
% Happens(RapidAscendToSurface(diver),time) ->
% Happens(Ascend(diver,0),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',503).
% From E: 
% 
% '->'(
%    happens(
%       rapidAscendToSurface(Diver), 
%       Time), 
%    happens(
%       ascend(Diver,0), 
%       Time)).
 %   [Time].
if(not(ascend(Diver,0)),not(rapidAscendToSurface(Diver))).

% event AscendLine(diver,line)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',505).
% From E: 
% 
% event(ascendLine(diver,line)).
events([ascendLine/2]).
mpred_prop(ascendLine(diver, line), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',505).
actions([ascendLine/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',508).
% [diver,line,time]
% Happens(AscendLine(diver,line),time) ->
% Happens(Ascend1(diver),time).
% From E: 
% 
% '->'(
%    happens(
%       ascendLine(Diver,Line), 
%       Time), 
%    happens(
%       ascend1(Diver), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',508).
if(not(ascend1(Diver)),not(ascendLine(Diver,Line))).

% fluent Disoriented(diver)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',511).
% From E: 
% 
% fluent(disoriented(diver)).
mpred_prop(disoriented(diver), fluent).
fluents([disoriented/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',514).
% event BecomeDisoriented(diver)
% From E: 
% 
% event(becomeDisoriented(diver)).
events([becomeDisoriented/1]).
mpred_prop(becomeDisoriented(diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',514).
actions([becomeDisoriented/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',516).
% event BecomeReoriented(diver)
% From E: 
% 
% event(becomeReoriented(diver)).
events([becomeReoriented/1]).
mpred_prop(becomeReoriented(diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',516).
actions([becomeReoriented/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',518).
% [diver,time]
% Initiates(BecomeDisoriented(diver),Disoriented(diver),time).
% From E: 
% 
% initiates_at(
%    becomeDisoriented(Diver), 
%    disoriented(Diver), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',518).
initiates(becomeDisoriented(Diver),disoriented(Diver)).


% [diver,time]
% Terminates(BecomeReoriented(diver),Disoriented(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',522).
% From E: 
% 
% terminates_at(
%    becomeReoriented(Diver), 
%    disoriented(Diver), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',522).
terminates(becomeReoriented(Diver),disoriented(Diver)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',524).
% fluent DisturbedSilt()
% From E: 
% 
% fluent(disturbedSilt()).
mpred_prop(disturbedSilt(), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',524).
fluents([disturbedSilt/0]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',526).
% event DisturbSilt(diver)
% From E: 
% 
% event(disturbSilt(diver)).
events([disturbSilt/1]).
mpred_prop(disturbSilt(diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',526).
actions([disturbSilt/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',528).
% [diver,time]
% Initiates(DisturbSilt(diver),DisturbedSilt(),time).
% From E: 
% 
% initiates_at(
%    disturbSilt(Diver), 
%    disturbedSilt(), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',528).
initiates(disturbSilt(Diver),disturbedSilt()).


% [diver,time]
% Happens(BecomeDisoriented(diver),time) ->
% (!HoldsAt(DisturbedSilt(),time-1) &
%  HoldsAt(DisturbedSilt(),time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',532).
% From E: 
% 
% '->'(
%    happens(
%       becomeDisoriented(Diver), 
%       Time), 
%    ','(
%       holds(
%          not(disturbedSilt()), 
%          Time-1), 
%       holds(disturbedSilt(),Time))).
 %   [Time, Time-1].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',532).
 if((holds(disturbedSilt(), Time-1);not(holds(disturbedSilt(), Time))),
      not(holds(becomeDisoriented(Diver), Time))).

% event Panic(diver)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',535).
% From E: 
% 
% event(panic(diver)).
events([panic/1]).
mpred_prop(panic(diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',535).
actions([panic/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',538).
% [diver,time]
 % Happens(Panic(diver),time) ->
% HoldsAt(Disoriented(diver),time) |
% HoldsAt(UncontrolledBuoyancy(diver),time) |
% ({equipment} Happens(Lose(diver,equipment),time-1)) |
% Happens(Vomit(diver),time-1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',538).
% From E: 
% 
% '->'(
%    happens(
%       panic(Diver), 
%       Time), 
%    ';'(
%       holds(
%          disoriented(Diver), 
%          Time), 
%       ';'(
%          holds(
%             uncontrolledBuoyancy(Diver), 
%             Time), 
%          ';'(
%             thereExists(Equipment, 
%                happens(
%                   lose(Diver,Equipment), 
%                   Time-1)), 
%             happens(
%                vomit(Diver), 
%                Time-1))))).
 %   [Time, Time-1].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',538).
if((not(holds(disoriented(Diver), Time)), not(holds(uncontrolledBuoyancy(Diver), Time)), not(thereExists(Equipment, at(lose(Diver, Equipment), Time-1))), not(holds(vomit(Diver), Time-1))), not(holds(panic(Diver), Time))).

% event Vomit(diver)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',543).
% From E: 
% 
% event(vomit(diver)).
events([vomit/1]).
mpred_prop(vomit(diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',543).
actions([vomit/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',546).
%; conditions

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',548).
% fluent Unconscious(diver)
% From E: 
% 
% fluent(unconscious(diver)).
mpred_prop(unconscious(diver), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',548).
fluents([unconscious/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',550).
% event GoUnconscious(diver)
% From E: 
% 
% event(goUnconscious(diver)).
events([goUnconscious/1]).
mpred_prop(goUnconscious(diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',550).
actions([goUnconscious/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',552).
% event RegainConsciousness(diver)
% From E: 
% 
% event(regainConsciousness(diver)).
events([regainConsciousness/1]).
mpred_prop(regainConsciousness(diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',552).
actions([regainConsciousness/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',554).
% [diver,time]
% Initiates(GoUnconscious(diver),Unconscious(diver),time).
% From E: 
% 
% initiates_at(
%    goUnconscious(Diver), 
%    unconscious(Diver), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',554).
initiates(goUnconscious(Diver),unconscious(Diver)).


% [diver,time]
% Terminates(RegainConsciousness(diver),Unconscious(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',558).
% From E: 
% 
% terminates_at(
%    regainConsciousness(Diver), 
%    unconscious(Diver), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',558).
terminates(regainConsciousness(Diver),unconscious(Diver)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',560).
% [diver,time]
% Happens(GoUnconscious(diver),time) ->
% Happens(RapidAscendToSurface(diver),time).
% From E: 
% 
% '->'(
%    happens(
%       goUnconscious(Diver), 
%       Time), 
%    happens(
%       rapidAscendToSurface(Diver), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',560).
if(not(rapidAscendToSurface(Diver)),
   not(goUnconscious(Diver))).

% fluent HasEarPain(diver)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',563).
% From E: 
% 
% fluent(hasEarPain(diver)).
mpred_prop(hasEarPain(diver), fluent).
fluents([hasEarPain/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',566).
% event StartEarPain(diver)
% From E: 
% 
% event(startEarPain(diver)).
events([startEarPain/1]).
mpred_prop(startEarPain(diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',566).
actions([startEarPain/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',568).
% [diver,time]
 % Initiates(StartEarPain(diver),HasEarPain(diver),time).
% From E: 
% 
% initiates_at(
%    startEarPain(Diver), 
%    hasEarPain(Diver), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',568).
initiates(startEarPain(Diver),hasEarPain(Diver)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',570).
% fluent HasRupturedEardrum(diver)
% From E: 
% 
% fluent(hasRupturedEardrum(diver)).
mpred_prop(hasRupturedEardrum(diver), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',570).
fluents([hasRupturedEardrum/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',572).
% event RuptureEardrum(diver)
% From E: 
% 
% event(ruptureEardrum(diver)).
events([ruptureEardrum/1]).
mpred_prop(ruptureEardrum(diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',572).
actions([ruptureEardrum/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',574).
% [diver,time]
% Initiates(RuptureEardrum(diver),HasRupturedEardrum(diver),time).
% From E: 
% 
% initiates_at(
%    ruptureEardrum(Diver), 
%    hasRupturedEardrum(Diver), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',574).
initiates(ruptureEardrum(Diver),hasRupturedEardrum(Diver)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',576).
% fluent ConditionOK(diver)
% From E: 
% 
% fluent(conditionOK(diver)).
mpred_prop(conditionOK(diver), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',576).
fluents([conditionOK/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',578).
% fluent HasDecompressionIllness(diver)
% From E: 
% 
% fluent(hasDecompressionIllness(diver)).
mpred_prop(hasDecompressionIllness(diver), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',578).
fluents([hasDecompressionIllness/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',580).
% event StartDecompressionIllness(diver)
% From E: 
% 
% event(startDecompressionIllness(diver)).
events([startDecompressionIllness/1]).
mpred_prop(startDecompressionIllness(diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',580).
actions([startDecompressionIllness/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',582).
% [diver,time]
% Initiates(StartDecompressionIllness(diver),
%           HasDecompressionIllness(diver),
%           time).
% From E: 
% 
% initiates_at(
%    startDecompressionIllness(Diver), 
%    hasDecompressionIllness(Diver), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',582).
initiates(startDecompressionIllness(Diver),
	  hasDecompressionIllness(Diver)).

% fluent SignalingDecompress(computer,diver)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',586).
% From E: 
% 
% fluent(signalingDecompress(computer,diver)).
mpred_prop(signalingDecompress(computer, diver), fluent).
fluents([signalingDecompress/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',589).
% fluent SignalingLowOnAir(computer,airtank,diver)
% From E: 
% 
% fluent(signalingLowOnAir(computer,airtank,diver)).
mpred_prop(signalingLowOnAir(computer, airtank, diver), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',589).
fluents([signalingLowOnAir/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',591).
% [computer,airtank,diver,time]
% HoldsAt(SignalingLowOnAir(computer,airtank,diver),time) ->
% HoldsAt(LowOnAir(airtank),time).
% From E: 
% 
% '->'(
%    holds(
%       signalingLowOnAir(Computer,Airtank,Diver), 
%       Time), 
%    holds(
%       lowOnAir(Airtank), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',591).
if(not(lowOnAir(Airtank)),
   not(signalingLowOnAir(Computer,Airtank,Diver))).


% [computer,diver,time]
% HoldsAt(SignalingDecompress(computer,diver),time) ->
% !{time1} time1<time & Happens(Decompress(diver),time1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',596).
% From E: 
% 
% '->'(
%    holds(
%       signalingDecompress(Computer,Diver), 
%       Time), 
%    not(thereExists(Time1, 
%           ','(
%              Time1<Time, 
%              happens(
%                 decompress(Diver), 
%                 Time1))))).
 %   [Time, Time1].
if(thereExists(Time1,  (comparison(Time1, Time, <), loc_at(decompress(Diver), Time1))), not(holds(signalingDecompress(Computer, Diver), Time))).

% event Decompress(diver)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',598).
% From E: 
% 
% event(decompress(diver)).
events([decompress/1]).
mpred_prop(decompress(diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',598).
actions([decompress/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',601).
% event EqualizeEars(diver)
% From E: 
% 
% event(equalizeEars(diver)).
events([equalizeEars/1]).
mpred_prop(equalizeEars(diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',601).
actions([equalizeEars/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',603).
% [diver,time]
% (Happens(Descend1(diver),time) | Happens(Ascend1(diver),time)) &
% !Happens(EqualizeEars(diver),time) ->
% Happens(StartEarPain(diver),time) &
% Happens(RuptureEardrum(diver),time).
% From E: 
% 
% '->'(
%    ','(
%       ';'(
%          happens(
%             descend1(Diver), 
%             Time), 
%          happens(
%             ascend1(Diver), 
%             Time)), 
%       not(happens(
%              equalizeEars(Diver), 
%              Time))), 
%    ','(
%       happens(
%          startEarPain(Diver), 
%          Time), 
%       happens(
%          ruptureEardrum(Diver), 
%          Time))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',603).
if((not(startEarPain(Diver));not(ruptureEardrum(Diver))),  (not(descend1(Diver)), not(ascend1(Diver));equalizeEars(Diver))).


% [diver,time]
% Happens(Ascend1(diver),time) &
% !Happens(Decompress(diver),time) ->
% Happens(StartDecompressionIllness(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',610).
% From E: 
% 
% '->'(
%    ','(
%       happens(
%          ascend1(Diver), 
%          Time), 
%       not(happens(
%              decompress(Diver), 
%              Time))), 
%    happens(
%       startDecompressionIllness(Diver), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',610).
 if(not(startDecompressionIllness(Diver)),
       (not(ascend1(Diver));decompress(Diver))).


% [diver1,diver2,time]
% HoldsAt(Holding(diver1,diver2),time) &
% Happens(Ascend1(diver1),time) &
% !Happens(Decompress(diver2),time) ->
% Happens(StartDecompressionIllness(diver2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',615).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          holding(Diver1,Diver2), 
%          Time), 
%       ','(
%          happens(
%             ascend1(Diver1), 
%             Time), 
%          not(happens(
%                 decompress(Diver2), 
%                 Time)))), 
%    happens(
%       startDecompressionIllness(Diver2), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',615).
 if(not(startDecompressionIllness(Diver2)),
       (not(holding(Diver1, Diver2));not(ascend1(Diver1));decompress(Diver2))).


% [diver,time]
% Happens(Decompress(diver),time) ->
% ({depth} depth>0 & HoldsAt(AtDepth(diver,depth),time)) &
% !HoldsAt(UncontrolledBuoyancy(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',621).
% From E: 
% 
% '->'(
%    happens(
%       decompress(Diver), 
%       Time), 
%    ','(
%       thereExists(Depth, 
%          ','(
%             Depth>0, 
%             holds(
%                atDepth(Diver,Depth), 
%                Time))), 
%       holds(
%          not(uncontrolledBuoyancy(Diver)), 
%          Time))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',621).
if((not(thereExists(Depth,  (comparison(Depth, 0, >), atDepth(Diver, Depth))));uncontrolledBuoyancy(Diver)), not(decompress(Diver))).

% fluent HasHeadache(diver)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',624).
% From E: 
% 
% fluent(hasHeadache(diver)).
mpred_prop(hasHeadache(diver), fluent).
fluents([hasHeadache/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',627).
% [diver,time]
% HoldsAt(ConditionOK(diver),time) ->
% !HoldsAt(Unconscious(diver),time) &
% !HoldsAt(HasEarPain(diver),time) &
% !HoldsAt(HasRupturedEardrum(diver),time) &
% !HoldsAt(HasDecompressionIllness(diver),time) &
% !HoldsAt(HasHeadache(diver),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',627).
% From E: 
% 
% '->'(
%    holds(
%       conditionOK(Diver), 
%       Time), 
%    ','(
%       holds(
%          not(unconscious(Diver)), 
%          Time), 
%       ','(
%          holds(
%             not(hasEarPain(Diver)), 
%             Time), 
%          ','(
%             holds(
%                not(hasRupturedEardrum(Diver)), 
%                Time), 
%             ','(
%                holds(
%                   not(hasDecompressionIllness(Diver)), 
%                   Time), 
%                holds(
%                   not(hasHeadache(Diver)), 
%                   Time)))))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',627).
 if((unconscious(Diver);hasEarPain(Diver);hasRupturedEardrum(Diver);hasDecompressionIllness(Diver);hasHeadache(Diver)),
      not(conditionOK(Diver))).

% event BeAirlifted(diver)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',634).
% From E: 
% 
% event(beAirlifted(diver)).
events([beAirlifted/1]).
mpred_prop(beAirlifted(diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',634).
actions([beAirlifted/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',637).
% event TakeInWater(diver)
% From E: 
% 
% event(takeInWater(diver)).
events([takeInWater/1]).
mpred_prop(takeInWater(diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',637).
actions([takeInWater/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',639).
% fluent LowOnAir(airtank)
% From E: 
% 
% fluent(lowOnAir(airtank)).
mpred_prop(lowOnAir(airtank), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',639).
fluents([lowOnAir/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',641).
% event BecomeLowOnAir(airtank)
% From E: 
% 
% event(becomeLowOnAir(airtank)).
mpred_prop(becomeLowOnAir(airtank), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',641).
events([becomeLowOnAir/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',643).
% [airtank,time]
% Initiates(BecomeLowOnAir(airtank),LowOnAir(airtank),time).
% From E: 
% 
% initiates_at(
%    becomeLowOnAir(Airtank), 
%    lowOnAir(Airtank), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',643).
initiates(becomeLowOnAir(Airtank),lowOnAir(Airtank)).


%; initial state
% [diver]
 % HoldsAt(ConditionOK(diver),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',647).
% From E: 
% 
% holds(
%    conditionOK(Diver), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',647).
initially(conditionOK(Diver)).


% [diver]
 % HoldsAt(Vertical(diver),0).
% From E: 
% 
% holds(
%    vertical(Diver), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',647).
initially(vertical(Diver)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',649).
% !HoldsAt(DisturbedSilt(),0).
% From E: 
% 
% holds(
%    not(disturbedSilt()), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',649).
initially(not(disturbedSilt())).


% [diver]
 % !HoldsAt(UncontrolledBuoyancy(diver),0).
% From E: 
% 
% holds(
%    not(uncontrolledBuoyancy(Diver)), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',649).
initially(not(uncontrolledBuoyancy(Diver))).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',651).
% [diver]
 % !HoldsAt(Disoriented(diver),0).
% From E: 
% 
% holds(
%    not(disoriented(Diver)), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',651).
initially(not(disoriented(Diver))).


% [diver]
 % !HoldsAt(PositivelyBuoyant(diver),0) &
%         !HoldsAt(NeutrallyBuoyant(diver),0) &
%         !HoldsAt(NegativelyBuoyant(diver),0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',651).
% From E: 
% 
% ','(
%    holds(
%       not(positivelyBuoyant(Diver)), 0), 
%    ','(
%       holds(
%          not(neutrallyBuoyant(Diver)), 0), 
%       holds(
%          not(negativelyBuoyant(Diver)), 0))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',651).
initially(not(positivelyBuoyant(Diver))),
initially(not(neutrallyBuoyant(Diver))),
initially(not(negativelyBuoyant(Diver))).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',655).
% [diver,object]
 % !HoldsAt(Wearing(diver,object),0).
% From E: 
% 
% holds(
%    not(wearing(Diver,Object)), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',655).
initially(not(wearing(Diver,Object))).


% [diver,object]
 % !HoldsAt(Holding(diver,object),0).
% From E: 
% 
% holds(
%    not(holding(Diver,Object)), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',655).
initially(not(holding(Diver,Object))).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',657).
% [diver1,diver2]
 % !HoldsAt(Separated(diver1,diver2),0).
% From E: 
% 
% holds(
%    not(separated(Diver1,Diver2)), 0).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',657).
initially(not(separated(Diver1,Diver2))).


%;[agent,object] !HoldsAt(See(agent,object),0).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',660).
% fluent Separated(diver,diver)
% From E: 
% 
% fluent(separated(diver,diver)).
mpred_prop(separated(diver, diver), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',660).
fluents([separated/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',662).
% [diver1,diver2,time]
% HoldsAt(Separated(diver1,diver2),time) ->
% HoldsAt(Separated(diver2,diver1),time).
% From E: 
% 
% '->'(
%    holds(
%       separated(Diver1,Diver2), 
%       Time), 
%    holds(
%       separated(Diver2,Diver1), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',662).
if(not(separated(Diver2,Diver1)),
   not(separated(Diver1,Diver2))).

% event BecomeSeparated(diver,diver)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',665).
% From E: 
% 
% event(becomeSeparated(diver,diver)).
events([becomeSeparated/2]).
mpred_prop(becomeSeparated(diver, diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',665).
actions([becomeSeparated/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',668).
% event BeReunitedWith(diver,diver)
% From E: 
% 
% event(beReunitedWith(diver,diver)).
events([beReunitedWith/2]).
mpred_prop(beReunitedWith(diver, diver), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',668).
actions([beReunitedWith/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',670).
% [diver1,diver2,time]
% Initiates(BecomeSeparated(diver1,diver2),Separated(diver1,diver2),time).
% From E: 
% 
% initiates_at(
%    becomeSeparated(Diver1,Diver2), 
%    separated(Diver1,Diver2), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',670).
initiates(becomeSeparated(Diver1,Diver2),
	  separated(Diver1,Diver2)).


% [diver1,diver2,time]
% Initiates(BecomeSeparated(diver1,diver2),Separated(diver2,diver1),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',674).
% From E: 
% 
% initiates_at(
%    becomeSeparated(Diver1,Diver2), 
%    separated(Diver2,Diver1), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',674).
initiates(becomeSeparated(Diver1,Diver2),
	  separated(Diver2,Diver1)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',676).
% [diver1,diver2,time]
% Terminates(BeReunitedWith(diver1,diver2),Separated(diver1,diver2),time).
% From E: 
% 
% terminates_at(
%    beReunitedWith(Diver1,Diver2), 
%    separated(Diver1,Diver2), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',676).
terminates(beReunitedWith(Diver1,Diver2),
	   separated(Diver1,Diver2)).


% [diver1,diver2,time]
% Terminates(BeReunitedWith(diver1,diver2),Separated(diver2,diver1),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',680).
% From E: 
% 
% terminates_at(
%    beReunitedWith(Diver1,Diver2), 
%    separated(Diver2,Diver1), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',680).
terminates(beReunitedWith(Diver1,Diver2),
	   separated(Diver2,Diver1)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',682).
%; End of file.
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.lps.pl')).
