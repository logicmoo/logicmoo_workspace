% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Ontology.e',105).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.lps.pl')).
% Sun, 21 Mar 2021 23:28:12 GMT File: <stream>(0x555567803900)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; OTSpace: object-scale topological space
%;
%; The OTSpace representation deals with topological space at
%; the scale of objects such as agents (humans and animals)
%; and physical objects.
%;
%; PartOf
%; physobj is a part of object.

% predicate PartOf(physobj,object)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',20).
% From E: 
% 
% predicate(partOf(physobj,object)).
mpred_prop(partOf(physobj, object), predicate).
predicates([partOf/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',23).
%; A state constraint says that if a physical object
%; is part of an object, the location of the
%; physical object is the same as the location of the object:
% [physobj,object,location,time]
% PartOf(physobj,object) &
% HoldsAt(At(object,location),time) ->
% HoldsAt(At(physobj,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',26).
% From E: 
% 
% '->'(
%    ','(
%       partOf(Physobj,Object), 
%       holds(
%          at_loc(Object,Location), 
%          Time)), 
%    holds(
%       at_loc(Physobj,Location), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',26).
 if(not(at_loc(Physobj, Location)),
       (not(partOf(Physobj, Object));not(at_loc(Object, Location)))).


%; rolling a snowball bigger
%; agent rolls stuff1 along stuff2.

% event RollAlong(agent,stuff,stuff)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',33).
% From E: 
% 
% event(rollAlong(agent,stuff,stuff)).
events([rollAlong/3]).
mpred_prop(rollAlong(agent, stuff, stuff), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',33).
actions([rollAlong/3]).


%; The diameter of ball is diameter.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',36).
% fluent Diameter(ball,diameter)
% From E: 
% 
% fluent(diameter(ball,diameter)).
mpred_prop(diameter(ball, diameter), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',36).
fluents([diameter/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',38).
%; A state constraint says that a ball has a unique diameter:
% [ball,diameter1,diameter2,time]
% HoldsAt(Diameter(ball,diameter1),time) &
% HoldsAt(Diameter(ball,diameter2),time) ->
% diameter1=diameter2.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',40).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          diameter(Ball,Diameter1), 
%          Time), 
%       holds(
%          diameter(Ball,Diameter2), 
%          Time)), 
%    Diameter1=Diameter2).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',40).
 if(not(equals(Diameter1, Diameter2)),
       (not(diameter(Ball, Diameter1));not(diameter(Ball, Diameter2)))).


%; Effect axiom state that if an agent rolls some snow along
%; some other snow, the diameter of the first snow will increase:
% [agent,snow1,snow2,diameter1,diameter2,time]
% HoldsAt(Diameter(snow1,diameter1),time) &
% diameter2 = diameter1+1 ->
% Initiates(RollAlong(agent,snow1,snow2),
%           Diameter(snow1,diameter2),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',46).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          diameter(Snow1,Diameter1), 
%          Time), 
%       Diameter2=Diameter1+1), 
%    initiates_at(
%       rollAlong(Agent,Snow1,Snow2), 
%       diameter(Snow1,Diameter2), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',46).
 if(not(initiates(rollAlong(Agent, Snow1, Snow2),
                    at(diameter(Snow1, Diameter2), Time))),
       (not(holds(diameter(Snow1, Diameter1), Time));not(equals(Diameter2, Diameter1+1)))).


% [agent,snow1,snow2,diameter1,time]
% HoldsAt(Diameter(snow1,diameter1),time) ->
% Terminates(RollAlong(agent,snow1,snow2),
%            Diameter(snow1,diameter1),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',54).
% From E: 
% 
% '->'(
%    holds(
%       diameter(Snow1,Diameter1), 
%       Time), 
%    terminates_at(
%       rollAlong(Agent,Snow1,Snow2), 
%       diameter(Snow1,Diameter1), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',54).
if(not(terminates(rollAlong(Agent,Snow1,Snow2),
		  at(diameter(Snow1,Diameter1),Time))),
   not(holds(diameter(Snow1,Diameter1),Time))).


%; A precondition axiom states that
%; for an agent to roll some snow along some other snow,
%; there must be a location such that
%; the agent is at the location,
%; the first snow is at the location, and
%; the second snow is at the location:
%;[agent,snow1,snow2,time]
%;Happens(RollAlong(agent,snow1,snow2),time) ->
%;{location}
%;HoldsAt(At(agent,location),time) &
%;HoldsAt(At(snow1,location),time) &
%;HoldsAt(At(snow2,location),time).
%; motion
%; object moves (in place).

% event Move(object)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',74).
% From E: 
% 
% event(move(object)).
mpred_prop(move(object), event).
events([move/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',77).
%; Holding
%; agent is holding physobj.

% fluent Holding(agent,physobj)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',79).
% From E: 
% 
% fluent(holding(agent,physobj)).
mpred_prop(holding(agent, physobj), fluent).
fluents([holding/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',79).
%; agent holds or picks up physobj.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',82).
% event Hold(agent,physobj)
% From E: 
% 
% event(hold(agent,physobj)).
events([hold/2]).
mpred_prop(hold(agent, physobj), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',82).
actions([hold/2]).


%; agent picks up some stuff1 from stuff2.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',84).
% event HoldSome(agent,stuff,stuff)
% From E: 
% 
% event(holdSome(agent,stuff,stuff)).
events([holdSome/3]).
mpred_prop(holdSome(agent, stuff, stuff), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',84).
actions([holdSome/3]).


%; agent releases or lets go of physobj.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',86).
% event LetGoOf(agent,physobj)
% From E: 
% 
% event(letGoOf(agent,physobj)).
events([letGoOf/2]).
mpred_prop(letGoOf(agent, physobj), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',86).
actions([letGoOf/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',88).
%; An effect axiom states that if an agent holds
%; a physical object, the agent will be holding the
%; physical object:
% [agent,physobj,time]
% Initiates(Hold(agent,physobj),Holding(agent,physobj),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',91).
% From E: 
% 
% initiates_at(
%    hold(Agent,Physobj), 
%    holding(Agent,Physobj), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',91).
initiates(hold(Agent,Physobj),
	  holding(Agent,Physobj)).


%; A precondition axiom states that
%; for an agent to hold a physical object,
%; there must be a location such that
%; the agent is at the location and
%; the physical object is at the location:
%;[agent,physobj,time]
%;Happens(Hold(agent,physobj),time) ->
%;{location}
%;  HoldsAt(At(agent,location),time) &
%;  HoldsAt(At(physobj,location),time).
%; An effect axiom states that if an agent
%; lets go of a physical object, the agent is no longer holding
%; the physical object:
% [agent,physobj,time]
% Terminates(LetGoOf(agent,physobj),Holding(agent,physobj),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',108).
% From E: 
% 
% terminates_at(
%    letGoOf(Agent,Physobj), 
%    holding(Agent,Physobj), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',108).
terminates(letGoOf(Agent,Physobj),
	   holding(Agent,Physobj)).


%; A precondition axiom states that
%; for an agent to let go of a physical object,
%; the agent must be holding the physical object:
% [agent,physobj,time]
% Happens(LetGoOf(agent,physobj),time) ->
% HoldsAt(Holding(agent,physobj),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',114).
% From E: 
% 
% '->'(
%    happens(
%       letGoOf(Agent,Physobj), 
%       Time), 
%    holds(
%       holding(Agent,Physobj), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',114).
if(not(holding(Agent,Physobj)),
   not(letGoOf(Agent,Physobj))).


%; A releases axiom states that if an agent holds
%; a physical object,
%; the physical object's location will be released
%; from inertia:
% [agent,physobj,location,time]
% Releases(Hold(agent,physobj),At(physobj,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',122).
% From E: 
% 
% releases_at(
%    hold(Agent,Physobj), 
%    at_loc(Physobj,Location), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',122).
releases(hold(Agent,Physobj),
	 at_loc(Physobj,Location)).


%; A state constraint says that if an agent is holding
%; a physical object and the agent is at a location,
%; the physical object is also at the location:
% [agent,physobj,location,time]
% HoldsAt(Holding(agent,physobj),time) &
% HoldsAt(At(agent,location),time) ->
% HoldsAt(At(physobj,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',128).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          holding(Agent,Physobj), 
%          Time), 
%       holds(
%          at_loc(Agent,Location), 
%          Time)), 
%    holds(
%       at_loc(Physobj,Location), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',128).
 if(not(at_loc(Physobj, Location)),
       (not(holding(Agent, Physobj));not(at_loc(Agent, Location)))).


%; A releases axiom states that if an agent holds
%; a physical object,
%; the locations of the parts of the physical object
%; will be released from inertia:
% [agent,physobj1,physobj2,location,time]
% PartOf(physobj1,physobj2) ->
% Releases(Hold(agent,physobj2),At(physobj1,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',137).
% From E: 
% 
% '->'(
%    partOf(Physobj1,Physobj2), 
%    releases_at(
%       hold(Agent,Physobj2), 
%       at_loc(Physobj1,Location), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',137).
if(not(terminates(hold(Agent,Physobj2),
		  at(at_loc(Physobj1,Location),Time))),
   not(partOf(Physobj1,Physobj2))).


%; Further, if an agent holds a physical object,
%; the locations of the physical objects of which
%; the physical object is a part
%; will be released from inertia:
% [agent,physobj1,physobj2,location,time]
% PartOf(physobj1,physobj2) ->
% Releases(Hold(agent,physobj1),At(physobj2,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',145).
% From E: 
% 
% '->'(
%    partOf(Physobj1,Physobj2), 
%    releases_at(
%       hold(Agent,Physobj1), 
%       at_loc(Physobj2,Location), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',145).
if(not(terminates(hold(Agent,Physobj1),
		  at(at_loc(Physobj2,Location),Time))),
   not(partOf(Physobj1,Physobj2))).


%;[agent,physobj,location1,location2,time]
%;(!{object} PartOf(physobj,object)) &
%;HoldsAt(At(agent,location1),time) &
%;location1 != location2 ->
%;Terminates(LetGoOf(agent,physobj),At(physobj,location2),time).
% [agent,physobj,location,time]
% (!{object} PartOf(physobj,object)) &
% HoldsAt(At(agent,location),time) ->
% Initiates(LetGoOf(agent,physobj),At(physobj,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',155).
% From E: 
% 
% '->'(
%    ','(
%       not(thereExists(Object, 
%              partOf(Physobj,Object))), 
%       holds(
%          at_loc(Agent,Location), 
%          Time)), 
%    initiates_at(
%       letGoOf(Agent,Physobj), 
%       at_loc(Physobj,Location), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',155).
 if(not(initiates(letGoOf(Agent, Physobj),
                    at(at_loc(Physobj, Location), Time))),
       (thereExists(Object, partOf(Physobj, Object));not(holds(at_loc(Agent, Location), Time)))).


%;[agent,physobj1,physobj2,location1,location2,time]
%;PartOf(physobj1,physobj2) &
%;(!{object} PartOf(physobj2,object)) &
%;HoldsAt(At(agent,location1),time) &
%;location1 != location2 ->
%;Terminates(LetGoOf(agent,physobj1),At(physobj2,location2),time).
% [agent,physobj1,physobj2,location,time]
% PartOf(physobj1,physobj2) &
% (!{object} PartOf(physobj2,object)) &
% HoldsAt(At(agent,location),time) ->
% Initiates(LetGoOf(agent,physobj1),At(physobj2,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',167).
% From E: 
% 
% '->'(
%    ','(
%       partOf(Physobj1,Physobj2), 
%       ','(
%          not(thereExists(Object, 
%                 partOf(Physobj2,Object))), 
%          holds(
%             at_loc(Agent,Location), 
%             Time))), 
%    initiates_at(
%       letGoOf(Agent,Physobj1), 
%       at_loc(Physobj2,Location), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',167).
 if(not(initiates(letGoOf(Agent, Physobj1),
                    at(at_loc(Physobj2, Location), Time))),
       (not(partOf(Physobj1, Physobj2));thereExists(Object, partOf(Physobj2, Object));not(holds(at_loc(Agent, Location), Time)))).


%; An effect axiom states that if an agent is at a location
%; and lets go of a physical object, the physical object
%; will be at the location:
% [agent,physobj,location,time]
% HoldsAt(At(agent,location),time) ->
% Initiates(LetGoOf(agent,physobj),At(physobj,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',176).
% From E: 
% 
% '->'(
%    holds(
%       at_loc(Agent,Location), 
%       Time), 
%    initiates_at(
%       letGoOf(Agent,Physobj), 
%       at_loc(Physobj,Location), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',176).
if(not(initiates(letGoOf(Agent,Physobj),
		 at(at_loc(Physobj,Location),Time))),
   not(holds(at_loc(Agent,Location),Time))).


%; An effect axiom states that if an agent picks up
%; some stuff out of some other stuff, the agent will
%; be holding the first stuff:
% [agent,stuff1,stuff2,time]
% Initiates(HoldSome(agent,stuff1,stuff2),
%           Holding(agent,stuff1),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',183).
% From E: 
% 
% initiates_at(
%    holdSome(Agent,Stuff1,Stuff2), 
%    holding(Agent,Stuff1), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',183).
initiates(holdSome(Agent,Stuff1,Stuff2),
	  holding(Agent,Stuff1)).


%; A precondition axiom states that
%; for an agent to pick up some stuff out of some other stuff,
%; the first stuff must be a part of the second stuff and
%; there must be a location such that the agent is at the location,
%; the first stuff is at the location, and the second stuff is
%; at the location:
% [agent,stuff1,stuff2,time]
% Happens(HoldSome(agent,stuff1,stuff2),time) ->
% PartOf(stuff1,stuff2) &
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',194).
% {location}% 
%   HoldsAt(At(agent,location),time) &
%   HoldsAt(At(stuff1,location),time) &
%   HoldsAt(At(stuff2,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',197).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          holdSome(Agent,Stuff1,Stuff2), 
%          Time), 
%       ','(
%          partOf(Stuff1,Stuff2), 
%          ','(
%             holds(
%                at_loc(Agent,Location), 
%                Time), 
%             ','(
%                holds(
%                   at_loc(Stuff1,Location), 
%                   Time), 
%                holds(
%                   at_loc(Stuff2,Location), 
%                   Time)))))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',197).
exists(Location,  (partOf(Stuff1, Stuff2), at_loc(Agent, Location), at_loc(Stuff1, Location), at_loc(Stuff2, Location);not(holdSome(Agent, Stuff1, Stuff2)))).


%; A releases axiom states that if an agent picks up some
%; stuff out of some other stuff,
%; the first stuff's location will be released
%; from inertia:
% [agent,stuff1,stuff2,location,time]
% Releases(HoldSome(agent,stuff1,stuff2),At(stuff1,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',206).
% From E: 
% 
% releases_at(
%    holdSome(Agent,Stuff1,Stuff2), 
%    at_loc(Stuff1,Location), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',206).
releases(holdSome(Agent,Stuff1,Stuff2),
	 at_loc(Stuff1,Location)).


%; Inside
%; physobj1 is inside physobj2.

% fluent Inside(physobj,physobj)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',211).
% From E: 
% 
% fluent(inside(physobj,physobj)).
mpred_prop(inside(physobj, physobj), fluent).
fluents([inside/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',211).
%; agent puts physobj1 inside physobj2.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',214).
% event PutInside(agent,physobj,physobj)
% From E: 
% 
% event(putInside(agent,physobj,physobj)).
events([putInside/3]).
mpred_prop(putInside(agent, physobj, physobj), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',214).
actions([putInside/3]).


%; agent takes physobj1 out of physobj2.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',216).
% event TakeOutOf(agent,physobj,physobj)
% From E: 
% 
% event(takeOutOf(agent,physobj,physobj)).
events([takeOutOf/3]).
mpred_prop(takeOutOf(agent, physobj, physobj), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',216).
actions([takeOutOf/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',218).
%; A state constraint says that a physical object cannot
%; be inside itself:
% [physobj1,physobj2,time]
% HoldsAt(Inside(physobj1,physobj2),time) ->
% physobj1!=physobj2.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',220).
% From E: 
% 
% '->'(
%    holds(
%       inside(Physobj1,Physobj2), 
%       Time), 
%    Physobj1\=Physobj2).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',220).
if(not({dif(Physobj1,Physobj2)}),
   not(inside(Physobj1,Physobj2))).


%; A state constraint says that if a physical object is
%; inside another physical object, the second physical object
%; is not inside the first physical object:
% [physobj1,physobj2,time]
% HoldsAt(Inside(physobj1,physobj2),time) ->
% !HoldsAt(Inside(physobj2,physobj1),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',227).
% From E: 
% 
% '->'(
%    holds(
%       inside(Physobj1,Physobj2), 
%       Time), 
%    holds(
%       not(inside(Physobj2,Physobj1)), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',227).
if(inside(Physobj2,Physobj1),
   not(inside(Physobj1,Physobj2))).


%; An effect axiom states that if an agent puts a physical
%; object inside another physical object, the first
%; physical object will be inside the second physical object:
% [agent,physobj1,physobj2,time]
% Initiates(PutInside(agent,physobj1,physobj2),
%           Inside(physobj1,physobj2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',234).
% From E: 
% 
% initiates_at(
%    putInside(Agent,Physobj1,Physobj2), 
%    inside(Physobj1,Physobj2), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',234).
initiates(putInside(Agent,Physobj1,Physobj2),
	  inside(Physobj1,Physobj2)).


%; An effect axiom states that if an agent puts a physical
%; object inside another physical object, the agent will
%; no longer be holding the first physical object:
% [agent,physobj1,physobj2,time]
% Terminates(PutInside(agent,physobj1,physobj2),
%            Holding(agent,physobj1),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',241).
% From E: 
% 
% terminates_at(
%    putInside(Agent,Physobj1,Physobj2), 
%    holding(Agent,Physobj1), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',241).
terminates(putInside(Agent,Physobj1,Physobj2),
	   holding(Agent,Physobj1)).


%; A precondition axiom states that
%; for an agent to put a physical object inside another
%; physical object,
%; the agent must be holding the first physical object
%; and there must be a location such that
%; the agent is at the location and
%; the second physical object is at the location:
%;[agent,physobj1,physobj2,time]
%;Happens(PutInside(agent,physobj1,physobj2),time) ->
%;HoldsAt(Holding(agent,physobj1),time) &
%;{location}
%; HoldsAt(At(agent,location),time) &
%; HoldsAt(At(physobj2,location),time).
%; An effect axiom states that
%; if an agent takes a physical object out of another
%; physical object, the first physical object
%; will no longer be inside the second physical object:
% [agent,physobj1,physobj2,time]
% Terminates(TakeOutOf(agent,physobj1,physobj2),
%            Inside(physobj1,physobj2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',263).
% From E: 
% 
% terminates_at(
%    takeOutOf(Agent,Physobj1,Physobj2), 
%    inside(Physobj1,Physobj2), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',263).
terminates(takeOutOf(Agent,Physobj1,Physobj2),
	   inside(Physobj1,Physobj2)).


%; A precondition axiom states that
%; for an agent to take a physical object out of another
%; physical object,
%; the first physical object must be inside the second physical object
%; and there must be a location such that
%; the agent is at the location,
%; the first physical object is at the location, and
%; the second physical object is at the location:
% [agent,physobj1,physobj2,time]
% Happens(TakeOutOf(agent,physobj1,physobj2),time) ->
% HoldsAt(Inside(physobj1,physobj2),time) &
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',275).
% {location}% 
%  HoldsAt(At(agent,location),time) &
%  HoldsAt(At(physobj1,location),time) &
%  HoldsAt(At(physobj2,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',278).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          takeOutOf(Agent,Physobj1,Physobj2), 
%          Time), 
%       ','(
%          holds(
%             inside(Physobj1,Physobj2), 
%             Time), 
%          ','(
%             holds(
%                at_loc(Agent,Location), 
%                Time), 
%             ','(
%                holds(
%                   at_loc(Physobj1,Location), 
%                   Time), 
%                holds(
%                   at_loc(Physobj2,Location), 
%                   Time)))))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',278).
exists(Location,  (inside(Physobj1, Physobj2), at_loc(Agent, Location), at_loc(Physobj1, Location), at_loc(Physobj2, Location);not(takeOutOf(Agent, Physobj1, Physobj2)))).


%; A releases axiom states that if an agent puts a physical
%; object inside another physical object,
%; the first physical object's location will be released
%; from inertia:
% [agent,physobj1,physobj2,location,time]
% Releases(PutInside(agent,physobj1,physobj2),
%          At(physobj1,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',287).
% From E: 
% 
% releases_at(
%    putInside(Agent,Physobj1,Physobj2), 
%    at_loc(Physobj1,Location), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',287).
releases(putInside(Agent,Physobj1,Physobj2),
	 at_loc(Physobj1,Location)).


%; A state constraint says that if a physical object is inside
%; another physical object and the second physical object is
%; at a location, the first physical object is also at the location:
% [physobj1,physobj2,location,time]
% HoldsAt(Inside(physobj1,physobj2),time) &
% HoldsAt(At(physobj2,location),time) ->
% HoldsAt(At(physobj1,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',294).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          inside(Physobj1,Physobj2), 
%          Time), 
%       holds(
%          at_loc(Physobj2,Location), 
%          Time)), 
%    holds(
%       at_loc(Physobj1,Location), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',294).
 if(not(at_loc(Physobj1, Location)),
       (not(inside(Physobj1, Physobj2));not(at_loc(Physobj2, Location)))).


%; An effect axiom states that if an agent takes a physical
%; object out of another physical object,
%; the agent will be holding the first physical object:
% [agent,physobj1,physobj2,time]
% Initiates(TakeOutOf(agent,physobj1,physobj2),
%           Holding(agent,physobj1),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',302).
% From E: 
% 
% initiates_at(
%    takeOutOf(Agent,Physobj1,Physobj2), 
%    holding(Agent,Physobj1), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',302).
initiates(takeOutOf(Agent,Physobj1,Physobj2),
	  holding(Agent,Physobj1)).


%; On
%; physobj1 is on physobj2.

% fluent On(physobj,physobj)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',309).
% From E: 
% 
% fluent(on(physobj,physobj)).
mpred_prop(on(physobj, physobj), fluent).
fluents([on/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',312).
%; agent places physobj1 on physobj2.

% event PlaceOn(agent,physobj,physobj)
% From E: 
% 
% event(placeOn(agent,physobj,physobj)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',312).
events([placeOn/3]).
mpred_prop(placeOn(agent, physobj, physobj), action).
actions([placeOn/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',312).
%; agent takes physobj1 off of physobj2.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',315).
% event TakeOffOf(agent,physobj,physobj)
% From E: 
% 
% event(takeOffOf(agent,physobj,physobj)).
events([takeOffOf/3]).
mpred_prop(takeOffOf(agent, physobj, physobj), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',315).
actions([takeOffOf/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',317).
%; A state constraint says that a physical object cannot
%; be on itself:
% [physobj1,physobj2,time]
% HoldsAt(On(physobj1,physobj2),time) ->
% physobj1!=physobj2.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',319).
% From E: 
% 
% '->'(
%    holds(
%       on(Physobj1,Physobj2), 
%       Time), 
%    Physobj1\=Physobj2).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',319).
if(not({dif(Physobj1,Physobj2)}),
   not(on(Physobj1,Physobj2))).


%; A state constraint says that if a physical object is
%; on another physical object, the second physical object
%; is not on the first physical object:
% [physobj1,physobj2,time]
% HoldsAt(On(physobj1,physobj2),time) ->
% !HoldsAt(On(physobj2,physobj1),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',326).
% From E: 
% 
% '->'(
%    holds(
%       on(Physobj1,Physobj2), 
%       Time), 
%    holds(
%       not(on(Physobj2,Physobj1)), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',326).
if(on(Physobj2,Physobj1),
   not(on(Physobj1,Physobj2))).


%; An effect axiom states that if an agent places a physical
%; object on another physical object, the first
%; physical object will be on the second physical object:
% [agent,physobj1,physobj2,time]
% Initiates(PlaceOn(agent,physobj1,physobj2),
%           On(physobj1,physobj2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',333).
% From E: 
% 
% initiates_at(
%    placeOn(Agent,Physobj1,Physobj2), 
%    on(Physobj1,Physobj2), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',333).
initiates(placeOn(Agent,Physobj1,Physobj2),
	  on(Physobj1,Physobj2)).


%; An effect axiom states that if an agent places a physical
%; object on another physical object, the agent will
%; no longer be holding the first physical object:
% [agent,physobj1,physobj2,time]
% Terminates(PlaceOn(agent,physobj1,physobj2),
%            Holding(agent,physobj1),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',340).
% From E: 
% 
% terminates_at(
%    placeOn(Agent,Physobj1,Physobj2), 
%    holding(Agent,Physobj1), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',340).
terminates(placeOn(Agent,Physobj1,Physobj2),
	   holding(Agent,Physobj1)).


%; A precondition axiom states that
%; for an agent to place a physical object on another
%; physical object,
%; the agent must be holding the first physical object
%; and there must be a location such that
%; the agent is at the location and
%; the second physical object is at the location:
%;[agent,physobj1,physobj2,time]
%;Happens(PlaceOn(agent,physobj1,physobj2),time) ->
%;HoldsAt(Holding(agent,physobj1),time) &
%;{location}
%; HoldsAt(At(agent,location),time) &
%; HoldsAt(At(physobj2,location),time).
%; An effect axiom states that
%; if an agent takes a physical object off of another
%; physical object, the first physical object
%; will no longer be on the second physical object:
% [agent,physobj1,physobj2,time]
% Terminates(TakeOffOf(agent,physobj1,physobj2),
%            On(physobj1,physobj2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',362).
% From E: 
% 
% terminates_at(
%    takeOffOf(Agent,Physobj1,Physobj2), 
%    on(Physobj1,Physobj2), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',362).
terminates(takeOffOf(Agent,Physobj1,Physobj2),
	   on(Physobj1,Physobj2)).


%; An effect axiom states that if an agent takes a physical
%; object off of another physical object,
%; the agent will be holding the first physical object:
% [agent,physobj1,physobj2,time]
% Initiates(TakeOffOf(agent,physobj1,physobj2),
%           Holding(agent,physobj1),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',369).
% From E: 
% 
% initiates_at(
%    takeOffOf(Agent,Physobj1,Physobj2), 
%    holding(Agent,Physobj1), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',369).
initiates(takeOffOf(Agent,Physobj1,Physobj2),
	  holding(Agent,Physobj1)).


%; A precondition axiom states that
%; for an agent to take a physical object off of another
%; physical object,
%; the first physical object must be on the second physical object
%; and there must be a location such that
%; the agent is at the location and
%; the first physical object is at the location:
%; the second physical object is at the location:
% [agent,physobj1,physobj2,time]
% Happens(TakeOffOf(agent,physobj1,physobj2),time) ->
% HoldsAt(On(physobj1,physobj2),time) &
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',381).
% {location}% 
%  HoldsAt(At(agent,location),time) &
%  HoldsAt(At(physobj1,location),time) &
%  HoldsAt(At(physobj2,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',384).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          takeOffOf(Agent,Physobj1,Physobj2), 
%          Time), 
%       ','(
%          holds(
%             on(Physobj1,Physobj2), 
%             Time), 
%          ','(
%             holds(
%                at_loc(Agent,Location), 
%                Time), 
%             ','(
%                holds(
%                   at_loc(Physobj1,Location), 
%                   Time), 
%                holds(
%                   at_loc(Physobj2,Location), 
%                   Time)))))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',384).
exists(Location,  (on(Physobj1, Physobj2), at_loc(Agent, Location), at_loc(Physobj1, Location), at_loc(Physobj2, Location);not(takeOffOf(Agent, Physobj1, Physobj2)))).


%; A releases axiom states that if an agent places a physical
%; object on another physical object,
%; the first physical object's location will be released
%; from inertia:
% [agent,physobj1,physobj2,location,time]
% Releases(PlaceOn(agent,physobj1,physobj2),
%          At(physobj1,location),
%          time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',393).
% From E: 
% 
% releases_at(
%    placeOn(Agent,Physobj1,Physobj2), 
%    at_loc(Physobj1,Location), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',393).
releases(placeOn(Agent,Physobj1,Physobj2),
	 at_loc(Physobj1,Location)).


%; A state constraint says that if a physical object is on
%; another physical object and the second physical object is
%; at a location, the first physical object is also at the location:
% [physobj1,physobj2,location,time]
% HoldsAt(On(physobj1,physobj2),time) &
% HoldsAt(At(physobj2,location),time) ->
% HoldsAt(At(physobj1,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',401).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          on(Physobj1,Physobj2), 
%          Time), 
%       holds(
%          at_loc(Physobj2,Location), 
%          Time)), 
%    holds(
%       at_loc(Physobj1,Location), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',401).
 if(not(at_loc(Physobj1, Location)),
       (not(on(Physobj1, Physobj2));not(at_loc(Physobj2, Location)))).

% fluent Near(agent,object)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',405).
% From E: 
% 
% fluent(near(agent,object)).
mpred_prop(near(agent, object), fluent).
fluents([near/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',407).
% event WalkFromTo(agent,object,object)
% From E: 
% 
% event(walkFromTo(agent,object,object)).
events([walkFromTo/3]).
mpred_prop(walkFromTo(agent, object, object), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',407).
actions([walkFromTo/3]).

% event WalkFrom(agent,object)
% From E: 
% 
% event(walkFrom(agent,object)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',407).
events([walkFrom/2]).
mpred_prop(walkFrom(agent, object), action).
actions([walkFrom/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',409).
% event RunFromTo(agent,object,object)
% From E: 
% 
% event(runFromTo(agent,object,object)).
events([runFromTo/3]).
mpred_prop(runFromTo(agent, object, object), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',409).
actions([runFromTo/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',411).
% [agent,object1,object2,time]
% Initiates(WalkFromTo(agent,object1,object2),
%           Near(agent,object2),
%           time).
% From E: 
% 
% initiates_at(
%    walkFromTo(Agent,Object1,Object2), 
%    near(Agent,Object2), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',411).
initiates(walkFromTo(Agent,Object1,Object2),
	  near(Agent,Object2)).


% [agent,object1,object2,time]
% Terminates(WalkFromTo(agent,object1,object2),
%            Near(agent,object1),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',417).
% From E: 
% 
% terminates_at(
%    walkFromTo(Agent,Object1,Object2), 
%    near(Agent,Object1), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',417).
terminates(walkFromTo(Agent,Object1,Object2),
	   near(Agent,Object1)).


% [agent,object1,object2,time]
% Happens(WalkFromTo(agent,object1,object2),time) ->
% {location}% 
% HoldsAt(At(agent,location),time) &
% HoldsAt(At(object1,location),time) &
% HoldsAt(At(object2,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',422).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          walkFromTo(Agent,Object1,Object2), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent,Location), 
%             Time), 
%          ','(
%             holds(
%                at_loc(Object1,Location), 
%                Time), 
%             holds(
%                at_loc(Object2,Location), 
%                Time))))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',422).
exists(Location,  (at_loc(Agent, Location), at_loc(Object1, Location), at_loc(Object2, Location);not(walkFromTo(Agent, Object1, Object2)))).


% [agent,object1,object2,time]
% Initiates(RunFromTo(agent,object1,object2),
%           Near(agent,object2),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',429).
% From E: 
% 
% initiates_at(
%    runFromTo(Agent,Object1,Object2), 
%    near(Agent,Object2), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',429).
initiates(runFromTo(Agent,Object1,Object2),
	  near(Agent,Object2)).


% [agent,object1,object2,time]
% Terminates(RunFromTo(agent,object1,object2),
%            Near(agent,object1),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',434).
% From E: 
% 
% terminates_at(
%    runFromTo(Agent,Object1,Object2), 
%    near(Agent,Object1), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',434).
terminates(runFromTo(Agent,Object1,Object2),
	   near(Agent,Object1)).


% [agent,object1,object2,time]
% Happens(RunFromTo(agent,object1,object2),time) ->
% {location}% 
% HoldsAt(At(agent,location),time) &
% HoldsAt(At(object1,location),time) &
% HoldsAt(At(object2,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',439).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          runFromTo(Agent,Object1,Object2), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent,Location), 
%             Time), 
%          ','(
%             holds(
%                at_loc(Object1,Location), 
%                Time), 
%             holds(
%                at_loc(Object2,Location), 
%                Time))))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',439).
exists(Location,  (at_loc(Agent, Location), at_loc(Object1, Location), at_loc(Object2, Location);not(runFromTo(Agent, Object1, Object2)))).


% [agent,object,time]
% Terminates(WalkFrom(agent,object),
%            Near(agent,object),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',446).
% From E: 
% 
% terminates_at(
%    walkFrom(Agent,Object), 
%    near(Agent,Object), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',446).
terminates(walkFrom(Agent,Object),
	   near(Agent,Object)).


% [agent,object,location,door,time]
% HoldsAt(Near(agent,object),time) &
% HoldsAt(At(agent,location),time) &
% HoldsAt(At(object,location),time) &
% Side1(door)=location &
% Happens(WalkThroughDoor12(agent,door),time) ->
% Happens(WalkFrom(agent,object),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',451).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          near(Agent,Object), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent,Location), 
%             Time), 
%          ','(
%             holds(
%                at_loc(Object,Location), 
%                Time), 
%             ','(
%                '='(
%                   side1(Door), 
%                   Location), 
%                happens(
%                   walkThroughDoor12(Agent,Door), 
%                   Time))))), 
%    happens(
%       walkFrom(Agent,Object), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',451).
 if(not(walkFrom(Agent, Object)),
       (not(near(Agent, Object));not(at_loc(Agent, Location));not(at_loc(Object, Location));not(side1(Door, Location));not(walkThroughDoor12(Agent, Door)))).


% [agent,object,location,door,time]
% HoldsAt(Near(agent,object),time) &
% HoldsAt(At(agent,location),time) &
% HoldsAt(At(object,location),time) &
% Side2(door)=location &
% Happens(WalkThroughDoor21(agent,door),time) ->
% Happens(WalkFrom(agent,object),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',459).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          near(Agent,Object), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent,Location), 
%             Time), 
%          ','(
%             holds(
%                at_loc(Object,Location), 
%                Time), 
%             ','(
%                '='(
%                   side2(Door), 
%                   Location), 
%                happens(
%                   walkThroughDoor21(Agent,Door), 
%                   Time))))), 
%    happens(
%       walkFrom(Agent,Object), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',459).
 if(not(walkFrom(Agent, Object)),
       (not(near(Agent, Object));not(at_loc(Agent, Location));not(at_loc(Object, Location));not(side2(Door, Location));not(walkThroughDoor21(Agent, Door)))).


% [agent,object,room,staircase,time]
% HoldsAt(Near(agent,object),time) &
% HoldsAt(At(agent,room),time) &
% HoldsAt(At(object,room),time) &
% Side1(staircase)=room &
% Happens(WalkUpStaircase(agent,staircase),time) ->
% Happens(WalkFrom(agent,object),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',467).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          near(Agent,Object), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent,Room), 
%             Time), 
%          ','(
%             holds(
%                at_loc(Object,Room), 
%                Time), 
%             ','(
%                '='(
%                   side1(Staircase), 
%                   Room), 
%                happens(
%                   walkUpStaircase(Agent,Staircase), 
%                   Time))))), 
%    happens(
%       walkFrom(Agent,Object), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',467).
 if(not(walkFrom(Agent, Object)),
       (not(near(Agent, Object));not(at_loc(Agent, Room));not(at_loc(Object, Room));not(side1(Staircase, Room));not(walkUpStaircase(Agent, Staircase)))).


% [agent,object,room,staircase,time]
% HoldsAt(Near(agent,object),time) &
% HoldsAt(At(agent,room),time) &
% HoldsAt(At(object,room),time) &
% Side2(staircase)=room &
% Happens(WalkDownStaircase(agent,staircase),time) ->
% Happens(WalkFrom(agent,object),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',475).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          near(Agent,Object), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent,Room), 
%             Time), 
%          ','(
%             holds(
%                at_loc(Object,Room), 
%                Time), 
%             ','(
%                '='(
%                   side2(Staircase), 
%                   Room), 
%                happens(
%                   walkDownStaircase(Agent,Staircase), 
%                   Time))))), 
%    happens(
%       walkFrom(Agent,Object), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',475).
 if(not(walkFrom(Agent, Object)),
       (not(near(Agent, Object));not(at_loc(Agent, Room));not(at_loc(Object, Room));not(side2(Staircase, Room));not(walkDownStaircase(Agent, Staircase)))).


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.e',481).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OTSpace.lps.pl')).
