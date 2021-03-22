% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ReadingALetter.e',117).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.lps.pl')).
% Sun, 21 Mar 2021 23:28:13 GMT File: <stream>(0x5555677c6400)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; @article{Mueller:InPress,
%;   author = "Erik T. Mueller",
%;   year = "in press",
%;   title = "Modelling space and time in narratives about restaurants",
%;   journal = "Literary and Linguistic Computing",
%; }
%;
%;sort boolean
%;sort integer
%;reified sort predicate
%;reified sort function
%;
%;sort time: integer
%;sort offset: integer
%;
%;reified sort fluent
%;reified sort event
%;
%;predicate Happens(event,time)
%;predicate HoldsAt(fluent,time)
%;predicate ReleasedAt(fluent,time)
%;predicate Initiates(event,fluent,time)
%;predicate Terminates(event,fluent,time)
%;predicate Releases(event,fluent,time)
%;
%;sort diameter: integer
%;
%;sort object
%;
%;sort agent: object
%;
%;sort physobj: object
%;sort bed: physobj
%;sort snowflake: physobj
%;sort sky: physobj
%;
%;sort stuff: physobj
%;
%;sort surface: physobj
%;sort ground: surface
%;
%;sort snow: stuff
%;sort ball
%;
%;sort food: physobj
%;sort fruit: food
%;sort orange: fruit
%;sort salad: food
%;
%;sort clothing: physobj
%;sort scarf: clothing
%;sort hat: clothing
%;
%;sort vegetablematter: physobj
%;sort coal: vegetablematter
%;
%;sort bodypart: physobj
%;sort hand: bodypart
%;
%;sort papertowels: physobj
%;sort device: physobj
%;sort electronicdevice: device
%;sort lamp: electronicdevice
%;
%;sort cat: physobj
%;
%;sort weapon: physobj
%;sort gun: weapon
%;sort bomb: weapon
%;sort bullet: weapon
%;
%;sort location
%;sort room: location, outside: location
%;
%;sort portal
%;sort door: portal, staircase: portal
%;sort street: portal
%;
%;sort building
%;
%;sort fire: object
%;
%;sort furniture: physobj
%;sort chair: furniture
%;sort table: furniture
%;
%;sort menu: physobj
%;sort bill: physobj
%;
%;sort script
%;

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',103).
% fluent Holding(agent,physobj)
% From E: 
% 
% fluent(holding(agent,physobj)).
mpred_prop(holding(agent, physobj), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',103).
fluents([holding/2]).

% event PickUp(agent,physobj)
% From E: 
% 
% event(pickUp(agent,physobj)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',103).
events([pickUp/2]).
mpred_prop(pickUp(agent, physobj), action).
actions([pickUp/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',105).
% event LetGoOf(agent,physobj)
% From E: 
% 
% event(letGoOf(agent,physobj)).
events([letGoOf/2]).
mpred_prop(letGoOf(agent, physobj), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',105).
actions([letGoOf/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',107).
% [agent,physobj,time]
% Initiates(PickUp(agent,physobj),Holding(agent,physobj),time).
% From E: 
% 
% initiates_at(
%    pickUp(Agent,Physobj), 
%    holding(Agent,Physobj), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',107).
initiates(pickUp(Agent,Physobj),
	  holding(Agent,Physobj)).


% [agent,physobj,time]
% Happens(PickUp(agent,physobj),time) ->
% {location}% 
%   HoldsAt(At(agent,location),time) &
%   HoldsAt(At(physobj,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',111).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          pickUp(Agent,Physobj), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent,Location), 
%             Time), 
%          holds(
%             at_loc(Physobj,Location), 
%             Time)))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',111).
exists(Location,  (at_loc(Agent, Location), at_loc(Physobj, Location);not(pickUp(Agent, Physobj)))).


% [agent,physobj,time]
% Terminates(LetGoOf(agent,physobj),Holding(agent,physobj),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',117).
% From E: 
% 
% terminates_at(
%    letGoOf(Agent,Physobj), 
%    holding(Agent,Physobj), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',117).
terminates(letGoOf(Agent,Physobj),
	   holding(Agent,Physobj)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',119).
% [agent,physobj,time]
% Happens(LetGoOf(agent,physobj),time) ->
% HoldsAt(Holding(agent,physobj),time).
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
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',119).
if(not(holding(Agent,Physobj)),
   not(letGoOf(Agent,Physobj))).


% [agent,physobj,location,time]
% Releases(PickUp(agent,physobj),At(physobj,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',124).
% From E: 
% 
% releases_at(
%    pickUp(Agent,Physobj), 
%    at_loc(Physobj,Location), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',124).
releases(pickUp(Agent,Physobj),
	 at_loc(Physobj,Location)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',126).
% [agent,physobj,location,time]
% HoldsAt(Holding(agent,physobj),time) &
% HoldsAt(At(agent,location),time) ->
% HoldsAt(At(physobj,location),time).
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
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',126).
 if(not(at_loc(Physobj, Location)),
       (not(holding(Agent, Physobj));not(at_loc(Agent, Location)))).


%;[agent,physobj,location1,location2,time]
%;HoldsAt(At(agent,location1),time) &
%;location1!=location2 ->
%;Terminates(LetGoOf(agent,physobj),At(physobj,location2),time).
% [agent,physobj,location,time]
% HoldsAt(At(agent,location),time) ->
% Initiates(LetGoOf(agent,physobj),At(physobj,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',136).
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
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',136).
if(not(initiates(letGoOf(Agent,Physobj),
		 at(at_loc(Physobj,Location),Time))),
   not(holds(at_loc(Agent,Location),Time))).

% fluent On(physobj,physobj)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',139).
% From E: 
% 
% fluent(on(physobj,physobj)).
mpred_prop(on(physobj, physobj), fluent).
fluents([on/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',142).
% event PlaceOn(agent,physobj,physobj)
% From E: 
% 
% event(placeOn(agent,physobj,physobj)).
events([placeOn/3]).
mpred_prop(placeOn(agent, physobj, physobj), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',142).
actions([placeOn/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',144).
% event TakeOffOf(agent,physobj,physobj)
% From E: 
% 
% event(takeOffOf(agent,physobj,physobj)).
events([takeOffOf/3]).
mpred_prop(takeOffOf(agent, physobj, physobj), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',144).
actions([takeOffOf/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',146).
% [physobj1,physobj2,time]
% HoldsAt(On(physobj1,physobj2),time) ->
% physobj1!=physobj2.
% From E: 
% 
% '->'(
%    holds(
%       on(Physobj1,Physobj2), 
%       Time), 
%    Physobj1\=Physobj2).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',146).
if(not({dif(Physobj1,Physobj2)}),
   not(on(Physobj1,Physobj2))).


% [physobj1,physobj2,time]
% HoldsAt(On(physobj1,physobj2),time) ->
% !HoldsAt(On(physobj2,physobj1),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',151).
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
if(on(Physobj2,Physobj1),
   not(on(Physobj1,Physobj2))).


% [agent,physobj1,physobj2,time]
% Initiates(PlaceOn(agent,physobj1,physobj2),
%           On(physobj1,physobj2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',155).
% From E: 
% 
% initiates_at(
%    placeOn(Agent,Physobj1,Physobj2), 
%    on(Physobj1,Physobj2), 
%    Time).
 %   [Time].
initiates(placeOn(Agent,Physobj1,Physobj2),
	  on(Physobj1,Physobj2)).


% [agent,physobj1,physobj2,time]
% Terminates(PlaceOn(agent,physobj1,physobj2),
%            Holding(agent,physobj1),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',159).
% From E: 
% 
% terminates_at(
%    placeOn(Agent,Physobj1,Physobj2), 
%    holding(Agent,Physobj1), 
%    Time).
 %   [Time].
terminates(placeOn(Agent,Physobj1,Physobj2),
	   holding(Agent,Physobj1)).


% [agent,physobj1,physobj2,time]
% Happens(PlaceOn(agent,physobj1,physobj2),time) ->
% HoldsAt(Holding(agent,physobj1),time) &
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',163).
% {location}% 
%  HoldsAt(At(agent,location),time) &
%  HoldsAt(At(physobj2,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',165).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          placeOn(Agent,Physobj1,Physobj2), 
%          Time), 
%       ','(
%          holds(
%             holding(Agent,Physobj1), 
%             Time), 
%          ','(
%             holds(
%                at_loc(Agent,Location), 
%                Time), 
%             holds(
%                at_loc(Physobj2,Location), 
%                Time))))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',165).
exists(Location,  (holding(Agent, Physobj1), at_loc(Agent, Location), at_loc(Physobj2, Location);not(placeOn(Agent, Physobj1, Physobj2)))).


% [agent,physobj1,physobj2,time]
% Terminates(TakeOffOf(agent,physobj1,physobj2),
%            On(physobj1,physobj2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',170).
% From E: 
% 
% terminates_at(
%    takeOffOf(Agent,Physobj1,Physobj2), 
%    on(Physobj1,Physobj2), 
%    Time).
 %   [Time].
terminates(takeOffOf(Agent,Physobj1,Physobj2),
	   on(Physobj1,Physobj2)).


% [agent,physobj1,physobj2,time]
% Initiates(TakeOffOf(agent,physobj1,physobj2),
%           Holding(agent,physobj1),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',174).
% From E: 
% 
% initiates_at(
%    takeOffOf(Agent,Physobj1,Physobj2), 
%    holding(Agent,Physobj1), 
%    Time).
 %   [Time].
initiates(takeOffOf(Agent,Physobj1,Physobj2),
	  holding(Agent,Physobj1)).


% [agent,physobj1,physobj2,location,time]
% Releases(TakeOffOf(agent,physobj1,physobj2),
%          At(physobj1,location),
%          time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',178).
% From E: 
% 
% releases_at(
%    takeOffOf(Agent,Physobj1,Physobj2), 
%    at_loc(Physobj1,Location), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',178).
releases(takeOffOf(Agent,Physobj1,Physobj2),
	 at_loc(Physobj1,Location)).


% [agent,physobj1,physobj2,time]
% Happens(TakeOffOf(agent,physobj1,physobj2),time) ->
% HoldsAt(On(physobj1,physobj2),time) &
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',183).
% {location}% 
%  HoldsAt(At(agent,location),time) &
%  HoldsAt(At(physobj1,location),time) &
%  HoldsAt(At(physobj2,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',185).
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
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',185).
exists(Location,  (on(Physobj1, Physobj2), at_loc(Agent, Location), at_loc(Physobj1, Location), at_loc(Physobj2, Location);not(takeOffOf(Agent, Physobj1, Physobj2)))).


% [agent,physobj1,physobj2,location,time]
% Releases(PlaceOn(agent,physobj1,physobj2),
%          At(physobj1,location),
%          time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',191).
% From E: 
% 
% releases_at(
%    placeOn(Agent,Physobj1,Physobj2), 
%    at_loc(Physobj1,Location), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',191).
releases(placeOn(Agent,Physobj1,Physobj2),
	 at_loc(Physobj1,Location)).


% [physobj1,physobj2,location,time]
% HoldsAt(On(physobj1,physobj2),time) &
% HoldsAt(At(physobj2,location),time) ->
% HoldsAt(At(physobj1,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',196).
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
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',196).
 if(not(at_loc(Physobj1, Location)),
       (not(on(Physobj1, Physobj2));not(at_loc(Physobj2, Location)))).

% fluent At(object,location)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',199).
% From E: 
% 
% fluent(at_loc(object,location)).
mpred_prop(at_loc(object, location), fluent).
fluents([at_loc/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',202).
% [object,time]
% {location} % HoldsAt(At(object,location),time).
% From E: 
% 
% exists(Location, 
%    holds(
%       at_loc(Object,Location), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',202).
exists(Location,
 at_loc(Object,Location)).


% [object,location1,location2,time]
% HoldsAt(At(object,location1),time) &
% HoldsAt(At(object,location2),time) ->
% location1=location2.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',206).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          at_loc(Object,Location1), 
%          Time), 
%       holds(
%          at_loc(Object,Location2), 
%          Time)), 
%    Location1=Location2).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',206).
 if(not(equals(Location1, Location2)),
       (not(at_loc(Object, Location1));not(at_loc(Object, Location2)))).

% function Side1(portal): location
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',209).
% From E: 
% 
% function(
%    side1(portal), 
%    location).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',209).
function(side1(portal),location).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',211).
% function Side2(portal): location
% From E: 
% 
% function(
%    side2(portal), 
%    location).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',211).
function(side2(portal),location).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',213).
% fluent NearPortal(object,portal)
% From E: 
% 
% fluent(nearPortal(object,portal)).
mpred_prop(nearPortal(object, portal), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',213).
fluents([nearPortal/2]).

% noninertial NearPortal
% From E: 
% 
% ':-'(call_pel_directive(noninertial(nearPortal))).
:- call_pel_directive(noninertial(nearPortal)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',216).
% [object,portal,time]
% HoldsAt(NearPortal(object,portal),time) <->
% {location}% 
%  (Side1(portal)=location|
%   Side2(portal)=location) &
%  HoldsAt(At(object,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',218).
% From E: 
% 
% exists(Location, 
%    <->(
%       holds(
%          nearPortal(Object,Portal), 
%          Time), 
%       ','(
%          ';'(
%             '='(
%                side1(Portal), 
%                Location), 
%             '='(
%                side2(Portal), 
%                Location)), 
%          holds(
%             at_loc(Object,Location), 
%             Time)))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',218).
exists(Location,  (((side1(Portal, Location);side2(Portal, Location)), at_loc(Object, Location);not(nearPortal(Object, Portal))), (nearPortal(Object, Portal);not(side1(Portal, Location)), not(side2(Portal, Location));not(at_loc(Object, Location))))).

% event WalkThroughDoor12(agent,door)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',222).
% From E: 
% 
% event(walkThroughDoor12(agent,door)).
events([walkThroughDoor12/2]).
mpred_prop(walkThroughDoor12(agent, door), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',222).
actions([walkThroughDoor12/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',224).
% event WalkThroughDoor21(agent,door)
% From E: 
% 
% event(walkThroughDoor21(agent,door)).
events([walkThroughDoor21/2]).
mpred_prop(walkThroughDoor21(agent, door), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',224).
actions([walkThroughDoor21/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',226).
% [agent,door,time]
% Happens(WalkThroughDoor12(agent,door),time) ->
% HoldsAt(Standing(agent),time) &
% HoldsAt(At(agent,Side1(door)),time).
% From E: 
% 
% '->'(
%    happens(
%       walkThroughDoor12(Agent,Door), 
%       Time), 
%    ','(
%       holds(
%          standing(Agent), 
%          Time), 
%       holds(
%          at_loc(Agent, 
%             side1(Door)), 
%          Time))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',226).
 if((not(standing(Agent));not(at_loc(Agent, side1(Door)))),
      not(walkThroughDoor12(Agent, Door))).


% [agent,door,time]
% Happens(WalkThroughDoor21(agent,door),time) ->
% HoldsAt(Standing(agent),time) &
% HoldsAt(At(agent,Side2(door)),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',232).
% From E: 
% 
% '->'(
%    happens(
%       walkThroughDoor21(Agent,Door), 
%       Time), 
%    ','(
%       holds(
%          standing(Agent), 
%          Time), 
%       holds(
%          at_loc(Agent, 
%             side2(Door)), 
%          Time))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',232).
 if((not(standing(Agent));not(at_loc(Agent, side2(Door)))),
      not(walkThroughDoor21(Agent, Door))).


% [agent,door,location,time]
% Side2(door)=location ->
% Initiates(WalkThroughDoor12(agent,door),At(agent,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',237).
% From E: 
% 
% '->'(
%    '='(
%       side2(Door), 
%       Location), 
%    initiates_at(
%       walkThroughDoor12(Agent,Door), 
%       at_loc(Agent,Location), 
%       Time)).
 %   [Time].
if(not(initiates(walkThroughDoor12(Agent,Door),
		 at(at_loc(Agent,Location),Time))),
   not(side2(Door,Location))).


% [agent,door,location,time]
% Side1(door)=location ->
% Initiates(WalkThroughDoor21(agent,door),At(agent,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',241).
% From E: 
% 
% '->'(
%    '='(
%       side1(Door), 
%       Location), 
%    initiates_at(
%       walkThroughDoor21(Agent,Door), 
%       at_loc(Agent,Location), 
%       Time)).
 %   [Time].
if(not(initiates(walkThroughDoor21(Agent,Door),
		 at(at_loc(Agent,Location),Time))),
   not(side1(Door,Location))).


% [agent,door,location,time]
% Side1(door)=location ->
% Terminates(WalkThroughDoor12(agent,door),At(agent,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',245).
% From E: 
% 
% '->'(
%    '='(
%       side1(Door), 
%       Location), 
%    terminates_at(
%       walkThroughDoor12(Agent,Door), 
%       at_loc(Agent,Location), 
%       Time)).
 %   [Time].
if(not(terminates(walkThroughDoor12(Agent,Door),
		  at(at_loc(Agent,Location),Time))),
   not(side1(Door,Location))).


% [agent,door,location,time]
% Side2(door)=location ->
% Terminates(WalkThroughDoor21(agent,door),At(agent,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',249).
% From E: 
% 
% '->'(
%    '='(
%       side2(Door), 
%       Location), 
%    terminates_at(
%       walkThroughDoor21(Agent,Door), 
%       at_loc(Agent,Location), 
%       Time)).
 %   [Time].
if(not(terminates(walkThroughDoor21(Agent,Door),
		  at(at_loc(Agent,Location),Time))),
   not(side2(Door,Location))).

% fluent Hungry(agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',251).
% From E: 
% 
% fluent(hungry(agent)).
mpred_prop(hungry(agent), fluent).
fluents([hungry/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',254).
% fluent Satiated(agent)
% From E: 
% 
% fluent(satiated(agent)).
mpred_prop(satiated(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',254).
fluents([satiated/1]).

% noninertial Satiated
% From E: 
% 
% ':-'(call_pel_directive(noninertial(satiated))).
:- call_pel_directive(noninertial(satiated)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',257).
% [agent,time]
 % HoldsAt(Hungry(agent),time) <-> !HoldsAt(Satiated(agent),time).
% From E: 
% 
% <->(
%    holds(
%       hungry(Agent), 
%       Time), 
%    holds(
%       not(satiated(Agent)), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',257).
if(satiated(Agent), not(hungry(Agent))),
if(not(hungry(Agent)), satiated(Agent)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',259).
% event Eat(agent,food)
% From E: 
% 
% event(eat(agent,food)).
events([eat/2]).
mpred_prop(eat(agent, food), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',259).
actions([eat/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',261).
% [agent,food,time]
% Happens(Eat(agent,food),time) ->
% {location}% 
% HoldsAt(At(agent,location),time) &
% HoldsAt(At(food,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',263).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          eat(Agent,Food), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent,Location), 
%             Time), 
%          holds(
%             at_loc(Food,Location), 
%             Time)))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',263).
exists(Location,  (at_loc(Agent, Location), at_loc(Food, Location);not(eat(Agent, Food)))).


% [agent,food,time]
% Terminates(Eat(agent,food),Hungry(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',268).
% From E: 
% 
% terminates_at(
%    eat(Agent,Food), 
%    hungry(Agent), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',268).
terminates(eat(Agent,Food),hungry(Agent)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',270).
% sort restaurant: script
% From E: 
% 
% subsort(restaurant,script).
subsort(restaurant, script).

% sort waiter: agent
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',270).
% From E: 
% 
% subsort(waiter,agent).
subsort(waiter, agent).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',272).
% sort cook: agent
% From E: 
% 
% subsort(cook,agent).
subsort(cook, agent).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',274).
% function BillOf(restaurant): bill
% From E: 
% 
% function(
%    billOf(restaurant), 
%    bill).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',274).
function(billOf(restaurant),bill).

% function CookOf(restaurant): cook
% From E: 
% 
% function(
%    cookOf(restaurant), 
%    cook).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',274).
function(cookOf(restaurant),cook).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',276).
% function TableOf(restaurant): table
% From E: 
% 
% function(
%    tableOf(restaurant), 
%    table).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',276).
function(tableOf(restaurant),table).

% function WaiterOf(restaurant): waiter
% From E: 
% 
% function(
%    waiterOf(restaurant), 
%    waiter).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',276).
function(waiterOf(restaurant),waiter).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',278).
% function KitchenDoorOf(restaurant): door
% From E: 
% 
% function(
%    kitchenDoorOf(restaurant), 
%    door).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',278).
function(kitchenDoorOf(restaurant),door).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',280).
% fluent BeWaiter0(waiter)
% From E: 
% 
% fluent(beWaiter0(waiter)).
mpred_prop(beWaiter0(waiter), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',280).
fluents([beWaiter0/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',282).
% fluent BeWaiter1(waiter)
% From E: 
% 
% fluent(beWaiter1(waiter)).
mpred_prop(beWaiter1(waiter), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',282).
fluents([beWaiter1/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',284).
% fluent BeWaiter2(waiter)
% From E: 
% 
% fluent(beWaiter2(waiter)).
mpred_prop(beWaiter2(waiter), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',284).
fluents([beWaiter2/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',286).
% fluent BeWaiter3(waiter)
% From E: 
% 
% fluent(beWaiter3(waiter)).
mpred_prop(beWaiter3(waiter), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',286).
fluents([beWaiter3/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',288).
% fluent BeWaiter4(waiter)
% From E: 
% 
% fluent(beWaiter4(waiter)).
mpred_prop(beWaiter4(waiter), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',288).
fluents([beWaiter4/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',290).
% fluent BeWaiter5(waiter)
% From E: 
% 
% fluent(beWaiter5(waiter)).
mpred_prop(beWaiter5(waiter), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',290).
fluents([beWaiter5/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',292).
% fluent BeWaiter6(waiter)
% From E: 
% 
% fluent(beWaiter6(waiter)).
mpred_prop(beWaiter6(waiter), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',292).
fluents([beWaiter6/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',294).
% fluent BeWaiter7(waiter)
% From E: 
% 
% fluent(beWaiter7(waiter)).
mpred_prop(beWaiter7(waiter), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',294).
fluents([beWaiter7/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',296).
% fluent BeWaiter8(waiter)
% From E: 
% 
% fluent(beWaiter8(waiter)).
mpred_prop(beWaiter8(waiter), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',296).
fluents([beWaiter8/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',298).
% fluent BeWaiter9(waiter)
% From E: 
% 
% fluent(beWaiter9(waiter)).
mpred_prop(beWaiter9(waiter), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',298).
fluents([beWaiter9/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',300).
% xor BeWaiter0, BeWaiter1, BeWaiter2, BeWaiter3, BeWaiter4, BeWaiter5, BeWaiter6, BeWaiter7, BeWaiter8, BeWaiter9
% From E: 
% 
% xor([beWaiter0,beWaiter1,beWaiter2,beWaiter3,beWaiter4,beWaiter5,beWaiter6,beWaiter7,beWaiter8,beWaiter9]).
xor([ beWaiter0,
      beWaiter1,
      beWaiter2,
      beWaiter3,
      beWaiter4,
      beWaiter5,
      beWaiter6,
      beWaiter7,
      beWaiter8,
      beWaiter9
    ]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',302).
% [waiter,agent,time]
% HoldsAt(BeWaiter0(waiter),time) ->
% Terminates(Greet(waiter,agent),
%            BeWaiter0(waiter),
%            time).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter0(Waiter), 
%       Time), 
%    terminates_at(
%       greet(Waiter,Agent), 
%       beWaiter0(Waiter), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',302).
if(not(terminates(greet(Waiter,Agent),
		  at(beWaiter0(Waiter),Time))),
   not(holds(beWaiter0(Waiter),Time))).


% [waiter,agent,time]
% HoldsAt(BeWaiter0(waiter),time) ->
% Initiates(Greet(waiter,agent),
%           BeWaiter1(waiter),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',309).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter0(Waiter), 
%       Time), 
%    initiates_at(
%       greet(Waiter,Agent), 
%       beWaiter1(Waiter), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',309).
if(not(initiates(greet(Waiter,Agent),
		 at(beWaiter1(Waiter),Time))),
   not(holds(beWaiter0(Waiter),Time))).


% [waiter,agent,food,time]
% HoldsAt(BeWaiter1(waiter),time) ->
% Terminates(Order(agent,waiter,food),
%            BeWaiter1(waiter),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',315).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter1(Waiter), 
%       Time), 
%    terminates_at(
%       order(Agent,Waiter,Food), 
%       beWaiter1(Waiter), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',315).
if(not(terminates(order(Agent,Waiter,Food),
		  at(beWaiter1(Waiter),Time))),
   not(holds(beWaiter1(Waiter),Time))).


% [waiter,agent,food,time]
% HoldsAt(BeWaiter1(waiter),time) ->
% Initiates(Order(agent,waiter,food),
%           BeWaiter2(waiter),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',321).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter1(Waiter), 
%       Time), 
%    initiates_at(
%       order(Agent,Waiter,Food), 
%       beWaiter2(Waiter), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',321).
if(not(initiates(order(Agent,Waiter,Food),
		 at(beWaiter2(Waiter),Time))),
   not(holds(beWaiter1(Waiter),Time))).


% [restaurant,waiter,time]
% WaiterOf(restaurant)=waiter &
% HoldsAt(BeWaiter2(waiter),time) ->
% Happens(WalkThroughDoor12(waiter,KitchenDoorOf(restaurant)),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',327).
% From E: 
% 
% '->'(
%    ','(
%       '='(
%          waiterOf(Restaurant), 
%          Waiter), 
%       holds(
%          beWaiter2(Waiter), 
%          Time)), 
%    happens(
%       walkThroughDoor12(Waiter, 
%          kitchenDoorOf(Restaurant)), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',327).
 if(not(walkThroughDoor12(Waiter, kitchenDoorOf(Restaurant))),
       (not(waiterOf(Restaurant, Waiter));not(beWaiter2(Waiter)))).


% [restaurant,waiter,door,time]
% HoldsAt(BeWaiter2(waiter),time) &
% WaiterOf(restaurant)=waiter &
% KitchenDoorOf(restaurant)=door ->
% Terminates(WalkThroughDoor12(waiter,door),
%            BeWaiter2(waiter),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',332).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          beWaiter2(Waiter), 
%          Time), 
%       ','(
%          '='(
%             waiterOf(Restaurant), 
%             Waiter), 
%          '='(
%             kitchenDoorOf(Restaurant), 
%             Door))), 
%    terminates_at(
%       walkThroughDoor12(Waiter,Door), 
%       beWaiter2(Waiter), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',332).
 if(not(terminates(walkThroughDoor12(Waiter, Door),
                     at(beWaiter2(Waiter), Time))),
       (not(holds(beWaiter2(Waiter), Time));not(waiterOf(Restaurant, Waiter));not(kitchenDoorOf(Restaurant, Door)))).


% [restaurant,waiter,door,time]
% HoldsAt(BeWaiter2(waiter),time) &
% WaiterOf(restaurant)=waiter &
% KitchenDoorOf(restaurant)=door ->
% Initiates(WalkThroughDoor12(waiter,door),
%           BeWaiter3(waiter),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',340).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          beWaiter2(Waiter), 
%          Time), 
%       ','(
%          '='(
%             waiterOf(Restaurant), 
%             Waiter), 
%          '='(
%             kitchenDoorOf(Restaurant), 
%             Door))), 
%    initiates_at(
%       walkThroughDoor12(Waiter,Door), 
%       beWaiter3(Waiter), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',340).
 if(not(initiates(walkThroughDoor12(Waiter, Door),
                    at(beWaiter3(Waiter), Time))),
       (not(holds(beWaiter2(Waiter), Time));not(waiterOf(Restaurant, Waiter));not(kitchenDoorOf(Restaurant, Door)))).


% [restaurant,food,time]
% HoldsAt(BeWaiter3(WaiterOf(restaurant)),time) &
% ({agent} HoldsAt(KnowOrder(WaiterOf(restaurant),agent,food),time)) ->
% Happens(Order(WaiterOf(restaurant),CookOf(restaurant),food),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',348).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          beWaiter3(waiterOf(Restaurant)), 
%          Time), 
%       thereExists(Agent, 
%          holds(
%             knowOrder(
%                waiterOf(Restaurant), 
%                Agent, 
%                Food), 
%             Time))), 
%    happens(
%       order(
%          waiterOf(Restaurant), 
%          cookOf(Restaurant), 
%          Food), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',348).
 if(not(order(waiterOf(Restaurant),
                cookOf(Restaurant),
                Food)),
       (not(beWaiter3(waiterOf(Restaurant)));not(thereExists(Agent, knowOrder(waiterOf(Restaurant), Agent, Food))))).


% [restaurant,waiter,cook,food,time]
% WaiterOf(restaurant)=waiter &
% CookOf(restaurant)=cook &
% HoldsAt(BeWaiter3(waiter),time) ->
% Terminates(Order(waiter,cook,food),
%            BeWaiter3(waiter),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',353).
% From E: 
% 
% '->'(
%    ','(
%       '='(
%          waiterOf(Restaurant), 
%          Waiter), 
%       ','(
%          '='(
%             cookOf(Restaurant), 
%             Cook), 
%          holds(
%             beWaiter3(Waiter), 
%             Time))), 
%    terminates_at(
%       order(Waiter,Cook,Food), 
%       beWaiter3(Waiter), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',353).
 if(not(terminates(order(Waiter, Cook, Food),
                     at(beWaiter3(Waiter), Time))),
       (not(waiterOf(Restaurant, Waiter));not(cookOf(Restaurant, Cook));not(holds(beWaiter3(Waiter), Time)))).


% [restaurant,waiter,cook,food,time]
% WaiterOf(restaurant)=waiter &
% CookOf(restaurant)=cook &
% HoldsAt(BeWaiter3(waiter),time) ->
% Initiates(Order(waiter,cook,food),
%           BeWaiter4(waiter),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',361).
% From E: 
% 
% '->'(
%    ','(
%       '='(
%          waiterOf(Restaurant), 
%          Waiter), 
%       ','(
%          '='(
%             cookOf(Restaurant), 
%             Cook), 
%          holds(
%             beWaiter3(Waiter), 
%             Time))), 
%    initiates_at(
%       order(Waiter,Cook,Food), 
%       beWaiter4(Waiter), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',361).
 if(not(initiates(order(Waiter, Cook, Food),
                    at(beWaiter4(Waiter), Time))),
       (not(waiterOf(Restaurant, Waiter));not(cookOf(Restaurant, Cook));not(holds(beWaiter3(Waiter), Time)))).


% [waiter,food,time]
% HoldsAt(BeWaiter4(waiter),time) &
% ({agent} HoldsAt(KnowOrder(waiter,agent,food),time)) &
% HoldsAt(FoodPrepared(food),time) ->
% Happens(PickUp(waiter,food),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',369).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          beWaiter4(Waiter), 
%          Time), 
%       ','(
%          thereExists(Agent, 
%             holds(
%                knowOrder(Waiter,Agent,Food), 
%                Time)), 
%          holds(
%             foodPrepared(Food), 
%             Time))), 
%    happens(
%       pickUp(Waiter,Food), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',369).
 if(not(pickUp(Waiter, Food)),
       (not(beWaiter4(Waiter));not(thereExists(Agent, knowOrder(Waiter, Agent, Food)));not(foodPrepared(Food)))).


% [waiter,food,time]
% HoldsAt(BeWaiter4(waiter),time) &
% ({agent} HoldsAt(KnowOrder(waiter,agent,food),time)) ->
% Terminates(PickUp(waiter,food),
%            BeWaiter4(waiter),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',375).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          beWaiter4(Waiter), 
%          Time), 
%       thereExists(Agent, 
%          holds(
%             knowOrder(Waiter,Agent,Food), 
%             Time))), 
%    terminates_at(
%       pickUp(Waiter,Food), 
%       beWaiter4(Waiter), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',375).
 if(not(terminates(pickUp(Waiter, Food),
                     at(beWaiter4(Waiter), Time))),
       (not(holds(beWaiter4(Waiter), Time));not(thereExists(Agent, holds(knowOrder(Waiter, Agent, Food), Time))))).


% [waiter,food,time]
% HoldsAt(BeWaiter4(waiter),time) &
% ({agent} HoldsAt(KnowOrder(waiter,agent,food),time)) ->
% Initiates(PickUp(waiter,food),
%           BeWaiter5(waiter),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',382).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          beWaiter4(Waiter), 
%          Time), 
%       thereExists(Agent, 
%          holds(
%             knowOrder(Waiter,Agent,Food), 
%             Time))), 
%    initiates_at(
%       pickUp(Waiter,Food), 
%       beWaiter5(Waiter), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',382).
 if(not(initiates(pickUp(Waiter, Food),
                    at(beWaiter5(Waiter), Time))),
       (not(holds(beWaiter4(Waiter), Time));not(thereExists(Agent, holds(knowOrder(Waiter, Agent, Food), Time))))).


% [restaurant,waiter,time]
% WaiterOf(restaurant)=waiter &
% HoldsAt(BeWaiter5(waiter),time) ->
% Happens(WalkThroughDoor21(waiter,KitchenDoorOf(restaurant)),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',389).
% From E: 
% 
% '->'(
%    ','(
%       '='(
%          waiterOf(Restaurant), 
%          Waiter), 
%       holds(
%          beWaiter5(Waiter), 
%          Time)), 
%    happens(
%       walkThroughDoor21(Waiter, 
%          kitchenDoorOf(Restaurant)), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',389).
 if(not(walkThroughDoor21(Waiter, kitchenDoorOf(Restaurant))),
       (not(waiterOf(Restaurant, Waiter));not(beWaiter5(Waiter)))).


% [restaurant,waiter,door,time]
% HoldsAt(BeWaiter5(waiter),time) &
% WaiterOf(restaurant)=waiter &
% KitchenDoorOf(restaurant)=door ->
% Terminates(WalkThroughDoor21(waiter,door),
%            BeWaiter5(waiter),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',394).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          beWaiter5(Waiter), 
%          Time), 
%       ','(
%          '='(
%             waiterOf(Restaurant), 
%             Waiter), 
%          '='(
%             kitchenDoorOf(Restaurant), 
%             Door))), 
%    terminates_at(
%       walkThroughDoor21(Waiter,Door), 
%       beWaiter5(Waiter), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',394).
 if(not(terminates(walkThroughDoor21(Waiter, Door),
                     at(beWaiter5(Waiter), Time))),
       (not(holds(beWaiter5(Waiter), Time));not(waiterOf(Restaurant, Waiter));not(kitchenDoorOf(Restaurant, Door)))).


% [restaurant,waiter,door,time]
% HoldsAt(BeWaiter5(waiter),time) &
% WaiterOf(restaurant)=waiter &
% KitchenDoorOf(restaurant)=door ->
% Initiates(WalkThroughDoor21(waiter,door),
%           BeWaiter6(waiter),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',402).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          beWaiter5(Waiter), 
%          Time), 
%       ','(
%          '='(
%             waiterOf(Restaurant), 
%             Waiter), 
%          '='(
%             kitchenDoorOf(Restaurant), 
%             Door))), 
%    initiates_at(
%       walkThroughDoor21(Waiter,Door), 
%       beWaiter6(Waiter), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',402).
 if(not(initiates(walkThroughDoor21(Waiter, Door),
                    at(beWaiter6(Waiter), Time))),
       (not(holds(beWaiter5(Waiter), Time));not(waiterOf(Restaurant, Waiter));not(kitchenDoorOf(Restaurant, Door)))).


% [restaurant,waiter,table,food,time]
% WaiterOf(restaurant)=waiter &
% TableOf(restaurant)=table &
% HoldsAt(BeWaiter6(waiter),time) &
% HoldsAt(Holding(waiter,food),time) ->
% Happens(PlaceOn(waiter,food,table),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',410).
% From E: 
% 
% '->'(
%    ','(
%       '='(
%          waiterOf(Restaurant), 
%          Waiter), 
%       ','(
%          '='(
%             tableOf(Restaurant), 
%             Table), 
%          ','(
%             holds(
%                beWaiter6(Waiter), 
%                Time), 
%             holds(
%                holding(Waiter,Food), 
%                Time)))), 
%    happens(
%       placeOn(Waiter,Food,Table), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',410).
 if(not(placeOn(Waiter, Food, Table)),
       (not(waiterOf(Restaurant, Waiter));not(tableOf(Restaurant, Table));not(beWaiter6(Waiter));not(holding(Waiter, Food)))).


% [waiter,food,table,time]
% HoldsAt(BeWaiter6(waiter),time) ->
% Terminates(PlaceOn(waiter,food,table),
%            BeWaiter6(waiter),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',417).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter6(Waiter), 
%       Time), 
%    terminates_at(
%       placeOn(Waiter,Food,Table), 
%       beWaiter6(Waiter), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',417).
if(not(terminates(placeOn(Waiter,Food,Table),
		  at(beWaiter6(Waiter),Time))),
   not(holds(beWaiter6(Waiter),Time))).


% [waiter,food,table,time]
% HoldsAt(BeWaiter6(waiter),time) ->
% Initiates(PlaceOn(waiter,food,table),
%           BeWaiter7(waiter),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',423).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter6(Waiter), 
%       Time), 
%    initiates_at(
%       placeOn(Waiter,Food,Table), 
%       beWaiter7(Waiter), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',423).
if(not(initiates(placeOn(Waiter,Food,Table),
		 at(beWaiter7(Waiter),Time))),
   not(holds(beWaiter6(Waiter),Time))).


% [waiter,agent,bill,time]
% HoldsAt(BeWaiter7(waiter),time) ->
% Terminates(Request(agent,waiter,bill),
%            BeWaiter7(waiter),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',429).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter7(Waiter), 
%       Time), 
%    terminates_at(
%       request(Agent,Waiter,Bill), 
%       beWaiter7(Waiter), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',429).
if(not(terminates(request(Agent,Waiter,Bill),
		  at(beWaiter7(Waiter),Time))),
   not(holds(beWaiter7(Waiter),Time))).


% [waiter,agent,bill,time]
% HoldsAt(BeWaiter7(waiter),time) ->
% Initiates(Request(agent,waiter,bill),
%           BeWaiter8(waiter),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',435).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter7(Waiter), 
%       Time), 
%    initiates_at(
%       request(Agent,Waiter,Bill), 
%       beWaiter8(Waiter), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',435).
if(not(initiates(request(Agent,Waiter,Bill),
		 at(beWaiter8(Waiter),Time))),
   not(holds(beWaiter7(Waiter),Time))).


% [restaurant,waiter,bill,time]
% WaiterOf(restaurant)=waiter &
% BillOf(restaurant)=bill &
% HoldsAt(BeWaiter8(waiter),time) ->
% Happens(PickUp(waiter,bill),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',441).
% From E: 
% 
% '->'(
%    ','(
%       '='(
%          waiterOf(Restaurant), 
%          Waiter), 
%       ','(
%          '='(
%             billOf(Restaurant), 
%             Bill), 
%          holds(
%             beWaiter8(Waiter), 
%             Time))), 
%    happens(
%       pickUp(Waiter,Bill), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',441).
 if(not(pickUp(Waiter, Bill)),
       (not(waiterOf(Restaurant, Waiter));not(billOf(Restaurant, Bill));not(beWaiter8(Waiter)))).


% [waiter,bill,time]
% HoldsAt(BeWaiter8(waiter),time) ->
% Terminates(PickUp(waiter,bill),
%            BeWaiter8(waiter),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',447).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter8(Waiter), 
%       Time), 
%    terminates_at(
%       pickUp(Waiter,Bill), 
%       beWaiter8(Waiter), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',447).
if(not(terminates(pickUp(Waiter,Bill),
		  at(beWaiter8(Waiter),Time))),
   not(holds(beWaiter8(Waiter),Time))).


% [waiter,bill,time]
% HoldsAt(BeWaiter8(waiter),time) ->
% Initiates(PickUp(waiter,bill),
%           BeWaiter9(waiter),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',453).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter8(Waiter), 
%       Time), 
%    initiates_at(
%       pickUp(Waiter,Bill), 
%       beWaiter9(Waiter), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',453).
if(not(initiates(pickUp(Waiter,Bill),
		 at(beWaiter9(Waiter),Time))),
   not(holds(beWaiter8(Waiter),Time))).


% [restaurant,waiter,bill,table,time]
% WaiterOf(restaurant)=waiter &
% BillOf(restaurant)=bill &
% TableOf(restaurant)=table &
% HoldsAt(BeWaiter9(waiter),time) ->
% Happens(PlaceOn(waiter,bill,table),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',459).
% From E: 
% 
% '->'(
%    ','(
%       '='(
%          waiterOf(Restaurant), 
%          Waiter), 
%       ','(
%          '='(
%             billOf(Restaurant), 
%             Bill), 
%          ','(
%             '='(
%                tableOf(Restaurant), 
%                Table), 
%             holds(
%                beWaiter9(Waiter), 
%                Time)))), 
%    happens(
%       placeOn(Waiter,Bill,Table), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',459).
 if(not(placeOn(Waiter, Bill, Table)),
       (not(waiterOf(Restaurant, Waiter));not(billOf(Restaurant, Bill));not(tableOf(Restaurant, Table));not(beWaiter9(Waiter)))).


% [waiter,bill,table,time]
% HoldsAt(BeWaiter9(waiter),time) ->
% Terminates(PlaceOn(waiter,bill,table),
%            BeWaiter9(waiter),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',466).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter9(Waiter), 
%       Time), 
%    terminates_at(
%       placeOn(Waiter,Bill,Table), 
%       beWaiter9(Waiter), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',466).
if(not(terminates(placeOn(Waiter,Bill,Table),
		  at(beWaiter9(Waiter),Time))),
   not(holds(beWaiter9(Waiter),Time))).


% [waiter,bill,table,time]
% HoldsAt(BeWaiter9(waiter),time) ->
% Initiates(PlaceOn(waiter,bill,table),
%           BeWaiter0(waiter),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',472).
% From E: 
% 
% '->'(
%    holds(
%       beWaiter9(Waiter), 
%       Time), 
%    initiates_at(
%       placeOn(Waiter,Bill,Table), 
%       beWaiter0(Waiter), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',472).
if(not(initiates(placeOn(Waiter,Bill,Table),
		 at(beWaiter0(Waiter),Time))),
   not(holds(beWaiter9(Waiter),Time))).

% fluent BeCook0(cook)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',476).
% From E: 
% 
% fluent(beCook0(cook)).
mpred_prop(beCook0(cook), fluent).
fluents([beCook0/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',479).
% fluent BeCook1(cook)
% From E: 
% 
% fluent(beCook1(cook)).
mpred_prop(beCook1(cook), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',479).
fluents([beCook1/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',481).
% xor BeCook0, BeCook1
% From E: 
% 
% xor([beCook0,beCook1]).
xor([beCook0,beCook1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',483).
% [cook,agent,food,time]
% HoldsAt(BeCook0(cook),time) ->
% Terminates(Order(agent,cook,food),
%            BeCook0(cook),
%            time).
% From E: 
% 
% '->'(
%    holds(
%       beCook0(Cook), 
%       Time), 
%    terminates_at(
%       order(Agent,Cook,Food), 
%       beCook0(Cook), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',483).
if(not(terminates(order(Agent,Cook,Food),
		  at(beCook0(Cook),Time))),
   not(holds(beCook0(Cook),Time))).


% [cook,agent,food,time]
% HoldsAt(BeCook0(cook),time) ->
% Initiates(Order(agent,cook,food),
%           BeCook1(cook),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',490).
% From E: 
% 
% '->'(
%    holds(
%       beCook0(Cook), 
%       Time), 
%    initiates_at(
%       order(Agent,Cook,Food), 
%       beCook1(Cook), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',490).
if(not(initiates(order(Agent,Cook,Food),
		 at(beCook1(Cook),Time))),
   not(holds(beCook0(Cook),Time))).

% event FoodPrepare(agent,food)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',494).
% From E: 
% 
% event(foodPrepare(agent,food)).
events([foodPrepare/2]).
mpred_prop(foodPrepare(agent, food), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',494).
actions([foodPrepare/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',497).
% fluent FoodPrepared(food)
% From E: 
% 
% fluent(foodPrepared(food)).
mpred_prop(foodPrepared(food), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',497).
fluents([foodPrepared/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',499).
% [agent,food,time]
% Initiates(FoodPrepare(agent,food),
%           FoodPrepared(food),
%           time).
% From E: 
% 
% initiates_at(
%    foodPrepare(Agent,Food), 
%    foodPrepared(Food), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',499).
initiates(foodPrepare(Agent,Food),foodPrepared(Food)).


% [agent,food,time]
% Happens(FoodPrepare(agent,food),time) ->
% {location}% 
% HoldsAt(At(agent,location),time) &
% HoldsAt(At(food,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',505).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          foodPrepare(Agent,Food), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent,Location), 
%             Time), 
%          holds(
%             at_loc(Food,Location), 
%             Time)))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',505).
exists(Location,  (at_loc(Agent, Location), at_loc(Food, Location);not(foodPrepare(Agent, Food)))).


% [cook,agent,food,time]
% HoldsAt(BeCook1(cook),time) &
% HoldsAt(KnowOrder(cook,agent,food),time) ->
% Happens(FoodPrepare(cook,food),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',511).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          beCook1(Cook), 
%          Time), 
%       holds(
%          knowOrder(Cook,Agent,Food), 
%          Time)), 
%    happens(
%       foodPrepare(Cook,Food), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',511).
 if(not(foodPrepare(Cook, Food)),
       (not(beCook1(Cook));not(knowOrder(Cook, Agent, Food)))).


% [cook,food,time]
% HoldsAt(BeCook1(cook),time) ->
% Terminates(FoodPrepare(cook,food),
%            BeCook1(cook),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',516).
% From E: 
% 
% '->'(
%    holds(
%       beCook1(Cook), 
%       Time), 
%    terminates_at(
%       foodPrepare(Cook,Food), 
%       beCook1(Cook), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',516).
if(not(terminates(foodPrepare(Cook,Food),
		  at(beCook1(Cook),Time))),
   not(holds(beCook1(Cook),Time))).


% [cook,food,time]
% HoldsAt(BeCook1(cook),time) ->
% Initiates(FoodPrepare(cook,food),
%           BeCook0(cook),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',522).
% From E: 
% 
% '->'(
%    holds(
%       beCook1(Cook), 
%       Time), 
%    initiates_at(
%       foodPrepare(Cook,Food), 
%       beCook0(Cook), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',522).
if(not(initiates(foodPrepare(Cook,Food),
		 at(beCook0(Cook),Time))),
   not(holds(beCook1(Cook),Time))).

% event Pay(agent,agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',526).
% From E: 
% 
% event(pay(agent,agent)).
events([pay/2]).
mpred_prop(pay(agent, agent), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',526).
actions([pay/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',529).
% event Tip(agent,agent)
% From E: 
% 
% event(tip(agent,agent)).
events([tip/2]).
mpred_prop(tip(agent, agent), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',529).
actions([tip/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',531).
% [agent,physobj,time]
% Happens(LieOn(agent,physobj),time) ->
% {room}% 
%  HoldsAt(At(agent,room),time) &
%  HoldsAt(At(physobj,room),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',533).
% From E: 
% 
% exists(Room, 
%    '->'(
%       happens(
%          lieOn(Agent,Physobj), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent,Room), 
%             Time), 
%          holds(
%             at_loc(Physobj,Room), 
%             Time)))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',533).
exists(Room,  (at_loc(Agent, Room), at_loc(Physobj, Room);not(lieOn(Agent, Physobj)))).


% [agent,physobj,time]
% Happens(SitOn(agent,physobj),time) ->
% {room}% 
%  HoldsAt(At(agent,room),time) &
%  HoldsAt(At(physobj,room),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',538).
% From E: 
% 
% exists(Room, 
%    '->'(
%       happens(
%          sitOn(Agent,Physobj), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent,Room), 
%             Time), 
%          holds(
%             at_loc(Physobj,Room), 
%             Time)))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',538).
exists(Room,  (at_loc(Agent, Room), at_loc(Physobj, Room);not(sitOn(Agent, Physobj)))).

% event LieOn(agent,physobj)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',542).
% From E: 
% 
% event(lieOn(agent,physobj)).
events([lieOn/2]).
mpred_prop(lieOn(agent, physobj), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',542).
actions([lieOn/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',545).
% event SitOn(agent,physobj)
% From E: 
% 
% event(sitOn(agent,physobj)).
events([sitOn/2]).
mpred_prop(sitOn(agent, physobj), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',545).
actions([sitOn/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',547).
% event RiseFrom(agent,physobj)
% From E: 
% 
% event(riseFrom(agent,physobj)).
events([riseFrom/2]).
mpred_prop(riseFrom(agent, physobj), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',547).
actions([riseFrom/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',549).
% fluent LyingOn(agent,physobj)
% From E: 
% 
% fluent(lyingOn(agent,physobj)).
mpred_prop(lyingOn(agent, physobj), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',549).
fluents([lyingOn/2]).

% fluent SittingOn(agent,physobj)
% From E: 
% 
% fluent(sittingOn(agent,physobj)).
mpred_prop(sittingOn(agent, physobj), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',549).
fluents([sittingOn/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',551).
% fluent Standing(agent)
% From E: 
% 
% fluent(standing(agent)).
mpred_prop(standing(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',551).
fluents([standing/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',553).
% fluent Lying(agent)
% From E: 
% 
% fluent(lying(agent)).
mpred_prop(lying(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',553).
fluents([lying/1]).

% fluent Sitting(agent)
% From E: 
% 
% fluent(sitting(agent)).
mpred_prop(sitting(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',553).
fluents([sitting/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',555).
% noninertial Lying
% From E: 
% 
% ':-'(call_pel_directive(noninertial(lying))).
:- call_pel_directive(noninertial(lying)).

% noninertial Sitting
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',555).
% From E: 
% 
% ':-'(call_pel_directive(noninertial(sitting))).
:- call_pel_directive(noninertial(sitting)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',558).
% xor Lying, Sitting, Standing
% From E: 
% 
% xor([lying,sitting,standing]).
xor([lying,sitting,standing]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',560).
% [agent,physobj,time]
% HoldsAt(LyingOn(agent,physobj),time) ->
% HoldsAt(Lying(agent),time).
% From E: 
% 
% '->'(
%    holds(
%       lyingOn(Agent,Physobj), 
%       Time), 
%    holds(
%       lying(Agent), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',560).
if(not(lying(Agent)),not(lyingOn(Agent,Physobj))).


% [agent,physobj,time]
% HoldsAt(SittingOn(agent,physobj),time) ->
% HoldsAt(Sitting(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',565).
% From E: 
% 
% '->'(
%    holds(
%       sittingOn(Agent,Physobj), 
%       Time), 
%    holds(
%       sitting(Agent), 
%       Time)).
 %   [Time].
if(not(sitting(Agent)),not(sittingOn(Agent,Physobj))).


% [agent,physobj1,physobj2,time]
% HoldsAt(LyingOn(agent,physobj1),time) &
% HoldsAt(LyingOn(agent,physobj2),time) ->
% physobj1=physobj2.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',569).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          lyingOn(Agent,Physobj1), 
%          Time), 
%       holds(
%          lyingOn(Agent,Physobj2), 
%          Time)), 
%    Physobj1=Physobj2).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',569).
 if(not(equals(Physobj1, Physobj2)),
       (not(lyingOn(Agent, Physobj1));not(lyingOn(Agent, Physobj2)))).


% [agent,physobj1,physobj2,time]
% HoldsAt(SittingOn(agent,physobj1),time) &
% HoldsAt(SittingOn(agent,physobj2),time) ->
% physobj1=physobj2.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',574).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          sittingOn(Agent,Physobj1), 
%          Time), 
%       holds(
%          sittingOn(Agent,Physobj2), 
%          Time)), 
%    Physobj1=Physobj2).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',574).
 if(not(equals(Physobj1, Physobj2)),
       (not(sittingOn(Agent, Physobj1));not(sittingOn(Agent, Physobj2)))).


% [agent,physobj,time]
% HoldsAt(Standing(agent),time) ->
% Initiates(LieOn(agent,physobj),
%           LyingOn(agent,physobj),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',579).
% From E: 
% 
% '->'(
%    holds(
%       standing(Agent), 
%       Time), 
%    initiates_at(
%       lieOn(Agent,Physobj), 
%       lyingOn(Agent,Physobj), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',579).
if(not(initiates(lieOn(Agent,Physobj),
		 at(lyingOn(Agent,Physobj),Time))),
   not(holds(standing(Agent),Time))).


% [agent,physobj,time]
% Terminates(LieOn(agent,physobj),
%            Standing(agent),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',585).
% From E: 
% 
% terminates_at(
%    lieOn(Agent,Physobj), 
%    standing(Agent), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',585).
terminates(lieOn(Agent,Physobj),standing(Agent)).


% [agent,physobj,time]
% HoldsAt(Standing(agent),time) ->
% Initiates(SitOn(agent,physobj),
%           SittingOn(agent,physobj),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',590).
% From E: 
% 
% '->'(
%    holds(
%       standing(Agent), 
%       Time), 
%    initiates_at(
%       sitOn(Agent,Physobj), 
%       sittingOn(Agent,Physobj), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',590).
if(not(initiates(sitOn(Agent,Physobj),
		 at(sittingOn(Agent,Physobj),Time))),
   not(holds(standing(Agent),Time))).


% [agent,physobj,time]
% Terminates(SitOn(agent,physobj),
%            Standing(agent),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',596).
% From E: 
% 
% terminates_at(
%    sitOn(Agent,Physobj), 
%    standing(Agent), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',596).
terminates(sitOn(Agent,Physobj),standing(Agent)).


% [agent,physobj,time]
% (HoldsAt(SittingOn(agent,physobj),time) |
%  HoldsAt(LyingOn(agent,physobj),time)) ->
% Initiates(RiseFrom(agent,physobj),
%           Standing(agent),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',601).
% From E: 
% 
% '->'(
%    ';'(
%       holds(
%          sittingOn(Agent,Physobj), 
%          Time), 
%       holds(
%          lyingOn(Agent,Physobj), 
%          Time)), 
%    initiates_at(
%       riseFrom(Agent,Physobj), 
%       standing(Agent), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',601).
if(not(initiates(riseFrom(Agent, Physobj), at(standing(Agent), Time))),  (not(holds(sittingOn(Agent, Physobj), Time)), not(holds(lyingOn(Agent, Physobj), Time)))).


% [agent,physobj,time]
% HoldsAt(LyingOn(agent,physobj),time) ->
% Terminates(RiseFrom(agent,physobj),
%            LyingOn(agent,physobj),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',608).
% From E: 
% 
% '->'(
%    holds(
%       lyingOn(Agent,Physobj), 
%       Time), 
%    terminates_at(
%       riseFrom(Agent,Physobj), 
%       lyingOn(Agent,Physobj), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',608).
if(not(terminates(riseFrom(Agent,Physobj),
		  at(lyingOn(Agent,Physobj),Time))),
   not(holds(lyingOn(Agent,Physobj),Time))).


% [agent,physobj,time]
% HoldsAt(SittingOn(agent,physobj),time) ->
% Terminates(RiseFrom(agent,physobj),
%            SittingOn(agent,physobj),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',614).
% From E: 
% 
% '->'(
%    holds(
%       sittingOn(Agent,Physobj), 
%       Time), 
%    terminates_at(
%       riseFrom(Agent,Physobj), 
%       sittingOn(Agent,Physobj), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',614).
if(not(terminates(riseFrom(Agent,Physobj),
		  at(sittingOn(Agent,Physobj),Time))),
   not(holds(sittingOn(Agent,Physobj),Time))).

% event Greet(agent,agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',618).
% From E: 
% 
% event(greet(agent,agent)).
events([greet/2]).
mpred_prop(greet(agent, agent), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',618).
actions([greet/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',621).
% event SayGoodbye(agent,agent)
% From E: 
% 
% event(sayGoodbye(agent,agent)).
events([sayGoodbye/2]).
mpred_prop(sayGoodbye(agent, agent), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',621).
actions([sayGoodbye/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',623).
% [agent1,agent2,time]
% Happens(Greet(agent1,agent2),time) ->
% {location}% 
% HoldsAt(At(agent1,location),time) &
% HoldsAt(At(agent2,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',625).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          greet(Agent1,Agent2), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent1,Location), 
%             Time), 
%          holds(
%             at_loc(Agent2,Location), 
%             Time)))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',625).
exists(Location,  (at_loc(Agent1, Location), at_loc(Agent2, Location);not(greet(Agent1, Agent2)))).


% [agent1,agent2,time]
% Happens(SayGoodbye(agent1,agent2),time) ->
% {location}% 
% HoldsAt(At(agent1,location),time) &
% HoldsAt(At(agent2,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',630).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          sayGoodbye(Agent1,Agent2), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent1,Location), 
%             Time), 
%          holds(
%             at_loc(Agent2,Location), 
%             Time)))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',630).
exists(Location,  (at_loc(Agent1, Location), at_loc(Agent2, Location);not(sayGoodbye(Agent1, Agent2)))).

% event Order(agent,agent,physobj)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',634).
% From E: 
% 
% event(order(agent,agent,physobj)).
events([order/3]).
mpred_prop(order(agent, agent, physobj), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',634).
actions([order/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',637).
% fluent KnowOrder(agent,agent,physobj)
% From E: 
% 
% fluent(knowOrder(agent,agent,physobj)).
mpred_prop(knowOrder(agent, agent, physobj), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',637).
fluents([knowOrder/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',639).
% [agent1,agent2,physobj,time]
% Initiates(Order(agent1,agent2,physobj),
%           KnowOrder(agent2,agent1,physobj),
%           time).
% From E: 
% 
% initiates_at(
%    order(Agent1,Agent2,Physobj), 
%    knowOrder(Agent2,Agent1,Physobj), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',639).
initiates(order(Agent1,Agent2,Physobj),
	  knowOrder(Agent2,Agent1,Physobj)).


% [agent1,agent2,physobj,time]
% Happens(Order(agent1,agent2,physobj),time) ->
% {location}% 
% HoldsAt(At(agent1,location),time) &
% HoldsAt(At(agent2,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',645).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          order(Agent1,Agent2,Physobj), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent1,Location), 
%             Time), 
%          holds(
%             at_loc(Agent2,Location), 
%             Time)))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',645).
exists(Location,  (at_loc(Agent1, Location), at_loc(Agent2, Location);not(order(Agent1, Agent2, Physobj)))).

% event Request(agent,agent,physobj)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',649).
% From E: 
% 
% event(request(agent,agent,physobj)).
events([request/3]).
mpred_prop(request(agent, agent, physobj), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',649).
actions([request/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',652).
% fluent KnowRequest(agent,agent,physobj)
% From E: 
% 
% fluent(knowRequest(agent,agent,physobj)).
mpred_prop(knowRequest(agent, agent, physobj), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',652).
fluents([knowRequest/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',654).
% [agent1,agent2,physobj,time]
% Initiates(Request(agent1,agent2,physobj),
%           KnowRequest(agent2,agent1,physobj),
%           time).
% From E: 
% 
% initiates_at(
%    request(Agent1,Agent2,Physobj), 
%    knowRequest(Agent2,Agent1,Physobj), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',654).
initiates(request(Agent1,Agent2,Physobj),
	  knowRequest(Agent2,Agent1,Physobj)).


% [agent1,agent2,physobj,time]
% Happens(Request(agent1,agent2,physobj),time) ->
% {location}% 
% HoldsAt(At(agent1,location),time) &
% HoldsAt(At(agent2,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',660).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          request(Agent1,Agent2,Physobj), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent1,Location), 
%             Time), 
%          holds(
%             at_loc(Agent2,Location), 
%             Time)))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',660).
exists(Location,  (at_loc(Agent1, Location), at_loc(Agent2, Location);not(request(Agent1, Agent2, Physobj)))).


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.e',664).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/RepRest.lps.pl')).
