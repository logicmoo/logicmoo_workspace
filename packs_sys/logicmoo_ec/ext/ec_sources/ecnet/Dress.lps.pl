% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',682).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.lps.pl')).
% Sun, 21 Mar 2021 23:28:08 GMT File: <stream>(0x5555686dbe00)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; Dress
%; (cf Sleep)
%;

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',15).
% event PutOn(agent,clothing)
% From E: 
% 
% event(putOn(agent,clothing)).
events([putOn/2]).
mpred_prop(putOn(agent, clothing), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',15).
actions([putOn/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',17).
% event TakeOff(agent,clothing)
% From E: 
% 
% event(takeOff(agent,clothing)).
events([takeOff/2]).
mpred_prop(takeOff(agent, clothing), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',17).
actions([takeOff/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',19).
% fluent Wearing(agent,clothing)
% From E: 
% 
% fluent(wearing(agent,clothing)).
mpred_prop(wearing(agent, clothing), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',19).
fluents([wearing/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',21).
% [agent,clothing,time]
% Initiates(PutOn(agent,clothing),
%           Wearing(agent,clothing),
%           time).
% From E: 
% 
% initiates_at(
%    putOn(Agent,Clothing), 
%    wearing(Agent,Clothing), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',21).
initiates(putOn(Agent,Clothing),
	  wearing(Agent,Clothing)).


% [agent,clothing,time]
% Happens(PutOn(agent,clothing),time) ->
% !HoldsAt(Wearing(agent,clothing),time) &
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',27).
% {location}%  HoldsAt(At(agent,location),time) &
%            HoldsAt(At(clothing,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',29).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          putOn(Agent,Clothing), 
%          Time), 
%       ','(
%          holds(
%             not(wearing(Agent,Clothing)), 
%             Time), 
%          ','(
%             holds(
%                at_loc(Agent,Location), 
%                Time), 
%             holds(
%                at_loc(Clothing,Location), 
%                Time))))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',29).
exists(Location,  (not(wearing(Agent, Clothing)), at_loc(Agent, Location), at_loc(Clothing, Location);not(putOn(Agent, Clothing)))).


% [agent,clothing,time]
% Terminates(TakeOff(agent,clothing),
%            Wearing(agent,clothing),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',33).
% From E: 
% 
% terminates_at(
%    takeOff(Agent,Clothing), 
%    wearing(Agent,Clothing), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',33).
terminates(takeOff(Agent,Clothing),
	   wearing(Agent,Clothing)).


% [agent,clothing,time]
% Happens(TakeOff(agent,clothing),time) ->
% HoldsAt(Wearing(agent,clothing),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',38).
% From E: 
% 
% '->'(
%    happens(
%       takeOff(Agent,Clothing), 
%       Time), 
%    holds(
%       wearing(Agent,Clothing), 
%       Time)).
 %   [Time].
if(not(wearing(Agent,Clothing)),
   not(takeOff(Agent,Clothing))).


% [agent,clothing,location,time]
% Releases(PutOn(agent,clothing),At(clothing,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',42).
% From E: 
% 
% releases_at(
%    putOn(Agent,Clothing), 
%    at_loc(Clothing,Location), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',42).
releases(putOn(Agent,Clothing),
	 at_loc(Clothing,Location)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',44).
% [agent,clothing,location,time]
% HoldsAt(Wearing(agent,clothing),time) &
% HoldsAt(At(agent,location),time) ->
% HoldsAt(At(clothing,location),time).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          wearing(Agent,Clothing), 
%          Time), 
%       holds(
%          at_loc(Agent,Location), 
%          Time)), 
%    holds(
%       at_loc(Clothing,Location), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',44).
 if(not(at_loc(Clothing, Location)),
       (not(wearing(Agent, Clothing));not(at_loc(Agent, Location)))).


%;[agent,clothing,location1,location2,time]
%;HoldsAt(At(agent,location1),time) &
%;location1 != location2 ->
%;Terminates(TakeOff(agent,clothing),At(clothing,location2),time).
% [agent,clothing,location,time]
% HoldsAt(At(agent,location),time) ->
% Initiates(TakeOff(agent,clothing),At(clothing,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',54).
% From E: 
% 
% '->'(
%    holds(
%       at_loc(Agent,Location), 
%       Time), 
%    initiates_at(
%       takeOff(Agent,Clothing), 
%       at_loc(Clothing,Location), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',54).
if(not(initiates(takeOff(Agent,Clothing),
		 at(at_loc(Clothing,Location),Time))),
   not(holds(at_loc(Agent,Location),Time))).


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',57).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.lps.pl')).
