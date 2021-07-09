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
% Fri, 26 Mar 2021 01:05:58 GMT File: <stream>(0x555567a69500)%;
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

% event PutOn(agent,clothing)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',15).
% From E: 
% 
% event(putOn(agent,clothing)).
events([putOn/2]).
mpred_prop(putOn(agent, clothing), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',15).
actions([putOn/2]).

% event TakeOff(agent,clothing)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',17).
% From E: 
% 
% event(takeOff(agent,clothing)).
events([takeOff/2]).
mpred_prop(takeOff(agent, clothing), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',17).
actions([takeOff/2]).

% fluent Wearing(agent,clothing)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',19).
% From E: 
% 
% fluent(wearing(agent,clothing)).
mpred_prop(wearing(agent, clothing), fluent).
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
putOn(Agent, Clothing)initiates wearing(Agent, Clothing).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',21).

 /*  initiated(happens(putOn(Agent,Clothing),
     		  Time_from,
     		  Time_until),
     	  wearing(Agent,Clothing),
     	  []).
 */
 %  % =================================.


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
exists(Location,  (not wearing(Agent, Clothing)at Time, at_loc(Agent, Location)at Time, at_loc(Clothing, Location)at Time;not happens(putOn(Agent, Clothing), Time))).
 %  exists(Location,  (at(not(wearing(Agent, Clothing)), Time), at(at_loc(Agent, Location), Time), at(at_loc(Clothing, Location), Time);not(happens(putOn(Agent, Clothing), Time)))).
 %  % =================================.


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
takeOff(Agent, Clothing)terminates wearing(Agent, Clothing).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',33).

 /*  terminated(happens(takeOff(Agent,Clothing),
     		   Time_from,
     		   Time_until),
     	   wearing(Agent,Clothing),
     	   []).
 */
 %  % =================================.


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
(   wearing(Agent, Clothing)at Time
;   not happens(takeOff(Agent, Clothing), Time)
).

 /*   (   at(wearing(Agent, Clothing), Time)
        ;   not(happens(takeOff(Agent, Clothing), Time))
        ).
 */
 %  % =================================.


% [agent,clothing,location,time]
% Releases(PutOn(agent,clothing),At(clothing,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',42).
% From E: 
% 
% releases_at(
%    putOn(Agent,Clothing), 
%    at_loc(Clothing,Location), 
%    Time).
releases(putOn(Agent, Clothing), at_loc(Clothing, Location)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',42).

 /*  releases(putOn(Agent,Clothing),
     	 at_loc(Clothing,Location)).
 */
 %  % =================================.


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
(   at_loc(Clothing, Location)at Time
;   not wearing(Agent, Clothing)at Time
;   not at_loc(Agent, Location)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',44).

 /*   (   at(at_loc(Clothing, Location), Time)
        ;   at(not(wearing(Agent, Clothing)), Time)
        ;   at(not(at_loc(Agent, Location)), Time)
        ).
 */
 %  % =================================.


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
(   initiates(takeOff(Agent, Clothing),
              at_loc(Clothing, Location)at Time)
;   not at_loc(Agent, Location)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',54).

 /*   (   initiates(takeOff(Agent, Clothing),
                      at(at_loc(Clothing, Location), Time))
        ;   at(not(at_loc(Agent, Location)), Time)
        ).
 */
 %  % =================================.


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.e',57).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Dress.lps.pl')).
