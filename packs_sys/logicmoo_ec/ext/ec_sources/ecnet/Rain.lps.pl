% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',116).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.lps.pl')).
% Fri, 26 Mar 2021 01:06:04 GMT File: <stream>(0x555566f09900)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; Rain
%;
%; It starts raining at location outside.

% event StartRaining(outside)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.e',14).
% From E: 
% 
% event(startRaining(outside)).
mpred_prop(startRaining(outside), event).
events([startRaining/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.e',17).
%; It stops raining at location outside.

% event StopRaining(outside)
% From E: 
% 
% event(stopRaining(outside)).
mpred_prop(stopRaining(outside), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.e',17).
events([stopRaining/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.e',20).
%; It is raining at location outside.

% fluent Raining(outside)
% From E: 
% 
% fluent(raining(outside)).
mpred_prop(raining(outside), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.e',20).
fluents([raining/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.e',23).
% event GetWet(object)
% From E: 
% 
% event(getWet(object)).
mpred_prop(getWet(object), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.e',23).
events([getWet/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.e',25).
% event Dry(object)
% From E: 
% 
% event(dry(object)).
mpred_prop(dry(object), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.e',25).
events([dry/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.e',27).
% fluent Wet(object)
% From E: 
% 
% fluent(wet(object)).
mpred_prop(wet(object), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.e',27).
fluents([wet/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.e',29).
% [agent,outside,time]
% HoldsAt(At(agent,outside),time) &
% HoldsAt(Raining(outside),time) &
% !HoldsAt(Wet(agent),time) &
% (!{umbrella} HoldsAt(Holding(agent,umbrella),time)) ->
% Happens(GetWet(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.e',29).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          at_loc(Agent,Outside), 
%          Time), 
%       ','(
%          holds(
%             raining(Outside), 
%             Time), 
%          ','(
%             holds(
%                not(wet(Agent)), 
%                Time), 
%             not(thereExists(Umbrella, 
%                    holds(
%                       holding(Agent,Umbrella), 
%                       Time)))))), 
%    happens(
%       getWet(Agent), 
%       Time)).
(   happens(getWet(Agent), Time)
;   not at_loc(Agent, Outside)at Time
;   not raining(Outside)at Time
;   wet(Agent)at Time
;   thereExists(Umbrella,
                holding(Agent, Umbrella)at Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.e',29).

 /*   (   happens(getWet(Agent), Time)
        ;   at(not(at_loc(Agent, Outside)), Time)
        ;   at(not(raining(Outside)), Time)
        ;   at(wet(Agent), Time)
        ;   thereExists(Umbrella,
                        at(holding(Agent, Umbrella), Time))
        ).
 */
 %  % =================================.


% [object,time]
% Initiates(GetWet(object),Wet(object),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.e',37).
% From E: 
% 
% initiates_at(
%    getWet(Object), 
%    wet(Object), 
%    Time).
getWet(Object)initiates wet(Object).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.e',37).

 /*  initiated(happens(getWet(Object),
     		  Time_from,
     		  Time_until),
     	  wet(Object),
     	  []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.e',39).
% [object,time]
% Terminates(Dry(object),Wet(object),time).
% From E: 
% 
% terminates_at(
%    dry(Object), 
%    wet(Object), 
%    Time).
dry(Object)terminates wet(Object).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.e',39).

 /*  terminated(happens(dry(Object),Time_from,Time_until),
     	   wet(Object),
     	   []).
 */
 %  % =================================.


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.e',41).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Rain.lps.pl')).
