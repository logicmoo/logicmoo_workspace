% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',30).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.lps.pl')).
% Fri, 26 Mar 2021 01:06:00 GMT File: <stream>(0x555567dd7d00)


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
%; hunger need
%;

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',14).
% fluent Hungry(agent)
% From E: 
% 
% fluent(hungry(agent)).
mpred_prop(hungry(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',14).
fluents([hungry/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',16).
% fluent Satiated(agent)
% From E: 
% 
% fluent(satiated(agent)).
mpred_prop(satiated(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',16).
fluents([satiated/1]).

% noninertial Satiated
% From E: 
% 
% ':-'(call_pel_directive(noninertial(satiated))).
:- call_pel_directive(noninertial(satiated)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',19).
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
satiated(Agent)at Time if not hungry(Agent)at Time.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',19).

 /*  l_int(holds(satiated(Agent),Time),
           [holds(not(hungry(Agent)),Time)]).
 */
 %  % =================================.
(   hungry(Agent)at Time
;   satiated(Agent)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',19).

 /*   (   at(hungry(Agent), Time)
        ;   at(satiated(Agent), Time)
        ).
 */
 %  % =================================.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',21).
% event Eat(agent,food)
% From E: 
% 
% event(eat(agent,food)).
events([eat/2]).
mpred_prop(eat(agent, food), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',21).
actions([eat/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',23).
% [agent,food,time]
% Happens(Eat(agent,food),time) ->
% {location}% 
% HoldsAt(At(agent,location),time) &
% HoldsAt(At(food,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',25).
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
exists(Location,  (at_loc(Agent, Location)at Time, at_loc(Food, Location)at Time;not happens(eat(Agent, Food), Time))).
 %  exists(Location,  (at(at_loc(Agent, Location), Time), at(at_loc(Food, Location), Time);not(happens(eat(Agent, Food), Time)))).
 %  % =================================.


% [agent,food,time]
% Terminates(Eat(agent,food),Hungry(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',30).
% From E: 
% 
% terminates_at(
%    eat(Agent,Food), 
%    hungry(Agent), 
%    Time).
eat(Agent, Food)terminates hungry(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',30).

 /*  terminated(happens(eat(Agent,Food),
     		   Time_from,
     		   Time_until),
     	   hungry(Agent),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',32).
%; End of file.
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.lps.pl')).
