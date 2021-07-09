% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',82).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.lps.pl')).
% Fri, 26 Mar 2021 01:06:00 GMT File: <stream>(0x555567a68d00)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;

% event HandTo(agent,agent,physobj)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',12).
% From E: 
% 
% event(handTo(agent,agent,physobj)).
events([handTo/3]).
mpred_prop(handTo(agent, agent, physobj), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',12).
actions([handTo/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',14).
% [agent1,agent2,physobj,time]
% Initiates(HandTo(agent1,agent2,physobj),
%           Holding(agent2,physobj),
%           time).
% From E: 
% 
% initiates_at(
%    handTo(Agent1,Agent2,Physobj), 
%    holding(Agent2,Physobj), 
%    Time).
handTo(Agent1, Agent2, Physobj)initiates holding(Agent2, Physobj).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',14).

 /*  initiated(happens(handTo(Agent1,Agent2,Physobj),
     		  Time_from,
     		  Time_until),
     	  holding(Agent2,Physobj),
     	  []).
 */
 %  % =================================.


% [agent1,agent2,physobj,time]
% Terminates(HandTo(agent1,agent2,physobj),
%            Holding(agent1,physobj),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',20).
% From E: 
% 
% terminates_at(
%    handTo(Agent1,Agent2,Physobj), 
%    holding(Agent1,Physobj), 
%    Time).
handTo(Agent1, Agent2, Physobj)terminates holding(Agent1, Physobj).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',20).

 /*  terminated(happens(handTo(Agent1,Agent2,Physobj),
     		   Time_from,
     		   Time_until),
     	   holding(Agent1,Physobj),
     	   []).
 */
 %  % =================================.


% [agent1,agent2,physobj,time]
% Happens(HandTo(agent1,agent2,physobj),time) ->
% HoldsAt(Holding(agent1,physobj),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',25).
% From E: 
% 
% '->'(
%    happens(
%       handTo(Agent1,Agent2,Physobj), 
%       Time), 
%    holds(
%       holding(Agent1,Physobj), 
%       Time)).
(   holding(Agent1, Physobj)at Time
;   not happens(handTo(Agent1, Agent2, Physobj), Time)
).

 /*   (   at(holding(Agent1, Physobj), Time)
        ;   not(happens(handTo(Agent1, Agent2, Physobj), Time))
        ).
 */
 %  % =================================.

% event ShakeHands(agent,agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',27).
% From E: 
% 
% event(shakeHands(agent,agent)).
events([shakeHands/2]).
mpred_prop(shakeHands(agent, agent), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',27).
actions([shakeHands/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',30).
% event WriteOn(agent,paper,pen)
% From E: 
% 
% event(writeOn(agent,paper,pen)).
events([writeOn/3]).
mpred_prop(writeOn(agent, paper, pen), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.e',30).
actions([writeOn/3]).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HandTo.lps.pl')).
