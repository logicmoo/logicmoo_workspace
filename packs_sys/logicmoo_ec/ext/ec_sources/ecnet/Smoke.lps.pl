% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/SmallFire.e',61).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.lps.pl')).
% Sun, 21 Mar 2021 23:28:17 GMT File: <stream>(0x555567430900)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; Smoking: smoking cigarettes and cigars
%;

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.e',14).
% fluent CraveNicotine(agent)
% From E: 
% 
% fluent(craveNicotine(agent)).
mpred_prop(craveNicotine(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.e',14).
fluents([craveNicotine/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.e',16).
% fluent NicotineCravingSatisfied(agent)
% From E: 
% 
% fluent(nicotineCravingSatisfied(agent)).
mpred_prop(nicotineCravingSatisfied(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.e',16).
fluents([nicotineCravingSatisfied/1]).

% noninertial NicotineCravingSatisfied
% From E: 
% 
% ':-'(call_pel_directive(noninertial(nicotineCravingSatisfied))).
:- call_pel_directive(noninertial(nicotineCravingSatisfied)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.e',19).
% [agent,time]
% HoldsAt(CraveNicotine(agent),time) <->
% !HoldsAt(NicotineCravingSatisfied(agent),time).
% From E: 
% 
% <->(
%    holds(
%       craveNicotine(Agent), 
%       Time), 
%    holds(
%       not(nicotineCravingSatisfied(Agent)), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.e',19).
if(nicotineCravingSatisfied(Agent), not(craveNicotine(Agent))),
if(not(craveNicotine(Agent)), nicotineCravingSatisfied(Agent)).

% event Smoke(agent,cigarette)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.e',22).
% From E: 
% 
% event(smoke(agent,cigarette)).
events([smoke/2]).
mpred_prop(smoke(agent, cigarette), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.e',22).
actions([smoke/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.e',25).
% [agent,cigarette,time]
% Happens(Smoke(agent,cigarette),time) ->
% HoldsAt(Holding(agent,cigarette),time).
% From E: 
% 
% '->'(
%    happens(
%       smoke(Agent,Cigarette), 
%       Time), 
%    holds(
%       holding(Agent,Cigarette), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.e',25).
if(not(holding(Agent,Cigarette)),
   not(smoke(Agent,Cigarette))).


% [agent,cigarette,time]
% Terminates(Smoke(agent,cigarette),CraveNicotine(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.e',30).
% From E: 
% 
% terminates_at(
%    smoke(Agent,Cigarette), 
%    craveNicotine(Agent), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.e',30).
terminates(smoke(Agent,Cigarette),
	   craveNicotine(Agent)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.e',32).
% event Puff(agent,cigarette)
% From E: 
% 
% event(puff(agent,cigarette)).
events([puff/2]).
mpred_prop(puff(agent, cigarette), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.e',32).
actions([puff/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.e',34).
% [agent,cigarette,time]
% Happens(Puff(agent,cigarette),time) ->
% Happens(Smoke(agent,cigarette),time).
% From E: 
% 
% '->'(
%    happens(
%       puff(Agent,Cigarette), 
%       Time), 
%    happens(
%       smoke(Agent,Cigarette), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.e',34).
if(not(smoke(Agent,Cigarette)),
   not(puff(Agent,Cigarette))).

% event BlowOutSmoke(agent,smoke)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.e',37).
% From E: 
% 
% event(blowOutSmoke(agent,smoke)).
events([blowOutSmoke/2]).
mpred_prop(blowOutSmoke(agent, smoke), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.e',37).
actions([blowOutSmoke/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.e',40).
%; End of file.
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Smoke.lps.pl')).
