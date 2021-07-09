% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Cognition.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Cognition.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Book.e',87).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Cognition.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Cognition.lps.pl')).
% Fri, 26 Mar 2021 01:05:56 GMT File: <stream>(0x555567a68200)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;

% event Read(agent,text,content)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Cognition.e',11).
% From E: 
% 
% event(read(agent,text,content)).
events([read/3]).
mpred_prop(read(agent, text, content), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Cognition.e',11).
actions([read/3]).

% event ThinkAbout(agent,content)
% From E: 
% 
% event(thinkAbout(agent,content)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Cognition.e',11).
events([thinkAbout/2]).
mpred_prop(thinkAbout(agent, content), action).
actions([thinkAbout/2]).

% event Think(agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Cognition.e',13).
% From E: 
% 
% event(think(agent)).
events([think/1]).
mpred_prop(think(agent), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Cognition.e',13).
actions([think/1]).

% event Understand(agent,content)
% From E: 
% 
% event(understand(agent,content)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Cognition.e',13).
events([understand/2]).
mpred_prop(understand(agent, content), action).
actions([understand/2]).

% event Dream(agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Cognition.e',15).
% From E: 
% 
% event(dream(agent)).
events([dream/1]).
mpred_prop(dream(agent), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Cognition.e',15).
actions([dream/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Cognition.e',17).
% [agent,text,content,time]
% Happens(Read(agent,text,content),time) ->
% HoldsAt(See(agent,text),time).
% From E: 
% 
% '->'(
%    happens(
%       read(Agent,Text,Content), 
%       Time), 
%    holds(
%       see(Agent,Text), 
%       Time)).
(   see(Agent, Text)at Time
;   not happens(read(Agent, Text, Content), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Cognition.e',17).

 /*   (   at(see(Agent, Text), Time)
        ;   not(happens(read(Agent, Text, Content), Time))
        ).
 */
 %  % =================================.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Cognition.e',20).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Cognition.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Cognition.lps.pl')).
