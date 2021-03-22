% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/HungerNeed.e',32).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.lps.pl')).
% Sun, 21 Mar 2021 23:28:10 GMT File: <stream>(0x55556865cf00)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; IPRel: interpersonal relations
%;

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',14).
% fluent FriendOf(agent,agent)
% From E: 
% 
% fluent(friendOf(agent,agent)).
mpred_prop(friendOf(agent, agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',14).
fluents([friendOf/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',16).
% fluent NeutralOf(agent,agent)
% From E: 
% 
% fluent(neutralOf(agent,agent)).
mpred_prop(neutralOf(agent, agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',16).
fluents([neutralOf/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',18).
% fluent EnemyOf(agent,agent)
% From E: 
% 
% fluent(enemyOf(agent,agent)).
mpred_prop(enemyOf(agent, agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',18).
fluents([enemyOf/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',20).
% event BecomeFriends(agent,agent)
% From E: 
% 
% event(becomeFriends(agent,agent)).
events([becomeFriends/2]).
mpred_prop(becomeFriends(agent, agent), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',20).
actions([becomeFriends/2]).

% event BecomeNeutral(agent,agent)
% From E: 
% 
% event(becomeNeutral(agent,agent)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',20).
events([becomeNeutral/2]).
mpred_prop(becomeNeutral(agent, agent), action).
actions([becomeNeutral/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',22).
% event BecomeEnemies(agent,agent)
% From E: 
% 
% event(becomeEnemies(agent,agent)).
events([becomeEnemies/2]).
mpred_prop(becomeEnemies(agent, agent), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',22).
actions([becomeEnemies/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',24).
% [agent1,agent2,time]
% HoldsAt(FriendOf(agent1,agent2),time) ->
% !Holds(EnemyOf(agent1,agent2),time).
% From E: 
% 
% '->'(
%    holds(
%       friendOf(Agent1,Agent2), 
%       Time), 
%    holds(
%       not(enemyOf(Agent1,Agent2)), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',24).
if(enemyOf(Agent1,Agent2),
   not(friendOf(Agent1,Agent2))).


% [agent1,agent2,time]
% HoldsAt(NeutralOf(agent1,agent2),time) ->
% !Holds(EnemyOf(agent1,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',29).
% From E: 
% 
% '->'(
%    holds(
%       neutralOf(Agent1,Agent2), 
%       Time), 
%    holds(
%       not(enemyOf(Agent1,Agent2)), 
%       Time)).
 %   [Time].
if(enemyOf(Agent1,Agent2),
   not(neutralOf(Agent1,Agent2))).


% [agent1,agent2,time]
% HoldsAt(FriendOf(agent1,agent2),time) ->
% HoldsAt(FriendOf(agent2,agent1),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',33).
% From E: 
% 
% '->'(
%    holds(
%       friendOf(Agent1,Agent2), 
%       Time), 
%    holds(
%       friendOf(Agent2,Agent1), 
%       Time)).
 %   [Time].
if(not(friendOf(Agent2,Agent1)),
   not(friendOf(Agent1,Agent2))).


% [agent1,agent2,time]
% HoldsAt(NeutralOf(agent1,agent2),time) ->
% HoldsAt(NeutralOf(agent2,agent1),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',37).
% From E: 
% 
% '->'(
%    holds(
%       neutralOf(Agent1,Agent2), 
%       Time), 
%    holds(
%       neutralOf(Agent2,Agent1), 
%       Time)).
 %   [Time].
if(not(neutralOf(Agent2,Agent1)),
   not(neutralOf(Agent1,Agent2))).


% [agent1,agent2,time]
% HoldsAt(EnemyOf(agent1,agent2),time) ->
% HoldsAt(EnemyOf(agent2,agent1),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',41).
% From E: 
% 
% '->'(
%    holds(
%       enemyOf(Agent1,Agent2), 
%       Time), 
%    holds(
%       enemyOf(Agent2,Agent1), 
%       Time)).
 %   [Time].
if(not(enemyOf(Agent2,Agent1)),
   not(enemyOf(Agent1,Agent2))).


% [agent1,agent2,time]
% Initiates(BecomeFriends(agent1,agent2),FriendOf(agent1,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',45).
% From E: 
% 
% initiates_at(
%    becomeFriends(Agent1,Agent2), 
%    friendOf(Agent1,Agent2), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',45).
initiates(becomeFriends(Agent1,Agent2),
	  friendOf(Agent1,Agent2)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',47).
% [agent1,agent2,time]
% Initiates(BecomeFriends(agent1,agent2),FriendOf(agent2,agent1),time).
% From E: 
% 
% initiates_at(
%    becomeFriends(Agent1,Agent2), 
%    friendOf(Agent2,Agent1), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',47).
initiates(becomeFriends(Agent1,Agent2),
	  friendOf(Agent2,Agent1)).


% [agent1,agent2,time]
% Terminates(BecomeFriends(agent1,agent2),NeutralOf(agent1,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',51).
% From E: 
% 
% terminates_at(
%    becomeFriends(Agent1,Agent2), 
%    neutralOf(Agent1,Agent2), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',51).
terminates(becomeFriends(Agent1,Agent2),
	   neutralOf(Agent1,Agent2)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',53).
% [agent1,agent2,time]
% Terminates(BecomeFriends(agent1,agent2),NeutralOf(agent2,agent1),time).
% From E: 
% 
% terminates_at(
%    becomeFriends(Agent1,Agent2), 
%    neutralOf(Agent2,Agent1), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',53).
terminates(becomeFriends(Agent1,Agent2),
	   neutralOf(Agent2,Agent1)).


% [agent1,agent2,time]
% Terminates(BecomeFriends(agent1,agent2),EnemyOf(agent1,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',57).
% From E: 
% 
% terminates_at(
%    becomeFriends(Agent1,Agent2), 
%    enemyOf(Agent1,Agent2), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',57).
terminates(becomeFriends(Agent1,Agent2),
	   enemyOf(Agent1,Agent2)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',59).
% [agent1,agent2,time]
% Terminates(BecomeFriends(agent1,agent2),EnemyOf(agent2,agent1),time).
% From E: 
% 
% terminates_at(
%    becomeFriends(Agent1,Agent2), 
%    enemyOf(Agent2,Agent1), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',59).
terminates(becomeFriends(Agent1,Agent2),
	   enemyOf(Agent2,Agent1)).


% [agent1,agent2,time]
% Initiates(BecomeEnemies(agent1,agent2),EnemyOf(agent1,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',63).
% From E: 
% 
% initiates_at(
%    becomeEnemies(Agent1,Agent2), 
%    enemyOf(Agent1,Agent2), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',63).
initiates(becomeEnemies(Agent1,Agent2),
	  enemyOf(Agent1,Agent2)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',65).
% [agent1,agent2,time]
% Initiates(BecomeEnemies(agent1,agent2),EnemyOf(agent2,agent1),time).
% From E: 
% 
% initiates_at(
%    becomeEnemies(Agent1,Agent2), 
%    enemyOf(Agent2,Agent1), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',65).
initiates(becomeEnemies(Agent1,Agent2),
	  enemyOf(Agent2,Agent1)).


% [agent1,agent2,time]
% Terminates(BecomeEnemies(agent1,agent2),NeutralOf(agent1,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',69).
% From E: 
% 
% terminates_at(
%    becomeEnemies(Agent1,Agent2), 
%    neutralOf(Agent1,Agent2), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',69).
terminates(becomeEnemies(Agent1,Agent2),
	   neutralOf(Agent1,Agent2)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',71).
% [agent1,agent2,time]
% Terminates(BecomeEnemies(agent1,agent2),NeutralOf(agent2,agent1),time).
% From E: 
% 
% terminates_at(
%    becomeEnemies(Agent1,Agent2), 
%    neutralOf(Agent2,Agent1), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',71).
terminates(becomeEnemies(Agent1,Agent2),
	   neutralOf(Agent2,Agent1)).


% [agent1,agent2,time]
% Terminates(BecomeEnemies(agent1,agent2),FriendOf(agent1,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',75).
% From E: 
% 
% terminates_at(
%    becomeEnemies(Agent1,Agent2), 
%    friendOf(Agent1,Agent2), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',75).
terminates(becomeEnemies(Agent1,Agent2),
	   friendOf(Agent1,Agent2)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',77).
% [agent1,agent2,time]
% Terminates(BecomeEnemies(agent1,agent2),FriendOf(agent2,agent1),time).
% From E: 
% 
% terminates_at(
%    becomeEnemies(Agent1,Agent2), 
%    friendOf(Agent2,Agent1), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',77).
terminates(becomeEnemies(Agent1,Agent2),
	   friendOf(Agent2,Agent1)).


% [agent1,agent2,time]
% Initiates(BecomeNeutral(agent1,agent2),NeutralOf(agent1,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',81).
% From E: 
% 
% initiates_at(
%    becomeNeutral(Agent1,Agent2), 
%    neutralOf(Agent1,Agent2), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',81).
initiates(becomeNeutral(Agent1,Agent2),
	  neutralOf(Agent1,Agent2)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',83).
% [agent1,agent2,time]
% Initiates(BecomeNeutral(agent1,agent2),NeutralOf(agent2,agent1),time).
% From E: 
% 
% initiates_at(
%    becomeNeutral(Agent1,Agent2), 
%    neutralOf(Agent2,Agent1), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',83).
initiates(becomeNeutral(Agent1,Agent2),
	  neutralOf(Agent2,Agent1)).


% [agent1,agent2,time]
% Terminates(BecomeNeutral(agent1,agent2),FriendOf(agent1,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',87).
% From E: 
% 
% terminates_at(
%    becomeNeutral(Agent1,Agent2), 
%    friendOf(Agent1,Agent2), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',87).
terminates(becomeNeutral(Agent1,Agent2),
	   friendOf(Agent1,Agent2)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',89).
% [agent1,agent2,time]
% Terminates(BecomeNeutral(agent1,agent2),FriendOf(agent2,agent1),time).
% From E: 
% 
% terminates_at(
%    becomeNeutral(Agent1,Agent2), 
%    friendOf(Agent2,Agent1), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',89).
terminates(becomeNeutral(Agent1,Agent2),
	   friendOf(Agent2,Agent1)).


% [agent1,agent2,time]
% Terminates(BecomeNeutral(agent1,agent2),EnemyOf(agent1,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',93).
% From E: 
% 
% terminates_at(
%    becomeNeutral(Agent1,Agent2), 
%    enemyOf(Agent1,Agent2), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',93).
terminates(becomeNeutral(Agent1,Agent2),
	   enemyOf(Agent1,Agent2)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',95).
% [agent1,agent2,time]
% Terminates(BecomeNeutral(agent1,agent2),EnemyOf(agent2,agent1),time).
% From E: 
% 
% terminates_at(
%    becomeNeutral(Agent1,Agent2), 
%    enemyOf(Agent2,Agent1), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',95).
terminates(becomeNeutral(Agent1,Agent2),
	   enemyOf(Agent2,Agent1)).


% [agent1,agent2,time]
% HoldsAt(FriendOf(agent1,agent2),time) ->
% HoldsAt(Like(agent1,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',99).
% From E: 
% 
% '->'(
%    holds(
%       friendOf(Agent1,Agent2), 
%       Time), 
%    holds(
%       like(Agent1,Agent2), 
%       Time)).
 %   [Time].
if(not(like(Agent1,Agent2)),
   not(friendOf(Agent1,Agent2))).


% [agent1,agent2,time]
% HoldsAt(EnemyOf(agent1,agent2),time) ->
% HoldsAt(Dislike(agent1,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',103).
% From E: 
% 
% '->'(
%    holds(
%       enemyOf(Agent1,Agent2), 
%       Time), 
%    holds(
%       dislike(Agent1,Agent2), 
%       Time)).
 %   [Time].
if(not(dislike(Agent1,Agent2)),
   not(enemyOf(Agent1,Agent2))).

% fluent AcquaintanceOf(agent,agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',105).
% From E: 
% 
% fluent(acquaintanceOf(agent,agent)).
mpred_prop(acquaintanceOf(agent, agent), fluent).
fluents([acquaintanceOf/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',108).
% [agent,time]
 % HoldsAt(AcquaintanceOf(agent,agent),time).
% From E: 
% 
% holds(
%    acquaintanceOf(Agent,Agent), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',108).
acquaintanceOf(Agent,Agent).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',110).
% [agent1,agent2,time]
% HoldsAt(AcquaintanceOf(agent1,agent2),time) ->
% HoldsAt(AcquaintanceOf(agent2,agent1),time).
% From E: 
% 
% '->'(
%    holds(
%       acquaintanceOf(Agent1,Agent2), 
%       Time), 
%    holds(
%       acquaintanceOf(Agent2,Agent1), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',110).
if(not(acquaintanceOf(Agent2,Agent1)),
   not(acquaintanceOf(Agent1,Agent2))).

% event Introduce(agent,agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',113).
% From E: 
% 
% event(introduce(agent,agent)).
events([introduce/2]).
mpred_prop(introduce(agent, agent), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',113).
actions([introduce/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',116).
% [agent1,agent2,time]
% Initiates(Introduce(agent1,agent2),
%           AcquaintanceOf(agent1,agent2),
%           time).
% From E: 
% 
% initiates_at(
%    introduce(Agent1,Agent2), 
%    acquaintanceOf(Agent1,Agent2), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',116).
initiates(introduce(Agent1,Agent2),
	  acquaintanceOf(Agent1,Agent2)).


% [agent1,agent2,time]
% Initiates(Introduce(agent1,agent2),
%           AcquaintanceOf(agent2,agent1),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',122).
% From E: 
% 
% initiates_at(
%    introduce(Agent1,Agent2), 
%    acquaintanceOf(Agent2,Agent1), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',122).
initiates(introduce(Agent1,Agent2),
	  acquaintanceOf(Agent2,Agent1)).

% event IntroduceMutual(agent,agent,agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',125).
% From E: 
% 
% event(introduceMutual(agent,agent,agent)).
events([introduceMutual/3]).
mpred_prop(introduceMutual(agent, agent, agent), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',125).
actions([introduceMutual/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',128).
% [agent1,agent2,agent3,time]
% Initiates(IntroduceMutual(agent1,agent2,agent3),
%           AcquaintanceOf(agent2,agent3),
%           time).
% From E: 
% 
% initiates_at(
%    introduceMutual(Agent1,Agent2,Agent3), 
%    acquaintanceOf(Agent2,Agent3), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',128).
initiates(introduceMutual(Agent1,Agent2,Agent3),
	  acquaintanceOf(Agent2,Agent3)).


% [agent1,agent2,agent3,time]
% Initiates(IntroduceMutual(agent1,agent2,agent3),
%           AcquaintanceOf(agent3,agent2),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',134).
% From E: 
% 
% initiates_at(
%    introduceMutual(Agent1,Agent2,Agent3), 
%    acquaintanceOf(Agent3,Agent2), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',134).
initiates(introduceMutual(Agent1,Agent2,Agent3),
	  acquaintanceOf(Agent3,Agent2)).


% [agent1,agent2,agent3,time]
% Happens(IntroduceMutual(agent1,agent2,agent3),time) ->
% HoldsAt(AcquaintanceOf(agent1,agent2),time) &
% HoldsAt(AcquaintanceOf(agent1,agent3),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',139).
% From E: 
% 
% '->'(
%    happens(
%       introduceMutual(Agent1,Agent2,Agent3), 
%       Time), 
%    ','(
%       holds(
%          acquaintanceOf(Agent1,Agent2), 
%          Time), 
%       holds(
%          acquaintanceOf(Agent1,Agent3), 
%          Time))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',139).
 if((not(acquaintanceOf(Agent1, Agent2));not(acquaintanceOf(Agent1, Agent3))),
      not(introduceMutual(Agent1, Agent2, Agent3))).


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',142).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.lps.pl')).
