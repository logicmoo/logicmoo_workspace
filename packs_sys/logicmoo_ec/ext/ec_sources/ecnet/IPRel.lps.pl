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
% Fri, 26 Mar 2021 01:06:00 GMT File: <stream>(0x555567a68200)%;
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

% fluent FriendOf(agent,agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',14).
% From E: 
% 
% fluent(friendOf(agent,agent)).
mpred_prop(friendOf(agent, agent), fluent).
fluents([friendOf/2]).

% fluent NeutralOf(agent,agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',16).
% From E: 
% 
% fluent(neutralOf(agent,agent)).
mpred_prop(neutralOf(agent, agent), fluent).
fluents([neutralOf/2]).

% fluent EnemyOf(agent,agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',18).
% From E: 
% 
% fluent(enemyOf(agent,agent)).
mpred_prop(enemyOf(agent, agent), fluent).
fluents([enemyOf/2]).

% event BecomeFriends(agent,agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',20).
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

% event BecomeEnemies(agent,agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',22).
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
enemyOf(Agent1, Agent2)at Time if not friendOf(Agent1, Agent2)at Time.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',24).

 /*  l_int(holds(enemyOf(Agent1,Agent2),Time),
           [holds(not(friendOf(Agent1,Agent2)),Time)]).
 */
 %  % =================================.


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
enemyOf(Agent1, Agent2)at Time if not neutralOf(Agent1, Agent2)at Time.

 /*  l_int(holds(enemyOf(Agent1,Agent2),Time),
           [holds(not(neutralOf(Agent1,Agent2)),Time)]).
 */
 %  % =================================.


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
(   friendOf(Agent2, Agent1)at Time
;   not friendOf(Agent1, Agent2)at Time
).

 /*   (   at(friendOf(Agent2, Agent1), Time)
        ;   at(not(friendOf(Agent1, Agent2)), Time)
        ).
 */
 %  % =================================.


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
(   neutralOf(Agent2, Agent1)at Time
;   not neutralOf(Agent1, Agent2)at Time
).

 /*   (   at(neutralOf(Agent2, Agent1), Time)
        ;   at(not(neutralOf(Agent1, Agent2)), Time)
        ).
 */
 %  % =================================.


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
(   enemyOf(Agent2, Agent1)at Time
;   not enemyOf(Agent1, Agent2)at Time
).

 /*   (   at(enemyOf(Agent2, Agent1), Time)
        ;   at(not(enemyOf(Agent1, Agent2)), Time)
        ).
 */
 %  % =================================.


% [agent1,agent2,time]
% Initiates(BecomeFriends(agent1,agent2),FriendOf(agent1,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',45).
% From E: 
% 
% initiates_at(
%    becomeFriends(Agent1,Agent2), 
%    friendOf(Agent1,Agent2), 
%    Time).
becomeFriends(Agent1, Agent2)initiates friendOf(Agent1, Agent2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',45).

 /*  initiated(happens(becomeFriends(Agent1,Agent2),
     		  Time_from,
     		  Time_until),
     	  friendOf(Agent1,Agent2),
     	  []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',47).
% [agent1,agent2,time]
% Initiates(BecomeFriends(agent1,agent2),FriendOf(agent2,agent1),time).
% From E: 
% 
% initiates_at(
%    becomeFriends(Agent1,Agent2), 
%    friendOf(Agent2,Agent1), 
%    Time).
becomeFriends(Agent1, Agent2)initiates friendOf(Agent2, Agent1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',47).

 /*  initiated(happens(becomeFriends(Agent1,Agent2),
     		  Time_from,
     		  Time_until),
     	  friendOf(Agent2,Agent1),
     	  []).
 */
 %  % =================================.


% [agent1,agent2,time]
% Terminates(BecomeFriends(agent1,agent2),NeutralOf(agent1,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',51).
% From E: 
% 
% terminates_at(
%    becomeFriends(Agent1,Agent2), 
%    neutralOf(Agent1,Agent2), 
%    Time).
becomeFriends(Agent1, Agent2)terminates neutralOf(Agent1, Agent2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',51).

 /*  terminated(happens(becomeFriends(Agent1,Agent2),
     		   Time_from,
     		   Time_until),
     	   neutralOf(Agent1,Agent2),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',53).
% [agent1,agent2,time]
% Terminates(BecomeFriends(agent1,agent2),NeutralOf(agent2,agent1),time).
% From E: 
% 
% terminates_at(
%    becomeFriends(Agent1,Agent2), 
%    neutralOf(Agent2,Agent1), 
%    Time).
becomeFriends(Agent1, Agent2)terminates neutralOf(Agent2, Agent1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',53).

 /*  terminated(happens(becomeFriends(Agent1,Agent2),
     		   Time_from,
     		   Time_until),
     	   neutralOf(Agent2,Agent1),
     	   []).
 */
 %  % =================================.


% [agent1,agent2,time]
% Terminates(BecomeFriends(agent1,agent2),EnemyOf(agent1,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',57).
% From E: 
% 
% terminates_at(
%    becomeFriends(Agent1,Agent2), 
%    enemyOf(Agent1,Agent2), 
%    Time).
becomeFriends(Agent1, Agent2)terminates enemyOf(Agent1, Agent2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',57).

 /*  terminated(happens(becomeFriends(Agent1,Agent2),
     		   Time_from,
     		   Time_until),
     	   enemyOf(Agent1,Agent2),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',59).
% [agent1,agent2,time]
% Terminates(BecomeFriends(agent1,agent2),EnemyOf(agent2,agent1),time).
% From E: 
% 
% terminates_at(
%    becomeFriends(Agent1,Agent2), 
%    enemyOf(Agent2,Agent1), 
%    Time).
becomeFriends(Agent1, Agent2)terminates enemyOf(Agent2, Agent1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',59).

 /*  terminated(happens(becomeFriends(Agent1,Agent2),
     		   Time_from,
     		   Time_until),
     	   enemyOf(Agent2,Agent1),
     	   []).
 */
 %  % =================================.


% [agent1,agent2,time]
% Initiates(BecomeEnemies(agent1,agent2),EnemyOf(agent1,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',63).
% From E: 
% 
% initiates_at(
%    becomeEnemies(Agent1,Agent2), 
%    enemyOf(Agent1,Agent2), 
%    Time).
becomeEnemies(Agent1, Agent2)initiates enemyOf(Agent1, Agent2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',63).

 /*  initiated(happens(becomeEnemies(Agent1,Agent2),
     		  Time_from,
     		  Time_until),
     	  enemyOf(Agent1,Agent2),
     	  []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',65).
% [agent1,agent2,time]
% Initiates(BecomeEnemies(agent1,agent2),EnemyOf(agent2,agent1),time).
% From E: 
% 
% initiates_at(
%    becomeEnemies(Agent1,Agent2), 
%    enemyOf(Agent2,Agent1), 
%    Time).
becomeEnemies(Agent1, Agent2)initiates enemyOf(Agent2, Agent1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',65).

 /*  initiated(happens(becomeEnemies(Agent1,Agent2),
     		  Time_from,
     		  Time_until),
     	  enemyOf(Agent2,Agent1),
     	  []).
 */
 %  % =================================.


% [agent1,agent2,time]
% Terminates(BecomeEnemies(agent1,agent2),NeutralOf(agent1,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',69).
% From E: 
% 
% terminates_at(
%    becomeEnemies(Agent1,Agent2), 
%    neutralOf(Agent1,Agent2), 
%    Time).
becomeEnemies(Agent1, Agent2)terminates neutralOf(Agent1, Agent2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',69).

 /*  terminated(happens(becomeEnemies(Agent1,Agent2),
     		   Time_from,
     		   Time_until),
     	   neutralOf(Agent1,Agent2),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',71).
% [agent1,agent2,time]
% Terminates(BecomeEnemies(agent1,agent2),NeutralOf(agent2,agent1),time).
% From E: 
% 
% terminates_at(
%    becomeEnemies(Agent1,Agent2), 
%    neutralOf(Agent2,Agent1), 
%    Time).
becomeEnemies(Agent1, Agent2)terminates neutralOf(Agent2, Agent1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',71).

 /*  terminated(happens(becomeEnemies(Agent1,Agent2),
     		   Time_from,
     		   Time_until),
     	   neutralOf(Agent2,Agent1),
     	   []).
 */
 %  % =================================.


% [agent1,agent2,time]
% Terminates(BecomeEnemies(agent1,agent2),FriendOf(agent1,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',75).
% From E: 
% 
% terminates_at(
%    becomeEnemies(Agent1,Agent2), 
%    friendOf(Agent1,Agent2), 
%    Time).
becomeEnemies(Agent1, Agent2)terminates friendOf(Agent1, Agent2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',75).

 /*  terminated(happens(becomeEnemies(Agent1,Agent2),
     		   Time_from,
     		   Time_until),
     	   friendOf(Agent1,Agent2),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',77).
% [agent1,agent2,time]
% Terminates(BecomeEnemies(agent1,agent2),FriendOf(agent2,agent1),time).
% From E: 
% 
% terminates_at(
%    becomeEnemies(Agent1,Agent2), 
%    friendOf(Agent2,Agent1), 
%    Time).
becomeEnemies(Agent1, Agent2)terminates friendOf(Agent2, Agent1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',77).

 /*  terminated(happens(becomeEnemies(Agent1,Agent2),
     		   Time_from,
     		   Time_until),
     	   friendOf(Agent2,Agent1),
     	   []).
 */
 %  % =================================.


% [agent1,agent2,time]
% Initiates(BecomeNeutral(agent1,agent2),NeutralOf(agent1,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',81).
% From E: 
% 
% initiates_at(
%    becomeNeutral(Agent1,Agent2), 
%    neutralOf(Agent1,Agent2), 
%    Time).
becomeNeutral(Agent1, Agent2)initiates neutralOf(Agent1, Agent2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',81).

 /*  initiated(happens(becomeNeutral(Agent1,Agent2),
     		  Time_from,
     		  Time_until),
     	  neutralOf(Agent1,Agent2),
     	  []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',83).
% [agent1,agent2,time]
% Initiates(BecomeNeutral(agent1,agent2),NeutralOf(agent2,agent1),time).
% From E: 
% 
% initiates_at(
%    becomeNeutral(Agent1,Agent2), 
%    neutralOf(Agent2,Agent1), 
%    Time).
becomeNeutral(Agent1, Agent2)initiates neutralOf(Agent2, Agent1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',83).

 /*  initiated(happens(becomeNeutral(Agent1,Agent2),
     		  Time_from,
     		  Time_until),
     	  neutralOf(Agent2,Agent1),
     	  []).
 */
 %  % =================================.


% [agent1,agent2,time]
% Terminates(BecomeNeutral(agent1,agent2),FriendOf(agent1,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',87).
% From E: 
% 
% terminates_at(
%    becomeNeutral(Agent1,Agent2), 
%    friendOf(Agent1,Agent2), 
%    Time).
becomeNeutral(Agent1, Agent2)terminates friendOf(Agent1, Agent2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',87).

 /*  terminated(happens(becomeNeutral(Agent1,Agent2),
     		   Time_from,
     		   Time_until),
     	   friendOf(Agent1,Agent2),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',89).
% [agent1,agent2,time]
% Terminates(BecomeNeutral(agent1,agent2),FriendOf(agent2,agent1),time).
% From E: 
% 
% terminates_at(
%    becomeNeutral(Agent1,Agent2), 
%    friendOf(Agent2,Agent1), 
%    Time).
becomeNeutral(Agent1, Agent2)terminates friendOf(Agent2, Agent1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',89).

 /*  terminated(happens(becomeNeutral(Agent1,Agent2),
     		   Time_from,
     		   Time_until),
     	   friendOf(Agent2,Agent1),
     	   []).
 */
 %  % =================================.


% [agent1,agent2,time]
% Terminates(BecomeNeutral(agent1,agent2),EnemyOf(agent1,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',93).
% From E: 
% 
% terminates_at(
%    becomeNeutral(Agent1,Agent2), 
%    enemyOf(Agent1,Agent2), 
%    Time).
becomeNeutral(Agent1, Agent2)terminates enemyOf(Agent1, Agent2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',93).

 /*  terminated(happens(becomeNeutral(Agent1,Agent2),
     		   Time_from,
     		   Time_until),
     	   enemyOf(Agent1,Agent2),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',95).
% [agent1,agent2,time]
% Terminates(BecomeNeutral(agent1,agent2),EnemyOf(agent2,agent1),time).
% From E: 
% 
% terminates_at(
%    becomeNeutral(Agent1,Agent2), 
%    enemyOf(Agent2,Agent1), 
%    Time).
becomeNeutral(Agent1, Agent2)terminates enemyOf(Agent2, Agent1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',95).

 /*  terminated(happens(becomeNeutral(Agent1,Agent2),
     		   Time_from,
     		   Time_until),
     	   enemyOf(Agent2,Agent1),
     	   []).
 */
 %  % =================================.


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
(   like(Agent1, Agent2)at Time
;   not friendOf(Agent1, Agent2)at Time
).

 /*   (   at(like(Agent1, Agent2), Time)
        ;   at(not(friendOf(Agent1, Agent2)), Time)
        ).
 */
 %  % =================================.


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
(   dislike(Agent1, Agent2)at Time
;   not enemyOf(Agent1, Agent2)at Time
).

 /*   (   at(dislike(Agent1, Agent2), Time)
        ;   at(not(enemyOf(Agent1, Agent2)), Time)
        ).
 */
 %  % =================================.

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
acquaintanceOf(Agent, Agent)at Time.
 %  l_int(holds(acquaintanceOf(Agent,Agent),Time),[]).
 %  % =================================.


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
(   acquaintanceOf(Agent2, Agent1)at Time
;   not acquaintanceOf(Agent1, Agent2)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',110).

 /*   (   at(acquaintanceOf(Agent2, Agent1), Time)
        ;   at(not(acquaintanceOf(Agent1, Agent2)), Time)
        ).
 */
 %  % =================================.

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
introduce(Agent1, Agent2)initiates acquaintanceOf(Agent1, Agent2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',116).

 /*  initiated(happens(introduce(Agent1,Agent2),
     		  Time_from,
     		  Time_until),
     	  acquaintanceOf(Agent1,Agent2),
     	  []).
 */
 %  % =================================.


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
introduce(Agent1, Agent2)initiates acquaintanceOf(Agent2, Agent1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',122).

 /*  initiated(happens(introduce(Agent1,Agent2),
     		  Time_from,
     		  Time_until),
     	  acquaintanceOf(Agent2,Agent1),
     	  []).
 */
 %  % =================================.

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
introduceMutual(Agent1, Agent2, Agent3)initiates acquaintanceOf(Agent2, Agent3).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',128).

 /*  initiated(happens(introduceMutual(Agent1,
     				  Agent2,
     				  Agent3),
     		  Time_from,
     		  Time_until),
     	  acquaintanceOf(Agent2,Agent3),
     	  []).
 */
 %  % =================================.


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
introduceMutual(Agent1, Agent2, Agent3)initiates acquaintanceOf(Agent3, Agent2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',134).

 /*  initiated(happens(introduceMutual(Agent1,
     				  Agent2,
     				  Agent3),
     		  Time_from,
     		  Time_until),
     	  acquaintanceOf(Agent3,Agent2),
     	  []).
 */
 %  % =================================.


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
(   acquaintanceOf(Agent1, Agent2)at Time,
    acquaintanceOf(Agent1, Agent3)at Time
;   not(happens(introduceMutual(Agent1, Agent2, Agent3),
                Time))
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',139).

 /*  (   at(acquaintanceOf(Agent1, Agent2), Time),
         at(acquaintanceOf(Agent1, Agent3), Time)
     ;   not(happens(introduceMutual(Agent1, Agent2, Agent3),
                     Time))
     ).
 */
 %  % =================================.


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.e',142).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/IPRel.lps.pl')).
