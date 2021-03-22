% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/EatingInAHouse.e',183).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.lps.pl')).
% Sun, 21 Mar 2021 23:28:09 GMT File: <stream>(0x555567bce300)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; feeling = emotion, attitude, ...
%;
%; The Feeling representation includes simple positive, neutral, and
%; negative emotions, and positive, neutral, and negative attitudes
%; toward objects.
%;
%; emotions
%; agent is happy.

% fluent Happy(agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',20).
% From E: 
% 
% fluent(happy(agent)).
mpred_prop(happy(agent), fluent).
fluents([happy/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',23).
%; agent is emotionally neutral or calm.

% fluent Calm(agent)
% From E: 
% 
% fluent(calm(agent)).
mpred_prop(calm(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',23).
fluents([calm/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',26).
%; agent is unhappy.

% fluent Unhappy(agent)
% From E: 
% 
% fluent(unhappy(agent)).
mpred_prop(unhappy(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',26).
fluents([unhappy/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',29).
%; At any moment, an agent is in one of three emotional states:

% xor Happy, Calm, Unhappy
% From E: 
% 
% xor([happy,calm,unhappy]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',29).
xor([happy,calm,unhappy]).
%; agent becomes happy.

% event BecomeHappy(agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',32).
% From E: 
% 
% event(becomeHappy(agent)).
events([becomeHappy/1]).
mpred_prop(becomeHappy(agent), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',32).
actions([becomeHappy/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',35).
%; agent becomes calm.

% event BecomeCalm(agent)
% From E: 
% 
% event(becomeCalm(agent)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',35).
events([becomeCalm/1]).
mpred_prop(becomeCalm(agent), action).
actions([becomeCalm/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',38).
%; agent becomes unhappy.

% event BecomeUnhappy(agent)
% From E: 
% 
% event(becomeUnhappy(agent)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',38).
events([becomeUnhappy/1]).
mpred_prop(becomeUnhappy(agent), action).
actions([becomeUnhappy/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',41).
%; A number of effect and precondition axioms deal with the transitions
%; from one emotional state to another:
% [agent,time]
% Initiates(BecomeHappy(agent),Happy(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',43).
% From E: 
% 
% initiates_at(
%    becomeHappy(Agent), 
%    happy(Agent), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',43).
initiates(becomeHappy(Agent),happy(Agent)).


% [agent,time]
% HoldsAt(Calm(agent),time) ->
% Terminates(BecomeHappy(agent),Calm(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',47).
% From E: 
% 
% '->'(
%    holds(
%       calm(Agent), 
%       Time), 
%    terminates_at(
%       becomeHappy(Agent), 
%       calm(Agent), 
%       Time)).
 %   [Time].
if(not(terminates(becomeHappy(Agent),
		  at(calm(Agent),Time))),
   not(holds(calm(Agent),Time))).


% [agent,time]
% HoldsAt(Unhappy(agent),time) ->
% Terminates(BecomeHappy(agent),Unhappy(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',51).
% From E: 
% 
% '->'(
%    holds(
%       unhappy(Agent), 
%       Time), 
%    terminates_at(
%       becomeHappy(Agent), 
%       unhappy(Agent), 
%       Time)).
 %   [Time].
if(not(terminates(becomeHappy(Agent),
		  at(unhappy(Agent),Time))),
   not(holds(unhappy(Agent),Time))).


% [agent,time]
% Happens(BecomeHappy(agent),time) ->
% !HoldsAt(Happy(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',55).
% From E: 
% 
% '->'(
%    happens(
%       becomeHappy(Agent), 
%       Time), 
%    holds(
%       not(happy(Agent)), 
%       Time)).
 %   [Time].
if(happy(Agent),not(becomeHappy(Agent))).


% [agent,time]
% Initiates(BecomeCalm(agent),Calm(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',59).
% From E: 
% 
% initiates_at(
%    becomeCalm(Agent), 
%    calm(Agent), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',59).
initiates(becomeCalm(Agent),calm(Agent)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',61).
% [agent,time]
% HoldsAt(Happy(agent),time) ->
% Terminates(BecomeCalm(agent),Happy(agent),time).
% From E: 
% 
% '->'(
%    holds(
%       happy(Agent), 
%       Time), 
%    terminates_at(
%       becomeCalm(Agent), 
%       happy(Agent), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',61).
if(not(terminates(becomeCalm(Agent),
		  at(happy(Agent),Time))),
   not(holds(happy(Agent),Time))).


% [agent,time]
% HoldsAt(Unhappy(agent),time) ->
% Terminates(BecomeCalm(agent),Unhappy(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',66).
% From E: 
% 
% '->'(
%    holds(
%       unhappy(Agent), 
%       Time), 
%    terminates_at(
%       becomeCalm(Agent), 
%       unhappy(Agent), 
%       Time)).
 %   [Time].
if(not(terminates(becomeCalm(Agent),
		  at(unhappy(Agent),Time))),
   not(holds(unhappy(Agent),Time))).


% [agent,time]
% Happens(BecomeCalm(agent),time) -> !HoldsAt(Calm(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',70).
% From E: 
% 
% '->'(
%    happens(
%       becomeCalm(Agent), 
%       Time), 
%    holds(
%       not(calm(Agent)), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',70).
if(calm(Agent),not(becomeCalm(Agent))).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',72).
% [agent,time]
% Initiates(BecomeUnhappy(agent),Unhappy(agent),time).
% From E: 
% 
% initiates_at(
%    becomeUnhappy(Agent), 
%    unhappy(Agent), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',72).
initiates(becomeUnhappy(Agent),unhappy(Agent)).


% [agent,time]
% HoldsAt(Happy(agent),time) ->
% Terminates(BecomeUnhappy(agent),Happy(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',76).
% From E: 
% 
% '->'(
%    holds(
%       happy(Agent), 
%       Time), 
%    terminates_at(
%       becomeUnhappy(Agent), 
%       happy(Agent), 
%       Time)).
 %   [Time].
if(not(terminates(becomeUnhappy(Agent),
		  at(happy(Agent),Time))),
   not(holds(happy(Agent),Time))).


% [agent,time]
% HoldsAt(Calm(agent),time) ->
% Terminates(BecomeUnhappy(agent),Calm(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',80).
% From E: 
% 
% '->'(
%    holds(
%       calm(Agent), 
%       Time), 
%    terminates_at(
%       becomeUnhappy(Agent), 
%       calm(Agent), 
%       Time)).
 %   [Time].
if(not(terminates(becomeUnhappy(Agent),
		  at(calm(Agent),Time))),
   not(holds(calm(Agent),Time))).


% [agent,time]
% Happens(BecomeUnhappy(agent),time) -> !HoldsAt(Unhappy(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',84).
% From E: 
% 
% '->'(
%    happens(
%       becomeUnhappy(Agent), 
%       Time), 
%    holds(
%       not(unhappy(Agent)), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',84).
if(unhappy(Agent),not(becomeUnhappy(Agent))).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',86).
%; anger

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',88).
% fluent AngryAt(agent,agent)
% From E: 
% 
% fluent(angryAt(agent,agent)).
mpred_prop(angryAt(agent, agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',88).
fluents([angryAt/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',90).
% event BecomeAngryAt(agent,agent)
% From E: 
% 
% event(becomeAngryAt(agent,agent)).
events([becomeAngryAt/2]).
mpred_prop(becomeAngryAt(agent, agent), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',90).
actions([becomeAngryAt/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',92).
% [agent1,agent2,time]
% Initiates(BecomeAngryAt(agent1,agent2),AngryAt(agent1,agent2),time).
% From E: 
% 
% initiates_at(
%    becomeAngryAt(Agent1,Agent2), 
%    angryAt(Agent1,Agent2), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',92).
initiates(becomeAngryAt(Agent1,Agent2),
	  angryAt(Agent1,Agent2)).


% [agent1,agent2,time]
% Terminates(BecomeHappy(agent1),AngryAt(agent1,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',96).
% From E: 
% 
% terminates_at(
%    becomeHappy(Agent1), 
%    angryAt(Agent1,Agent2), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',96).
terminates(becomeHappy(Agent1),
	   angryAt(Agent1,Agent2)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',98).
% [agent1,agent2,time]
% Happens(BecomeAngryAt(agent1,agent2),time) ->
% Happens(BecomeUnhappy(agent1),time).
% From E: 
% 
% '->'(
%    happens(
%       becomeAngryAt(Agent1,Agent2), 
%       Time), 
%    happens(
%       becomeUnhappy(Agent1), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',98).
if(not(becomeUnhappy(Agent1)),
   not(becomeAngryAt(Agent1,Agent2))).


%; attitudes
%; agent likes object.

% fluent Like(agent,object)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',104).
% From E: 
% 
% fluent(like(agent,object)).
mpred_prop(like(agent, object), fluent).
fluents([like/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',104).
%; agent loves object.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',107).
% fluent Love(agent,object)
% From E: 
% 
% fluent(love(agent,object)).
mpred_prop(love(agent, object), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',107).
fluents([love/2]).


%; agent dislikes object.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',109).
% fluent Dislike(agent,object)
% From E: 
% 
% fluent(dislike(agent,object)).
mpred_prop(dislike(agent, object), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',109).
fluents([dislike/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',111).
%; agent likes snow.

% fluent LikeSnow(agent)
% From E: 
% 
% fluent(likeSnow(agent)).
mpred_prop(likeSnow(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',111).
fluents([likeSnow/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',114).
%; A trigger axiom states that
%; if an agent is awake, likes snow, and is in a room that
%; looks out onto a location where it is snowing, that agent
%; becomes happy:
% [agent,room,outside,time]
% !HoldsAt(Happy(agent),time) &
% HoldsAt(Awake(agent),time) &
% HoldsAt(LikeSnow(agent),time) &
% HoldsAt(At(agent,room),time) &
% LookOutOnto(room)=outside &
% HoldsAt(Snowing(outside),time) ->
% Happens(BecomeHappy(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',118).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          not(happy(Agent)), 
%          Time), 
%       ','(
%          holds(
%             awake(Agent), 
%             Time), 
%          ','(
%             holds(
%                likeSnow(Agent), 
%                Time), 
%             ','(
%                holds(
%                   at_loc(Agent,Room), 
%                   Time), 
%                ','(
%                   '='(
%                      lookOutOnto(Room), 
%                      Outside), 
%                   holds(
%                      snowing(Outside), 
%                      Time)))))), 
%    happens(
%       becomeHappy(Agent), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',118).
 if(not(becomeHappy(Agent)),
       (happy(Agent);not(awake(Agent));not(likeSnow(Agent));not(at_loc(Agent, Room));not(lookOutOnto(Room, Outside));not(snowing(Outside)))).


%; We introduced LikeSnow above since Like
%; can only be used to represent that an agent likes a
%; particular object, not snow in general.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',131).
% event Smile(agent)
% From E: 
% 
% event(smile(agent)).
events([smile/1]).
mpred_prop(smile(agent), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',131).
actions([smile/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.e',133).
%; End of file.
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Feeling.lps.pl')).
