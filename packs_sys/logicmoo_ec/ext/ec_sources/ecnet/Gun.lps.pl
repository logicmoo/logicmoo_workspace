% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.e',70).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.lps.pl')).
% Sun, 21 Mar 2021 23:28:09 GMT File: <stream>(0x555567b43200)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',12).
% fluent Loaded(gun,bullet)
% From E: 
% 
% fluent(loaded(gun,bullet)).
mpred_prop(loaded(gun, bullet), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',12).
fluents([loaded/2]).

% noninertial Loaded
% From E: 
% 
% ':-'(call_pel_directive(noninertial(loaded))).
:- call_pel_directive(noninertial(loaded)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',15).
% [gun,bullet,time]
% HoldsAt(Inside(bullet,gun),time) <->
% HoldsAt(Loaded(gun,bullet),time).
% From E: 
% 
% <->(
%    holds(
%       inside(Bullet,Gun), 
%       Time), 
%    holds(
%       loaded(Gun,Bullet), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',15).
if(not(loaded(Gun, Bullet)), not(inside(Bullet, Gun))),
if(not(inside(Bullet, Gun)), not(loaded(Gun, Bullet))).

% event Shoot(agent,gun,object)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',18).
% From E: 
% 
% event(shoot(agent,gun,object)).
events([shoot/3]).
mpred_prop(shoot(agent, gun, object), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',18).
actions([shoot/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',21).
% event ShootInjure(agent,gun,agent)
% From E: 
% 
% event(shootInjure(agent,gun,agent)).
events([shootInjure/3]).
mpred_prop(shootInjure(agent, gun, agent), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',21).
actions([shootInjure/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',23).
% event ShootKill(agent,gun,agent)
% From E: 
% 
% event(shootKill(agent,gun,agent)).
events([shootKill/3]).
mpred_prop(shootKill(agent, gun, agent), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',23).
actions([shootKill/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',25).
% event ShootDamage(agent,gun,physobj)
% From E: 
% 
% event(shootDamage(agent,gun,physobj)).
events([shootDamage/3]).
mpred_prop(shootDamage(agent, gun, physobj), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',25).
actions([shootDamage/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',27).
% event ShootDestroy(agent,gun,physobj)
% From E: 
% 
% event(shootDestroy(agent,gun,physobj)).
events([shootDestroy/3]).
mpred_prop(shootDestroy(agent, gun, physobj), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',27).
actions([shootDestroy/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',29).
% [agent,gun,bullet,object,time]
% HoldsAt(Inside(bullet,gun),time) ->
% Terminates(Shoot(agent,gun,object),
%            Inside(bullet,gun),
%            time).
% From E: 
% 
% '->'(
%    holds(
%       inside(Bullet,Gun), 
%       Time), 
%    terminates_at(
%       shoot(Agent,Gun,Object), 
%       inside(Bullet,Gun), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',29).
if(not(terminates(shoot(Agent,Gun,Object),
		  at(inside(Bullet,Gun),Time))),
   not(holds(inside(Bullet,Gun),Time))).


% [agent,gun,bullet,object,location1,location2,time]
% HoldsAt(Inside(bullet,gun),time) &
% HoldsAt(At(gun,location1),time) &
% location1 != location2 ->
% Terminates(Shoot(agent,gun,object),At(bullet,location2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',36).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          inside(Bullet,Gun), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Gun,Location1), 
%             Time), 
%          Location1\=Location2)), 
%    terminates_at(
%       shoot(Agent,Gun,Object), 
%       at_loc(Bullet,Location2), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',36).
 if(not(terminates(shoot(Agent, Gun, Object),
                     at(at_loc(Bullet, Location2), Time))),
       (not(holds(inside(Bullet, Gun), Time));not(holds(at_loc(Gun, Location1), Time));not({dif(Location1, Location2)}))).


% [agent,gun,bullet,object,location,time]
% HoldsAt(At(object,location),time) &
% HoldsAt(Inside(bullet,gun),time) ->
% Initiates(Shoot(agent,gun,object),At(bullet,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',42).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          at_loc(Object,Location), 
%          Time), 
%       holds(
%          inside(Bullet,Gun), 
%          Time)), 
%    initiates_at(
%       shoot(Agent,Gun,Object), 
%       at_loc(Bullet,Location), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',42).
 if(not(initiates(shoot(Agent, Gun, Object),
                    at(at_loc(Bullet, Location), Time))),
       (not(holds(at_loc(Object, Location), Time));not(holds(inside(Bullet, Gun), Time)))).


% [agent,gun,object,time]
% Happens(Shoot(agent,gun,object),time) ->
% HoldsAt(Holding(agent,gun),time) &
% ({bullet} HoldsAt(Loaded(gun,bullet),time)) &
% ({location} HoldsAt(At(agent,location),time) &
%             HoldsAt(At(object,location),time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',47).
% From E: 
% 
% '->'(
%    happens(
%       shoot(Agent,Gun,Object), 
%       Time), 
%    ','(
%       holds(
%          holding(Agent,Gun), 
%          Time), 
%       ','(
%          thereExists(Bullet, 
%             holds(
%                loaded(Gun,Bullet), 
%                Time)), 
%          thereExists(Location, 
%             ','(
%                holds(
%                   at_loc(Agent,Location), 
%                   Time), 
%                holds(
%                   at_loc(Object,Location), 
%                   Time)))))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',47).
if((not(holding(Agent, Gun));not(thereExists(Bullet, loaded(Gun, Bullet)));not(thereExists(Location,  (at_loc(Agent, Location), at_loc(Object, Location))))), not(shoot(Agent, Gun, Object))).


% [agent1,gun,agent2,time]
% Happens(Shoot(agent1,gun,agent2),time) ->
% Happens(ShootInjure(agent1,gun,agent2),time) |
% Happens(ShootKill(agent1,gun,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',54).
% From E: 
% 
% '->'(
%    happens(
%       shoot(Agent1,Gun,Agent2), 
%       Time), 
%    ';'(
%       happens(
%          shootInjure(Agent1,Gun,Agent2), 
%          Time), 
%       happens(
%          shootKill(Agent1,Gun,Agent2), 
%          Time))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',54).
if((not(shootInjure(Agent1, Gun, Agent2)), not(shootKill(Agent1, Gun, Agent2))), not(shoot(Agent1, Gun, Agent2))).


% [agent1,gun,bullet,agent2,time]
% HoldsAt(Inside(bullet,gun),time) &
% Happens(ShootKill(agent1,gun,agent2),time) ->
% Happens(Kill(bullet,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',59).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          inside(Bullet,Gun), 
%          Time), 
%       happens(
%          shootKill(Agent1,Gun,Agent2), 
%          Time)), 
%    happens(
%       kill(Bullet,Agent2), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',59).
 if(not(kill(Bullet, Agent2)),
       (not(inside(Bullet, Gun));not(shootKill(Agent1, Gun, Agent2)))).


% [agent1,gun,bullet,agent2,time]
% HoldsAt(Inside(bullet,gun),time) &
% Happens(ShootInjure(agent1,gun,agent2),time) ->
% Happens(Injure(bullet,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',64).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          inside(Bullet,Gun), 
%          Time), 
%       happens(
%          shootInjure(Agent1,Gun,Agent2), 
%          Time)), 
%    happens(
%       injure(Bullet,Agent2), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',64).
 if(not(injure(Bullet, Agent2)),
       (not(inside(Bullet, Gun));not(shootInjure(Agent1, Gun, Agent2)))).


% [agent,gun,physobj,time]
% Happens(Shoot(agent,gun,physobj),time) ->
% Happens(ShootDamage(agent,gun,physobj),time) |
% Happens(ShootDestroy(agent,gun,physobj),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',69).
% From E: 
% 
% '->'(
%    happens(
%       shoot(Agent,Gun,Physobj), 
%       Time), 
%    ';'(
%       happens(
%          shootDamage(Agent,Gun,Physobj), 
%          Time), 
%       happens(
%          shootDestroy(Agent,Gun,Physobj), 
%          Time))).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',69).
if((not(shootDamage(Agent, Gun, Physobj)), not(shootDestroy(Agent, Gun, Physobj))), not(shoot(Agent, Gun, Physobj))).


% [agent,gun,bullet,physobj,time]
% HoldsAt(Inside(bullet,gun),time) &
% Happens(ShootDamage(agent,gun,physobj),time) ->
% Happens(Damage(bullet,physobj),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',74).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          inside(Bullet,Gun), 
%          Time), 
%       happens(
%          shootDamage(Agent,Gun,Physobj), 
%          Time)), 
%    happens(
%       damage(Bullet,Physobj), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',74).
 if(not(damage(Bullet, Physobj)),
       (not(inside(Bullet, Gun));not(shootDamage(Agent, Gun, Physobj)))).


% [agent,gun,bullet,physobj,time]
% HoldsAt(Inside(bullet,gun),time) &
% Happens(ShootDestroy(agent,gun,physobj),time) ->
% Happens(Destroy(bullet,physobj),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',79).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          inside(Bullet,Gun), 
%          Time), 
%       happens(
%          shootDestroy(Agent,Gun,Physobj), 
%          Time)), 
%    happens(
%       destroy(Bullet,Physobj), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',79).
 if(not(destroy(Bullet, Physobj)),
       (not(inside(Bullet, Gun));not(shootDestroy(Agent, Gun, Physobj)))).


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.e',82).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Gun.lps.pl')).
