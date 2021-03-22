% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',157).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.lps.pl')).
% Sun, 21 Mar 2021 23:28:06 GMT File: <stream>(0x5555671e8d00)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; bomb
%; agent is nondeterministically killed.

% fluent KilledDeterminingFluent(agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',13).
% From E: 
% 
% fluent(killedDeterminingFluent(agent)).
mpred_prop(killedDeterminingFluent(agent), fluent).
fluents([killedDeterminingFluent/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',15).
% noninertial KilledDeterminingFluent
% From E: 
% 
% ':-'(call_pel_directive(noninertial(killedDeterminingFluent))).
:- call_pel_directive(noninertial(killedDeterminingFluent)).
%; agent is nondeterministically injured.

% fluent InjuredDeterminingFluent(agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',17).
% From E: 
% 
% fluent(injuredDeterminingFluent(agent)).
mpred_prop(injuredDeterminingFluent(agent), fluent).
fluents([injuredDeterminingFluent/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',19).
% noninertial InjuredDeterminingFluent
% From E: 
% 
% ':-'(call_pel_directive(noninertial(injuredDeterminingFluent))).
:- call_pel_directive(noninertial(injuredDeterminingFluent)).
%; physobj is nondeterministically destroyed.

% fluent DestroyedDeterminingFluent(physobj)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',21).
% From E: 
% 
% fluent(destroyedDeterminingFluent(physobj)).
mpred_prop(destroyedDeterminingFluent(physobj), fluent).
fluents([destroyedDeterminingFluent/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',23).
% noninertial DestroyedDeterminingFluent
% From E: 
% 
% ':-'(call_pel_directive(noninertial(destroyedDeterminingFluent))).
:- call_pel_directive(noninertial(destroyedDeterminingFluent)).
%; physobj is nondeterministically damaged.

% fluent DamagedDeterminingFluent(physobj)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',25).
% From E: 
% 
% fluent(damagedDeterminingFluent(physobj)).
mpred_prop(damagedDeterminingFluent(physobj), fluent).
fluents([damagedDeterminingFluent/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',27).
% noninertial DamagedDeterminingFluent
% From E: 
% 
% ':-'(call_pel_directive(noninertial(damagedDeterminingFluent))).
:- call_pel_directive(noninertial(damagedDeterminingFluent)).
%; agent activates bomb.

% event BombActivate(agent,bomb)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',29).
% From E: 
% 
% event(bombActivate(agent,bomb)).
events([bombActivate/2]).
mpred_prop(bombActivate(agent, bomb), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',29).
actions([bombActivate/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',32).
%; agent deactivates bomb.

% event BombDeactivate(agent,bomb)
% From E: 
% 
% event(bombDeactivate(agent,bomb)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',32).
events([bombDeactivate/2]).
mpred_prop(bombDeactivate(agent, bomb), action).
actions([bombDeactivate/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',35).
%; bomb explodes.

% event BombExplode(bomb)
% From E: 
% 
% event(bombExplode(bomb)).
mpred_prop(bombExplode(bomb), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',35).
events([bombExplode/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',38).
%; bomb is activated.

% fluent BombActivated(bomb)
% From E: 
% 
% fluent(bombActivated(bomb)).
mpred_prop(bombActivated(bomb), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',38).
fluents([bombActivated/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',41).
%; The timer value of bomb is offset.

% fluent BombTimerValue(bomb,offset)
% From E: 
% 
% fluent(bombTimerValue(bomb,offset)).
mpred_prop(bombTimerValue(bomb, offset), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',41).
fluents([bombTimerValue/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',44).
%; The timer value of bomb is decremented.

% event BombDecrementTimer(bomb)
% From E: 
% 
% event(bombDecrementTimer(bomb)).
mpred_prop(bombDecrementTimer(bomb), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',44).
events([bombDecrementTimer/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',47).
%; The time delay of bomb is offset.

% function BombTimeDelay(bomb): offset
% From E: 
% 
% function(
%    bombTimeDelay(bomb), 
%    offset).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',47).
function(bombTimeDelay(bomb),offset).
%; A state constraint says that a bomb has one timer
%; value at a time:
% [bomb,offset1,offset2,time]
% HoldsAt(BombTimerValue(bomb,offset1),time) &
% HoldsAt(BombTimerValue(bomb,offset2),time) ->
% offset1=offset2.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',52).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          bombTimerValue(Bomb,Offset1), 
%          Time), 
%       holds(
%          bombTimerValue(Bomb,Offset2), 
%          Time)), 
%    Offset1=Offset2).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',52).
 if(not(equals(Offset1, Offset2)),
       (not(bombTimerValue(Bomb, Offset1));not(bombTimerValue(Bomb, Offset2)))).


%; An effect axiom states that if a bomb is intact and
%; an agent activates the bomb,
%; the bomb will be activated:
% [agent,bomb,time]
% HoldsAt(Intact(bomb),time) ->
% Initiates(BombActivate(agent,bomb),
%           BombActivated(bomb),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',60).
% From E: 
% 
% '->'(
%    holds(
%       intact(Bomb), 
%       Time), 
%    initiates_at(
%       bombActivate(Agent,Bomb), 
%       bombActivated(Bomb), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',60).
if(not(initiates(bombActivate(Agent,Bomb),
		 at(bombActivated(Bomb),Time))),
   not(holds(intact(Bomb),Time))).


%; A precondition axiom states that
%; for an agent to activate a bomb,
%; the agent must be holding the bomb:
% [agent,bomb,time]
% Happens(BombActivate(agent,bomb),time) ->
% HoldsAt(Holding(agent,bomb),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',69).
% From E: 
% 
% '->'(
%    happens(
%       bombActivate(Agent,Bomb), 
%       Time), 
%    holds(
%       holding(Agent,Bomb), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',69).
if(not(holding(Agent,Bomb)),
   not(bombActivate(Agent,Bomb))).


%; An effect axiom states that if a bomb is intact and
%; an agent deactivates the bomb,
%; the bomb will no longer be activated:
% [agent,bomb,time]
% HoldsAt(Intact(bomb),time) ->
% Terminates(BombDeactivate(agent,bomb),
%            BombActivated(bomb),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',76).
% From E: 
% 
% '->'(
%    holds(
%       intact(Bomb), 
%       Time), 
%    terminates_at(
%       bombDeactivate(Agent,Bomb), 
%       bombActivated(Bomb), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',76).
if(not(terminates(bombDeactivate(Agent,Bomb),
		  at(bombActivated(Bomb),Time))),
   not(holds(intact(Bomb),Time))).


%; An axiom states that if a bomb explodes, the
%; bomb destroys the bomb:
% [bomb,time]
% Happens(BombExplode(bomb),time) ->
% Happens(Destroy(bomb,bomb),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',84).
% From E: 
% 
% '->'(
%    happens(
%       bombExplode(Bomb), 
%       Time), 
%    happens(
%       destroy(Bomb,Bomb), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',84).
if(not(destroy(Bomb,Bomb)),not(bombExplode(Bomb))).


%; An effect axiom states that if a bomb explodes,
%; the bomb is no longer activated:
% [bomb,time]
% Terminates(BombExplode(bomb),BombActivated(bomb),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',90).
% From E: 
% 
% terminates_at(
%    bombExplode(Bomb), 
%    bombActivated(Bomb), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',90).
terminates(bombExplode(Bomb),bombActivated(Bomb)).


%; A trigger axiom states that
%; if a bomb is activated,
%; the timer value of the bomb is a timer value, and
%; the timer value is greater than zero,
%; the timer value of the bomb will be decremented:
% [bomb,offset,time]
% HoldsAt(BombActivated(bomb),time) &
% HoldsAt(BombTimerValue(bomb,offset),time) &
% (offset > 0) ->
% Happens(BombDecrementTimer(bomb),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',98).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          bombActivated(Bomb), 
%          Time), 
%       ','(
%          holds(
%             bombTimerValue(Bomb,Offset), 
%             Time), 
%          Offset>0)), 
%    happens(
%       bombDecrementTimer(Bomb), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',98).
 if(not(bombDecrementTimer(Bomb)),
       (not(bombActivated(Bomb));not(bombTimerValue(Bomb, Offset));not(comparison(Offset, 0, >)))).


%; An effect axiom states that
%; if the timer value of the bomb is a timer value and
%; the timer value of the bomb is decremented,
%; the timer value of the bomb will be the timer value minus one:
% [bomb,offset1,offset2,time]
% HoldsAt(BombTimerValue(bomb,offset1),time) &
% offset2 = offset1-1 ->
% Initiates(BombDecrementTimer(bomb),
%           BombTimerValue(bomb,offset2),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',108).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          bombTimerValue(Bomb,Offset1), 
%          Time), 
%       Offset2=Offset1-1), 
%    initiates_at(
%       bombDecrementTimer(Bomb), 
%       bombTimerValue(Bomb,Offset2), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',108).
 if(not(initiates(bombDecrementTimer(Bomb),
                    at(bombTimerValue(Bomb, Offset2), Time))),
       (not(holds(bombTimerValue(Bomb, Offset1), Time));not(equals(Offset2, Offset1-1)))).


%; An effect axiom states that
%; if the timer value of the bomb is a timer value and
%; the timer value of the bomb is decremented,
%; the timer value of the bomb will no longer be the timer value:
% [bomb,offset,time]
% HoldsAt(BombTimerValue(bomb,offset),time) ->
% Terminates(BombDecrementTimer(bomb),
%            BombTimerValue(bomb,offset),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',119).
% From E: 
% 
% '->'(
%    holds(
%       bombTimerValue(Bomb,Offset), 
%       Time), 
%    terminates_at(
%       bombDecrementTimer(Bomb), 
%       bombTimerValue(Bomb,Offset), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',119).
if(not(terminates(bombDecrementTimer(Bomb),
		  at(bombTimerValue(Bomb,Offset),
		     Time))),
   not(holds(bombTimerValue(Bomb,Offset),Time))).


%; An effect axiom states that if a bomb explodes,
%; the bomb will no longer be activated:
% [bomb,time]
% Terminates(BombExplode(bomb),BombActivated(bomb),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',127).
% From E: 
% 
% terminates_at(
%    bombExplode(Bomb), 
%    bombActivated(Bomb), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',127).
terminates(bombExplode(Bomb),bombActivated(Bomb)).


%; A trigger axiom states that if the timer value
%; of a bomb is zero, the bomb will explode:
% [bomb,time]
% HoldsAt(BombTimerValue(bomb,0),time) ->
% Happens(BombExplode(bomb),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',132).
% From E: 
% 
% '->'(
%    holds(
%       bombTimerValue(Bomb,0), 
%       Time), 
%    happens(
%       bombExplode(Bomb), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',132).
if(not(bombExplode(Bomb)),not(bombTimerValue(Bomb,0))).


%; An axiom states that if an agent is at a location,
%; a bomb is at the location,
%; the agent is nondeterministically injured, and
%; the bomb explodes, then
%; the bomb will injure the agent:
% [agent,location,bomb,time]
% HoldsAt(At(agent,location),time) &
% HoldsAt(At(bomb,location),time) &
% HoldsAt(InjuredDeterminingFluent(agent),time) &
% Happens(BombExplode(bomb),time) ->
% Happens(Injure(bomb,agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',141).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          at_loc(Agent,Location), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Bomb,Location), 
%             Time), 
%          ','(
%             holds(
%                injuredDeterminingFluent(Agent), 
%                Time), 
%             happens(
%                bombExplode(Bomb), 
%                Time)))), 
%    happens(
%       injure(Bomb,Agent), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',141).
 if(not(injure(Bomb, Agent)),
       (not(at_loc(Agent, Location));not(at_loc(Bomb, Location));not(injuredDeterminingFluent(Agent));not(bombExplode(Bomb)))).


%; An axiom states that if an agent is at a location,
%; a bomb is at the location,
%; the agent is nondeterministically killed, and
%; the bomb explodes, then
%; the bomb will kill the agent:
% [agent,location,bomb,time]
% HoldsAt(At(agent,location),time) &
% HoldsAt(At(bomb,location),time) &
% HoldsAt(KilledDeterminingFluent(agent),time) &
% Happens(BombExplode(bomb),time) ->
% Happens(Kill(bomb,agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',153).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          at_loc(Agent,Location), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Bomb,Location), 
%             Time), 
%          ','(
%             holds(
%                killedDeterminingFluent(Agent), 
%                Time), 
%             happens(
%                bombExplode(Bomb), 
%                Time)))), 
%    happens(
%       kill(Bomb,Agent), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',153).
 if(not(kill(Bomb, Agent)),
       (not(at_loc(Agent, Location));not(at_loc(Bomb, Location));not(killedDeterminingFluent(Agent));not(bombExplode(Bomb)))).


%; An axiom states that if an physical object is at a location,
%; a bomb is at the location,
%; the physical object is nondeterministically damaged, and
%; the bomb explodes, then
%; the bomb will damage the physical object:
% [physobj,location,bomb,time]
% HoldsAt(At(physobj,location),time) &
% HoldsAt(At(bomb,location),time) &
% HoldsAt(DamagedDeterminingFluent(physobj),time) &
% Happens(BombExplode(bomb),time) ->
% Happens(Damage(bomb,physobj),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',165).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          at_loc(Physobj,Location), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Bomb,Location), 
%             Time), 
%          ','(
%             holds(
%                damagedDeterminingFluent(Physobj), 
%                Time), 
%             happens(
%                bombExplode(Bomb), 
%                Time)))), 
%    happens(
%       damage(Bomb,Physobj), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',165).
 if(not(damage(Bomb, Physobj)),
       (not(at_loc(Physobj, Location));not(at_loc(Bomb, Location));not(damagedDeterminingFluent(Physobj));not(bombExplode(Bomb)))).


%; An axiom states that if an physical object is at a location,
%; a bomb is at the location,
%; the physical object is nondeterministically destroyed, and
%; the bomb explodes, then
%; the bomb will destroy the physical object:
% [physobj,location,bomb,time]
% HoldsAt(At(physobj,location),time) &
% HoldsAt(At(bomb,location),time) &
% HoldsAt(DestroyedDeterminingFluent(physobj),time) &
% Happens(BombExplode(bomb),time) ->
% Happens(Destroy(bomb,physobj),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',177).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          at_loc(Physobj,Location), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Bomb,Location), 
%             Time), 
%          ','(
%             holds(
%                destroyedDeterminingFluent(Physobj), 
%                Time), 
%             happens(
%                bombExplode(Bomb), 
%                Time)))), 
%    happens(
%       destroy(Bomb,Physobj), 
%       Time)).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',177).
 if(not(destroy(Bomb, Physobj)),
       (not(at_loc(Physobj, Location));not(at_loc(Bomb, Location));not(destroyedDeterminingFluent(Physobj));not(bombExplode(Bomb)))).


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.e',183).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Bomb.lps.pl')).
