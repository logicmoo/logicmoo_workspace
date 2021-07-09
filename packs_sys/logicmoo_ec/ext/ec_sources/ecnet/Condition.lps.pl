% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Cognition.e',20).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.lps.pl')).
% Fri, 26 Mar 2021 01:05:56 GMT File: <stream>(0x55556759a400)


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
%; human health

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',13).
% fluent Alive(agent)
% From E: 
% 
% fluent(alive(agent)).
mpred_prop(alive(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',13).
fluents([alive/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',15).
% fluent Dead(agent)
% From E: 
% 
% fluent(dead(agent)).
mpred_prop(dead(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',15).
fluents([dead/1]).

% noninertial Dead
% From E: 
% 
% ':-'(call_pel_directive(noninertial(dead))).
:- call_pel_directive(noninertial(dead)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',18).
% fluent Injured(agent)
% From E: 
% 
% fluent(injured(agent)).
mpred_prop(injured(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',18).
fluents([injured/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',20).
% event Kill(object,agent)
% From E: 
% 
% event(kill(object,agent)).
mpred_prop(kill(object, agent), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',20).
events([kill/2]).

% event Injure(object,agent)
% From E: 
% 
% event(injure(object,agent)).
mpred_prop(injure(object, agent), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',20).
events([injure/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',22).
% event HealInjured(agent)
% From E: 
% 
% event(healInjured(agent)).
events([healInjured/1]).
mpred_prop(healInjured(agent), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',22).
actions([healInjured/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',24).
% [agent,time]
 % HoldsAt(Alive(agent),time) <-> !HoldsAt(Dead(agent),time).
% From E: 
% 
% <->(
%    holds(
%       alive(Agent), 
%       Time), 
%    holds(
%       not(dead(Agent)), 
%       Time)).
dead(Agent)at Time if not alive(Agent)at Time.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',24).

 /*  l_int(holds(dead(Agent),Time),
           [holds(not(alive(Agent)),Time)]).
 */
 %  % =================================.
(   alive(Agent)at Time
;   dead(Agent)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',24).

 /*   (   at(alive(Agent), Time)
        ;   at(dead(Agent), Time)
        ).
 */
 %  % =================================.


% [agent,time]
 % HoldsAt(Injured(agent),time) -> HoldsAt(Alive(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',24).
% From E: 
% 
% '->'(
%    holds(
%       injured(Agent), 
%       Time), 
%    holds(
%       alive(Agent), 
%       Time)).
(   alive(Agent)at Time
;   not injured(Agent)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',24).

 /*   (   at(alive(Agent), Time)
        ;   at(not(injured(Agent)), Time)
        ).
 */
 %  % =================================.


% [object,agent,time]
% Terminates(Kill(object,agent),Alive(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',28).
% From E: 
% 
% terminates_at(
%    kill(Object,Agent), 
%    alive(Agent), 
%    Time).
kill(Object, Agent)terminates alive(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',28).

 /*  terminated(happens(kill(Object,Agent),
     		   Time_from,
     		   Time_until),
     	   alive(Agent),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',30).
% [object,agent,time]
% Initiates(Injure(object,agent),Injured(agent),time).
% From E: 
% 
% initiates_at(
%    injure(Object,Agent), 
%    injured(Agent), 
%    Time).
injure(Object, Agent)initiates injured(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',30).

 /*  initiated(happens(injure(Object,Agent),
     		  Time_from,
     		  Time_until),
     	  injured(Agent),
     	  []).
 */
 %  % =================================.


% [agent,time]
% Terminates(HealInjured(agent),Injured(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',34).
% From E: 
% 
% terminates_at(
%    healInjured(Agent), 
%    injured(Agent), 
%    Time).
healInjured(Agent)terminates injured(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',34).

 /*  terminated(happens(healInjured(Agent),
     		   Time_from,
     		   Time_until),
     	   injured(Agent),
     	   []).
 */
 %  % =================================.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',36).
% fluent Intact(physobj)
% From E: 
% 
% fluent(intact(physobj)).
mpred_prop(intact(physobj), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',36).
fluents([intact/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',38).
% fluent Damaged(physobj)
% From E: 
% 
% fluent(damaged(physobj)).
mpred_prop(damaged(physobj), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',38).
fluents([damaged/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',40).
% fluent Destroyed(physobj)
% From E: 
% 
% fluent(destroyed(physobj)).
mpred_prop(destroyed(physobj), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',40).
fluents([destroyed/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',42).
%; At any time, a physical object is either intact, damaged, or destroyed:

% xor Intact, Damaged, Destroyed
% From E: 
% 
% xor([intact,damaged,destroyed]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',42).
xor([intact,damaged,destroyed]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',45).
% event Damage(object,physobj)
% From E: 
% 
% event(damage(object,physobj)).
mpred_prop(damage(object, physobj), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',45).
events([damage/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',47).
% event Destroy(object,physobj)
% From E: 
% 
% event(destroy(object,physobj)).
mpred_prop(destroy(object, physobj), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',47).
events([destroy/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',49).
% event Repair(object,physobj)
% From E: 
% 
% event(repair(object,physobj)).
mpred_prop(repair(object, physobj), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',49).
events([repair/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',51).
% [object,physobj,time]
% Happens(Damage(object,physobj),time) ->
% HoldsAt(Intact(physobj),time).
% From E: 
% 
% '->'(
%    happens(
%       damage(Object,Physobj), 
%       Time), 
%    holds(
%       intact(Physobj), 
%       Time)).
(   intact(Physobj)at Time
;   not happens(damage(Object, Physobj), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',51).

 /*   (   at(intact(Physobj), Time)
        ;   not(happens(damage(Object, Physobj), Time))
        ).
 */
 %  % =================================.


% [object,physobj,time]
% Initiates(Damage(object,physobj),Damaged(physobj),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',56).
% From E: 
% 
% initiates_at(
%    damage(Object,Physobj), 
%    damaged(Physobj), 
%    Time).
damage(Object, Physobj)initiates damaged(Physobj).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',56).

 /*  initiated(happens(damage(Object,Physobj),
     		  Time_from,
     		  Time_until),
     	  damaged(Physobj),
     	  []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',58).
% [object,physobj,time]
% Terminates(Damage(object,physobj),Intact(physobj),time).
% From E: 
% 
% terminates_at(
%    damage(Object,Physobj), 
%    intact(Physobj), 
%    Time).
damage(Object, Physobj)terminates intact(Physobj).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',58).

 /*  terminated(happens(damage(Object,Physobj),
     		   Time_from,
     		   Time_until),
     	   intact(Physobj),
     	   []).
 */
 %  % =================================.


% [object,physobj,time]
% Happens(Destroy(object,physobj),time) ->
% (HoldsAt(Intact(physobj),time)|
%  HoldsAt(Damaged(physobj),time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',62).
% From E: 
% 
% '->'(
%    happens(
%       destroy(Object,Physobj), 
%       Time), 
%    ';'(
%       holds(
%          intact(Physobj), 
%          Time), 
%       holds(
%          damaged(Physobj), 
%          Time))).
(   (   intact(Physobj)at Time
    ;   damaged(Physobj)at Time
    )
;   not happens(destroy(Object, Physobj), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',62).

 /*   (   (   at(intact(Physobj), Time)
            ;   at(damaged(Physobj), Time)
            )
        ;   not(happens(destroy(Object, Physobj), Time))
        ).
 */
 %  % =================================.


% [object,physobj,time]
% Initiates(Destroy(object,physobj),Destroyed(physobj),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',67).
% From E: 
% 
% initiates_at(
%    destroy(Object,Physobj), 
%    destroyed(Physobj), 
%    Time).
destroy(Object, Physobj)initiates destroyed(Physobj).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',67).

 /*  initiated(happens(destroy(Object,Physobj),
     		  Time_from,
     		  Time_until),
     	  destroyed(Physobj),
     	  []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',69).
% [object,physobj,time]
% Terminates(Destroy(object,physobj),Intact(physobj),time).
% From E: 
% 
% terminates_at(
%    destroy(Object,Physobj), 
%    intact(Physobj), 
%    Time).
destroy(Object, Physobj)terminates intact(Physobj).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',69).

 /*  terminated(happens(destroy(Object,Physobj),
     		   Time_from,
     		   Time_until),
     	   intact(Physobj),
     	   []).
 */
 %  % =================================.


% [object,physobj,time]
% Terminates(Destroy(object,physobj),Damaged(physobj),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',73).
% From E: 
% 
% terminates_at(
%    destroy(Object,Physobj), 
%    damaged(Physobj), 
%    Time).
destroy(Object, Physobj)terminates damaged(Physobj).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',73).

 /*  terminated(happens(destroy(Object,Physobj),
     		   Time_from,
     		   Time_until),
     	   damaged(Physobj),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',75).
% [object,physobj,time]
% Initiates(Repair(object,physobj),Intact(physobj),time).
% From E: 
% 
% initiates_at(
%    repair(Object,Physobj), 
%    intact(Physobj), 
%    Time).
repair(Object, Physobj)initiates intact(Physobj).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',75).

 /*  initiated(happens(repair(Object,Physobj),
     		  Time_from,
     		  Time_until),
     	  intact(Physobj),
     	  []).
 */
 %  % =================================.


%; end of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',77).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.lps.pl')).
