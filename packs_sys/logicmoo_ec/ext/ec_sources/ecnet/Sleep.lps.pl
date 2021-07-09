% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/ShootingAttack.e',107).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.lps.pl')).
% Fri, 26 Mar 2021 01:06:09 GMT File: <stream>(0x555567c82e00)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; The Sleep representation deals with the activity of sleeping and
%; body posture.
%; It is similar to the finite automaton representation of sleep
%; used in ThoughtTreasure \fullcite[chap. 7]{Mueller:1998}.
%;
%; @book{Mueller:1998,
%;   author = "Erik T. Mueller",
%;   year = "1998",
%;   title = "Natural Language Processing with \uppercase{T}hought\uppercase{T}reasure",
%;   address = "New York",
%;   publisher = "Signiform",
%; }
%;
%; sleep
%; agent wakes up.

% event WakeUp(agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',27).
% From E: 
% 
% event(wakeUp(agent)).
events([wakeUp/1]).
mpred_prop(wakeUp(agent), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',27).
actions([wakeUp/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',30).
%; agent gets tired.

% event GetTired(agent)
% From E: 
% 
% event(getTired(agent)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',30).
events([getTired/1]).
mpred_prop(getTired(agent), action).
actions([getTired/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',33).
%; agent falls asleep.

% event FallAsleep(agent)
% From E: 
% 
% event(fallAsleep(agent)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',33).
events([fallAsleep/1]).
mpred_prop(fallAsleep(agent), action).
actions([fallAsleep/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',36).
%; agent is asleep.

% fluent Sleep0(agent)
% From E: 
% 
% fluent(sleep0(agent)).
mpred_prop(sleep0(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',36).
fluents([sleep0/1]).


%; agent is awake and in bed.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',39).
% fluent Sleep1(agent)
% From E: 
% 
% fluent(sleep1(agent)).
mpred_prop(sleep1(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',39).
fluents([sleep1/1]).


%; agent is awake, out of bed, and undressed.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',41).
% fluent Sleep2(agent)
% From E: 
% 
% fluent(sleep2(agent)).
mpred_prop(sleep2(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',41).
fluents([sleep2/1]).


%; agent is awake and dressed.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',43).
% fluent Sleep3(agent)
% From E: 
% 
% fluent(sleep3(agent)).
mpred_prop(sleep3(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',43).
fluents([sleep3/1]).


%; agent is tired and dressed.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',45).
% fluent Sleep4(agent)
% From E: 
% 
% fluent(sleep4(agent)).
mpred_prop(sleep4(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',45).
fluents([sleep4/1]).


%; agent is tired and undressed.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',47).
% fluent Sleep5(agent)
% From E: 
% 
% fluent(sleep5(agent)).
mpred_prop(sleep5(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',47).
fluents([sleep5/1]).


%; agent is in bed, waiting to fall asleep.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',49).
% fluent Sleep6(agent)
% From E: 
% 
% fluent(sleep6(agent)).
mpred_prop(sleep6(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',49).
fluents([sleep6/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',51).
%; At any time, an agent is in one of seven sleep states:

% xor Sleep0, Sleep1, Sleep2, Sleep3, Sleep4, Sleep5, Sleep6
% From E: 
% 
% xor([sleep0,sleep1,sleep2,sleep3,sleep4,sleep5,sleep6]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',51).
xor([sleep0,sleep1,sleep2,sleep3,sleep4,sleep5,sleep6]).
%; constraints
%; agent is asleep.

% fluent Asleep(agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',56).
% From E: 
% 
% fluent(asleep(agent)).
mpred_prop(asleep(agent), fluent).
fluents([asleep/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',56).
%; agent is awake.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',59).
% fluent Awake(agent)
% From E: 
% 
% fluent(awake(agent)).
mpred_prop(awake(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',59).
fluents([awake/1]).

% noninertial Asleep
% From E: 
% 
% ':-'(call_pel_directive(noninertial(asleep))).
:- call_pel_directive(noninertial(asleep)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',61).
% noninertial Awake
% From E: 
% 
% ':-'(call_pel_directive(noninertial(awake))).
:- call_pel_directive(noninertial(awake)).
%; Sleep0 indicates that the agent is asleep:
% [agent,time]
 % HoldsAt(Asleep(agent),time) <-> HoldsAt(Sleep0(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',63).
% From E: 
% 
% <->(
%    holds(
%       asleep(Agent), 
%       Time), 
%    holds(
%       sleep0(Agent), 
%       Time)).
(   sleep0(Agent)at Time
;   not asleep(Agent)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',63).

 /*   (   at(sleep0(Agent), Time)
        ;   at(not(asleep(Agent)), Time)
        ).
 */
 %  % =================================.
(   asleep(Agent)at Time
;   not sleep0(Agent)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',63).

 /*   (   at(asleep(Agent), Time)
        ;   at(not(sleep0(Agent)), Time)
        ).
 */
 %  % =================================.


%; In all other sleep states, the agent is awake:
% [agent,time]
% HoldsAt(Awake(agent),time) <->
% HoldsAt(Sleep1(agent),time) |
% HoldsAt(Sleep2(agent),time) |
% HoldsAt(Sleep3(agent),time) |
% HoldsAt(Sleep4(agent),time) |
% HoldsAt(Sleep5(agent),time) |
% HoldsAt(Sleep6(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',67).
% From E: 
% 
% <->(
%    holds(
%       awake(Agent), 
%       Time), 
%    ';'(
%       holds(
%          sleep1(Agent), 
%          Time), 
%       ';'(
%          holds(
%             sleep2(Agent), 
%             Time), 
%          ';'(
%             holds(
%                sleep3(Agent), 
%                Time), 
%             ';'(
%                holds(
%                   sleep4(Agent), 
%                   Time), 
%                ';'(
%                   holds(
%                      sleep5(Agent), 
%                      Time), 
%                   holds(
%                      sleep6(Agent), 
%                      Time))))))).
(   (   sleep1(Agent)at Time
    ;   sleep2(Agent)at Time
    ;   sleep3(Agent)at Time
    ;   sleep4(Agent)at Time
    ;   sleep5(Agent)at Time
    ;   sleep6(Agent)at Time
    )
;   not awake(Agent)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',67).

 /*   (   (   at(sleep1(Agent), Time)
            ;   at(sleep2(Agent), Time)
            ;   at(sleep3(Agent), Time)
            ;   at(sleep4(Agent), Time)
            ;   at(sleep5(Agent), Time)
            ;   at(sleep6(Agent), Time)
            )
        ;   at(not(awake(Agent)), Time)
        ).
 */
 %  % =================================.
(   awake(Agent)at Time
;   not sleep1(Agent)at Time,
    not sleep2(Agent)at Time,
    not sleep3(Agent)at Time,
    not sleep4(Agent)at Time,
    not sleep5(Agent)at Time,
    not sleep6(Agent)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',67).

 /*  (   at(awake(Agent), Time)
     ;   at(not(sleep1(Agent)), Time),
         at(not(sleep2(Agent)), Time),
         at(not(sleep3(Agent)), Time),
         at(not(sleep4(Agent)), Time),
         at(not(sleep5(Agent)), Time),
         at(not(sleep6(Agent)), Time)
     ).
 */
 %  % =================================.


%; A number of axioms are used to specify the transitions of
%; a finite automaton.
%;--
%; Waking up causes a transition from Sleep0
%; to Sleep1:
% [agent,time]
 
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',82).
% Terminates(WakeUp(agent),Sleep0(agent),time).
% From E: 
% 
% terminates_at(
%    wakeUp(Agent), 
%    sleep0(Agent), 
%    Time).
wakeUp(Agent)terminates sleep0(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',82).

 /*  terminated(happens(wakeUp(Agent),
     		   Time_from,
     		   Time_until),
     	   sleep0(Agent),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',84).
% [agent,time]
 % Initiates(WakeUp(agent),Sleep1(agent),time).
% From E: 
% 
% initiates_at(
%    wakeUp(Agent), 
%    sleep1(Agent), 
%    Time).
wakeUp(Agent)initiates sleep1(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',84).

 /*  initiated(happens(wakeUp(Agent),Time_from,Time_until),
     	  sleep1(Agent),
     	  []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',86).
% [agent,time]
 % Happens(WakeUp(agent),time) -> HoldsAt(Sleep0(agent),time).
% From E: 
% 
% '->'(
%    happens(
%       wakeUp(Agent), 
%       Time), 
%    holds(
%       sleep0(Agent), 
%       Time)).
(   sleep0(Agent)at Time
;   not happens(wakeUp(Agent), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',86).

 /*   (   at(sleep0(Agent), Time)
        ;   not(happens(wakeUp(Agent), Time))
        ).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',88).
%;--
%; Getting out of bed causes a transition from Sleep1
%; to Sleep2:
% [agent,bed,time]
 
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',92).
% Terminates(RiseFrom(agent,bed),Sleep1(agent),time).
% From E: 
% 
% terminates_at(
%    riseFrom(Agent,Bed), 
%    sleep1(Agent), 
%    Time).
riseFrom(Agent, Bed)terminates sleep1(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',92).

 /*  terminated(happens(riseFrom(Agent,Bed),
     		   Time_from,
     		   Time_until),
     	   sleep1(Agent),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',94).
% [agent,bed,time]
 % Initiates(RiseFrom(agent,bed),Sleep2(agent),time).
% From E: 
% 
% initiates_at(
%    riseFrom(Agent,Bed), 
%    sleep2(Agent), 
%    Time).
riseFrom(Agent, Bed)initiates sleep2(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',94).

 /*  initiated(happens(riseFrom(Agent,Bed),
     		  Time_from,
     		  Time_until),
     	  sleep2(Agent),
     	  []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',96).
% [agent,bed,time]
% Happens(RiseFrom(agent,bed),time) -> HoldsAt(Sleep1(agent),time).
% From E: 
% 
% '->'(
%    happens(
%       riseFrom(Agent,Bed), 
%       Time), 
%    holds(
%       sleep1(Agent), 
%       Time)).
(   sleep1(Agent)at Time
;   not happens(riseFrom(Agent, Bed), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',96).

 /*   (   at(sleep1(Agent), Time)
        ;   not(happens(riseFrom(Agent, Bed), Time))
        ).
 */
 %  % =================================.


%;--
%; Getting dressed causes a transition from Sleep2
%; to Sleep3, the normal state of awakeness:
% [agent,time]
 
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',103).
% Terminates(GetDressed(agent),Sleep2(agent),time).
% From E: 
% 
% terminates_at(
%    getDressed(Agent), 
%    sleep2(Agent), 
%    Time).
getDressed(Agent)terminates sleep2(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',103).

 /*  terminated(happens(getDressed(Agent),
     		   Time_from,
     		   Time_until),
     	   sleep2(Agent),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',105).
% [agent,time]
 % Initiates(GetDressed(agent),Sleep3(agent),time).
% From E: 
% 
% initiates_at(
%    getDressed(Agent), 
%    sleep3(Agent), 
%    Time).
getDressed(Agent)initiates sleep3(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',105).

 /*  initiated(happens(getDressed(Agent),
     		  Time_from,
     		  Time_until),
     	  sleep3(Agent),
     	  []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',107).
% [agent,time]
 % Happens(GetDressed(agent),time) -> HoldsAt(Sleep2(agent),time).
% From E: 
% 
% '->'(
%    happens(
%       getDressed(Agent), 
%       Time), 
%    holds(
%       sleep2(Agent), 
%       Time)).
(   sleep2(Agent)at Time
;   not happens(getDressed(Agent), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',107).

 /*   (   at(sleep2(Agent), Time)
        ;   not(happens(getDressed(Agent), Time))
        ).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',109).
%;--
%; Getting tired causes a transition from Sleep3
%; to Sleep4:
% [agent,time]
 
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',113).
% Terminates(GetTired(agent),Sleep3(agent),time).
% From E: 
% 
% terminates_at(
%    getTired(Agent), 
%    sleep3(Agent), 
%    Time).
getTired(Agent)terminates sleep3(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',113).

 /*  terminated(happens(getTired(Agent),
     		   Time_from,
     		   Time_until),
     	   sleep3(Agent),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',115).
% [agent,time]
 % Initiates(GetTired(agent),Sleep4(agent),time).
% From E: 
% 
% initiates_at(
%    getTired(Agent), 
%    sleep4(Agent), 
%    Time).
getTired(Agent)initiates sleep4(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',115).

 /*  initiated(happens(getTired(Agent),
     		  Time_from,
     		  Time_until),
     	  sleep4(Agent),
     	  []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',117).
% [agent,time]
 % Happens(GetTired(agent),time) -> HoldsAt(Sleep3(agent),time).
% From E: 
% 
% '->'(
%    happens(
%       getTired(Agent), 
%       Time), 
%    holds(
%       sleep3(Agent), 
%       Time)).
(   sleep3(Agent)at Time
;   not happens(getTired(Agent), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',117).

 /*   (   at(sleep3(Agent), Time)
        ;   not(happens(getTired(Agent), Time))
        ).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',119).
%;--
%; Getting undressed causes a transition from Sleep4
%; to Sleep5:
% [agent,time]
 
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',123).
% Terminates(GetUndressed(agent),Sleep4(agent),time).
% From E: 
% 
% terminates_at(
%    getUndressed(Agent), 
%    sleep4(Agent), 
%    Time).
getUndressed(Agent)terminates sleep4(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',123).

 /*  terminated(happens(getUndressed(Agent),
     		   Time_from,
     		   Time_until),
     	   sleep4(Agent),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',125).
% [agent,time]
 % Initiates(GetUndressed(agent),Sleep5(agent),time).
% From E: 
% 
% initiates_at(
%    getUndressed(Agent), 
%    sleep5(Agent), 
%    Time).
getUndressed(Agent)initiates sleep5(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',125).

 /*  initiated(happens(getUndressed(Agent),
     		  Time_from,
     		  Time_until),
     	  sleep5(Agent),
     	  []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',127).
% [agent,time]
 % Happens(GetUndressed(agent),time) -> HoldsAt(Sleep4(agent),time).
% From E: 
% 
% '->'(
%    happens(
%       getUndressed(Agent), 
%       Time), 
%    holds(
%       sleep4(Agent), 
%       Time)).
(   sleep4(Agent)at Time
;   not happens(getUndressed(Agent), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',127).

 /*   (   at(sleep4(Agent), Time)
        ;   not(happens(getUndressed(Agent), Time))
        ).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',129).
%;--
%; Lying on a bed causes a transition from Sleep5
%; to Sleep6:
% [agent,bed,time]
 
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',133).
% Terminates(LieOn(agent,bed),Sleep5(agent),time).
% From E: 
% 
% terminates_at(
%    lieOn(Agent,Bed), 
%    sleep5(Agent), 
%    Time).
lieOn(Agent, Bed)terminates sleep5(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',133).

 /*  terminated(happens(lieOn(Agent,Bed),
     		   Time_from,
     		   Time_until),
     	   sleep5(Agent),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',135).
% [agent,bed,time]
 % Initiates(LieOn(agent,bed),Sleep6(agent),time).
% From E: 
% 
% initiates_at(
%    lieOn(Agent,Bed), 
%    sleep6(Agent), 
%    Time).
lieOn(Agent, Bed)initiates sleep6(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',135).

 /*  initiated(happens(lieOn(Agent,Bed),
     		  Time_from,
     		  Time_until),
     	  sleep6(Agent),
     	  []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',137).
% [agent,bed,time]
 % Happens(LieOn(agent,bed),time) -> HoldsAt(Sleep5(agent),time).
% From E: 
% 
% '->'(
%    happens(
%       lieOn(Agent,Bed), 
%       Time), 
%    holds(
%       sleep5(Agent), 
%       Time)).
(   sleep5(Agent)at Time
;   not happens(lieOn(Agent, Bed), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',137).

 /*   (   at(sleep5(Agent), Time)
        ;   not(happens(lieOn(Agent, Bed), Time))
        ).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',139).
%;--
%; Falling asleep causes a transition from Sleep6
%; to Sleep0:
% [agent,time]
 
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',143).
% Terminates(FallAsleep(agent),Sleep6(agent),time).
% From E: 
% 
% terminates_at(
%    fallAsleep(Agent), 
%    sleep6(Agent), 
%    Time).
fallAsleep(Agent)terminates sleep6(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',143).

 /*  terminated(happens(fallAsleep(Agent),
     		   Time_from,
     		   Time_until),
     	   sleep6(Agent),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',145).
% [agent,time]
 % Initiates(FallAsleep(agent),Sleep0(agent),time).
% From E: 
% 
% initiates_at(
%    fallAsleep(Agent), 
%    sleep0(Agent), 
%    Time).
fallAsleep(Agent)initiates sleep0(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',145).

 /*  initiated(happens(fallAsleep(Agent),
     		  Time_from,
     		  Time_until),
     	  sleep0(Agent),
     	  []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',147).
% [agent,time]
 % Happens(FallAsleep(agent),time) -> HoldsAt(Sleep6(agent),time).
% From E: 
% 
% '->'(
%    happens(
%       fallAsleep(Agent), 
%       Time), 
%    holds(
%       sleep6(Agent), 
%       Time)).
(   sleep6(Agent)at Time
;   not happens(fallAsleep(Agent), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',147).

 /*   (   at(sleep6(Agent), Time)
        ;   not(happens(fallAsleep(Agent), Time))
        ).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',149).
%;--
%; agent acts on being in state Sleep5.

% fluent ActOnSleep5(agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',151).
% From E: 
% 
% fluent(actOnSleep5(agent)).
mpred_prop(actOnSleep5(agent), fluent).
fluents([actOnSleep5/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',153).
% noninertial ActOnSleep5
% From E: 
% 
% ':-'(call_pel_directive(noninertial(actOnSleep5))).
:- call_pel_directive(noninertial(actOnSleep5)).
%; We reduce the number of models by asserting that
%; an agent only acts on being in state Sleep5 while in
%; that state:
% [agent,time]
% !HoldsAt(Sleep5(agent),time) ->
% !HoldsAt(ActOnSleep5(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',158).
% From E: 
% 
% '->'(
%    holds(
%       not(sleep5(Agent)), 
%       Time), 
%    holds(
%       not(actOnSleep5(Agent)), 
%       Time)).
actOnSleep5(Agent)at Time if sleep5(Agent)at Time.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',158).

 /*  l_int(holds(actOnSleep5(Agent),Time),
           [holds(sleep5(Agent),Time)]).
 */
 %  % =================================.


%; Undressed is like IntentionToPlay
%; ActOnSleep5 is like ActOnIntentionToPlay
%; A trigger axiom states that if an agent is in state Sleep5,
%; the agent acts on this state, the agent is in a room, and
%; a bed is at the room, the agent lies on the bed:
% [agent,room,bed,time]
% HoldsAt(Sleep5(agent),time) &
% HoldsAt(ActOnSleep5(agent),time) &
% HoldsAt(At(agent,room),time) &
% HoldsAt(At(bed,room),time) ->
% Happens(LieOn(agent,bed),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',168).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          sleep5(Agent), 
%          Time), 
%       ','(
%          holds(
%             actOnSleep5(Agent), 
%             Time), 
%          ','(
%             holds(
%                at_loc(Agent,Room), 
%                Time), 
%             holds(
%                at_loc(Bed,Room), 
%                Time)))), 
%    happens(
%       lieOn(Agent,Bed), 
%       Time)).
(   happens(lieOn(Agent, Bed), Time)
;   not sleep5(Agent)at Time
;   not actOnSleep5(Agent)at Time
;   not at_loc(Agent, Room)at Time
;   not at_loc(Bed, Room)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',168).

 /*   (   happens(lieOn(Agent, Bed), Time)
        ;   at(not(sleep5(Agent)), Time)
        ;   at(not(actOnSleep5(Agent)), Time)
        ;   at(not(at_loc(Agent, Room)), Time)
        ;   at(not(at_loc(Bed, Room)), Time)
        ).
 */
 %  % =================================.


%; A precondition axiom states that for
%; an agent to lie on a bed,
%; the agent must be in state Sleep5,
%; the agent must act on this state, and
%; there must be a room such that
%; the agent is in the room and the bed is in the room:
% [agent,bed,time]
% Happens(LieOn(agent,bed),time) ->
% HoldsAt(Sleep5(agent),time) &
% HoldsAt(ActOnSleep5(agent),time) &
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',181).
% {room}% 
%  HoldsAt(At(agent,room),time) &
%  HoldsAt(At(bed,room),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',185).
% From E: 
% 
% exists(Room, 
%    '->'(
%       happens(
%          lieOn(Agent,Bed), 
%          Time), 
%       ','(
%          holds(
%             sleep5(Agent), 
%             Time), 
%          ','(
%             holds(
%                actOnSleep5(Agent), 
%                Time), 
%             ','(
%                holds(
%                   at_loc(Agent,Room), 
%                   Time), 
%                holds(
%                   at_loc(Bed,Room), 
%                   Time)))))).
exists(Room,  (sleep5(Agent)at Time, actOnSleep5(Agent)at Time, at_loc(Agent, Room)at Time, at_loc(Bed, Room)at Time;not happens(lieOn(Agent, Bed), Time))).
 %  exists(Room,  (at(sleep5(Agent), Time), at(actOnSleep5(Agent), Time), at(at_loc(Agent, Room), Time), at(at_loc(Bed, Room), Time);not(happens(lieOn(Agent, Bed), Time)))).
 %  % =================================.


%; (body) posture
%; agent lies on physobj.

% event LieOn(agent,physobj)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',191).
% From E: 
% 
% event(lieOn(agent,physobj)).
events([lieOn/2]).
mpred_prop(lieOn(agent, physobj), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',191).
actions([lieOn/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',194).
%; agent sits on physobj.

% event SitOn(agent,physobj)
% From E: 
% 
% event(sitOn(agent,physobj)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',194).
events([sitOn/2]).
mpred_prop(sitOn(agent, physobj), action).
actions([sitOn/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',197).
% [agent,physobj,time]
% Happens(SitOn(agent,physobj),time) ->
% {location}% 
%  HoldsAt(At(agent,location),time) &
%  HoldsAt(At(physobj,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',199).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          sitOn(Agent,Physobj), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent,Location), 
%             Time), 
%          holds(
%             at_loc(Physobj,Location), 
%             Time)))).
exists(Location,  (at_loc(Agent, Location)at Time, at_loc(Physobj, Location)at Time;not happens(sitOn(Agent, Physobj), Time))).
 %  exists(Location,  (at(at_loc(Agent, Location), Time), at(at_loc(Physobj, Location), Time);not(happens(sitOn(Agent, Physobj), Time)))).
 %  % =================================.


%; agent rises from physobj.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',204).
% event RiseFrom(agent,physobj)
% From E: 
% 
% event(riseFrom(agent,physobj)).
events([riseFrom/2]).
mpred_prop(riseFrom(agent, physobj), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',204).
actions([riseFrom/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',206).
%; agent is lying on physobj.

% fluent LyingOn(agent,physobj)
% From E: 
% 
% fluent(lyingOn(agent,physobj)).
mpred_prop(lyingOn(agent, physobj), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',206).
fluents([lyingOn/2]).


%; agent is sitting on physobj.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',209).
% fluent SittingOn(agent,physobj)
% From E: 
% 
% fluent(sittingOn(agent,physobj)).
mpred_prop(sittingOn(agent, physobj), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',209).
fluents([sittingOn/2]).


%; agent is standing.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',211).
% fluent Standing(agent)
% From E: 
% 
% fluent(standing(agent)).
mpred_prop(standing(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',211).
fluents([standing/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',213).
%; agent is lying down.

% fluent Lying(agent)
% From E: 
% 
% fluent(lying(agent)).
mpred_prop(lying(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',213).
fluents([lying/1]).


%; agent is sitting.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',216).
% fluent Sitting(agent)
% From E: 
% 
% fluent(sitting(agent)).
mpred_prop(sitting(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',216).
fluents([sitting/1]).

% noninertial Lying
% From E: 
% 
% ':-'(call_pel_directive(noninertial(lying))).
:- call_pel_directive(noninertial(lying)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',218).
% noninertial Sitting
% From E: 
% 
% ':-'(call_pel_directive(noninertial(sitting))).
:- call_pel_directive(noninertial(sitting)).
%; At any time, an agent is either lying, sitting, or standing:

% xor Lying, Sitting, Standing
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',220).
% From E: 
% 
% xor([lying,sitting,standing]).
xor([lying,sitting,standing]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',223).
% [agent,physobj,time]
% HoldsAt(LyingOn(agent,physobj),time) ->
% HoldsAt(Lying(agent),time).
% From E: 
% 
% '->'(
%    holds(
%       lyingOn(Agent,Physobj), 
%       Time), 
%    holds(
%       lying(Agent), 
%       Time)).
(   lying(Agent)at Time
;   not lyingOn(Agent, Physobj)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',223).

 /*   (   at(lying(Agent), Time)
        ;   at(not(lyingOn(Agent, Physobj)), Time)
        ).
 */
 %  % =================================.


% [agent,physobj,time]
% HoldsAt(SittingOn(agent,physobj),time) ->
% HoldsAt(Sitting(agent),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',228).
% From E: 
% 
% '->'(
%    holds(
%       sittingOn(Agent,Physobj), 
%       Time), 
%    holds(
%       sitting(Agent), 
%       Time)).
(   sitting(Agent)at Time
;   not sittingOn(Agent, Physobj)at Time
).

 /*   (   at(sitting(Agent), Time)
        ;   at(not(sittingOn(Agent, Physobj)), Time)
        ).
 */
 %  % =================================.


%; State constraints represent that an agent can lie or sit
%; on at most one object at a time:
% [agent,physobj1,physobj2,time]
% HoldsAt(LyingOn(agent,physobj1),time) &
% HoldsAt(LyingOn(agent,physobj2),time) ->
% physobj1=physobj2.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',233).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          lyingOn(Agent,Physobj1), 
%          Time), 
%       holds(
%          lyingOn(Agent,Physobj2), 
%          Time)), 
%    Physobj1=Physobj2).
(   equals(Physobj1, Physobj2)
;   not lyingOn(Agent, Physobj1)at Time
;   not lyingOn(Agent, Physobj2)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',233).

 /*   (   equals(Physobj1, Physobj2)
        ;   at(not(lyingOn(Agent, Physobj1)), Time)
        ;   at(not(lyingOn(Agent, Physobj2)), Time)
        ).
 */
 %  % =================================.


% [agent,physobj1,physobj2,time]
% HoldsAt(SittingOn(agent,physobj1),time) &
% HoldsAt(SittingOn(agent,physobj2),time) ->
% physobj1=physobj2.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',239).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          sittingOn(Agent,Physobj1), 
%          Time), 
%       holds(
%          sittingOn(Agent,Physobj2), 
%          Time)), 
%    Physobj1=Physobj2).
(   equals(Physobj1, Physobj2)
;   not sittingOn(Agent, Physobj1)at Time
;   not sittingOn(Agent, Physobj2)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',239).

 /*   (   equals(Physobj1, Physobj2)
        ;   at(not(sittingOn(Agent, Physobj1)), Time)
        ;   at(not(sittingOn(Agent, Physobj2)), Time)
        ).
 */
 %  % =================================.


%; An effect axiom states that if an agent is standing and
%; lies on a physical object, the agent will be lying on
%; the physical object:
% [agent,physobj,time]
% HoldsAt(Standing(agent),time) ->
% Initiates(LieOn(agent,physobj),
%           LyingOn(agent,physobj),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',246).
% From E: 
% 
% '->'(
%    holds(
%       standing(Agent), 
%       Time), 
%    initiates_at(
%       lieOn(Agent,Physobj), 
%       lyingOn(Agent,Physobj), 
%       Time)).
(   initiates(lieOn(Agent, Physobj),
              lyingOn(Agent, Physobj)at Time)
;   not standing(Agent)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',246).

 /*   (   initiates(lieOn(Agent, Physobj),
                      at(lyingOn(Agent, Physobj), Time))
        ;   at(not(standing(Agent)), Time)
        ).
 */
 %  % =================================.


%; An effect axiom states that if an agent
%; lies on a physical object, the agent will no longer
%; be standing:
% [agent,physobj,time]
% Terminates(LieOn(agent,physobj),
%            Standing(agent),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',255).
% From E: 
% 
% terminates_at(
%    lieOn(Agent,Physobj), 
%    standing(Agent), 
%    Time).
lieOn(Agent, Physobj)terminates standing(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',255).

 /*  terminated(happens(lieOn(Agent,Physobj),
     		   Time_from,
     		   Time_until),
     	   standing(Agent),
     	   []).
 */
 %  % =================================.


%; An effect axiom states that if an agent is standing and
%; sits on a physical object, the agent will be sitting on
%; the physical object:
% [agent,physobj,time]
% HoldsAt(Standing(agent),time) ->
% Initiates(SitOn(agent,physobj),
%           SittingOn(agent,physobj),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',263).
% From E: 
% 
% '->'(
%    holds(
%       standing(Agent), 
%       Time), 
%    initiates_at(
%       sitOn(Agent,Physobj), 
%       sittingOn(Agent,Physobj), 
%       Time)).
(   initiates(sitOn(Agent, Physobj),
              sittingOn(Agent, Physobj)at Time)
;   not standing(Agent)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',263).

 /*   (   initiates(sitOn(Agent, Physobj),
                      at(sittingOn(Agent, Physobj), Time))
        ;   at(not(standing(Agent)), Time)
        ).
 */
 %  % =================================.


%; An effect axiom states that if an agent
%; sits on a physical object, the agent will no longer
%; be standing:
% [agent,physobj,time]
% Terminates(SitOn(agent,physobj),
%            Standing(agent),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',272).
% From E: 
% 
% terminates_at(
%    sitOn(Agent,Physobj), 
%    standing(Agent), 
%    Time).
sitOn(Agent, Physobj)terminates standing(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',272).

 /*  terminated(happens(sitOn(Agent,Physobj),
     		   Time_from,
     		   Time_until),
     	   standing(Agent),
     	   []).
 */
 %  % =================================.


%; An effect axiom states that if an agent
%; is sitting or lying on a physical object and
%; the agent rises from the physical object,
%; the agent will be standing:
% [agent,physobj,time]
% (HoldsAt(SittingOn(agent,physobj),time) |
%  HoldsAt(LyingOn(agent,physobj),time)) ->
% Initiates(RiseFrom(agent,physobj),
%           Standing(agent),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',281).
% From E: 
% 
% '->'(
%    ';'(
%       holds(
%          sittingOn(Agent,Physobj), 
%          Time), 
%       holds(
%          lyingOn(Agent,Physobj), 
%          Time)), 
%    initiates_at(
%       riseFrom(Agent,Physobj), 
%       standing(Agent), 
%       Time)).
(   initiates(riseFrom(Agent, Physobj),
              standing(Agent)at Time)
;   not sittingOn(Agent, Physobj)at Time,
    not lyingOn(Agent, Physobj)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',281).

 /*  (   initiates(riseFrom(Agent, Physobj),
                   at(standing(Agent), Time))
     ;   at(not(sittingOn(Agent, Physobj)), Time),
         at(not(lyingOn(Agent, Physobj)), Time)
     ).
 */
 %  % =================================.


%; An effect axiom states that if an agent is sitting on
%; a physical object and the agent rises from the physical
%; object, the agent will no longer be sitting on the
%; physical object:
% [agent,physobj,time]
% HoldsAt(SittingOn(agent,physobj),time) ->
% Terminates(RiseFrom(agent,physobj),
%            SittingOn(agent,physobj),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',292).
% From E: 
% 
% '->'(
%    holds(
%       sittingOn(Agent,Physobj), 
%       Time), 
%    terminates_at(
%       riseFrom(Agent,Physobj), 
%       sittingOn(Agent,Physobj), 
%       Time)).
(   terminates(riseFrom(Agent, Physobj),
               sittingOn(Agent, Physobj)at Time)
;   not sittingOn(Agent, Physobj)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',292).

 /*   (   terminates(riseFrom(Agent, Physobj),
                       at(sittingOn(Agent, Physobj), Time))
        ;   at(not(sittingOn(Agent, Physobj)), Time)
        ).
 */
 %  % =================================.


%; An effect axiom states that if an agent is lying on
%; a physical object and the agent rises from the physical
%; object, the agent will no longer be lying on the
%; physical object:
% [agent,physobj,time]
% HoldsAt(LyingOn(agent,physobj),time) ->
% Terminates(RiseFrom(agent,physobj),
%            LyingOn(agent,physobj),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',302).
% From E: 
% 
% '->'(
%    holds(
%       lyingOn(Agent,Physobj), 
%       Time), 
%    terminates_at(
%       riseFrom(Agent,Physobj), 
%       lyingOn(Agent,Physobj), 
%       Time)).
(   terminates(riseFrom(Agent, Physobj),
               lyingOn(Agent, Physobj)at Time)
;   not lyingOn(Agent, Physobj)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',302).

 /*   (   terminates(riseFrom(Agent, Physobj),
                       at(lyingOn(Agent, Physobj), Time))
        ;   at(not(lyingOn(Agent, Physobj)), Time)
        ).
 */
 %  % =================================.


%; dressing
%; agent gets undressed.

% event GetDressed(agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',310).
% From E: 
% 
% event(getDressed(agent)).
events([getDressed/1]).
mpred_prop(getDressed(agent), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',310).
actions([getDressed/1]).


%; agent gets dressed.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',313).
% event GetUndressed(agent)
% From E: 
% 
% event(getUndressed(agent)).
events([getUndressed/1]).
mpred_prop(getUndressed(agent), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',313).
actions([getUndressed/1]).


%; agent is dressed.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',315).
% fluent Dressed(agent)
% From E: 
% 
% fluent(dressed(agent)).
mpred_prop(dressed(agent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',315).
fluents([dressed/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',317).
%; Effect axioms deal with getting dressed and undressed:
% [agent,time]
 % Initiates(GetDressed(agent),Dressed(agent),time).
% From E: 
% 
% initiates_at(
%    getDressed(Agent), 
%    dressed(Agent), 
%    Time).
getDressed(Agent)initiates dressed(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',317).

 /*  initiated(happens(getDressed(Agent),
     		  Time_from,
     		  Time_until),
     	  dressed(Agent),
     	  []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',319).
% [agent,time]
 % Terminates(GetUndressed(agent),Dressed(agent),time).
% From E: 
% 
% terminates_at(
%    getUndressed(Agent), 
%    dressed(Agent), 
%    Time).
getUndressed(Agent)terminates dressed(Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',319).

 /*  terminated(happens(getUndressed(Agent),
     		   Time_from,
     		   Time_until),
     	   dressed(Agent),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.e',321).
%; End of file.
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Sleep.lps.pl')).
