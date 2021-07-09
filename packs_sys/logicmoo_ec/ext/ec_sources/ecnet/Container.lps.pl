% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Condition.e',77).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.lps.pl')).
% Fri, 26 Mar 2021 01:05:56 GMT File: <stream>(0x555567dd7600)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%;
%; Container: container
%;
%; linkage to OTSpace(M):
% [agent,container1,container2,time]
% Happens(TakeOutOf(agent,container1,container2),time) ->
% HoldsAt(ContainerIsOpen(container2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',17).
% From E: 
% 
% '->'(
%    happens(
%       takeOutOf(Agent,Container1,Container2), 
%       Time), 
%    holds(
%       containerIsOpen(Container2), 
%       Time)).
(   containerIsOpen(Container2)at Time
;   not(happens(takeOutOf(Agent, Container1, Container2),
                Time))
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',17).

 /*   (   at(containerIsOpen(Container2), Time)
        ;   not(happens(takeOutOf(Agent, Container1, Container2),
                        Time))
        ).
 */
 %  % =================================.


% [agent,container1,container2,time]
% Happens(PutInside(agent,container1,container2),time) ->
% HoldsAt(ContainerIsOpen(container2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',21).
% From E: 
% 
% '->'(
%    happens(
%       putInside(Agent,Container1,Container2), 
%       Time), 
%    holds(
%       containerIsOpen(Container2), 
%       Time)).
(   containerIsOpen(Container2)at Time
;   not(happens(putInside(Agent, Container1, Container2),
                Time))
).

 /*   (   at(containerIsOpen(Container2), Time)
        ;   not(happens(putInside(Agent, Container1, Container2),
                        Time))
        ).
 */
 %  % =================================.


%; agent opens container.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',25).
% event ContainerOpen(agent,container)
% From E: 
% 
% event(containerOpen(agent,container)).
events([containerOpen/2]).
mpred_prop(containerOpen(agent, container), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',25).
actions([containerOpen/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',27).
%; agent closes container.

% event ContainerClose(agent,container)
% From E: 
% 
% event(containerClose(agent,container)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',27).
events([containerClose/2]).
mpred_prop(containerClose(agent, container), action).
actions([containerClose/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',30).
%; container is open.

% fluent ContainerIsOpen(container)
% From E: 
% 
% fluent(containerIsOpen(container)).
mpred_prop(containerIsOpen(container), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',30).
fluents([containerIsOpen/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',33).
% fluent ContainerClosed(container)
% From E: 
% 
% fluent(containerClosed(container)).
mpred_prop(containerClosed(container), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',33).
fluents([containerClosed/1]).

% noninertial ContainerClosed
% From E: 
% 
% ':-'(call_pel_directive(noninertial(containerClosed))).
:- call_pel_directive(noninertial(containerClosed)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',36).
% [container,time]
% HoldsAt(ContainerClosed(container),time) <->
% !HoldsAt(ContainerIsOpen(container),time).
% From E: 
% 
% <->(
%    holds(
%       containerClosed(Container), 
%       Time), 
%    holds(
%       not(containerIsOpen(Container)), 
%       Time)).
containerIsOpen(Container)at Time if not containerClosed(Container)at Time.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',36).

 /*  l_int(holds(containerIsOpen(Container),Time),
           [holds(not(containerClosed(Container)),Time)]).
 */
 %  % =================================.
(   containerClosed(Container)at Time
;   containerIsOpen(Container)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',36).

 /*   (   at(containerClosed(Container), Time)
        ;   at(containerIsOpen(Container), Time)
        ).
 */
 %  % =================================.


%; A precondition axiom states that
%; for an agent to open a container,
%; the agent must be awake,
%; the container must not already be open, and
%; the agent must be holding the container.
% [agent,container,time]
% Happens(ContainerOpen(agent,container),time) ->
% HoldsAt(Awake(agent),time) &
% !HoldsAt(ContainerIsOpen(container),time) &
% HoldsAt(Holding(agent,container),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',45).
% From E: 
% 
% '->'(
%    happens(
%       containerOpen(Agent,Container), 
%       Time), 
%    ','(
%       holds(
%          awake(Agent), 
%          Time), 
%       ','(
%          holds(
%             not(containerIsOpen(Container)), 
%             Time), 
%          holds(
%             holding(Agent,Container), 
%             Time)))).
(   awake(Agent)at Time,
    not containerIsOpen(Container)at Time,
    holding(Agent, Container)at Time
;   not happens(containerOpen(Agent, Container), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',45).

 /*  (   at(awake(Agent), Time),
         at(not(containerIsOpen(Container)), Time),
         at(holding(Agent, Container), Time)
     ;   not(happens(containerOpen(Agent, Container), Time))
     ).
 */
 %  % =================================.


%; An effect axiom states that
%; if an agent opens a container,
%; the container will be open:
% [agent,container,time]
% Initiates(ContainerOpen(agent,container),ContainerIsOpen(container),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',54).
% From E: 
% 
% initiates_at(
%    containerOpen(Agent,Container), 
%    containerIsOpen(Container), 
%    Time).
containerOpen(Agent, Container)initiates containerIsOpen(Container).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',54).

 /*  initiated(happens(containerOpen(Agent,Container),
     		  Time_from,
     		  Time_until),
     	  containerIsOpen(Container),
     	  []).
 */
 %  % =================================.


%; A precondition axiom states that
%; for an agent to close a container,
%; the agent must be awake,
%; the container must be open, and
%; the agent must be holding the container.
% [agent,container,time]
% Happens(ContainerClose(agent,container),time) ->
% HoldsAt(Awake(agent),time) &
% HoldsAt(ContainerIsOpen(container),time) &
% HoldsAt(Holding(agent,container),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',62).
% From E: 
% 
% '->'(
%    happens(
%       containerClose(Agent,Container), 
%       Time), 
%    ','(
%       holds(
%          awake(Agent), 
%          Time), 
%       ','(
%          holds(
%             containerIsOpen(Container), 
%             Time), 
%          holds(
%             holding(Agent,Container), 
%             Time)))).
(   awake(Agent)at Time,
    containerIsOpen(Container)at Time,
    holding(Agent, Container)at Time
;   not happens(containerClose(Agent, Container), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',62).

 /*  (   at(awake(Agent), Time),
         at(containerIsOpen(Container), Time),
         at(holding(Agent, Container), Time)
     ;   not(happens(containerClose(Agent, Container), Time))
     ).
 */
 %  % =================================.


%; An effect axiom states that
%; if an agent closes a container,
%; the container will no longer be open:
% [agent,container,time]
% Terminates(ContainerClose(agent,container),ContainerIsOpen(container),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',71).
% From E: 
% 
% terminates_at(
%    containerClose(Agent,Container), 
%    containerIsOpen(Container), 
%    Time).
containerClose(Agent, Container)terminates containerIsOpen(Container).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',71).

 /*  terminated(happens(containerClose(Agent,Container),
     		   Time_from,
     		   Time_until),
     	   containerIsOpen(Container),
     	   []).
 */
 %  % =================================.


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',73).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.lps.pl')).
