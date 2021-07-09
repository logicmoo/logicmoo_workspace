% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECTraj.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECTraj.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/EC.e',41).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECTraj.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECTraj.lps.pl')).
% Fri, 26 Mar 2021 01:05:54 GMT File: <stream>(0x555567c94000)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; @incollection{MillerShanahan:2002,
%;   author = "Rob Miller and Murray Shanahan",
%;   year = "2002",
%;   title = "Some alternative formulations of the event calculus",
%;   editor = "Antonis C. Kakas and Fariba Sadri",
%;   booktitle = "Computational Logic: Logic Programming and Beyond: Essays in Honour of \uppercase{R}obert \uppercase{A}. \uppercase{K}owalski, Part \uppercase{II}",
%;   series = "Lecture Notes in Computer Science",
%;   volume = "2408",
%;   pages = "452--490",
%;   address = "Berlin",
%;   publisher = "Springer",
%; }
%;

% predicate Clipped(time,fluent,time)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECTraj.e',25).
% From E: 
% 
% predicate(clipped(time,fluent,time)).
mpred_prop(clipped(time, fluent, time), predicate).
predicates([clipped/3]).

% predicate Declipped(time,fluent,time)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECTraj.e',25).
% From E: 
% 
% predicate(declipped(time,fluent,time)).
mpred_prop(declipped(time, fluent, time), predicate).
predicates([declipped/3]).

% predicate Trajectory(fluent,time,fluent,offset)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECTraj.e',28).
% From E: 
% 
% predicate(trajectory(fluent, time, fluent, 
%              offset)).
mpred_prop(trajectory(fluent, time, fluent, offset), predicate).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECTraj.e',28).
predicates([trajectory/4]).

% predicate AntiTrajectory(fluent,time,fluent,offset)
% From E: 
% 
% predicate(antiTrajectory(fluent, time, fluent, 
%              offset)).
mpred_prop(antiTrajectory(fluent, time, fluent, offset), predicate).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECTraj.e',28).
predicates([antiTrajectory/4]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECTraj.e',31).
% [event,fluent,fluent2,offset,time]
% Happens(event,time) &
% Initiates(event,fluent,time) &
% 0 < offset &
% Trajectory(fluent,time,fluent2,offset) &
% !Clipped(time,fluent,time+offset) ->
% HoldsAt(fluent2,time+offset).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECTraj.e',31).
% From E: 
% 
% '->'(
%    ','(
%       happens(Event,Time), 
%       ','(
%          initiates_at(Event,Fluent,Time), 
%          ','(
%             0<Offset, 
%             ','(
%                trajectory(Fluent, Time, Fluent2, 
%                   Offset), 
%                not(clipped(Time,Fluent,Time+Offset)))))), 
%    holds(Fluent2,Time+Offset)).
(   Fluent2 at Time+Offset
;   not happens(Event, Time)
;   not (Event initiates Fluent at Time)
;   not comparison(0, Offset, <)
;   not trajectory(Fluent, Time, Fluent2, Offset)
;   clipped(Time, Fluent, Time+Offset)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECTraj.e',31).

 /*   (   at(Fluent2, Time+Offset)
        ;   not(happens(Event, Time))
        ;   not(initiates(Event, at(Fluent, Time)))
        ;   not(comparison(0, Offset, <))
        ;   not(trajectory(Fluent, Time, Fluent2, Offset))
        ;   clipped(Time, Fluent, Time+Offset)
        ).
 */
 %  % =================================.


% [event,fluent,fluent2,offset,time]
% Happens(event,time) &
% Terminates(event,fluent,time) &
% 0 < offset &
% AntiTrajectory(fluent,time,fluent2,offset) &
% !Declipped(time,fluent,time+offset) ->
% HoldsAt(fluent2,time+offset).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECTraj.e',40).
% From E: 
% 
% '->'(
%    ','(
%       happens(Event,Time), 
%       ','(
%          terminates_at(Event,Fluent,Time), 
%          ','(
%             0<Offset, 
%             ','(
%                antiTrajectory(Fluent, Time, Fluent2, 
%                   Offset), 
%                not(declipped(Time,Fluent,Time+Offset)))))), 
%    holds(Fluent2,Time+Offset)).
(   Fluent2 at Time+Offset
;   not happens(Event, Time)
;   not (Event terminates Fluent at Time)
;   not comparison(0, Offset, <)
;   not antiTrajectory(Fluent, Time, Fluent2, Offset)
;   declipped(Time, Fluent, Time+Offset)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECTraj.e',40).

 /*   (   at(Fluent2, Time+Offset)
        ;   not(happens(Event, Time))
        ;   not(terminates(Event, at(Fluent, Time)))
        ;   not(comparison(0, Offset, <))
        ;   not(antiTrajectory(Fluent, Time, Fluent2, Offset))
        ;   declipped(Time, Fluent, Time+Offset)
        ).
 */
 %  % =================================.


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECTraj.e',46).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECTraj.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECTraj.lps.pl')).
