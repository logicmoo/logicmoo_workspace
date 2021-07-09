% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Diving.e',682).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.lps.pl')).
% Fri, 26 Mar 2021 01:05:54 GMT File: <stream>(0x555567ca8600)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; Discrete Event Calculus (DEC)
%;
%; @article{Mueller:2004a,
%;   author = "Erik T. Mueller",
%;   year = "2004",
%;   title = "Event calculus reasoning through satisfiability",
%;   journal = "Journal of Logic and Computation",
%;   volume = "14",
%;   number = "5",
%;   pages = "703--730",
%; }
%;

% sort time: integer
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.e',24).
% From E: 
% 
% subsort(time,integer).
subsort(time, integer).

% sort offset: integer
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.e',24).
% From E: 
% 
% subsort(offset,integer).
subsort(offset, integer).

% reified sort fluent
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.e',27).
% From E: 
% 
% reified_sort(fluent).
reified_sorts([fluent/0]).

% reified sort event
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.e',27).
% From E: 
% 
% reified_sort(event).
reified_sorts([event/0]).

% predicate Happens(event,time)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.e',30).
% From E: 
% 
% predicate(happens(event,time)).
mpred_prop(happens(event, time), predicate).
predicates([happens/2]).

% predicate HoldsAt(fluent,time)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.e',30).
% From E: 
% 
% predicate(holds(fluent,time)).
mpred_prop(holds(fluent, time), predicate).
predicates([holds/2]).

% predicate ReleasedAt(fluent,time)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.e',32).
% From E: 
% 
% predicate(released_at(fluent,time)).
mpred_prop(released_at(fluent, time), predicate).
predicates([released_at/2]).

% predicate Initiates(event,fluent,time)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.e',34).
% From E: 
% 
% predicate(initiates_at(event,fluent,time)).
mpred_prop(initiates(event, fluent, time), predicate).
predicates([initiates/3]).

% predicate Terminates(event,fluent,time)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.e',34).
% From E: 
% 
% predicate(terminates_at(event,fluent,time)).
mpred_prop(terminates(event, fluent, time), predicate).
predicates([terminates/3]).

% predicate Releases(event,fluent,time)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.e',36).
% From E: 
% 
% predicate(releases_at(event,fluent,time)).
mpred_prop(releases(event, fluent, time), predicate).
predicates([releases/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.e',38).
% [fluent,time]
% (HoldsAt(fluent,time) &
%  !ReleasedAt(fluent,time+1) &
%  !({event} Happens(event,time) & Terminates(event,fluent,time))) ->
% HoldsAt(fluent,time+1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.e',38).
% From E: 
% 
% '->'(
%    ','(
%       holds(Fluent,Time), 
%       ','(
%          not(released_at(Fluent,Time+1)), 
%          not(thereExists(Event, 
%                 ','(
%                    happens(Event,Time), 
%                    terminates_at(Event,Fluent,Time)))))), 
%    holds(Fluent,Time+1)).
(   Fluent at Time+1
;   not Fluent at Time
;   released_at(Fluent, Time+1)
;   thereExists(Event,
                 (happens(Event, Time), (Event terminates Fluent at Time)))
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.e',38).

 /*  (   at(Fluent, Time+1)
     ;   at(not(Fluent), Time)
     ;   released_at(Fluent, Time+1)
     ;   thereExists(Event,
                      (happens(Event, Time), terminates(Event, at(Fluent, Time))))
     ).
 */
 %  % =================================.


% [fluent,time]
% (!HoldsAt(fluent,time) &
%  !ReleasedAt(fluent,time+1) &
%  !({event} Happens(event,time) & Initiates(event,fluent,time))) ->
% !HoldsAt(fluent,time+1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.e',45).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          not(Fluent), 
%          Time), 
%       ','(
%          not(released_at(Fluent,Time+1)), 
%          not(thereExists(Event, 
%                 ','(
%                    happens(Event,Time), 
%                    initiates_at(Event,Fluent,Time)))))), 
%    holds(
%       not(Fluent), 
%       Time+1)).
Fluent at Time+1 if Fluent at Time;released_at(Fluent, Time+1);thereExists(Event,  (happens(Event, Time), (Event initiates Fluent at Time))).
 %  l_int(holds(not(Not), Time+1), [(at(not(Not), Time);released_at(not(Not), Time+1);thereExists(Event,  (happens(Event, Time), initiates(Event, at(not(Not), Time)))))]).
 %  % =================================.


% [fluent,time]
% (!ReleasedAt(fluent,time) &
%  !({event} Happens(event,time) & Releases(event,fluent,time))) ->
% !ReleasedAt(fluent,time+1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.e',51).
% From E: 
% 
% '->'(
%    ','(
%       not(released_at(Fluent,Time)), 
%       not(thereExists(Event, 
%              ','(
%                 happens(Event,Time), 
%                 releases_at(Event,Fluent,Time))))), 
%    not(released_at(Fluent,Time+1))).
released_at(Fluent, Time+1)if released_at(Fluent, Time);thereExists(Event,  (happens(Event, Time), (Event terminates Fluent at Time))).
 %  if(released_at(Fluent, Time+1),  (released_at(Fluent, Time);thereExists(Event,  (happens(Event, Time), terminates(Event, at(Fluent, Time)))))).
 %  % =================================.


% [fluent,time]
% (ReleasedAt(fluent,time) &
%  !({event} Happens(event,time) &
%    (Initiates(event,fluent,time) |
%     Terminates(event,fluent,time)))) ->
% ReleasedAt(fluent,time+1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.e',56).
% From E: 
% 
% '->'(
%    ','(
%       released_at(Fluent,Time), 
%       not(thereExists(Event, 
%              ','(
%                 happens(Event,Time), 
%                 ';'(
%                    initiates_at(Event,Fluent,Time), 
%                    terminates_at(Event,Fluent,Time)))))), 
%    released_at(Fluent,Time+1)).
(   released_at(Fluent, Time+1)
;   not released_at(Fluent, Time)
;   thereExists(Event,
                 (happens(Event, Time), (Event initiates Fluent at Time;Event terminates Fluent at Time)))
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.e',56).

 /*  (   released_at(Fluent, Time+1)
     ;   not(released_at(Fluent, Time))
     ;   thereExists(Event,
                      (happens(Event, Time), (initiates(Event, at(Fluent, Time));terminates(Event, at(Fluent, Time)))))
     ).
 */
 %  % =================================.


% [event,fluent,time]
% (Happens(event,time) & Initiates(event,fluent,time)) ->
% (HoldsAt(fluent,time+1) & !ReleasedAt(fluent,time+1)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.e',63).
% From E: 
% 
% '->'(
%    ','(
%       happens(Event,Time), 
%       initiates_at(Event,Fluent,Time)), 
%    ','(
%       holds(Fluent,Time+1), 
%       not(released_at(Fluent,Time+1)))).
(   Fluent at Time+1,
    not released_at(Fluent, Time+1)
;   not happens(Event, Time)
;   not (Event initiates Fluent at Time)
).

 /*  (   at(Fluent, Time+1),
         not(released_at(Fluent, Time+1))
     ;   not(happens(Event, Time))
     ;   not(initiates(Event, at(Fluent, Time)))
     ).
 */
 %  % =================================.


% [event,fluent,time]
% (Happens(event,time) & Terminates(event,fluent,time)) ->
% (!HoldsAt(fluent,time+1) & !ReleasedAt(fluent,time+1)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.e',67).
% From E: 
% 
% '->'(
%    ','(
%       happens(Event,Time), 
%       terminates_at(Event,Fluent,Time)), 
%    ','(
%       holds(
%          not(Fluent), 
%          Time+1), 
%       not(released_at(Fluent,Time+1)))).
(   not Fluent at Time+1,
    not released_at(Fluent, Time+1)
;   not happens(Event, Time)
;   not (Event terminates Fluent at Time)
).

 /*  (   at(not(Fluent), Time+1),
         not(released_at(Fluent, Time+1))
     ;   not(happens(Event, Time))
     ;   not(terminates(Event, at(Fluent, Time)))
     ).
 */
 %  % =================================.


% [event,fluent,time]
% (Happens(event,time) & Releases(event,fluent,time)) ->
% ReleasedAt(fluent,time+1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.e',71).
% From E: 
% 
% '->'(
%    ','(
%       happens(Event,Time), 
%       releases_at(Event,Fluent,Time)), 
%    released_at(Fluent,Time+1)).
(   released_at(Fluent, Time+1)
;   not happens(Event, Time)
;   not (Event terminates Fluent at Time)
).

 /*   (   released_at(Fluent, Time+1)
        ;   not(happens(Event, Time))
        ;   not(terminates(Event, at(Fluent, Time)))
        ).
 */
 %  % =================================.


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.e',73).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.lps.pl')).
