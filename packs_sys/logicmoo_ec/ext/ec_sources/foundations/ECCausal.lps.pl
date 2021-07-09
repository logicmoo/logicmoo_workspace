% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/DEC.e',73).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.lps.pl')).
% Fri, 26 Mar 2021 01:05:54 GMT File: <stream>(0x555567a68b00)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; Causal Constraints
%;
%; @inproceedings{Shanahan:1999a,
%;   author = "Murray Shanahan",
%;   year = "1999",
%;   title = "The ramification problem in the event calculus",
%;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}ixteenth \uppercase{I}nternational \uppercase{J}oint \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
%;   pages = "140--146",
%;   address = "San Mateo, CA",
%;   publisher = "Morgan Kaufmann",
%; }
%;

% predicate Started(fluent,time)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.e',24).
% From E: 
% 
% predicate(started(fluent,time)).
mpred_prop(started(fluent, time), predicate).
predicates([started/2]).

% predicate Stopped(fluent,time)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.e',24).
% From E: 
% 
% predicate(stopped(fluent,time)).
mpred_prop(stopped(fluent, time), predicate).
predicates([stopped/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.e',27).
% [fluent,time]
% Started(fluent,time) <->
% (HoldsAt(fluent,time) |
%  ({event} Happens(event,time) & Initiates(event,fluent,time))).
% From E: 
% 
% <->(
%    started(Fluent,Time), 
%    ';'(
%       holds(Fluent,Time), 
%       thereExists(Event, 
%          ','(
%             happens(Event,Time), 
%             initiates_at(Event,Fluent,Time))))).
(   (   Fluent at Time
    ;   thereExists(Event,
                     (happens(Event, Time), (Event initiates Fluent at Time)))
    )
;   not started(Fluent, Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.e',27).

 /*  (   (   at(Fluent, Time)
         ;   thereExists(Event,
                          (happens(Event, Time), initiates(Event, at(Fluent, Time))))
         )
     ;   not(started(Fluent, Time))
     ).
 */
 %  % =================================.
(   started(Fluent, Time)
;   not Fluent at Time,
    not(thereExists(Event,
                     (happens(Event, Time), (Event initiates Fluent at Time))))
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.e',27).

 /*  (   started(Fluent, Time)
     ;   at(not(Fluent), Time),
         not(thereExists(Event,
                          (happens(Event, Time), initiates(Event, at(Fluent, Time)))))
     ).
 */
 %  % =================================.


% [fluent,time]
% Stopped(fluent,time) <->
% (!HoldsAt(fluent,time) |
%  ({event} Happens(event,time) & Terminates(event,fluent,time))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.e',33).
% From E: 
% 
% <->(
%    stopped(Fluent,Time), 
%    ';'(
%       holds(
%          not(Fluent), 
%          Time), 
%       thereExists(Event, 
%          ','(
%             happens(Event,Time), 
%             terminates_at(Event,Fluent,Time))))).
(   (   not Fluent at Time
    ;   thereExists(Event,
                     (happens(Event, Time), (Event terminates Fluent at Time)))
    )
;   not stopped(Fluent, Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.e',33).

 /*  (   (   at(not(Fluent), Time)
         ;   thereExists(Event,
                          (happens(Event, Time), terminates(Event, at(Fluent, Time))))
         )
     ;   not(stopped(Fluent, Time))
     ).
 */
 %  % =================================.
(   stopped(Fluent, Time)
;   Fluent at Time,
    not(thereExists(Event,
                     (happens(Event, Time), (Event terminates Fluent at Time))))
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.e',33).

 /*  (   stopped(Fluent, Time)
     ;   at(Fluent, Time),
         not(thereExists(Event,
                          (happens(Event, Time), terminates(Event, at(Fluent, Time)))))
     ).
 */
 %  % =================================.

% predicate Initiated(fluent,time)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.e',36).
% From E: 
% 
% predicate(initiated(fluent,time)).
mpred_prop(initiated(fluent, time), predicate).
predicates([initiated/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.e',38).
% predicate Terminated(fluent,time)
% From E: 
% 
% predicate(terminated(fluent,time)).
mpred_prop(terminated(fluent, time), predicate).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.e',38).
predicates([terminated/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.e',40).
% [fluent,time]
% Initiated(fluent,time) <->
% (Started(fluent,time) &
%  !({event} Happens(event,time) & Terminates(event,fluent,time))).
% From E: 
% 
% <->(
%    initiated(Fluent,Time), 
%    ','(
%       started(Fluent,Time), 
%       not(thereExists(Event, 
%              ','(
%                 happens(Event,Time), 
%                 terminates_at(Event,Fluent,Time)))))).
(   started(Fluent, Time),
    not(thereExists(Event,
                     (happens(Event, Time), (Event terminates Fluent at Time))))
;   not initiated(Fluent, Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.e',40).

 /*  (   started(Fluent, Time),
         not(thereExists(Event,
                          (happens(Event, Time), terminates(Event, at(Fluent, Time)))))
     ;   not(initiated(Fluent, Time))
     ).
 */
 %  % =================================.
(   initiated(Fluent, Time)
;   not started(Fluent, Time)
;   thereExists(Event,
                 (happens(Event, Time), (Event terminates Fluent at Time)))
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.e',40).

 /*  (   initiated(Fluent, Time)
     ;   not(started(Fluent, Time))
     ;   thereExists(Event,
                      (happens(Event, Time), terminates(Event, at(Fluent, Time))))
     ).
 */
 %  % =================================.


% [fluent,time]
% Terminated(fluent,time) <->
% (Stopped(fluent,time) &
%  !({event} Happens(event,time) & Initiates(event,fluent,time))).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.e',46).
% From E: 
% 
% <->(
%    terminated(Fluent,Time), 
%    ','(
%       stopped(Fluent,Time), 
%       not(thereExists(Event, 
%              ','(
%                 happens(Event,Time), 
%                 initiates_at(Event,Fluent,Time)))))).
(   stopped(Fluent, Time),
    not(thereExists(Event,
                     (happens(Event, Time), (Event initiates Fluent at Time))))
;   not terminated(Fluent, Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.e',46).

 /*  (   stopped(Fluent, Time),
         not(thereExists(Event,
                          (happens(Event, Time), initiates(Event, at(Fluent, Time)))))
     ;   not(terminated(Fluent, Time))
     ).
 */
 %  % =================================.
(   terminated(Fluent, Time)
;   not stopped(Fluent, Time)
;   thereExists(Event,
                 (happens(Event, Time), (Event initiates Fluent at Time)))
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.e',46).

 /*  (   terminated(Fluent, Time)
     ;   not(stopped(Fluent, Time))
     ;   thereExists(Event,
                      (happens(Event, Time), initiates(Event, at(Fluent, Time))))
     ).
 */
 %  % =================================.


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.e',49).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/foundations/ECCausal.lps.pl')).
