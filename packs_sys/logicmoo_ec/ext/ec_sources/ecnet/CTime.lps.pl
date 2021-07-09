% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Container.e',73).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.lps.pl')).
% Fri, 26 Mar 2021 01:05:56 GMT File: <stream>(0x55556759a200)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; clock time
%; The CTime representation maps the time points of ECTime to clock time.
%; part of the day
%; time is in the daytime.

% predicate Daytime(time)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',17).
% From E: 
% 
% predicate(daytime(time)).
mpred_prop(daytime(time), predicate).
predicates([daytime/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',17).
%; time is in the nighttime.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',20).
% predicate Nighttime(time)
% From E: 
% 
% predicate(nighttime(time)).
mpred_prop(nighttime(time), predicate).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',20).
predicates([nighttime/1]).

% xor Daytime, Nighttime
% From E: 
% 
% xor([daytime,nighttime]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',20).
xor([daytime,nighttime]).
%; time is in the morning.

% predicate Morning(time)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',23).
% From E: 
% 
% predicate(morning(time)).
mpred_prop(morning(time), predicate).
predicates([morning/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',23).
%; time is in the afternoon.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',26).
% predicate Afternoon(time)
% From E: 
% 
% predicate(afternoon(time)).
mpred_prop(afternoon(time), predicate).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',26).
predicates([afternoon/1]).


%; time is in the evening.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',28).
% predicate Evening(time)
% From E: 
% 
% predicate(evening(time)).
mpred_prop(evening(time), predicate).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',28).
predicates([evening/1]).


%; time is in the night.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',30).
% predicate Night(time)
% From E: 
% 
% predicate(night(time)).
mpred_prop(night(time), predicate).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',30).
predicates([night/1]).


%; time is in the late night.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',32).
% predicate LateNight(time)
% From E: 
% 
% predicate(lateNight(time)).
mpred_prop(lateNight(time), predicate).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',32).
predicates([lateNight/1]).

% xor Morning, Afternoon, Evening, Night, LateNight
% From E: 
% 
% xor([morning,afternoon,evening,night,lateNight]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',32).
xor([morning,afternoon,evening,night,lateNight]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',35).
% [time]
 % Daytime(time) <-> Morning(time)|Afternoon(time)|Evening(time).
% From E: 
% 
% <->(
%    daytime(Time), 
%    ';'(
%       morning(Time), 
%       ';'(
%          afternoon(Time), 
%          evening(Time)))).
(   (   morning(Time)
    ;   afternoon(Time)
    ;   evening(Time)
    )
;   not daytime(Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',35).

 /*   (   (   morning(Time)
            ;   afternoon(Time)
            ;   evening(Time)
            )
        ;   not(daytime(Time))
        ).
 */
 %  % =================================.
(   daytime(Time)
;   not morning(Time),
    not afternoon(Time),
    not evening(Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',35).

 /*  (   daytime(Time)
     ;   not(morning(Time)),
         not(afternoon(Time)),
         not(evening(Time))
     ).
 */
 %  % =================================.


% [time]
 % Nighttime(time) <-> Night(time)|LateNight(time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',35).
% From E: 
% 
% <->(
%    nighttime(Time), 
%    ';'(
%       night(Time), 
%       lateNight(Time))).
(   (   night(Time)
    ;   lateNight(Time)
    )
;   not nighttime(Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',35).

 /*   (   (   night(Time)
            ;   lateNight(Time)
            )
        ;   not(nighttime(Time))
        ).
 */
 %  % =================================.
(   nighttime(Time)
;   not night(Time),
    not lateNight(Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',35).

 /*  (   nighttime(Time)
     ;   not(night(Time)),
         not(lateNight(Time))
     ).
 */
 %  % =================================.


%; dreams
%; time is part of a dream sequence.

% predicate DreamSequence(time)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',40).
% From E: 
% 
% predicate(dreamSequence(time)).
mpred_prop(dreamSequence(time), predicate).
predicates([dreamSequence/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.e',43).
%; End of file.
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/CTime.lps.pl')).
