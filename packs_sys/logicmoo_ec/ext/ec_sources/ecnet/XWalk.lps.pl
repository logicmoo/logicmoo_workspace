% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/WritingABook.e',97).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.lps.pl')).
% Fri, 26 Mar 2021 01:06:14 GMT File: <stream>(0x555567ca9700)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; XWalk: WALK x-schema representation of walking
%;
%; @phdthesis{Narayanan:1997,
%;   author = "Srinivas S. Narayanan",
%;   year = "1997",
%;   title = "Knowledge-based Action Representations for Metaphor and Aspect (\uppercase{KARMA})",
%;   address = "Berkeley, CA",
%;   school = "University of California, Berkeley",
%; }
%;

% option trajectory on
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',22).
% From E: 
% 
% ':-'(call_pel_directive(option(trajectory,on))).
:- call_pel_directive(option(trajectory, on)).

% sort xschema
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',24).
% From E: 
% 
% sort(xschema).
sort(xschema).
%; parameters

% predicate XWalkAgent(xschema,agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',28).
% From E: 
% 
% predicate(xWalkAgent(xschema,agent)).
mpred_prop(xWalkAgent(xschema, agent), predicate).
predicates([xWalkAgent/2]).

% function XWalkRate(xschema): offset ; step duration
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',28).
% From E: 
% 
% function(
%    xWalkRate(xschema), 
%    [offset,;,step,duration]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',28).
function(xWalkRate(xschema),[offset,;,step,duration]).

% function XWalkSize(xschema): offset ; step size
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',30).
% From E: 
% 
% function(
%    xWalkSize(xschema), 
%    [offset,;,step,size]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',30).
function(xWalkSize(xschema),[offset,;,step,size]).
%; TTL input lines

% fluent XWalkEnabled(xschema)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',34).
% From E: 
% 
% fluent(xWalkEnabled(xschema)).
mpred_prop(xWalkEnabled(xschema), fluent).
fluents([xWalkEnabled/1]).

% fluent XWalkGroundStable(xschema)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',34).
% From E: 
% 
% fluent(xWalkGroundStable(xschema)).
mpred_prop(xWalkGroundStable(xschema), fluent).
fluents([xWalkGroundStable/1]).

% fluent XWalkPosture(xschema)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',36).
% From E: 
% 
% fluent(xWalkPosture(xschema)).
mpred_prop(xWalkPosture(xschema), fluent).
fluents([xWalkPosture/1]).

% fluent XWalkFootingOK(xschema)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',36).
% From E: 
% 
% fluent(xWalkFootingOK(xschema)).
mpred_prop(xWalkFootingOK(xschema), fluent).
fluents([xWalkFootingOK/1]).

% noninertial XWalkEnabled, XWalkGroundStable, XWalkPosture, XWalkFootingOK
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',38).
% From E: 
% 
% ':-'(call_pel_directive(noninertial(xWalkEnabled))).
:- call_pel_directive(noninertial(xWalkEnabled)).
% From E: 
% 
% ':-'(call_pel_directive(noninertial(xWalkGroundStable))).
:- call_pel_directive(noninertial(xWalkGroundStable)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',38).
% From E: 
% 
% ':-'(call_pel_directive(noninertial(xWalkPosture))).
:- call_pel_directive(noninertial(xWalkPosture)).
% From E: 
% 
% ':-'(call_pel_directive(noninertial(xWalkFootingOK))).
:- call_pel_directive(noninertial(xWalkFootingOK)).
%; fluents

% fluent XWalkDistance(xschema,distance)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',42).
% From E: 
% 
% fluent(xWalkDistance(xschema,distance)).
mpred_prop(xWalkDistance(xschema, distance), fluent).
fluents([xWalkDistance/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',44).
% [xschema,distance1,distance2,time]
% HoldsAt(XWalkDistance(xschema,distance1),time) &
% HoldsAt(XWalkDistance(xschema,distance2),time) ->
% distance1=distance2.
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          xWalkDistance(Xschema,Distance1), 
%          Time), 
%       holds(
%          xWalkDistance(Xschema,Distance2), 
%          Time)), 
%    Distance1=Distance2).
(   equals(Distance1, Distance2)
;   not xWalkDistance(Xschema, Distance1)at Time
;   not xWalkDistance(Xschema, Distance2)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',44).

 /*   (   equals(Distance1, Distance2)
        ;   at(not(xWalkDistance(Xschema, Distance1)), Time)
        ;   at(not(xWalkDistance(Xschema, Distance2)), Time)
        ).
 */
 %  % =================================.


%; logic gate behavior

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',51).
% fluent XWalkVision(xschema)
% From E: 
% 
% fluent(xWalkVision(xschema)).
mpred_prop(xWalkVision(xschema), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',51).
fluents([xWalkVision/1]).

% fluent XWalkVisionOK(xschema)
% From E: 
% 
% fluent(xWalkVisionOK(xschema)).
mpred_prop(xWalkVisionOK(xschema), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',51).
fluents([xWalkVisionOK/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',53).
% fluent XWalkAtDestination(xschema)
% From E: 
% 
% fluent(xWalkAtDestination(xschema)).
mpred_prop(xWalkAtDestination(xschema), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',53).
fluents([xWalkAtDestination/1]).

% fluent XWalkDone(xschema)
% From E: 
% 
% fluent(xWalkDone(xschema)).
mpred_prop(xWalkDone(xschema), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',53).
fluents([xWalkDone/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',55).
% noninertial XWalkVision, XWalkVisionOK, XWalkAtDestination, XWalkDone
% From E: 
% 
% ':-'(call_pel_directive(noninertial(xWalkVision))).
:- call_pel_directive(noninertial(xWalkVision)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',55).
% From E: 
% 
% ':-'(call_pel_directive(noninertial(xWalkVisionOK))).
:- call_pel_directive(noninertial(xWalkVisionOK)).
% From E: 
% 
% ':-'(call_pel_directive(noninertial(xWalkAtDestination))).
:- call_pel_directive(noninertial(xWalkAtDestination)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',55).
% From E: 
% 
% ':-'(call_pel_directive(noninertial(xWalkDone))).
:- call_pel_directive(noninertial(xWalkDone)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',57).
% [xschema,time]
% HoldsAt(XWalkGroundStable(xschema),time) <->
% HoldsAt(XWalkVision(xschema),time).
% From E: 
% 
% <->(
%    holds(
%       xWalkGroundStable(Xschema), 
%       Time), 
%    holds(
%       xWalkVision(Xschema), 
%       Time)).
(   xWalkVision(Xschema)at Time
;   not xWalkGroundStable(Xschema)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',57).

 /*   (   at(xWalkVision(Xschema), Time)
        ;   at(not(xWalkGroundStable(Xschema)), Time)
        ).
 */
 %  % =================================.
(   xWalkGroundStable(Xschema)at Time
;   not xWalkVision(Xschema)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',57).

 /*   (   at(xWalkGroundStable(Xschema), Time)
        ;   at(not(xWalkVision(Xschema)), Time)
        ).
 */
 %  % =================================.


% [xschema,time]
% HoldsAt(XWalkEnabled(xschema),time) &
% HoldsAt(XWalkVision(xschema),time) &
% HoldsAt(XWalkPosture(xschema),time) <->
% HoldsAt(XWalkVisionOK(xschema),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',62).
% From E: 
% 
% <->(
%    ','(
%       holds(
%          xWalkEnabled(Xschema), 
%          Time), 
%       ','(
%          holds(
%             xWalkVision(Xschema), 
%             Time), 
%          holds(
%             xWalkPosture(Xschema), 
%             Time))), 
%    holds(
%       xWalkVisionOK(Xschema), 
%       Time)).
(   xWalkVisionOK(Xschema)at Time
;   not xWalkEnabled(Xschema)at Time
;   not xWalkVision(Xschema)at Time
;   not xWalkPosture(Xschema)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',62).

 /*   (   at(xWalkVisionOK(Xschema), Time)
        ;   at(not(xWalkEnabled(Xschema)), Time)
        ;   at(not(xWalkVision(Xschema)), Time)
        ;   at(not(xWalkPosture(Xschema)), Time)
        ).
 */
 %  % =================================.
(   xWalkEnabled(Xschema)at Time,
    xWalkVision(Xschema)at Time,
    xWalkPosture(Xschema)at Time
;   not xWalkVisionOK(Xschema)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',62).

 /*  (   at(xWalkEnabled(Xschema), Time),
         at(xWalkVision(Xschema), Time),
         at(xWalkPosture(Xschema), Time)
     ;   at(not(xWalkVisionOK(Xschema)), Time)
     ).
 */
 %  % =================================.


% [xschema,time]
% HoldsAt(XWalkDistance(xschema,0),time) <->
% HoldsAt(XWalkAtDestination(xschema),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',68).
% From E: 
% 
% <->(
%    holds(
%       xWalkDistance(Xschema,0), 
%       Time), 
%    holds(
%       xWalkAtDestination(Xschema), 
%       Time)).
(   xWalkAtDestination(Xschema)at Time
;   not xWalkDistance(Xschema, 0)at Time
).

 /*   (   at(xWalkAtDestination(Xschema), Time)
        ;   at(not(xWalkDistance(Xschema, 0)), Time)
        ).
 */
 %  % =================================.
(   xWalkDistance(Xschema, 0)at Time
;   not xWalkAtDestination(Xschema)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',68).

 /*   (   at(xWalkDistance(Xschema, 0), Time)
        ;   at(not(xWalkAtDestination(Xschema)), Time)
        ).
 */
 %  % =================================.


% [xschema,time]
% HoldsAt(XWalkAtDestination(xschema),time) <->
% HoldsAt(XWalkDone(xschema),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',72).
% From E: 
% 
% <->(
%    holds(
%       xWalkAtDestination(Xschema), 
%       Time), 
%    holds(
%       xWalkDone(Xschema), 
%       Time)).
(   xWalkDone(Xschema)at Time
;   not xWalkAtDestination(Xschema)at Time
).

 /*   (   at(xWalkDone(Xschema), Time)
        ;   at(not(xWalkAtDestination(Xschema)), Time)
        ).
 */
 %  % =================================.
(   xWalkAtDestination(Xschema)at Time
;   not xWalkDone(Xschema)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',72).

 /*   (   at(xWalkAtDestination(Xschema), Time)
        ;   at(not(xWalkDone(Xschema)), Time)
        ).
 */
 %  % =================================.


%; durative events
%; distance is the goal

% fluent XWalkStepping(xschema,distance) 
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',77).
% From E: 
% 
% fluent(xWalkStepping(xschema,distance)).
mpred_prop(xWalkStepping(xschema, distance), fluent).
fluents([xWalkStepping/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',79).
% event XWalkSteppingOn(xschema)
% From E: 
% 
% event(xWalkSteppingOn(xschema)).
mpred_prop(xWalkSteppingOn(xschema), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',79).
events([xWalkSteppingOn/1]).

% event XWalkSteppingOff(xschema)
% From E: 
% 
% event(xWalkSteppingOff(xschema)).
mpred_prop(xWalkSteppingOff(xschema), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',79).
events([xWalkSteppingOff/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',82).
% [xschema,distance1,distance2,time]
% HoldsAt(XWalkDistance(xschema,distance1),time) &
% distance2 = distance1 - XWalkSize(xschema) ->
% Initiates(XWalkSteppingOn(xschema),XWalkStepping(xschema,distance2),time).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          xWalkDistance(Xschema,Distance1), 
%          Time), 
%       Distance2=Distance1-xWalkSize(Xschema)), 
%    initiates_at(
%       xWalkSteppingOn(Xschema), 
%       xWalkStepping(Xschema,Distance2), 
%       Time)).
(   initiates(xWalkSteppingOn(Xschema),
              xWalkStepping(Xschema, Distance2)at Time)
;   not xWalkDistance(Xschema, Distance1)at Time
;   not equals(Distance2, Distance1-xWalkSize(Xschema))
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',82).

 /*   (   initiates(xWalkSteppingOn(Xschema),
                      at(xWalkStepping(Xschema, Distance2), Time))
        ;   at(not(xWalkDistance(Xschema, Distance1)), Time)
        ;   not(equals(Distance2, Distance1-xWalkSize(Xschema)))
        ).
 */
 %  % =================================.


% [xschema,distance,time]
% Terminates(XWalkSteppingOff(xschema),XWalkStepping(xschema,distance),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',88).
% From E: 
% 
% terminates_at(
%    xWalkSteppingOff(Xschema), 
%    xWalkStepping(Xschema,Distance), 
%    Time).
xWalkSteppingOff(Xschema)terminates xWalkStepping(Xschema, Distance).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',88).

 /*  terminated(happens(xWalkSteppingOff(Xschema),
     		   Time_from,
     		   Time_until),
     	   xWalkStepping(Xschema,Distance),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',90).
% [xschema,distance,time]
% Releases(XWalkSteppingOn(xschema),XWalkDistance(xschema,distance),time).
% From E: 
% 
% releases_at(
%    xWalkSteppingOn(Xschema), 
%    xWalkDistance(Xschema,Distance), 
%    Time).
releases(xWalkSteppingOn(Xschema), xWalkDistance(Xschema, Distance)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',90).

 /*  releases(xWalkSteppingOn(Xschema),
     	 xWalkDistance(Xschema,Distance)).
 */
 %  % =================================.


% [xschema,distance1,distance2,time]
% HoldsAt(XWalkDistance(xschema,distance1),time) &
% distance1 != distance2 ->
% Terminates(XWalkSteppingOff(xschema),XWalkDistance(xschema,distance2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',94).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          xWalkDistance(Xschema,Distance1), 
%          Time), 
%       Distance1\=Distance2), 
%    terminates_at(
%       xWalkSteppingOff(Xschema), 
%       xWalkDistance(Xschema,Distance2), 
%       Time)).
(   terminates(xWalkSteppingOff(Xschema),
               xWalkDistance(Xschema, Distance2)at Time)
;   not xWalkDistance(Xschema, Distance1)at Time
;   not {dif(Distance1, Distance2)}
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',94).

 /*   (   terminates(xWalkSteppingOff(Xschema),
                       at(xWalkDistance(Xschema, Distance2), Time))
        ;   at(not(xWalkDistance(Xschema, Distance1)), Time)
        ;   not({dif(Distance1, Distance2)})
        ).
 */
 %  % =================================.


% [xschema,distance,time]
% HoldsAt(XWalkDistance(xschema,distance),time) ->
% Initiates(XWalkSteppingOff(xschema),XWalkDistance(xschema,distance),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',99).
% From E: 
% 
% '->'(
%    holds(
%       xWalkDistance(Xschema,Distance), 
%       Time), 
%    initiates_at(
%       xWalkSteppingOff(Xschema), 
%       xWalkDistance(Xschema,Distance), 
%       Time)).
(   initiates(xWalkSteppingOff(Xschema),
              xWalkDistance(Xschema, Distance)at Time)
;   not xWalkDistance(Xschema, Distance)at Time
).

 /*   (   initiates(xWalkSteppingOff(Xschema),
                      at(xWalkDistance(Xschema, Distance), Time))
        ;   at(not(xWalkDistance(Xschema, Distance)), Time)
        ).
 */
 %  % =================================.


% [xschema,distance01,distance02,distance03,offset,time]
% HoldsAt(XWalkDistance(xschema,distance01),time) &
% (distance03=(distance01-(offset*(XWalkSize(xschema)/XWalkRate(xschema))))) ->
% Trajectory(XWalkStepping(xschema,distance02),
%            time,
%            XWalkDistance(xschema,distance03),
%            offset).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',103).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          xWalkDistance(Xschema,Distance01), 
%          Time), 
%       Distance03=Distance01-Offset*(xWalkSize(Xschema)/xWalkRate(Xschema))), 
%    trajectory(
%       xWalkStepping(Xschema,Distance02), 
%       Time, 
%       xWalkDistance(Xschema,Distance03), 
%       Offset)).
(   trajectory(xWalkStepping(Xschema, Distance02),
               Time,
               xWalkDistance(Xschema, Distance03),
               Offset)
;   not xWalkDistance(Xschema, Distance01)at Time
;   not(equals(Distance03,
               Distance01-Offset*(xWalkSize(Xschema)/xWalkRate(Xschema))))
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',103).

 /*   (   trajectory(xWalkStepping(Xschema, Distance02),
                       Time,
                       xWalkDistance(Xschema, Distance03),
                       Offset)
        ;   at(not(xWalkDistance(Xschema, Distance01)), Time)
        ;   not(equals(Distance03,
                       Distance01-Offset*(xWalkSize(Xschema)/xWalkRate(Xschema))))
        ).
 */
 %  % =================================.


% [xschema,distance,time]
% HoldsAt(XWalkStepping(xschema,distance),time) &
% HoldsAt(XWalkDistance(xschema,distance),time) ->
% Happens(XWalkSteppingOff(xschema),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',111).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          xWalkStepping(Xschema,Distance), 
%          Time), 
%       holds(
%          xWalkDistance(Xschema,Distance), 
%          Time)), 
%    happens(
%       xWalkSteppingOff(Xschema), 
%       Time)).
(   happens(xWalkSteppingOff(Xschema), Time)
;   not xWalkStepping(Xschema, Distance)at Time
;   not xWalkDistance(Xschema, Distance)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',111).

 /*   (   happens(xWalkSteppingOff(Xschema), Time)
        ;   at(not(xWalkStepping(Xschema, Distance)), Time)
        ;   at(not(xWalkDistance(Xschema, Distance)), Time)
        ).
 */
 %  % =================================.


%; punctual events

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',117).
% event XWalkTestFooting(xschema)
% From E: 
% 
% event(xWalkTestFooting(xschema)).
mpred_prop(xWalkTestFooting(xschema), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',117).
events([xWalkTestFooting/1]).

% event XWalkMoveFoot(xschema)
% From E: 
% 
% event(xWalkMoveFoot(xschema)).
mpred_prop(xWalkMoveFoot(xschema), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',117).
events([xWalkMoveFoot/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',120).
% [xschema,time]
% Happens(XWalkTestFooting(xschema),time) &
% !HoldsAt(XWalkFootingOK(xschema),time) ->
% Happens(XWalkMoveFoot(xschema),time+1).
% From E: 
% 
% '->'(
%    ','(
%       happens(
%          xWalkTestFooting(Xschema), 
%          Time), 
%       holds(
%          not(xWalkFootingOK(Xschema)), 
%          Time)), 
%    happens(
%       xWalkMoveFoot(Xschema), 
%       Time+1)).
(   happens(xWalkMoveFoot(Xschema), Time+1)
;   not happens(xWalkTestFooting(Xschema), Time)
;   xWalkFootingOK(Xschema)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',120).

 /*   (   happens(xWalkMoveFoot(Xschema), Time+1)
        ;   not(happens(xWalkTestFooting(Xschema), Time))
        ;   at(xWalkFootingOK(Xschema), Time)
        ).
 */
 %  % =================================.


% [xschema,time]
% Happens(XWalkMoveFoot(xschema),time) ->
% Happens(XWalkReadyOn(xschema),time+1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',126).
% From E: 
% 
% '->'(
%    happens(
%       xWalkMoveFoot(Xschema), 
%       Time), 
%    happens(
%       xWalkReadyOn(Xschema), 
%       Time+1)).
(   happens(xWalkReadyOn(Xschema), Time+1)
;   not happens(xWalkMoveFoot(Xschema), Time)
).

 /*   (   happens(xWalkReadyOn(Xschema), Time+1)
        ;   not(happens(xWalkMoveFoot(Xschema), Time))
        ).
 */
 %  % =================================.


%; Petri net behavior

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',131).
% fluent XWalkReady(xschema)
% From E: 
% 
% fluent(xWalkReady(xschema)).
mpred_prop(xWalkReady(xschema), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',131).
fluents([xWalkReady/1]).

% event XWalkReadyOn(xschema)
% From E: 
% 
% event(xWalkReadyOn(xschema)).
mpred_prop(xWalkReadyOn(xschema), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',131).
events([xWalkReadyOn/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',133).
% event XWalkReadyOff(xschema)
% From E: 
% 
% event(xWalkReadyOff(xschema)).
mpred_prop(xWalkReadyOff(xschema), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',133).
events([xWalkReadyOff/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',135).
% [xschema,time]
% HoldsAt(XWalkEnabled(xschema),time) &
% HoldsAt(XWalkVision(xschema),time) &
% HoldsAt(XWalkPosture(xschema),time) &
% !({distance} HoldsAt(XWalkStepping(xschema,distance),time)) & ; !!! pulse
% !HoldsAt(XWalkReady(xschema),time) ->
% Happens(XWalkReadyOn(xschema),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',135).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          xWalkEnabled(Xschema), 
%          Time), 
%       ','(
%          holds(
%             xWalkVision(Xschema), 
%             Time), 
%          ','(
%             holds(
%                xWalkPosture(Xschema), 
%                Time), 
%             ','(
%                not(thereExists(Distance, 
%                       holds(
%                          xWalkStepping(Xschema,Distance), 
%                          Time))), 
%                holds(
%                   not(xWalkReady(Xschema)), 
%                   Time))))), 
%    happens(
%       xWalkReadyOn(Xschema), 
%       Time)).
(   happens(xWalkReadyOn(Xschema), Time)
;   not xWalkEnabled(Xschema)at Time
;   not xWalkVision(Xschema)at Time
;   not xWalkPosture(Xschema)at Time
;   thereExists(Distance,
                xWalkStepping(Xschema, Distance)at Time)
;   xWalkReady(Xschema)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',135).

 /*   (   happens(xWalkReadyOn(Xschema), Time)
        ;   at(not(xWalkEnabled(Xschema)), Time)
        ;   at(not(xWalkVision(Xschema)), Time)
        ;   at(not(xWalkPosture(Xschema)), Time)
        ;   thereExists(Distance,
                        at(xWalkStepping(Xschema, Distance), Time))
        ;   at(xWalkReady(Xschema), Time)
        ).
 */
 %  % =================================.


% [xschema,time]
% Initiates(XWalkReadyOn(xschema),XWalkReady(xschema),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',144).
% From E: 
% 
% initiates_at(
%    xWalkReadyOn(Xschema), 
%    xWalkReady(Xschema), 
%    Time).
xWalkReadyOn(Xschema)initiates xWalkReady(Xschema).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',144).

 /*  initiated(happens(xWalkReadyOn(Xschema),
     		  Time_from,
     		  Time_until),
     	  xWalkReady(Xschema),
     	  []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',146).
% [xschema,time]
% Terminates(XWalkReadyOff(xschema),XWalkReady(xschema),time).
% From E: 
% 
% terminates_at(
%    xWalkReadyOff(Xschema), 
%    xWalkReady(Xschema), 
%    Time).
xWalkReadyOff(Xschema)terminates xWalkReady(Xschema).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',146).

 /*  terminated(happens(xWalkReadyOff(Xschema),
     		   Time_from,
     		   Time_until),
     	   xWalkReady(Xschema),
     	   []).
 */
 %  % =================================.


%; bypass_ok
% [xschema,time]
% !(% {distance} HoldsAt(XWalkStepping(xschema,distance),time)) &
% HoldsAt(XWalkVisionOK(xschema),time) &
% HoldsAt(XWalkReady(xschema),time) ->
% Happens(XWalkSteppingOn(xschema),time) &
% Happens(XWalkReadyOff(xschema),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',150).
% From E: 
% 
% exists(Distance, 
%    '->'(
%       ','(
%          holds(
%             not(xWalkStepping(Xschema,Distance)), 
%             Time), 
%          ','(
%             holds(
%                xWalkVisionOK(Xschema), 
%                Time), 
%             holds(
%                xWalkReady(Xschema), 
%                Time))), 
%       ','(
%          happens(
%             xWalkSteppingOn(Xschema), 
%             Time), 
%          happens(
%             xWalkReadyOff(Xschema), 
%             Time)))).
exists(Distance,  (happens(xWalkSteppingOn(Xschema), Time), happens(xWalkReadyOff(Xschema), Time);xWalkStepping(Xschema, Distance)at Time;not xWalkVisionOK(Xschema)at Time;not xWalkReady(Xschema)at Time)).
 %  exists(Distance,  (happens(xWalkSteppingOn(Xschema), Time), happens(xWalkReadyOff(Xschema), Time);at(xWalkStepping(Xschema, Distance), Time);at(not(xWalkVisionOK(Xschema)), Time);at(not(xWalkReady(Xschema)), Time))).
 %  % =================================.


%; !bypass_ok
% [xschema,time]
% !(% {distance} HoldsAt(XWalkStepping(xschema,distance),time)) &
% !HoldsAt(XWalkVisionOK(xschema),time) &
% HoldsAt(XWalkReady(xschema),time) ->
% Happens(XWalkTestFooting(xschema),time) &
% Happens(XWalkReadyOff(xschema),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',158).
% From E: 
% 
% exists(Distance, 
%    '->'(
%       ','(
%          holds(
%             not(xWalkStepping(Xschema,Distance)), 
%             Time), 
%          ','(
%             holds(
%                not(xWalkVisionOK(Xschema)), 
%                Time), 
%             holds(
%                xWalkReady(Xschema), 
%                Time))), 
%       ','(
%          happens(
%             xWalkTestFooting(Xschema), 
%             Time), 
%          happens(
%             xWalkReadyOff(Xschema), 
%             Time)))).
exists(Distance,  (happens(xWalkTestFooting(Xschema), Time), happens(xWalkReadyOff(Xschema), Time);xWalkStepping(Xschema, Distance)at Time;xWalkVisionOK(Xschema)at Time;not xWalkReady(Xschema)at Time)).
 %  exists(Distance,  (happens(xWalkTestFooting(Xschema), Time), happens(xWalkReadyOff(Xschema), Time);at(xWalkStepping(Xschema, Distance), Time);at(xWalkVisionOK(Xschema), Time);at(not(xWalkReady(Xschema)), Time))).
 %  % =================================.


% [xschema,distance,time]
% HoldsAt(XWalkStepping(xschema,distance),time) &
% HoldsAt(XWalkDistance(xschema,distance),time) &
% (distance > 0) ->
% Happens(XWalkReadyOn(xschema),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',166).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          xWalkStepping(Xschema,Distance), 
%          Time), 
%       ','(
%          holds(
%             xWalkDistance(Xschema,Distance), 
%             Time), 
%          Distance>0)), 
%    happens(
%       xWalkReadyOn(Xschema), 
%       Time)).
(   happens(xWalkReadyOn(Xschema), Time)
;   not xWalkStepping(Xschema, Distance)at Time
;   not xWalkDistance(Xschema, Distance)at Time
;   not comparison(Distance, 0, >)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',166).

 /*   (   happens(xWalkReadyOn(Xschema), Time)
        ;   at(not(xWalkStepping(Xschema, Distance)), Time)
        ;   at(not(xWalkDistance(Xschema, Distance)), Time)
        ;   not(comparison(Distance, 0, >))
        ).
 */
 %  % =================================.


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',170).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.lps.pl')).
