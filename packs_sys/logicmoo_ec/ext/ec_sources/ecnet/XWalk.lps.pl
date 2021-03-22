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
% Sun, 21 Mar 2021 23:28:20 GMT File: <stream>(0x555567431900)%;
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

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',22).
% option trajectory on
% From E: 
% 
% ':-'(call_pel_directive(option(trajectory,on))).
:- call_pel_directive(option(trajectory, on)).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',24).
% sort xschema
% From E: 
% 
% sort(xschema).
sort(xschema).
%; parameters

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',28).
% predicate XWalkAgent(xschema,agent)
% From E: 
% 
% predicate(xWalkAgent(xschema,agent)).
mpred_prop(xWalkAgent(xschema, agent), predicate).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',28).
predicates([xWalkAgent/2]).

% function XWalkRate(xschema): offset ; step duration
% From E: 
% 
% function(
%    xWalkRate(xschema), 
%    [offset,;,step,duration]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',28).
function(xWalkRate(xschema),[offset,;,step,duration]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',30).
% function XWalkSize(xschema): offset ; step size
% From E: 
% 
% function(
%    xWalkSize(xschema), 
%    [offset,;,step,size]).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',30).
function(xWalkSize(xschema),[offset,;,step,size]).
%; TTL input lines

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',34).
% fluent XWalkEnabled(xschema)
% From E: 
% 
% fluent(xWalkEnabled(xschema)).
mpred_prop(xWalkEnabled(xschema), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',34).
fluents([xWalkEnabled/1]).

% fluent XWalkGroundStable(xschema)
% From E: 
% 
% fluent(xWalkGroundStable(xschema)).
mpred_prop(xWalkGroundStable(xschema), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',34).
fluents([xWalkGroundStable/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',36).
% fluent XWalkPosture(xschema)
% From E: 
% 
% fluent(xWalkPosture(xschema)).
mpred_prop(xWalkPosture(xschema), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',36).
fluents([xWalkPosture/1]).

% fluent XWalkFootingOK(xschema)
% From E: 
% 
% fluent(xWalkFootingOK(xschema)).
mpred_prop(xWalkFootingOK(xschema), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',36).
fluents([xWalkFootingOK/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',38).
% noninertial XWalkEnabled, XWalkGroundStable, XWalkPosture, XWalkFootingOK
% From E: 
% 
% ':-'(call_pel_directive(noninertial(xWalkEnabled))).
:- call_pel_directive(noninertial(xWalkEnabled)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',38).
% From E: 
% 
% ':-'(call_pel_directive(noninertial(xWalkGroundStable))).
:- call_pel_directive(noninertial(xWalkGroundStable)).
% From E: 
% 
% ':-'(call_pel_directive(noninertial(xWalkPosture))).
:- call_pel_directive(noninertial(xWalkPosture)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',38).
% From E: 
% 
% ':-'(call_pel_directive(noninertial(xWalkFootingOK))).
:- call_pel_directive(noninertial(xWalkFootingOK)).
%; fluents

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',42).
% fluent XWalkDistance(xschema,distance)
% From E: 
% 
% fluent(xWalkDistance(xschema,distance)).
mpred_prop(xWalkDistance(xschema, distance), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',42).
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
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',44).
 if(not(equals(Distance1, Distance2)),
       (not(xWalkDistance(Xschema, Distance1));not(xWalkDistance(Xschema, Distance2)))).


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
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',57).
if(not(xWalkVision(Xschema)), not(xWalkGroundStable(Xschema))),
if(not(xWalkGroundStable(Xschema)), not(xWalkVision(Xschema))).


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
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',62).
if(not(xWalkVisionOK(Xschema)),  (not(xWalkEnabled(Xschema));not(xWalkVision(Xschema));not(xWalkPosture(Xschema)))),
if((not(xWalkEnabled(Xschema));not(xWalkVision(Xschema));not(xWalkPosture(Xschema))), not(xWalkVisionOK(Xschema))).


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
 %   [Time].
if(not(xWalkAtDestination(Xschema)), not(xWalkDistance(Xschema, 0))),
if(not(xWalkDistance(Xschema, 0)), not(xWalkAtDestination(Xschema))).


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
 %   [Time].
if(not(xWalkDone(Xschema)), not(xWalkAtDestination(Xschema))),
if(not(xWalkAtDestination(Xschema)), not(xWalkDone(Xschema))).


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
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',82).
 if(not(initiates(xWalkSteppingOn(Xschema),
                    at(xWalkStepping(Xschema, Distance2),
                       Time))),
       (not(holds(xWalkDistance(Xschema, Distance1), Time));not(equals(Distance2, Distance1-xWalkSize(Xschema))))).


% [xschema,distance,time]
% Terminates(XWalkSteppingOff(xschema),XWalkStepping(xschema,distance),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',88).
% From E: 
% 
% terminates_at(
%    xWalkSteppingOff(Xschema), 
%    xWalkStepping(Xschema,Distance), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',88).
terminates(xWalkSteppingOff(Xschema),
	   xWalkStepping(Xschema,Distance)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',90).
% [xschema,distance,time]
% Releases(XWalkSteppingOn(xschema),XWalkDistance(xschema,distance),time).
% From E: 
% 
% releases_at(
%    xWalkSteppingOn(Xschema), 
%    xWalkDistance(Xschema,Distance), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',90).
releases(xWalkSteppingOn(Xschema),
	 xWalkDistance(Xschema,Distance)).


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
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',94).
 if(not(terminates(xWalkSteppingOff(Xschema),
                     at(xWalkDistance(Xschema, Distance2),
                        Time))),
       (not(holds(xWalkDistance(Xschema, Distance1), Time));not({dif(Distance1, Distance2)}))).


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
 %   [Time].
if(not(initiates(xWalkSteppingOff(Xschema),
		 at(xWalkDistance(Xschema,Distance),
		    Time))),
   not(holds(xWalkDistance(Xschema,Distance),Time))).


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
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',103).
 if(not(trajectory(xWalkStepping(Xschema, Distance02),
                     Time,
                     xWalkDistance(Xschema, Distance03),
                     Offset)),
       (not(holds(xWalkDistance(Xschema, Distance01), Time));not(equals(Distance03, Distance01-Offset*(xWalkSize(Xschema)/xWalkRate(Xschema)))))).


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
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',111).
 if(not(xWalkSteppingOff(Xschema)),
       (not(xWalkStepping(Xschema, Distance));not(xWalkDistance(Xschema, Distance)))).


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
 %   [Time, Time+1].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',120).
 if(not(holds(xWalkMoveFoot(Xschema), Time+1)),
       (not(holds(xWalkTestFooting(Xschema), Time));holds(xWalkFootingOK(Xschema), Time))).


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
 %   [Time, Time+1].
if(not(holds(xWalkReadyOn(Xschema),Time+1)),
   not(holds(xWalkMoveFoot(Xschema),Time))).


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
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',135).
 if(not(xWalkReadyOn(Xschema)),
       (not(xWalkEnabled(Xschema));not(xWalkVision(Xschema));not(xWalkPosture(Xschema));thereExists(Distance, xWalkStepping(Xschema, Distance));xWalkReady(Xschema))).


% [xschema,time]
% Initiates(XWalkReadyOn(xschema),XWalkReady(xschema),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',144).
% From E: 
% 
% initiates_at(
%    xWalkReadyOn(Xschema), 
%    xWalkReady(Xschema), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',144).
initiates(xWalkReadyOn(Xschema),xWalkReady(Xschema)).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',146).
% [xschema,time]
% Terminates(XWalkReadyOff(xschema),XWalkReady(xschema),time).
% From E: 
% 
% terminates_at(
%    xWalkReadyOff(Xschema), 
%    xWalkReady(Xschema), 
%    Time).
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',146).
terminates(xWalkReadyOff(Xschema),xWalkReady(Xschema)).


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
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',150).
exists(Distance,  (xWalkSteppingOn(Xschema), xWalkReadyOff(Xschema);xWalkStepping(Xschema, Distance);not(xWalkVisionOK(Xschema));not(xWalkReady(Xschema)))).


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
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',158).
exists(Distance,  (xWalkTestFooting(Xschema), xWalkReadyOff(Xschema);xWalkStepping(Xschema, Distance);xWalkVisionOK(Xschema);not(xWalkReady(Xschema)))).


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
 %   [Time].
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',166).
 if(not(xWalkReadyOn(Xschema)),
       (not(xWalkStepping(Xschema, Distance));not(xWalkDistance(Xschema, Distance));not(comparison(Distance, 0, >)))).


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.e',170).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/XWalk.lps.pl')).
