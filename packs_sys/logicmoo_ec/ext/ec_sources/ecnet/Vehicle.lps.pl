% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/TimeDelayBombing.e',147).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.lps.pl')).
% Fri, 26 Mar 2021 01:06:12 GMT File: <stream>(0x555566f09a00)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; Vehicle: transportation vehicles
%;

% sort vehicle: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',14).
% From E: 
% 
% subsort(vehicle,physobj).
subsort(vehicle, physobj).

% sort vehiclein: vehicle
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',14).
% From E: 
% 
% subsort(vehiclein,vehicle).
subsort(vehiclein, vehicle).

% sort vehicleon: vehicle
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',16).
% From E: 
% 
% subsort(vehicleon,vehicle).
subsort(vehicleon, vehicle).

% sort train: vehicleon
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',16).
% From E: 
% 
% subsort(train,vehicleon).
subsort(train, vehicleon).

% sort carriage: vehiclein
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',18).
% From E: 
% 
% subsort(carriage,vehiclein).
subsort(carriage, vehiclein).

% sort vehicledoor
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',20).
% From E: 
% 
% sort(vehicledoor).
sort(vehicledoor).
%; RideTrack

% event RideTrack12(train,track)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',24).
% From E: 
% 
% event(rideTrack12(train,track)).
mpred_prop(rideTrack12(train, track), event).
events([rideTrack12/2]).

% event RideTrack21(train,track)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',26).
% From E: 
% 
% event(rideTrack21(train,track)).
mpred_prop(rideTrack21(train, track), event).
events([rideTrack21/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',28).
% [train,track,time]
% Happens(RideTrack12(train,track),time) ->
% HoldsAt(At(train,Side1(track)),time).
% From E: 
% 
% '->'(
%    happens(
%       rideTrack12(Train,Track), 
%       Time), 
%    holds(
%       at_loc(Train, 
%          side1(Track)), 
%       Time)).
(   at_loc(Train, side1(Track))at Time
;   not happens(rideTrack12(Train, Track), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',28).

 /*   (   at(at_loc(Train, side1(Track)), Time)
        ;   not(happens(rideTrack12(Train, Track), Time))
        ).
 */
 %  % =================================.


% [train,track,time]
% Happens(RideTrack21(train,track),time) ->
% HoldsAt(At(train,Side2(track)),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',33).
% From E: 
% 
% '->'(
%    happens(
%       rideTrack21(Train,Track), 
%       Time), 
%    holds(
%       at_loc(Train, 
%          side2(Track)), 
%       Time)).
(   at_loc(Train, side2(Track))at Time
;   not happens(rideTrack21(Train, Track), Time)
).

 /*   (   at(at_loc(Train, side2(Track)), Time)
        ;   not(happens(rideTrack21(Train, Track), Time))
        ).
 */
 %  % =================================.


% [train,track,location,time]
% Side2(track)=location ->
% Initiates(RideTrack12(train,track),At(train,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',37).
% From E: 
% 
% '->'(
%    '='(
%       side2(Track), 
%       Location), 
%    initiates_at(
%       rideTrack12(Train,Track), 
%       at_loc(Train,Location), 
%       Time)).
(   initiates(rideTrack12(Train, Track),
              at_loc(Train, Location)at Time)
;   not equals(side2(Track), Location)
).

 /*   (   initiates(rideTrack12(Train, Track),
                      at(at_loc(Train, Location), Time))
        ;   not(equals(side2(Track), Location))
        ).
 */
 %  % =================================.


% [train,track,location,time]
% Side1(track)=location ->
% Initiates(RideTrack21(train,track),At(train,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',41).
% From E: 
% 
% '->'(
%    '='(
%       side1(Track), 
%       Location), 
%    initiates_at(
%       rideTrack21(Train,Track), 
%       at_loc(Train,Location), 
%       Time)).
(   initiates(rideTrack21(Train, Track),
              at_loc(Train, Location)at Time)
;   not equals(side1(Track), Location)
).

 /*   (   initiates(rideTrack21(Train, Track),
                      at(at_loc(Train, Location), Time))
        ;   not(equals(side1(Track), Location))
        ).
 */
 %  % =================================.


% [train,track,location,time]
% Side1(track)=location ->
% Terminates(RideTrack12(train,track),At(train,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',45).
% From E: 
% 
% '->'(
%    '='(
%       side1(Track), 
%       Location), 
%    terminates_at(
%       rideTrack12(Train,Track), 
%       at_loc(Train,Location), 
%       Time)).
(   terminates(rideTrack12(Train, Track),
               at_loc(Train, Location)at Time)
;   not equals(side1(Track), Location)
).

 /*   (   terminates(rideTrack12(Train, Track),
                       at(at_loc(Train, Location), Time))
        ;   not(equals(side1(Track), Location))
        ).
 */
 %  % =================================.


% [train,track,location,time]
% Side2(track)=location ->
% Terminates(RideTrack21(train,track),At(train,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',49).
% From E: 
% 
% '->'(
%    '='(
%       side2(Track), 
%       Location), 
%    terminates_at(
%       rideTrack21(Train,Track), 
%       at_loc(Train,Location), 
%       Time)).
(   terminates(rideTrack21(Train, Track),
               at_loc(Train, Location)at Time)
;   not equals(side2(Track), Location)
).

 /*   (   terminates(rideTrack21(Train, Track),
                       at(at_loc(Train, Location), Time))
        ;   not(equals(side2(Track), Location))
        ).
 */
 %  % =================================.


%; DriveStreet

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',54).
% event DriveStreet12(vehicle,street)
% From E: 
% 
% event(driveStreet12(vehicle,street)).
mpred_prop(driveStreet12(vehicle, street), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',54).
events([driveStreet12/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',56).
% event DriveStreet21(vehicle,street)
% From E: 
% 
% event(driveStreet21(vehicle,street)).
mpred_prop(driveStreet21(vehicle, street), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',56).
events([driveStreet21/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',58).
% [vehicle,street,time]
% Happens(DriveStreet12(vehicle,street),time) ->
% HoldsAt(At(vehicle,Side1(street)),time).
% From E: 
% 
% '->'(
%    happens(
%       driveStreet12(Vehicle,Street), 
%       Time), 
%    holds(
%       at_loc(Vehicle, 
%          side1(Street)), 
%       Time)).
(   at_loc(Vehicle, side1(Street))at Time
;   not happens(driveStreet12(Vehicle, Street), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',58).

 /*   (   at(at_loc(Vehicle, side1(Street)), Time)
        ;   not(happens(driveStreet12(Vehicle, Street), Time))
        ).
 */
 %  % =================================.


% [vehicle,street,time]
% Happens(DriveStreet21(vehicle,street),time) ->
% HoldsAt(At(vehicle,Side2(street)),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',63).
% From E: 
% 
% '->'(
%    happens(
%       driveStreet21(Vehicle,Street), 
%       Time), 
%    holds(
%       at_loc(Vehicle, 
%          side2(Street)), 
%       Time)).
(   at_loc(Vehicle, side2(Street))at Time
;   not happens(driveStreet21(Vehicle, Street), Time)
).

 /*   (   at(at_loc(Vehicle, side2(Street)), Time)
        ;   not(happens(driveStreet21(Vehicle, Street), Time))
        ).
 */
 %  % =================================.


% [vehicle,street,location,time]
% Side2(street)=location ->
% Initiates(DriveStreet12(vehicle,street),At(vehicle,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',67).
% From E: 
% 
% '->'(
%    '='(
%       side2(Street), 
%       Location), 
%    initiates_at(
%       driveStreet12(Vehicle,Street), 
%       at_loc(Vehicle,Location), 
%       Time)).
(   initiates(driveStreet12(Vehicle, Street),
              at_loc(Vehicle, Location)at Time)
;   not equals(side2(Street), Location)
).

 /*   (   initiates(driveStreet12(Vehicle, Street),
                      at(at_loc(Vehicle, Location), Time))
        ;   not(equals(side2(Street), Location))
        ).
 */
 %  % =================================.


% [vehicle,street,location,time]
% Side1(street)=location ->
% Initiates(DriveStreet21(vehicle,street),At(vehicle,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',71).
% From E: 
% 
% '->'(
%    '='(
%       side1(Street), 
%       Location), 
%    initiates_at(
%       driveStreet21(Vehicle,Street), 
%       at_loc(Vehicle,Location), 
%       Time)).
(   initiates(driveStreet21(Vehicle, Street),
              at_loc(Vehicle, Location)at Time)
;   not equals(side1(Street), Location)
).

 /*   (   initiates(driveStreet21(Vehicle, Street),
                      at(at_loc(Vehicle, Location), Time))
        ;   not(equals(side1(Street), Location))
        ).
 */
 %  % =================================.


% [vehicle,street,location,time]
% Side1(street)=location ->
% Terminates(DriveStreet12(vehicle,street),At(vehicle,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',75).
% From E: 
% 
% '->'(
%    '='(
%       side1(Street), 
%       Location), 
%    terminates_at(
%       driveStreet12(Vehicle,Street), 
%       at_loc(Vehicle,Location), 
%       Time)).
(   terminates(driveStreet12(Vehicle, Street),
               at_loc(Vehicle, Location)at Time)
;   not equals(side1(Street), Location)
).

 /*   (   terminates(driveStreet12(Vehicle, Street),
                       at(at_loc(Vehicle, Location), Time))
        ;   not(equals(side1(Street), Location))
        ).
 */
 %  % =================================.


% [vehicle,street,location,time]
% Side2(street)=location ->
% Terminates(DriveStreet21(vehicle,street),At(vehicle,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',79).
% From E: 
% 
% '->'(
%    '='(
%       side2(Street), 
%       Location), 
%    terminates_at(
%       driveStreet21(Vehicle,Street), 
%       at_loc(Vehicle,Location), 
%       Time)).
(   terminates(driveStreet21(Vehicle, Street),
               at_loc(Vehicle, Location)at Time)
;   not equals(side2(Street), Location)
).

 /*   (   terminates(driveStreet21(Vehicle, Street),
                       at(at_loc(Vehicle, Location), Time))
        ;   not(equals(side2(Street), Location))
        ).
 */
 %  % =================================.


%; Pulling

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',84).
% event PointToward(agent,horse,street)
% From E: 
% 
% event(pointToward(agent,horse,street)).
events([pointToward/3]).
mpred_prop(pointToward(agent, horse, street), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',84).
actions([pointToward/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',86).
% fluent PointedToward(horse,street)
% From E: 
% 
% fluent(pointedToward(horse,street)).
mpred_prop(pointedToward(horse, street), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',86).
fluents([pointedToward/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',88).
% [horse,street1,street2,time]
% HoldsAt(PointedToward(horse,street1),time) &
% HoldsAt(PointedToward(horse,street2),time) ->
% street1=street2.
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          pointedToward(Horse,Street1), 
%          Time), 
%       holds(
%          pointedToward(Horse,Street2), 
%          Time)), 
%    Street1=Street2).
(   equals(Street1, Street2)
;   not pointedToward(Horse, Street1)at Time
;   not pointedToward(Horse, Street2)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',88).

 /*   (   equals(Street1, Street2)
        ;   at(not(pointedToward(Horse, Street1)), Time)
        ;   at(not(pointedToward(Horse, Street2)), Time)
        ).
 */
 %  % =================================.


% [agent,horse,street,time]
% Initiates(PointToward(agent,horse,street),
%           PointedToward(horse,street),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',94).
% From E: 
% 
% initiates_at(
%    pointToward(Agent,Horse,Street), 
%    pointedToward(Horse,Street), 
%    Time).
pointToward(Agent, Horse, Street)initiates pointedToward(Horse, Street).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',94).

 /*  initiated(happens(pointToward(Agent,Horse,Street),
     		  Time_from,
     		  Time_until),
     	  pointedToward(Horse,Street),
     	  []).
 */
 %  % =================================.


% [agent,horse,street1,street2,time]
% HoldsAt(PointedToward(horse,street1),time) ->
% Terminates(PointToward(agent,horse,street2),
%            PointedToward(horse,street1),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',99).
% From E: 
% 
% '->'(
%    holds(
%       pointedToward(Horse,Street1), 
%       Time), 
%    terminates_at(
%       pointToward(Agent,Horse,Street2), 
%       pointedToward(Horse,Street1), 
%       Time)).
(   terminates(pointToward(Agent, Horse, Street2),
               pointedToward(Horse, Street1)at Time)
;   not pointedToward(Horse, Street1)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',99).

 /*   (   terminates(pointToward(Agent, Horse, Street2),
                       at(pointedToward(Horse, Street1), Time))
        ;   at(not(pointedToward(Horse, Street1)), Time)
        ).
 */
 %  % =================================.


% [horse,vehicle,street,time]
% Terminates(PullStreet12(horse,vehicle,street),
%            PointedToward(horse,street),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',105).
% From E: 
% 
% terminates_at(
%    pullStreet12(Horse,Vehicle,Street), 
%    pointedToward(Horse,Street), 
%    Time).
pullStreet12(Horse, Vehicle, Street)terminates pointedToward(Horse, Street).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',105).

 /*  terminated(happens(pullStreet12(Horse,Vehicle,Street),
     		   Time_from,
     		   Time_until),
     	   pointedToward(Horse,Street),
     	   []).
 */
 %  % =================================.


% [horse,vehicle,street,time]
% Terminates(PullStreet21(horse,vehicle,street),
%            PointedToward(horse,street),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',110).
% From E: 
% 
% terminates_at(
%    pullStreet21(Horse,Vehicle,Street), 
%    pointedToward(Horse,Street), 
%    Time).
pullStreet21(Horse, Vehicle, Street)terminates pointedToward(Horse, Street).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',110).

 /*  terminated(happens(pullStreet21(Horse,Vehicle,Street),
     		   Time_from,
     		   Time_until),
     	   pointedToward(Horse,Street),
     	   []).
 */
 %  % =================================.


% [horse,street,time]
% HoldsAt(PointedToward(horse,street),time) ->
% HoldsAt(NearPortal(horse,street),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',115).
% From E: 
% 
% '->'(
%    holds(
%       pointedToward(Horse,Street), 
%       Time), 
%    holds(
%       nearPortal(Horse,Street), 
%       Time)).
(   nearPortal(Horse, Street)at Time
;   not pointedToward(Horse, Street)at Time
).

 /*   (   at(nearPortal(Horse, Street), Time)
        ;   at(not(pointedToward(Horse, Street)), Time)
        ).
 */
 %  % =================================.

% event Lash(agent,horse)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',117).
% From E: 
% 
% event(lash(agent,horse)).
events([lash/2]).
mpred_prop(lash(agent, horse), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',117).
actions([lash/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',120).
% fluent HitchedTo(horse,vehicle)
% From E: 
% 
% fluent(hitchedTo(horse,vehicle)).
mpred_prop(hitchedTo(horse, vehicle), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',120).
fluents([hitchedTo/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',122).
% [horse,vehicle,location,time]
% HoldsAt(HitchedTo(horse,vehicle),time) &
% HoldsAt(At(vehicle,location),time) ->
% HoldsAt(At(horse,location),time).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          hitchedTo(Horse,Vehicle), 
%          Time), 
%       holds(
%          at_loc(Vehicle,Location), 
%          Time)), 
%    holds(
%       at_loc(Horse,Location), 
%       Time)).
(   at_loc(Horse, Location)at Time
;   not hitchedTo(Horse, Vehicle)at Time
;   not at_loc(Vehicle, Location)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',122).

 /*   (   at(at_loc(Horse, Location), Time)
        ;   at(not(hitchedTo(Horse, Vehicle)), Time)
        ;   at(not(at_loc(Vehicle, Location)), Time)
        ).
 */
 %  % =================================.


% [agent,horse,vehicle,street,time]
% Happens(Lash(agent,horse),time) &
% HoldsAt(PointedToward(horse,street),time) &
% HoldsAt(HitchedTo(horse,vehicle),time) &
% HoldsAt(At(horse,Side1(street)),time) ->
% Happens(PullStreet12(horse,vehicle,street),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',128).
% From E: 
% 
% '->'(
%    ','(
%       happens(
%          lash(Agent,Horse), 
%          Time), 
%       ','(
%          holds(
%             pointedToward(Horse,Street), 
%             Time), 
%          ','(
%             holds(
%                hitchedTo(Horse,Vehicle), 
%                Time), 
%             holds(
%                at_loc(Horse, 
%                   side1(Street)), 
%                Time)))), 
%    happens(
%       pullStreet12(Horse,Vehicle,Street), 
%       Time)).
(   happens(pullStreet12(Horse, Vehicle, Street), Time)
;   not happens(lash(Agent, Horse), Time)
;   not pointedToward(Horse, Street)at Time
;   not hitchedTo(Horse, Vehicle)at Time
;   not at_loc(Horse, side1(Street))at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',128).

 /*   (   happens(pullStreet12(Horse, Vehicle, Street),
                    Time)
        ;   not(happens(lash(Agent, Horse), Time))
        ;   at(not(pointedToward(Horse, Street)), Time)
        ;   at(not(hitchedTo(Horse, Vehicle)), Time)
        ;   at(not(at_loc(Horse, side1(Street))), Time)
        ).
 */
 %  % =================================.


% [agent,horse,vehicle,street,time]
% Happens(Lash(agent,horse),time) &
% HoldsAt(PointedToward(horse,street),time) &
% HoldsAt(HitchedTo(horse,vehicle),time) &
% HoldsAt(At(horse,Side2(street)),time) ->
% Happens(PullStreet21(horse,vehicle,street),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',135).
% From E: 
% 
% '->'(
%    ','(
%       happens(
%          lash(Agent,Horse), 
%          Time), 
%       ','(
%          holds(
%             pointedToward(Horse,Street), 
%             Time), 
%          ','(
%             holds(
%                hitchedTo(Horse,Vehicle), 
%                Time), 
%             holds(
%                at_loc(Horse, 
%                   side2(Street)), 
%                Time)))), 
%    happens(
%       pullStreet21(Horse,Vehicle,Street), 
%       Time)).
(   happens(pullStreet21(Horse, Vehicle, Street), Time)
;   not happens(lash(Agent, Horse), Time)
;   not pointedToward(Horse, Street)at Time
;   not hitchedTo(Horse, Vehicle)at Time
;   not at_loc(Horse, side2(Street))at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',135).

 /*   (   happens(pullStreet21(Horse, Vehicle, Street),
                    Time)
        ;   not(happens(lash(Agent, Horse), Time))
        ;   at(not(pointedToward(Horse, Street)), Time)
        ;   at(not(hitchedTo(Horse, Vehicle)), Time)
        ;   at(not(at_loc(Horse, side2(Street))), Time)
        ).
 */
 %  % =================================.

% event PullStreet12(horse,vehicle,street)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',140).
% From E: 
% 
% event(pullStreet12(horse,vehicle,street)).
mpred_prop(pullStreet12(horse, vehicle, street), event).
events([pullStreet12/3]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',143).
% event PullStreet21(horse,vehicle,street)
% From E: 
% 
% event(pullStreet21(horse,vehicle,street)).
mpred_prop(pullStreet21(horse, vehicle, street), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',143).
events([pullStreet21/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',145).
% [horse,vehicle,street,time]
% Happens(PullStreet12(horse,vehicle,street),time) ->
% Happens(DriveStreet12(vehicle,street),time).
% From E: 
% 
% '->'(
%    happens(
%       pullStreet12(Horse,Vehicle,Street), 
%       Time), 
%    happens(
%       driveStreet12(Vehicle,Street), 
%       Time)).
(   happens(driveStreet12(Vehicle, Street), Time)
;   not(happens(pullStreet12(Horse, Vehicle, Street),
                Time))
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',145).

 /*   (   happens(driveStreet12(Vehicle, Street), Time)
        ;   not(happens(pullStreet12(Horse, Vehicle, Street),
                        Time))
        ).
 */
 %  % =================================.


% [horse,vehicle,street,time]
% Happens(PullStreet21(horse,vehicle,street),time) ->
% Happens(DriveStreet21(vehicle,street),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',150).
% From E: 
% 
% '->'(
%    happens(
%       pullStreet21(Horse,Vehicle,Street), 
%       Time), 
%    happens(
%       driveStreet21(Vehicle,Street), 
%       Time)).
(   happens(driveStreet21(Vehicle, Street), Time)
;   not(happens(pullStreet21(Horse, Vehicle, Street),
                Time))
).

 /*   (   happens(driveStreet21(Vehicle, Street), Time)
        ;   not(happens(pullStreet21(Horse, Vehicle, Street),
                        Time))
        ).
 */
 %  % =================================.


% [horse,vehicle,street,time]
% Happens(PullStreet12(horse,vehicle,street),time) ->
% HoldsAt(At(horse,Side1(street)),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',154).
% From E: 
% 
% '->'(
%    happens(
%       pullStreet12(Horse,Vehicle,Street), 
%       Time), 
%    holds(
%       at_loc(Horse, 
%          side1(Street)), 
%       Time)).
(   at_loc(Horse, side1(Street))at Time
;   not(happens(pullStreet12(Horse, Vehicle, Street),
                Time))
).

 /*   (   at(at_loc(Horse, side1(Street)), Time)
        ;   not(happens(pullStreet12(Horse, Vehicle, Street),
                        Time))
        ).
 */
 %  % =================================.


% [horse,vehicle,street,time]
% Happens(PullStreet21(horse,vehicle,street),time) ->
% HoldsAt(At(horse,Side2(street)),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',158).
% From E: 
% 
% '->'(
%    happens(
%       pullStreet21(Horse,Vehicle,Street), 
%       Time), 
%    holds(
%       at_loc(Horse, 
%          side2(Street)), 
%       Time)).
(   at_loc(Horse, side2(Street))at Time
;   not(happens(pullStreet21(Horse, Vehicle, Street),
                Time))
).

 /*   (   at(at_loc(Horse, side2(Street)), Time)
        ;   not(happens(pullStreet21(Horse, Vehicle, Street),
                        Time))
        ).
 */
 %  % =================================.


% [horse,vehicle,street,location,time]
% Side2(street)=location ->
% Initiates(PullStreet12(horse,vehicle,street),At(horse,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',162).
% From E: 
% 
% '->'(
%    '='(
%       side2(Street), 
%       Location), 
%    initiates_at(
%       pullStreet12(Horse,Vehicle,Street), 
%       at_loc(Horse,Location), 
%       Time)).
(   initiates(pullStreet12(Horse, Vehicle, Street),
              at_loc(Horse, Location)at Time)
;   not equals(side2(Street), Location)
).

 /*   (   initiates(pullStreet12(Horse, Vehicle, Street),
                      at(at_loc(Horse, Location), Time))
        ;   not(equals(side2(Street), Location))
        ).
 */
 %  % =================================.


% [horse,vehicle,street,location,time]
% Side1(street)=location ->
% Initiates(PullStreet21(horse,vehicle,street),At(horse,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',166).
% From E: 
% 
% '->'(
%    '='(
%       side1(Street), 
%       Location), 
%    initiates_at(
%       pullStreet21(Horse,Vehicle,Street), 
%       at_loc(Horse,Location), 
%       Time)).
(   initiates(pullStreet21(Horse, Vehicle, Street),
              at_loc(Horse, Location)at Time)
;   not equals(side1(Street), Location)
).

 /*   (   initiates(pullStreet21(Horse, Vehicle, Street),
                      at(at_loc(Horse, Location), Time))
        ;   not(equals(side1(Street), Location))
        ).
 */
 %  % =================================.


% [horse,vehicle,street,location,time]
% Side1(street)=location ->
% Terminates(PullStreet12(horse,vehicle,street),At(horse,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',170).
% From E: 
% 
% '->'(
%    '='(
%       side1(Street), 
%       Location), 
%    terminates_at(
%       pullStreet12(Horse,Vehicle,Street), 
%       at_loc(Horse,Location), 
%       Time)).
(   terminates(pullStreet12(Horse, Vehicle, Street),
               at_loc(Horse, Location)at Time)
;   not equals(side1(Street), Location)
).

 /*   (   terminates(pullStreet12(Horse, Vehicle, Street),
                       at(at_loc(Horse, Location), Time))
        ;   not(equals(side1(Street), Location))
        ).
 */
 %  % =================================.


% [horse,vehicle,street,location,time]
% Side2(street)=location ->
% Terminates(PullStreet21(horse,vehicle,street),At(horse,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',174).
% From E: 
% 
% '->'(
%    '='(
%       side2(Street), 
%       Location), 
%    terminates_at(
%       pullStreet21(Horse,Vehicle,Street), 
%       at_loc(Horse,Location), 
%       Time)).
(   terminates(pullStreet21(Horse, Vehicle, Street),
               at_loc(Horse, Location)at Time)
;   not equals(side2(Street), Location)
).

 /*   (   terminates(pullStreet21(Horse, Vehicle, Street),
                       at(at_loc(Horse, Location), Time))
        ;   not(equals(side2(Street), Location))
        ).
 */
 %  % =================================.


%; OnVehicle

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',179).
% fluent OnVehicle(object,vehicleon)
% From E: 
% 
% fluent(onVehicle(object,vehicleon)).
mpred_prop(onVehicle(object, vehicleon), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',179).
fluents([onVehicle/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',181).
% event GetOnVehicle(agent,vehicleon)
% From E: 
% 
% event(getOnVehicle(agent,vehicleon)).
events([getOnVehicle/2]).
mpred_prop(getOnVehicle(agent, vehicleon), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',181).
actions([getOnVehicle/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',183).
% event GetOffVehicle(agent,vehicleon)
% From E: 
% 
% event(getOffVehicle(agent,vehicleon)).
events([getOffVehicle/2]).
mpred_prop(getOffVehicle(agent, vehicleon), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',183).
actions([getOffVehicle/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',185).
% [vehicleon1,vehicleon2,time]
% HoldsAt(OnVehicle(vehicleon1,vehicleon2),time) ->
% vehicleon1!=vehicleon2.
% From E: 
% 
% '->'(
%    holds(
%       onVehicle(Vehicleon1,Vehicleon2), 
%       Time), 
%    Vehicleon1\=Vehicleon2).
(   { dif(Vehicleon1, Vehicleon2)
    }
;   not onVehicle(Vehicleon1, Vehicleon2)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',185).

 /*   (   { dif(Vehicleon1, Vehicleon2)
            }
        ;   at(not(onVehicle(Vehicleon1, Vehicleon2)), Time)
        ).
 */
 %  % =================================.


% [vehicleon1,vehicleon2,time]
% HoldsAt(OnVehicle(vehicleon1,vehicleon2),time) ->
% !HoldsAt(OnVehicle(vehicleon2,vehicleon1),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',190).
% From E: 
% 
% '->'(
%    holds(
%       onVehicle(Vehicleon1,Vehicleon2), 
%       Time), 
%    holds(
%       not(onVehicle(Vehicleon2,Vehicleon1)), 
%       Time)).
onVehicle(Vehicleon2, Vehicleon1)at Time if not onVehicle(Vehicleon1, Vehicleon2)at Time.

 /*  l_int(holds(onVehicle(Vehicleon2,Vehicleon1),Time),
           [ holds(not(onVehicle(Vehicleon1,Vehicleon2)),
     	      Time)
           ]).
 */
 %  % =================================.


% [agent,vehicleon,time]
% Initiates(GetOnVehicle(agent,vehicleon),OnVehicle(agent,vehicleon),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',194).
% From E: 
% 
% initiates_at(
%    getOnVehicle(Agent,Vehicleon), 
%    onVehicle(Agent,Vehicleon), 
%    Time).
getOnVehicle(Agent, Vehicleon)initiates onVehicle(Agent, Vehicleon).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',194).

 /*  initiated(happens(getOnVehicle(Agent,Vehicleon),
     		  Time_from,
     		  Time_until),
     	  onVehicle(Agent,Vehicleon),
     	  []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',196).
% [agent,vehicleon,time]
% Happens(GetOnVehicle(agent,vehicleon),time) ->
% {location}% 
%  HoldsAt(At(agent,location),time) &
%  HoldsAt(At(vehicleon,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',198).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          getOnVehicle(Agent,Vehicleon), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent,Location), 
%             Time), 
%          holds(
%             at_loc(Vehicleon,Location), 
%             Time)))).
exists(Location,  (at_loc(Agent, Location)at Time, at_loc(Vehicleon, Location)at Time;not happens(getOnVehicle(Agent, Vehicleon), Time))).
 %  exists(Location,  (at(at_loc(Agent, Location), Time), at(at_loc(Vehicleon, Location), Time);not(happens(getOnVehicle(Agent, Vehicleon), Time)))).
 %  % =================================.


% [agent,vehicleon,time]
% Terminates(GetOffVehicle(agent,vehicleon),OnVehicle(agent,vehicleon),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',203).
% From E: 
% 
% terminates_at(
%    getOffVehicle(Agent,Vehicleon), 
%    onVehicle(Agent,Vehicleon), 
%    Time).
getOffVehicle(Agent, Vehicleon)terminates onVehicle(Agent, Vehicleon).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',203).

 /*  terminated(happens(getOffVehicle(Agent,Vehicleon),
     		   Time_from,
     		   Time_until),
     	   onVehicle(Agent,Vehicleon),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',205).
% [agent,vehicleon,time]
% Happens(GetOffVehicle(agent,vehicleon),time) ->
% HoldsAt(OnVehicle(agent,vehicleon),time).
% From E: 
% 
% '->'(
%    happens(
%       getOffVehicle(Agent,Vehicleon), 
%       Time), 
%    holds(
%       onVehicle(Agent,Vehicleon), 
%       Time)).
(   onVehicle(Agent, Vehicleon)at Time
;   not happens(getOffVehicle(Agent, Vehicleon), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',205).

 /*   (   at(onVehicle(Agent, Vehicleon), Time)
        ;   not(happens(getOffVehicle(Agent, Vehicleon), Time))
        ).
 */
 %  % =================================.


% [agent,vehicleon,location,time]
% Releases(GetOnVehicle(agent,vehicleon),
%          At(agent,location),
%          time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',210).
% From E: 
% 
% releases_at(
%    getOnVehicle(Agent,Vehicleon), 
%    at_loc(Agent,Location), 
%    Time).
releases(getOnVehicle(Agent, Vehicleon), at_loc(Agent, Location)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',210).

 /*  releases(getOnVehicle(Agent,Vehicleon),
     	 at_loc(Agent,Location)).
 */
 %  % =================================.


%;[agent,vehicleon,location1,location2,time]
%;HoldsAt(At(vehicleon,location1),time) &
%;location1 != location2 ->
%;Terminates(GetOffVehicle(agent,vehicleon),
%;           At(agent,location2),
%;           time).
% [agent,vehicleon,location,time]
% HoldsAt(At(vehicleon,location),time) ->
% Initiates(GetOffVehicle(agent,vehicleon),
%           At(agent,location),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',221).
% From E: 
% 
% '->'(
%    holds(
%       at_loc(Vehicleon,Location), 
%       Time), 
%    initiates_at(
%       getOffVehicle(Agent,Vehicleon), 
%       at_loc(Agent,Location), 
%       Time)).
(   initiates(getOffVehicle(Agent, Vehicleon),
              at_loc(Agent, Location)at Time)
;   not at_loc(Vehicleon, Location)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',221).

 /*   (   initiates(getOffVehicle(Agent, Vehicleon),
                      at(at_loc(Agent, Location), Time))
        ;   at(not(at_loc(Vehicleon, Location)), Time)
        ).
 */
 %  % =================================.


% [object,vehicleon,location,time]
% HoldsAt(OnVehicle(object,vehicleon),time) &
% HoldsAt(At(vehicleon,location),time) ->
% HoldsAt(At(object,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',228).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          onVehicle(Object,Vehicleon), 
%          Time), 
%       holds(
%          at_loc(Vehicleon,Location), 
%          Time)), 
%    holds(
%       at_loc(Object,Location), 
%       Time)).
(   at_loc(Object, Location)at Time
;   not onVehicle(Object, Vehicleon)at Time
;   not at_loc(Vehicleon, Location)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',228).

 /*   (   at(at_loc(Object, Location), Time)
        ;   at(not(onVehicle(Object, Vehicleon)), Time)
        ;   at(not(at_loc(Vehicleon, Location)), Time)
        ).
 */
 %  % =================================.


%; InVehicle

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',234).
% fluent InVehicle(object,vehiclein)
% From E: 
% 
% fluent(inVehicle(object,vehiclein)).
mpred_prop(inVehicle(object, vehiclein), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',234).
fluents([inVehicle/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',236).
% event GetInVehicle(agent,vehiclein)
% From E: 
% 
% event(getInVehicle(agent,vehiclein)).
events([getInVehicle/2]).
mpred_prop(getInVehicle(agent, vehiclein), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',236).
actions([getInVehicle/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',238).
% event GetOutOfVehicle(agent,vehiclein)
% From E: 
% 
% event(getOutOfVehicle(agent,vehiclein)).
events([getOutOfVehicle/2]).
mpred_prop(getOutOfVehicle(agent, vehiclein), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',238).
actions([getOutOfVehicle/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',240).
% [vehiclein1,vehiclein2,time]
% HoldsAt(InVehicle(vehiclein1,vehiclein2),time) ->
% vehiclein1!=vehiclein2.
% From E: 
% 
% '->'(
%    holds(
%       inVehicle(Vehiclein1,Vehiclein2), 
%       Time), 
%    Vehiclein1\=Vehiclein2).
(   { dif(Vehiclein1, Vehiclein2)
    }
;   not inVehicle(Vehiclein1, Vehiclein2)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',240).

 /*   (   { dif(Vehiclein1, Vehiclein2)
            }
        ;   at(not(inVehicle(Vehiclein1, Vehiclein2)), Time)
        ).
 */
 %  % =================================.


% [vehiclein1,vehiclein2,time]
% HoldsAt(InVehicle(vehiclein1,vehiclein2),time) ->
% !HoldsAt(InVehicle(vehiclein2,vehiclein1),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',245).
% From E: 
% 
% '->'(
%    holds(
%       inVehicle(Vehiclein1,Vehiclein2), 
%       Time), 
%    holds(
%       not(inVehicle(Vehiclein2,Vehiclein1)), 
%       Time)).
inVehicle(Vehiclein2, Vehiclein1)at Time if not inVehicle(Vehiclein1, Vehiclein2)at Time.

 /*  l_int(holds(inVehicle(Vehiclein2,Vehiclein1),Time),
           [ holds(not(inVehicle(Vehiclein1,Vehiclein2)),
     	      Time)
           ]).
 */
 %  % =================================.


% [agent,vehiclein,time]
% Initiates(GetInVehicle(agent,vehiclein),InVehicle(agent,vehiclein),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',249).
% From E: 
% 
% initiates_at(
%    getInVehicle(Agent,Vehiclein), 
%    inVehicle(Agent,Vehiclein), 
%    Time).
getInVehicle(Agent, Vehiclein)initiates inVehicle(Agent, Vehiclein).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',249).

 /*  initiated(happens(getInVehicle(Agent,Vehiclein),
     		  Time_from,
     		  Time_until),
     	  inVehicle(Agent,Vehiclein),
     	  []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',251).
% [agent,vehiclein,time]
% Happens(GetInVehicle(agent,vehiclein),time) ->
% {location}% 
%  HoldsAt(At(agent,location),time) &
%  HoldsAt(At(vehiclein,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',253).
% From E: 
% 
% exists(Location, 
%    '->'(
%       happens(
%          getInVehicle(Agent,Vehiclein), 
%          Time), 
%       ','(
%          holds(
%             at_loc(Agent,Location), 
%             Time), 
%          holds(
%             at_loc(Vehiclein,Location), 
%             Time)))).
exists(Location,  (at_loc(Agent, Location)at Time, at_loc(Vehiclein, Location)at Time;not happens(getInVehicle(Agent, Vehiclein), Time))).
 %  exists(Location,  (at(at_loc(Agent, Location), Time), at(at_loc(Vehiclein, Location), Time);not(happens(getInVehicle(Agent, Vehiclein), Time)))).
 %  % =================================.


% [agent,vehiclein,time]
% Terminates(GetOutOfVehicle(agent,vehiclein),InVehicle(agent,vehiclein),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',258).
% From E: 
% 
% terminates_at(
%    getOutOfVehicle(Agent,Vehiclein), 
%    inVehicle(Agent,Vehiclein), 
%    Time).
getOutOfVehicle(Agent, Vehiclein)terminates inVehicle(Agent, Vehiclein).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',258).

 /*  terminated(happens(getOutOfVehicle(Agent,Vehiclein),
     		   Time_from,
     		   Time_until),
     	   inVehicle(Agent,Vehiclein),
     	   []).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',260).
% [agent,vehiclein,time]
% Happens(GetOutOfVehicle(agent,vehiclein),time) ->
% HoldsAt(InVehicle(agent,vehiclein),time).
% From E: 
% 
% '->'(
%    happens(
%       getOutOfVehicle(Agent,Vehiclein), 
%       Time), 
%    holds(
%       inVehicle(Agent,Vehiclein), 
%       Time)).
(   inVehicle(Agent, Vehiclein)at Time
;   not happens(getOutOfVehicle(Agent, Vehiclein), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',260).

 /*   (   at(inVehicle(Agent, Vehiclein), Time)
        ;   not(happens(getOutOfVehicle(Agent, Vehiclein), Time))
        ).
 */
 %  % =================================.


% [agent,vehiclein,location,time]
% Releases(GetInVehicle(agent,vehiclein),
%          At(agent,location),
%          time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',265).
% From E: 
% 
% releases_at(
%    getInVehicle(Agent,Vehiclein), 
%    at_loc(Agent,Location), 
%    Time).
releases(getInVehicle(Agent, Vehiclein), at_loc(Agent, Location)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',265).

 /*  releases(getInVehicle(Agent,Vehiclein),
     	 at_loc(Agent,Location)).
 */
 %  % =================================.


%;[agent,vehiclein,location1,location2,time]
%;HoldsAt(At(vehiclein,location1),time) &
%;location1 != location2 ->
%;Terminates(GetOutOfVehicle(agent,vehiclein),
%;           At(agent,location2),
%;           time).
% [agent,vehiclein,location,time]
% HoldsAt(At(vehiclein,location),time) ->
% Initiates(GetOutOfVehicle(agent,vehiclein),
%           At(agent,location),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',276).
% From E: 
% 
% '->'(
%    holds(
%       at_loc(Vehiclein,Location), 
%       Time), 
%    initiates_at(
%       getOutOfVehicle(Agent,Vehiclein), 
%       at_loc(Agent,Location), 
%       Time)).
(   initiates(getOutOfVehicle(Agent, Vehiclein),
              at_loc(Agent, Location)at Time)
;   not at_loc(Vehiclein, Location)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',276).

 /*   (   initiates(getOutOfVehicle(Agent, Vehiclein),
                      at(at_loc(Agent, Location), Time))
        ;   at(not(at_loc(Vehiclein, Location)), Time)
        ).
 */
 %  % =================================.


% [object,vehiclein,location,time]
% HoldsAt(InVehicle(object,vehiclein),time) &
% HoldsAt(At(vehiclein,location),time) ->
% HoldsAt(At(object,location),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',283).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          inVehicle(Object,Vehiclein), 
%          Time), 
%       holds(
%          at_loc(Vehiclein,Location), 
%          Time)), 
%    holds(
%       at_loc(Object,Location), 
%       Time)).
(   at_loc(Object, Location)at Time
;   not inVehicle(Object, Vehiclein)at Time
;   not at_loc(Vehiclein, Location)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',283).

 /*   (   at(at_loc(Object, Location), Time)
        ;   at(not(inVehicle(Object, Vehiclein)), Time)
        ;   at(not(at_loc(Vehiclein, Location)), Time)
        ).
 */
 %  % =================================.


%; vehicle door
%; door does not have to be open for entry; passenger can jump in

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',290).
% event VehicleDoorOpen(agent,vehicledoor)
% From E: 
% 
% event(vehicleDoorOpen(agent,vehicledoor)).
events([vehicleDoorOpen/2]).
mpred_prop(vehicleDoorOpen(agent, vehicledoor), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',290).
actions([vehicleDoorOpen/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',292).
% event VehicleDoorClose(agent,vehicledoor)
% From E: 
% 
% event(vehicleDoorClose(agent,vehicledoor)).
events([vehicleDoorClose/2]).
mpred_prop(vehicleDoorClose(agent, vehicledoor), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',292).
actions([vehicleDoorClose/2]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',294).
% fluent VehicleDoorIsOpen(vehicledoor)
% From E: 
% 
% fluent(vehicleDoorIsOpen(vehicledoor)).
mpred_prop(vehicleDoorIsOpen(vehicledoor), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',294).
fluents([vehicleDoorIsOpen/1]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',296).
% [agent,vehicledoor,time]
% Happens(VehicleDoorOpen(agent,vehicledoor),time) ->
% HoldsAt(Awake(agent),time) &
% !HoldsAt(VehicleDoorIsOpen(vehicledoor),time).
% From E: 
% 
% '->'(
%    happens(
%       vehicleDoorOpen(Agent,Vehicledoor), 
%       Time), 
%    ','(
%       holds(
%          awake(Agent), 
%          Time), 
%       holds(
%          not(vehicleDoorIsOpen(Vehicledoor)), 
%          Time))).
(   awake(Agent)at Time,
    not vehicleDoorIsOpen(Vehicledoor)at Time
;   not happens(vehicleDoorOpen(Agent, Vehicledoor), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',296).

 /*  (   at(awake(Agent), Time),
         at(not(vehicleDoorIsOpen(Vehicledoor)), Time)
     ;   not(happens(vehicleDoorOpen(Agent, Vehicledoor), Time))
     ).
 */
 %  % =================================.


% [agent,vehicledoor,time]
% Initiates(VehicleDoorOpen(agent,vehicledoor),
%           VehicleDoorIsOpen(vehicledoor),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',302).
% From E: 
% 
% initiates_at(
%    vehicleDoorOpen(Agent,Vehicledoor), 
%    vehicleDoorIsOpen(Vehicledoor), 
%    Time).
vehicleDoorOpen(Agent, Vehicledoor)initiates vehicleDoorIsOpen(Vehicledoor).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',302).

 /*  initiated(happens(vehicleDoorOpen(Agent,Vehicledoor),
     		  Time_from,
     		  Time_until),
     	  vehicleDoorIsOpen(Vehicledoor),
     	  []).
 */
 %  % =================================.


% [agent,vehicledoor,time]
% Happens(VehicleDoorClose(agent,vehicledoor),time) ->
% HoldsAt(Awake(agent),time) &
% HoldsAt(VehicleDoorIsOpen(vehicledoor),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',307).
% From E: 
% 
% '->'(
%    happens(
%       vehicleDoorClose(Agent,Vehicledoor), 
%       Time), 
%    ','(
%       holds(
%          awake(Agent), 
%          Time), 
%       holds(
%          vehicleDoorIsOpen(Vehicledoor), 
%          Time))).
(   awake(Agent)at Time,
    vehicleDoorIsOpen(Vehicledoor)at Time
;   not happens(vehicleDoorClose(Agent, Vehicledoor), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',307).

 /*  (   at(awake(Agent), Time),
         at(vehicleDoorIsOpen(Vehicledoor), Time)
     ;   not(happens(vehicleDoorClose(Agent, Vehicledoor), Time))
     ).
 */
 %  % =================================.


% [agent,vehicledoor,time]
% Terminates(VehicleDoorClose(agent,vehicledoor),
%            VehicleDoorIsOpen(vehicledoor),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',312).
% From E: 
% 
% terminates_at(
%    vehicleDoorClose(Agent,Vehicledoor), 
%    vehicleDoorIsOpen(Vehicledoor), 
%    Time).
vehicleDoorClose(Agent, Vehicledoor)terminates vehicleDoorIsOpen(Vehicledoor).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',312).

 /*  terminated(happens(vehicleDoorClose(Agent,Vehicledoor),
     		   Time_from,
     		   Time_until),
     	   vehicleDoorIsOpen(Vehicledoor),
     	   []).
 */
 %  % =================================.


%; ticketagent

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',318).
% sort ticketagent: agent
% From E: 
% 
% subsort(ticketagent,agent).
subsort(ticketagent, agent).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',320).
% fluent BeTicketAgent0(ticketagent)
% From E: 
% 
% fluent(beTicketAgent0(ticketagent)).
mpred_prop(beTicketAgent0(ticketagent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',320).
fluents([beTicketAgent0/1]).

% fluent BeTicketAgent1(ticketagent)
% From E: 
% 
% fluent(beTicketAgent1(ticketagent)).
mpred_prop(beTicketAgent1(ticketagent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',320).
fluents([beTicketAgent1/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',322).
% fluent BeTicketAgent2(ticketagent)
% From E: 
% 
% fluent(beTicketAgent2(ticketagent)).
mpred_prop(beTicketAgent2(ticketagent), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',322).
fluents([beTicketAgent2/1]).

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',324).
% xor BeTicketAgent0, BeTicketAgent1, BeTicketAgent2
% From E: 
% 
% xor([beTicketAgent0,beTicketAgent1,beTicketAgent2]).
xor([beTicketAgent0,beTicketAgent1,beTicketAgent2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',326).
% [ticketagent,agent,ticket,time]
% HoldsAt(BeTicketAgent0(ticketagent),time) ->
% Terminates(Request(agent,ticketagent,ticket),
%            BeTicketAgent0(ticketagent),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',326).
% From E: 
% 
% '->'(
%    holds(
%       beTicketAgent0(Ticketagent), 
%       Time), 
%    terminates_at(
%       request(Agent,Ticketagent,Ticket), 
%       beTicketAgent0(Ticketagent), 
%       Time)).
(   terminates(request(Agent, Ticketagent, Ticket),
               beTicketAgent0(Ticketagent)at Time)
;   not beTicketAgent0(Ticketagent)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',326).

 /*   (   terminates(request(Agent, Ticketagent, Ticket),
                       at(beTicketAgent0(Ticketagent), Time))
        ;   at(not(beTicketAgent0(Ticketagent)), Time)
        ).
 */
 %  % =================================.


% [ticketagent,agent,ticket,time]
% HoldsAt(BeTicketAgent0(ticketagent),time) ->
% Initiates(Request(agent,ticketagent,ticket),
%           BeTicketAgent1(ticketagent),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',333).
% From E: 
% 
% '->'(
%    holds(
%       beTicketAgent0(Ticketagent), 
%       Time), 
%    initiates_at(
%       request(Agent,Ticketagent,Ticket), 
%       beTicketAgent1(Ticketagent), 
%       Time)).
(   initiates(request(Agent, Ticketagent, Ticket),
              beTicketAgent1(Ticketagent)at Time)
;   not beTicketAgent0(Ticketagent)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',333).

 /*   (   initiates(request(Agent, Ticketagent, Ticket),
                      at(beTicketAgent1(Ticketagent), Time))
        ;   at(not(beTicketAgent0(Ticketagent)), Time)
        ).
 */
 %  % =================================.


% [ticketagent,agent,ticket,time]
% HoldsAt(BeTicketAgent1(ticketagent),time) &
% HoldsAt(KnowRequest(ticketagent,agent,ticket),time) ->
% Happens(PickUp(ticketagent,ticket),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',339).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          beTicketAgent1(Ticketagent), 
%          Time), 
%       holds(
%          knowRequest(Ticketagent,Agent,Ticket), 
%          Time)), 
%    happens(
%       pickUp(Ticketagent,Ticket), 
%       Time)).
(   happens(pickUp(Ticketagent, Ticket), Time)
;   not beTicketAgent1(Ticketagent)at Time
;   not knowRequest(Ticketagent, Agent, Ticket)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',339).

 /*   (   happens(pickUp(Ticketagent, Ticket), Time)
        ;   at(not(beTicketAgent1(Ticketagent)), Time)
        ;   at(not(knowRequest(Ticketagent, Agent, Ticket)),
               Time)
        ).
 */
 %  % =================================.


% [ticketagent,ticket,time]
% HoldsAt(BeTicketAgent1(ticketagent),time) ->
% Terminates(PickUp(ticketagent,ticket),
%            BeTicketAgent1(ticketagent),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',344).
% From E: 
% 
% '->'(
%    holds(
%       beTicketAgent1(Ticketagent), 
%       Time), 
%    terminates_at(
%       pickUp(Ticketagent,Ticket), 
%       beTicketAgent1(Ticketagent), 
%       Time)).
(   terminates(pickUp(Ticketagent, Ticket),
               beTicketAgent1(Ticketagent)at Time)
;   not beTicketAgent1(Ticketagent)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',344).

 /*   (   terminates(pickUp(Ticketagent, Ticket),
                       at(beTicketAgent1(Ticketagent), Time))
        ;   at(not(beTicketAgent1(Ticketagent)), Time)
        ).
 */
 %  % =================================.


% [ticketagent,ticket,time]
% HoldsAt(BeTicketAgent1(ticketagent),time) ->
% Initiates(PickUp(ticketagent,ticket),
%           BeTicketAgent2(ticketagent),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',350).
% From E: 
% 
% '->'(
%    holds(
%       beTicketAgent1(Ticketagent), 
%       Time), 
%    initiates_at(
%       pickUp(Ticketagent,Ticket), 
%       beTicketAgent2(Ticketagent), 
%       Time)).
(   initiates(pickUp(Ticketagent, Ticket),
              beTicketAgent2(Ticketagent)at Time)
;   not beTicketAgent1(Ticketagent)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',350).

 /*   (   initiates(pickUp(Ticketagent, Ticket),
                      at(beTicketAgent2(Ticketagent), Time))
        ;   at(not(beTicketAgent1(Ticketagent)), Time)
        ).
 */
 %  % =================================.


% [ticketagent,agent,ticket,time]
% HoldsAt(BeTicketAgent2(ticketagent),time) &
% HoldsAt(KnowRequest(ticketagent,agent,ticket),time) ->
% Happens(HandTo(ticketagent,agent,ticket),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',356).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          beTicketAgent2(Ticketagent), 
%          Time), 
%       holds(
%          knowRequest(Ticketagent,Agent,Ticket), 
%          Time)), 
%    happens(
%       handTo(Ticketagent,Agent,Ticket), 
%       Time)).
(   happens(handTo(Ticketagent, Agent, Ticket), Time)
;   not beTicketAgent2(Ticketagent)at Time
;   not knowRequest(Ticketagent, Agent, Ticket)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',356).

 /*   (   happens(handTo(Ticketagent, Agent, Ticket), Time)
        ;   at(not(beTicketAgent2(Ticketagent)), Time)
        ;   at(not(knowRequest(Ticketagent, Agent, Ticket)),
               Time)
        ).
 */
 %  % =================================.


% [ticketagent,ticket,agent,time]
% HoldsAt(BeTicketAgent2(ticketagent),time) ->
% Terminates(HandTo(ticketagent,agent,ticket),
%            BeTicketAgent2(ticketagent),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',361).
% From E: 
% 
% '->'(
%    holds(
%       beTicketAgent2(Ticketagent), 
%       Time), 
%    terminates_at(
%       handTo(Ticketagent,Agent,Ticket), 
%       beTicketAgent2(Ticketagent), 
%       Time)).
(   terminates(handTo(Ticketagent, Agent, Ticket),
               beTicketAgent2(Ticketagent)at Time)
;   not beTicketAgent2(Ticketagent)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',361).

 /*   (   terminates(handTo(Ticketagent, Agent, Ticket),
                       at(beTicketAgent2(Ticketagent), Time))
        ;   at(not(beTicketAgent2(Ticketagent)), Time)
        ).
 */
 %  % =================================.


% [ticketagent,ticket,agent,time]
% HoldsAt(BeTicketAgent2(ticketagent),time) ->
% Initiates(HandTo(ticketagent,agent,ticket),
%           BeTicketAgent0(ticketagent),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',367).
% From E: 
% 
% '->'(
%    holds(
%       beTicketAgent2(Ticketagent), 
%       Time), 
%    initiates_at(
%       handTo(Ticketagent,Agent,Ticket), 
%       beTicketAgent0(Ticketagent), 
%       Time)).
(   initiates(handTo(Ticketagent, Agent, Ticket),
              beTicketAgent0(Ticketagent)at Time)
;   not beTicketAgent2(Ticketagent)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',367).

 /*   (   initiates(handTo(Ticketagent, Agent, Ticket),
                      at(beTicketAgent0(Ticketagent), Time))
        ;   at(not(beTicketAgent2(Ticketagent)), Time)
        ).
 */
 %  % =================================.


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.e',371).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Vehicle.lps.pl')).
