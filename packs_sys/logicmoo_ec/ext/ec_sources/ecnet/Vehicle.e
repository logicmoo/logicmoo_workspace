;
; Copyright (c) 2005 IBM Corporation and others.
; All rights reserved. This program and the accompanying materials
; are made available under the terms of the Common Public License v1.0
; which accompanies this distribution, and is available at
; http://www.eclipse.org/legal/cpl-v10.html
;
; Contributors:
; IBM - Initial implementation
;
; Vehicle: transportation vehicles
;

sort vehicle: physobj
sort vehiclein: vehicle
sort vehicleon: vehicle
sort train: vehicleon
sort carriage: vehiclein

sort vehicledoor

; RideTrack

event RideTrack12(train,track)

event RideTrack21(train,track)

[train,track,time]
Happens(RideTrack12(train,track),time) ->
HoldsAt(At(train,Side1(track)),time).

[train,track,time]
Happens(RideTrack21(train,track),time) ->
HoldsAt(At(train,Side2(track)),time).

[train,track,location,time]
Side2(track)=location ->
Initiates(RideTrack12(train,track),At(train,location),time).

[train,track,location,time]
Side1(track)=location ->
Initiates(RideTrack21(train,track),At(train,location),time).

[train,track,location,time]
Side1(track)=location ->
Terminates(RideTrack12(train,track),At(train,location),time).

[train,track,location,time]
Side2(track)=location ->
Terminates(RideTrack21(train,track),At(train,location),time).

; DriveStreet

event DriveStreet12(vehicle,street)

event DriveStreet21(vehicle,street)

[vehicle,street,time]
Happens(DriveStreet12(vehicle,street),time) ->
HoldsAt(At(vehicle,Side1(street)),time).

[vehicle,street,time]
Happens(DriveStreet21(vehicle,street),time) ->
HoldsAt(At(vehicle,Side2(street)),time).

[vehicle,street,location,time]
Side2(street)=location ->
Initiates(DriveStreet12(vehicle,street),At(vehicle,location),time).

[vehicle,street,location,time]
Side1(street)=location ->
Initiates(DriveStreet21(vehicle,street),At(vehicle,location),time).

[vehicle,street,location,time]
Side1(street)=location ->
Terminates(DriveStreet12(vehicle,street),At(vehicle,location),time).

[vehicle,street,location,time]
Side2(street)=location ->
Terminates(DriveStreet21(vehicle,street),At(vehicle,location),time).

; Pulling

event PointToward(agent,horse,street)

fluent PointedToward(horse,street)

[horse,street1,street2,time]
HoldsAt(PointedToward(horse,street1),time) &
HoldsAt(PointedToward(horse,street2),time) ->
street1=street2.

[agent,horse,street,time]
Initiates(PointToward(agent,horse,street),
          PointedToward(horse,street),
          time).

[agent,horse,street1,street2,time]
HoldsAt(PointedToward(horse,street1),time) ->
Terminates(PointToward(agent,horse,street2),
           PointedToward(horse,street1),
           time).

[horse,vehicle,street,time]
Terminates(PullStreet12(horse,vehicle,street),
           PointedToward(horse,street),
           time).

[horse,vehicle,street,time]
Terminates(PullStreet21(horse,vehicle,street),
           PointedToward(horse,street),
           time).

[horse,street,time]
HoldsAt(PointedToward(horse,street),time) ->
HoldsAt(NearPortal(horse,street),time).

event Lash(agent,horse)

fluent HitchedTo(horse,vehicle)

[horse,vehicle,location,time]
HoldsAt(HitchedTo(horse,vehicle),time) &
HoldsAt(At(vehicle,location),time) ->
HoldsAt(At(horse,location),time).

[agent,horse,vehicle,street,time]
Happens(Lash(agent,horse),time) &
HoldsAt(PointedToward(horse,street),time) &
HoldsAt(HitchedTo(horse,vehicle),time) &
HoldsAt(At(horse,Side1(street)),time) ->
Happens(PullStreet12(horse,vehicle,street),time).

[agent,horse,vehicle,street,time]
Happens(Lash(agent,horse),time) &
HoldsAt(PointedToward(horse,street),time) &
HoldsAt(HitchedTo(horse,vehicle),time) &
HoldsAt(At(horse,Side2(street)),time) ->
Happens(PullStreet21(horse,vehicle,street),time).

event PullStreet12(horse,vehicle,street)

event PullStreet21(horse,vehicle,street)

[horse,vehicle,street,time]
Happens(PullStreet12(horse,vehicle,street),time) ->
Happens(DriveStreet12(vehicle,street),time).

[horse,vehicle,street,time]
Happens(PullStreet21(horse,vehicle,street),time) ->
Happens(DriveStreet21(vehicle,street),time).

[horse,vehicle,street,time]
Happens(PullStreet12(horse,vehicle,street),time) ->
HoldsAt(At(horse,Side1(street)),time).

[horse,vehicle,street,time]
Happens(PullStreet21(horse,vehicle,street),time) ->
HoldsAt(At(horse,Side2(street)),time).

[horse,vehicle,street,location,time]
Side2(street)=location ->
Initiates(PullStreet12(horse,vehicle,street),At(horse,location),time).

[horse,vehicle,street,location,time]
Side1(street)=location ->
Initiates(PullStreet21(horse,vehicle,street),At(horse,location),time).

[horse,vehicle,street,location,time]
Side1(street)=location ->
Terminates(PullStreet12(horse,vehicle,street),At(horse,location),time).

[horse,vehicle,street,location,time]
Side2(street)=location ->
Terminates(PullStreet21(horse,vehicle,street),At(horse,location),time).

; OnVehicle

fluent OnVehicle(object,vehicleon)

event GetOnVehicle(agent,vehicleon)

event GetOffVehicle(agent,vehicleon)

[vehicleon1,vehicleon2,time]
HoldsAt(OnVehicle(vehicleon1,vehicleon2),time) ->
vehicleon1!=vehicleon2.

[vehicleon1,vehicleon2,time]
HoldsAt(OnVehicle(vehicleon1,vehicleon2),time) ->
!HoldsAt(OnVehicle(vehicleon2,vehicleon1),time).

[agent,vehicleon,time]
Initiates(GetOnVehicle(agent,vehicleon),OnVehicle(agent,vehicleon),time).

[agent,vehicleon,time]
Happens(GetOnVehicle(agent,vehicleon),time) ->
{location}
 HoldsAt(At(agent,location),time) &
 HoldsAt(At(vehicleon,location),time).

[agent,vehicleon,time]
Terminates(GetOffVehicle(agent,vehicleon),OnVehicle(agent,vehicleon),time).

[agent,vehicleon,time]
Happens(GetOffVehicle(agent,vehicleon),time) ->
HoldsAt(OnVehicle(agent,vehicleon),time).

[agent,vehicleon,location,time]
Releases(GetOnVehicle(agent,vehicleon),
         At(agent,location),
         time).

;[agent,vehicleon,location1,location2,time]
;HoldsAt(At(vehicleon,location1),time) &
;location1 != location2 ->
;Terminates(GetOffVehicle(agent,vehicleon),
;           At(agent,location2),
;           time).

[agent,vehicleon,location,time]
HoldsAt(At(vehicleon,location),time) ->
Initiates(GetOffVehicle(agent,vehicleon),
          At(agent,location),
          time).

[object,vehicleon,location,time]
HoldsAt(OnVehicle(object,vehicleon),time) &
HoldsAt(At(vehicleon,location),time) ->
HoldsAt(At(object,location),time).

; InVehicle

fluent InVehicle(object,vehiclein)

event GetInVehicle(agent,vehiclein)

event GetOutOfVehicle(agent,vehiclein)

[vehiclein1,vehiclein2,time]
HoldsAt(InVehicle(vehiclein1,vehiclein2),time) ->
vehiclein1!=vehiclein2.

[vehiclein1,vehiclein2,time]
HoldsAt(InVehicle(vehiclein1,vehiclein2),time) ->
!HoldsAt(InVehicle(vehiclein2,vehiclein1),time).

[agent,vehiclein,time]
Initiates(GetInVehicle(agent,vehiclein),InVehicle(agent,vehiclein),time).

[agent,vehiclein,time]
Happens(GetInVehicle(agent,vehiclein),time) ->
{location}
 HoldsAt(At(agent,location),time) &
 HoldsAt(At(vehiclein,location),time).

[agent,vehiclein,time]
Terminates(GetOutOfVehicle(agent,vehiclein),InVehicle(agent,vehiclein),time).

[agent,vehiclein,time]
Happens(GetOutOfVehicle(agent,vehiclein),time) ->
HoldsAt(InVehicle(agent,vehiclein),time).

[agent,vehiclein,location,time]
Releases(GetInVehicle(agent,vehiclein),
         At(agent,location),
         time).

;[agent,vehiclein,location1,location2,time]
;HoldsAt(At(vehiclein,location1),time) &
;location1 != location2 ->
;Terminates(GetOutOfVehicle(agent,vehiclein),
;           At(agent,location2),
;           time).

[agent,vehiclein,location,time]
HoldsAt(At(vehiclein,location),time) ->
Initiates(GetOutOfVehicle(agent,vehiclein),
          At(agent,location),
          time).

[object,vehiclein,location,time]
HoldsAt(InVehicle(object,vehiclein),time) &
HoldsAt(At(vehiclein,location),time) ->
HoldsAt(At(object,location),time).

; vehicle door
; door does not have to be open for entry; passenger can jump in

event VehicleDoorOpen(agent,vehicledoor)

event VehicleDoorClose(agent,vehicledoor)

fluent VehicleDoorIsOpen(vehicledoor)

[agent,vehicledoor,time]
Happens(VehicleDoorOpen(agent,vehicledoor),time) ->
HoldsAt(Awake(agent),time) &
!HoldsAt(VehicleDoorIsOpen(vehicledoor),time).

[agent,vehicledoor,time]
Initiates(VehicleDoorOpen(agent,vehicledoor),
          VehicleDoorIsOpen(vehicledoor),
          time).

[agent,vehicledoor,time]
Happens(VehicleDoorClose(agent,vehicledoor),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(VehicleDoorIsOpen(vehicledoor),time).

[agent,vehicledoor,time]
Terminates(VehicleDoorClose(agent,vehicledoor),
           VehicleDoorIsOpen(vehicledoor),
           time).

; ticketagent

sort ticketagent: agent

fluent BeTicketAgent0(ticketagent)
fluent BeTicketAgent1(ticketagent)
fluent BeTicketAgent2(ticketagent)

xor BeTicketAgent0, BeTicketAgent1, BeTicketAgent2

[ticketagent,agent,ticket,time]
HoldsAt(BeTicketAgent0(ticketagent),time) ->
Terminates(Request(agent,ticketagent,ticket),
           BeTicketAgent0(ticketagent),
           time).

[ticketagent,agent,ticket,time]
HoldsAt(BeTicketAgent0(ticketagent),time) ->
Initiates(Request(agent,ticketagent,ticket),
          BeTicketAgent1(ticketagent),
          time).

[ticketagent,agent,ticket,time]
HoldsAt(BeTicketAgent1(ticketagent),time) &
HoldsAt(KnowRequest(ticketagent,agent,ticket),time) ->
Happens(PickUp(ticketagent,ticket),time).

[ticketagent,ticket,time]
HoldsAt(BeTicketAgent1(ticketagent),time) ->
Terminates(PickUp(ticketagent,ticket),
           BeTicketAgent1(ticketagent),
           time).

[ticketagent,ticket,time]
HoldsAt(BeTicketAgent1(ticketagent),time) ->
Initiates(PickUp(ticketagent,ticket),
          BeTicketAgent2(ticketagent),
          time).

[ticketagent,agent,ticket,time]
HoldsAt(BeTicketAgent2(ticketagent),time) &
HoldsAt(KnowRequest(ticketagent,agent,ticket),time) ->
Happens(HandTo(ticketagent,agent,ticket),time).

[ticketagent,ticket,agent,time]
HoldsAt(BeTicketAgent2(ticketagent),time) ->
Terminates(HandTo(ticketagent,agent,ticket),
           BeTicketAgent2(ticketagent),
           time).

[ticketagent,ticket,agent,time]
HoldsAt(BeTicketAgent2(ticketagent),time) ->
Initiates(HandTo(ticketagent,agent,ticket),
          BeTicketAgent0(ticketagent),
          time).

; End of file.
