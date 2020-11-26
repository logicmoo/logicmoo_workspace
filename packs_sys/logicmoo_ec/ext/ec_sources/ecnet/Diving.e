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
; scuba diving
;

sort object
sort agent: object
sort diver: agent
sort depth: integer
sort boat: object

; reference line, anchor line, shotline, SMB line, ...
sort line: object

sort equipment: object
sort weight: equipment
sort fin: equipment
sort airtank: equipment

; buoyancy compensator (BC)
; buoyancy control device (BCD)
sort computer: equipment
sort bc: equipment

fluent AtDepth(object,depth)

[object,depth1,depth2,time]
HoldsAt(AtDepth(object,depth1),time) &
HoldsAt(AtDepth(object,depth2),time) ->
depth1 = depth2.

event Ascend(diver,depth)

event Descend(diver,depth)

[diver,depth1,depth2,time]
HoldsAt(AtDepth(diver,depth1),time) &
Happens(Descend(diver,depth2),time) ->
depth2>depth1.

[diver,depth1,depth2,time]
HoldsAt(AtDepth(diver,depth1),time) &
Happens(Ascend(diver,depth2),time) ->
depth2<depth1.

[diver,depth,time]
Initiates(Descend(diver,depth),AtDepth(diver,depth),time).

[diver,depth1,depth2,time]
HoldsAt(AtDepth(diver,depth1),time) ->
Terminates(Descend(diver,depth2),AtDepth(diver,depth1),time).

[diver,depth,time]
Initiates(Ascend(diver,depth),AtDepth(diver,depth),time).

[diver,depth1,depth2,time]
HoldsAt(AtDepth(diver,depth1),time) ->
Terminates(Ascend(diver,depth2),AtDepth(diver,depth1),time).

fluent Wearing(diver,equipment)

event PutOn(diver,equipment)

event TakeOff(diver,equipment)

event Lose(diver,equipment)

[diver,equipment,depth,time]
Releases(PutOn(diver,equipment),AtDepth(equipment,depth),time).

[diver,equipment,time]
Releases(PutOn(diver,equipment),UnderWater(equipment),time).

[diver,equipment,time]
Happens(PutOn(diver,equipment),time) ->
!{diver1} HoldsAt(Wearing(diver1,equipment),time).

[diver,depth,equipment,time]
HoldsAt(Wearing(diver,equipment),time) ->
(HoldsAt(AtDepth(diver,depth),time) <->
 HoldsAt(AtDepth(equipment,depth),time)).

[diver,depth,object,time]
HoldsAt(Holding(diver,object),time) ->
(HoldsAt(AtDepth(diver,depth),time) <->
 HoldsAt(AtDepth(object,depth),time)).

[diver,equipment,time]
HoldsAt(Wearing(diver,equipment),time) ->
(HoldsAt(UnderWater(diver),time) <->
 HoldsAt(UnderWater(equipment),time)).

[diver,object,time]
HoldsAt(Holding(diver,object),time) ->
(HoldsAt(UnderWater(diver),time) <->
 HoldsAt(UnderWater(object),time)).

[diver,depth,equipment,time]
HoldsAt(AtDepth(diver,depth),time) &
HoldsAt(Wearing(diver,equipment),time) ->
Initiates(TakeOff(diver,equipment),AtDepth(equipment,depth),time).

[diver,depth,equipment,time]
!HoldsAt(AtDepth(diver,depth),time) &
HoldsAt(Wearing(diver,equipment),time) ->
Terminates(TakeOff(diver,equipment),AtDepth(equipment,depth),time).

[diver,equipment,time]
HoldsAt(UnderWater(diver),time) ->
Initiates(TakeOff(diver,equipment),UnderWater(equipment),time).

[diver,equipment,time]
!HoldsAt(UnderWater(diver),time) ->
Terminates(TakeOff(diver,equipment),UnderWater(equipment),time).

[diver,equipment,depth,time]
HoldsAt(AtDepth(diver,depth),time) &
HoldsAt(Wearing(diver,equipment),time) ->
Initiates(Lose(diver,equipment),AtDepth(equipment,depth),time).

[diver,equipment,depth,time]
!HoldsAt(AtDepth(diver,depth),time) &
HoldsAt(Wearing(diver,equipment),time) ->
Terminates(Lose(diver,equipment),AtDepth(equipment,depth),time).

[diver,equipment,time]
HoldsAt(UnderWater(diver),time) ->
Initiates(Lose(diver,equipment),UnderWater(equipment),time).

[diver,equipment,time]
!HoldsAt(UnderWater(diver),time) ->
Terminates(Lose(diver,equipment),UnderWater(equipment),time).

fluent Holding(diver,object)

[diver1,diver2,time]
HoldsAt(Holding(diver1,diver2),time) ->
!HoldsAt(Holding(diver2,diver1),time).

event Grab(diver,object)

event LetGoOf(diver,object)

[diver,object,time]
Initiates(Grab(diver,object),Holding(diver,object),time).

[diver,object,time]
Terminates(LetGoOf(diver,object),Holding(diver,object),time).

[diver,object,depth,time]
Releases(Grab(diver,object),AtDepth(object,depth),time).

[diver,object,time]
Releases(Grab(diver,object),UnderWater(object),time).

[diver,object,depth,time]
HoldsAt(AtDepth(diver,depth),time) &
HoldsAt(Holding(diver,object),time) ->
Initiates(LetGoOf(diver,object),AtDepth(object,depth),time).

[diver,object,depth,time]
!HoldsAt(AtDepth(diver,depth),time) &
HoldsAt(Holding(diver,object),time) ->
Terminates(LetGoOf(diver,object),AtDepth(object,depth),time).

[diver,object,time]
HoldsAt(UnderWater(diver),time) ->
Initiates(LetGoOf(diver,object),UnderWater(object),time).

[diver,object,time]
!HoldsAt(UnderWater(diver),time) ->
Terminates(LetGoOf(diver,object),UnderWater(object),time).

[diver,equipment,time]
Initiates(PutOn(diver,equipment),Wearing(diver,equipment),time).

[diver,equipment,time]
Happens(PutOn(diver,equipment),time) ->
!HoldsAt(UnderWater(diver),time).

[diver,equipment,time]
Terminates(TakeOff(diver,equipment),Wearing(diver,equipment),time).

[diver,equipment,time]
Terminates(Lose(diver,equipment),Wearing(diver,equipment),time).

fluent Vertical(diver)

fluent HorizontalDown(diver)

fluent Inverted(diver)

fluent HorizontalUp(diver)

xor Vertical, HorizontalDown, Inverted, HorizontalUp

event RotatePitch(diver)

[diver,time]
HoldsAt(Vertical(diver),time) ->
Initiates(RotatePitch(diver),HorizontalDown(diver),time).

[diver,time]
HoldsAt(HorizontalDown(diver),time) ->
Initiates(RotatePitch(diver),Inverted(diver),time).

[diver,time]
HoldsAt(HorizontalDown(diver),time) ->
Terminates(RotatePitch(diver),HorizontalDown(diver),time).

[diver,time]
HoldsAt(Inverted(diver),time) ->
Initiates(RotatePitch(diver),HorizontalUp(diver),time).

[diver,time]
HoldsAt(Inverted(diver),time) ->
Terminates(RotatePitch(diver),Inverted(diver),time).

[diver,time]
HoldsAt(HorizontalUp(diver),time) ->
Initiates(RotatePitch(diver),Vertical(diver),time).

[diver,time]
HoldsAt(HorizontalUp(diver),time) ->
Terminates(RotatePitch(diver),HorizontalUp(diver),time).

event RotateYaw(diver)

; try taking out Holding condition here
[diver,time]
Happens(Ascend1(diver),time) &
!Happens(RapidAscendToSurface(diver),time) &
!({diver1} HoldsAt(Holding(diver,diver1),time)) ->
Happens(RotateYaw(diver),time).

fluent UnderWater(object)

[object,depth,time]
depth>0 &
HoldsAt(AtDepth(object,depth),time) ->
HoldsAt(UnderWater(object),time).

event EnterWater(object)

event Surface(object)

[object,time]
Initiates(EnterWater(object),UnderWater(object),time).

[diver,time]
Happens(EnterWater(diver),time) ->
!{diver1} HoldsAt(Holding(diver1,diver),time).

[object,depth,time]
depth=0 ->
Initiates(EnterWater(object),AtDepth(object,depth),time).

[object,time]
Terminates(Surface(object),UnderWater(object),time).

[diver,time]
Terminates(Surface(diver),PositivelyBuoyant(diver),time).

[diver,time]
Terminates(Surface(diver),NegativelyBuoyant(diver),time).

[diver,time]
Terminates(Surface(diver),NeutrallyBuoyant(diver),time).

[object,depth,time]
Terminates(Surface(object),AtDepth(object,depth),time).

[diver,time] Happens(EnterWater(diver),time) ->
HoldsAt(Vertical(diver),time).

fluent StandingOn(diver,boat)

event StandOn(diver,boat)

[diver,boat,time]
Terminates(EnterWater(diver),StandingOn(diver,boat),time).

[diver,boat,time]
Initiates(StandOn(diver,boat),StandingOn(diver,boat),time).

fluent PositivelyBuoyant(diver)

fluent NeutrallyBuoyant(diver)

fluent NegativelyBuoyant(diver)

mutex PositivelyBuoyant, NeutrallyBuoyant, NegativelyBuoyant

[diver,time]
HoldsAt(PositivelyBuoyant(diver),time) ->
HoldsAt(UnderWater(diver),time).

[diver,time]
HoldsAt(NeutrallyBuoyant(diver),time) ->
HoldsAt(UnderWater(diver),time).

[diver,time]
HoldsAt(NegativelyBuoyant(diver),time) ->
HoldsAt(UnderWater(diver),time).

event PressDeflateButton(diver,bc)

event PressDumpButton(diver,bc)

event PressInflateButton(diver,bc)

[diver,bc,time]
Happens(PressDeflateButton(diver,bc),time) ->
HoldsAt(Vertical(diver),time) &
HoldsAt(UnderWater(bc),time).

[diver,bc,time]
Happens(PressDumpButton(diver,bc),time) ->
HoldsAt(Vertical(diver),time) &
HoldsAt(UnderWater(bc),time).

[diver,bc,time] Happens(PressDumpButton(diver,bc),time) ->
HoldsAt(UncontrolledBuoyancy(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Initiates(PressDeflateButton(diver,bc),NegativelyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Terminates(PressDeflateButton(diver,bc),NeutrallyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Terminates(PressDeflateButton(diver,bc),PositivelyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Initiates(PressDumpButton(diver,bc),NegativelyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Terminates(PressDumpButton(diver,bc),NeutrallyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Terminates(PressDumpButton(diver,bc),PositivelyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Initiates(PressInflateButton(diver,bc),NeutrallyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Terminates(PressInflateButton(diver,bc),PositivelyBuoyant(diver),time).

[diver,bc,time]
HoldsAt(Wearing(diver,bc),time) ->
Terminates(PressInflateButton(diver,bc),NegativelyBuoyant(diver),time).

[diver,weight,time]
HoldsAt(Wearing(diver,weight),time) ->
Initiates(TakeOff(diver,weight),PositivelyBuoyant(diver),time).

[diver,weight,time]
HoldsAt(Wearing(diver,weight),time) ->
Terminates(TakeOff(diver,weight),NegativelyBuoyant(diver),time).

[diver,weight,time]
HoldsAt(Wearing(diver,weight),time) ->
Terminates(TakeOff(diver,weight),NeutrallyBuoyant(diver),time).

fluent UncontrolledBuoyancy(diver)

event LoseBuoyancyControl(diver)

predicate IsInexperiencedDiver(diver)

[diver,time]
Happens(LoseBuoyancyControl(diver),time) ->
IsInexperiencedDiver(diver).

[diver,time]
Initiates(LoseBuoyancyControl(diver),UncontrolledBuoyancy(diver),time).

[diver,time]
Initiates(LoseBuoyancyControl(diver),PositivelyBuoyant(diver),time).

[diver,time]
Terminates(LoseBuoyancyControl(diver),NegativelyBuoyant(diver),time).

[diver,time]
Terminates(LoseBuoyancyControl(diver),NeutrallyBuoyant(diver),time).

; determining fluent
fluent AscendDescendAmount(diver,depth)
noninertial AscendDescendAmount

[diver,depth1,depth2,time]
HoldsAt(AscendDescendAmount(diver,depth1),time) &
HoldsAt(AscendDescendAmount(diver,depth2),time) ->
depth1=depth2.

[diver,depth,time]
Happens(Descend(diver,depth),time) ->
HoldsAt(NegativelyBuoyant(diver),time) &
({depth1}
 HoldsAt(AscendDescendAmount(diver,depth1),time) &
 HoldsAt(AtDepth(diver,depth-depth1),time)).

event KickUp(diver)

[diver,depth,time]
Happens(Ascend(diver,depth),time) ->
(HoldsAt(PositivelyBuoyant(diver),time) |
 (HoldsAt(NeutrallyBuoyant(diver),time) & Happens(KickUp(diver),time))) &
({depth1}
 HoldsAt(AscendDescendAmount(diver,depth1),time) &
 HoldsAt(AtDepth(diver,depth+depth1),time)).

[diver,time]
Happens(KickUp(diver),time) ->
HoldsAt(Vertical(diver),time).

event SwimAround(diver)

[diver,time]
Happens(SwimAround(diver),time) ->
HoldsAt(HorizontalDown(diver),time).

; signaling

event SignalDescend(diver,diver)

event SignalOutOfTime(diver,diver)

event SignalAscend(diver,diver)

;[diver1,diver2,time]
;Happens(SignalAscend(diver1,diver2),time) ->
;Happens(SignalOutOfTime(diver1,diver2),time-1).

;[diver1,diver2,time]
;Happens(SignalDescend(diver1,diver2),time) ->
;HoldsAt(See(diver1,diver2),time) &
;HoldsAt(See(diver2,diver1),time).

;[diver1,diver2,time]
;Happens(SignalOutOfTime(diver1,diver2),time) ->
;HoldsAt(See(diver1,diver2),time) &
;HoldsAt(See(diver2,diver1),time).

;[diver1,diver2,time]
;Happens(SignalAscend(diver1,diver2),time) ->
;HoldsAt(See(diver1,diver2),time) &
;HoldsAt(See(diver2,diver1),time).

;event LookAt(agent,object)

;fluent See(agent,object)

;[agent,object,time]
;Initiates(LookAt(agent,object),See(agent,object),time).

;[agent,object1,object2,time]
;object1!=object2 ->
;Terminates(LookAt(agent,object1),
;           See(agent,object2),
;           time).

event Descend1(diver)

event Ascend1(diver)

;[diver,object,time]
;Terminates(Descend1(diver),See(diver,object),time).

;[diver,object,time]
;Terminates(Ascend1(diver),See(diver,object),time).

;[diver,object,time]
;Terminates(RotateYaw(diver),See(diver,object),time).

event RapidAscendToSurface(diver)

[diver,time]
Happens(Descend1(diver),time) <->
({depth} Happens(Descend(diver,depth),time)).

[diver,time]
Happens(Ascend1(diver),time) <->
({depth} Happens(Ascend(diver,depth),time)).

[diver,time]
Happens(RapidAscendToSurface(diver),time) ->
Happens(Ascend(diver,0),time).

event AscendLine(diver,line)

[diver,line,time]
Happens(AscendLine(diver,line),time) ->
Happens(Ascend1(diver),time).

fluent Disoriented(diver)

event BecomeDisoriented(diver)

event BecomeReoriented(diver)

[diver,time]
Initiates(BecomeDisoriented(diver),Disoriented(diver),time).

[diver,time]
Terminates(BecomeReoriented(diver),Disoriented(diver),time).

fluent DisturbedSilt()

event DisturbSilt(diver)

[diver,time]
Initiates(DisturbSilt(diver),DisturbedSilt(),time).

[diver,time]
Happens(BecomeDisoriented(diver),time) ->
(!HoldsAt(DisturbedSilt(),time-1) &
 HoldsAt(DisturbedSilt(),time)).

event Panic(diver)

[diver,time] Happens(Panic(diver),time) ->
HoldsAt(Disoriented(diver),time) |
HoldsAt(UncontrolledBuoyancy(diver),time) |
({equipment} Happens(Lose(diver,equipment),time-1)) |
Happens(Vomit(diver),time-1).

event Vomit(diver)

; conditions

fluent Unconscious(diver)

event GoUnconscious(diver)

event RegainConsciousness(diver)

[diver,time]
Initiates(GoUnconscious(diver),Unconscious(diver),time).

[diver,time]
Terminates(RegainConsciousness(diver),Unconscious(diver),time).

[diver,time]
Happens(GoUnconscious(diver),time) ->
Happens(RapidAscendToSurface(diver),time).

fluent HasEarPain(diver)

event StartEarPain(diver)

[diver,time] Initiates(StartEarPain(diver),HasEarPain(diver),time).

fluent HasRupturedEardrum(diver)

event RuptureEardrum(diver)

[diver,time]
Initiates(RuptureEardrum(diver),HasRupturedEardrum(diver),time).
fluent ConditionOK(diver)

fluent HasDecompressionIllness(diver)

event StartDecompressionIllness(diver)

[diver,time]
Initiates(StartDecompressionIllness(diver),
          HasDecompressionIllness(diver),
          time).

fluent SignalingDecompress(computer,diver)

fluent SignalingLowOnAir(computer,airtank,diver)

[computer,airtank,diver,time]
HoldsAt(SignalingLowOnAir(computer,airtank,diver),time) ->
HoldsAt(LowOnAir(airtank),time).

[computer,diver,time]
HoldsAt(SignalingDecompress(computer,diver),time) ->
!{time1} time1<time & Happens(Decompress(diver),time1).

event Decompress(diver)

event EqualizeEars(diver)

[diver,time]
(Happens(Descend1(diver),time) | Happens(Ascend1(diver),time)) &
!Happens(EqualizeEars(diver),time) ->
Happens(StartEarPain(diver),time) &
Happens(RuptureEardrum(diver),time).

[diver,time]
Happens(Ascend1(diver),time) &
!Happens(Decompress(diver),time) ->
Happens(StartDecompressionIllness(diver),time).

[diver1,diver2,time]
HoldsAt(Holding(diver1,diver2),time) &
Happens(Ascend1(diver1),time) &
!Happens(Decompress(diver2),time) ->
Happens(StartDecompressionIllness(diver2),time).

[diver,time]
Happens(Decompress(diver),time) ->
({depth} depth>0 & HoldsAt(AtDepth(diver,depth),time)) &
!HoldsAt(UncontrolledBuoyancy(diver),time).

fluent HasHeadache(diver)

[diver,time]
HoldsAt(ConditionOK(diver),time) ->
!HoldsAt(Unconscious(diver),time) &
!HoldsAt(HasEarPain(diver),time) &
!HoldsAt(HasRupturedEardrum(diver),time) &
!HoldsAt(HasDecompressionIllness(diver),time) &
!HoldsAt(HasHeadache(diver),time).

event BeAirlifted(diver)

event TakeInWater(diver)

fluent LowOnAir(airtank)

event BecomeLowOnAir(airtank)

[airtank,time]
Initiates(BecomeLowOnAir(airtank),LowOnAir(airtank),time).

; initial state
[diver] HoldsAt(ConditionOK(diver),0).
[diver] HoldsAt(Vertical(diver),0).
!HoldsAt(DisturbedSilt(),0).
[diver] !HoldsAt(UncontrolledBuoyancy(diver),0).
[diver] !HoldsAt(Disoriented(diver),0).
[diver] !HoldsAt(PositivelyBuoyant(diver),0) &
        !HoldsAt(NeutrallyBuoyant(diver),0) &
        !HoldsAt(NegativelyBuoyant(diver),0).
[diver,object] !HoldsAt(Wearing(diver,object),0).
[diver,object] !HoldsAt(Holding(diver,object),0).
[diver1,diver2] !HoldsAt(Separated(diver1,diver2),0).
;[agent,object] !HoldsAt(See(agent,object),0).

fluent Separated(diver,diver)

[diver1,diver2,time]
HoldsAt(Separated(diver1,diver2),time) ->
HoldsAt(Separated(diver2,diver1),time).

event BecomeSeparated(diver,diver)

event BeReunitedWith(diver,diver)

[diver1,diver2,time]
Initiates(BecomeSeparated(diver1,diver2),Separated(diver1,diver2),time).

[diver1,diver2,time]
Initiates(BecomeSeparated(diver1,diver2),Separated(diver2,diver1),time).

[diver1,diver2,time]
Terminates(BeReunitedWith(diver1,diver2),Separated(diver1,diver2),time).

[diver1,diver2,time]
Terminates(BeReunitedWith(diver1,diver2),Separated(diver2,diver1),time).

; End of file.
