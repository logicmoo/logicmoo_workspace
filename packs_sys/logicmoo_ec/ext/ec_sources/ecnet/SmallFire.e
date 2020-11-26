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
; SmallFire: matches, lighters, cigarettes, etc.
;

event Light(agent,physobj)
event LightWith(agent,physobj,physobj)
event PutOut(agent,physobj)
event BlowOut(agent,physobj)
fluent IsBurning(physobj)

[agent,physobj1,physobj2,time]
HoldsAt(IsBurning(physobj2),time) ->
Initiates(LightWith(agent,physobj1,physobj2),
          IsBurning(physobj1),
          time).

[agent,physobj1,physobj2,time]
Happens(LightWith(agent,physobj1,physobj2),time) ->
HoldsAt(Holding(agent,physobj1),time) &
HoldsAt(Holding(agent,physobj2),time) &
!HoldsAt(IsBurning(physobj1),time).

[agent,physobj,time]
Initiates(Light(agent,physobj),
          IsBurning(physobj),
          time).

[agent,physobj,time]
Happens(Light(agent,physobj),time) ->
HoldsAt(Holding(agent,physobj),time) &
!HoldsAt(IsBurning(physobj),time).

[agent,physobj,time]
Terminates(PutOut(agent,physobj),
           IsBurning(physobj),
           time).

[agent,physobj,time]
Happens(PutOut(agent,physobj),time) ->
HoldsAt(Holding(agent,physobj),time) &
HoldsAt(IsBurning(physobj),time).

[agent,physobj,time]
Terminates(BlowOut(agent,physobj),
           IsBurning(physobj),
           time).

[agent,physobj,time]
Happens(BlowOut(agent,physobj),time) ->
HoldsAt(Holding(agent,physobj),time) &
HoldsAt(IsBurning(physobj),time).

; End of file.
