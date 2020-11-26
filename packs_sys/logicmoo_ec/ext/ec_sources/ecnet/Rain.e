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
; Rain
;

; It starts raining at location outside.
event StartRaining(outside)

; It stops raining at location outside.
event StopRaining(outside)

; It is raining at location outside.
fluent Raining(outside)

event GetWet(object)

event Dry(object)

fluent Wet(object)

[agent,outside,time]
HoldsAt(At(agent,outside),time) &
HoldsAt(Raining(outside),time) &
!HoldsAt(Wet(agent),time) &
(!{umbrella} HoldsAt(Holding(agent,umbrella),time)) ->
Happens(GetWet(agent),time).

[object,time]
Initiates(GetWet(object),Wet(object),time).

[object,time]
Terminates(Dry(object),Wet(object),time).

; End of file.
