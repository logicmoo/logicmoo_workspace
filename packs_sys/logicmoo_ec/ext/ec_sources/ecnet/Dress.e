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
; Dress
; (cf Sleep)
;

event PutOn(agent,clothing)

event TakeOff(agent,clothing)

fluent Wearing(agent,clothing)

[agent,clothing,time]
Initiates(PutOn(agent,clothing),
          Wearing(agent,clothing),
          time).

[agent,clothing,time]
Happens(PutOn(agent,clothing),time) ->
!HoldsAt(Wearing(agent,clothing),time) &
{location} HoldsAt(At(agent,location),time) &
           HoldsAt(At(clothing,location),time).

[agent,clothing,time]
Terminates(TakeOff(agent,clothing),
           Wearing(agent,clothing),
           time).

[agent,clothing,time]
Happens(TakeOff(agent,clothing),time) ->
HoldsAt(Wearing(agent,clothing),time).

[agent,clothing,location,time]
Releases(PutOn(agent,clothing),At(clothing,location),time).

[agent,clothing,location,time]
HoldsAt(Wearing(agent,clothing),time) &
HoldsAt(At(agent,location),time) ->
HoldsAt(At(clothing,location),time).

;[agent,clothing,location1,location2,time]
;HoldsAt(At(agent,location1),time) &
;location1 != location2 ->
;Terminates(TakeOff(agent,clothing),At(clothing,location2),time).

[agent,clothing,location,time]
HoldsAt(At(agent,location),time) ->
Initiates(TakeOff(agent,clothing),At(clothing,location),time).

; End of file.
