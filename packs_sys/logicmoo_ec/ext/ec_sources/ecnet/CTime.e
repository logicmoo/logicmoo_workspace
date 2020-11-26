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
; clock time

; The CTime representation maps the time points of ECTime to clock time.

; part of the day

; time is in the daytime.
predicate Daytime(time)
; time is in the nighttime.
predicate Nighttime(time)
xor Daytime, Nighttime

; time is in the morning.
predicate Morning(time)
; time is in the afternoon.
predicate Afternoon(time)
; time is in the evening.
predicate Evening(time)
; time is in the night.
predicate Night(time)
; time is in the late night.
predicate LateNight(time)
xor Morning, Afternoon, Evening, Night, LateNight

[time] Daytime(time) <-> Morning(time)|Afternoon(time)|Evening(time).
[time] Nighttime(time) <-> Night(time)|LateNight(time).

; dreams

; time is part of a dream sequence.
predicate DreamSequence(time)

; End of file.
