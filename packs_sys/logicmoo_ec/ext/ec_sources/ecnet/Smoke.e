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
; Smoking: smoking cigarettes and cigars
;

fluent CraveNicotine(agent)

fluent NicotineCravingSatisfied(agent)
noninertial NicotineCravingSatisfied

[agent,time]
HoldsAt(CraveNicotine(agent),time) <->
!HoldsAt(NicotineCravingSatisfied(agent),time).

event Smoke(agent,cigarette)

[agent,cigarette,time]
Happens(Smoke(agent,cigarette),time) ->
HoldsAt(Holding(agent,cigarette),time).

[agent,cigarette,time]
Terminates(Smoke(agent,cigarette),CraveNicotine(agent),time).

event Puff(agent,cigarette)

[agent,cigarette,time]
Happens(Puff(agent,cigarette),time) ->
Happens(Smoke(agent,cigarette),time).

event BlowOutSmoke(agent,smoke)

; End of file.
