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
; hunger need
;

fluent Hungry(agent)

fluent Satiated(agent)
noninertial Satiated

[agent,time] HoldsAt(Hungry(agent),time) <-> !HoldsAt(Satiated(agent),time).

event Eat(agent,food)

[agent,food,time]
Happens(Eat(agent,food),time) ->
{location}
HoldsAt(At(agent,location),time) &
HoldsAt(At(food,location),time).

[agent,food,time]
Terminates(Eat(agent,food),Hungry(agent),time).

; End of file.
