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
; human health

fluent Alive(agent)

fluent Dead(agent)
noninertial Dead

fluent Injured(agent)

event Kill(object,agent)
event Injure(object,agent)
event HealInjured(agent)

[agent,time] HoldsAt(Alive(agent),time) <-> !HoldsAt(Dead(agent),time).
[agent,time] HoldsAt(Injured(agent),time) -> HoldsAt(Alive(agent),time).

[object,agent,time]
Terminates(Kill(object,agent),Alive(agent),time).

[object,agent,time]
Initiates(Injure(object,agent),Injured(agent),time).

[agent,time]
Terminates(HealInjured(agent),Injured(agent),time).

fluent Intact(physobj)

fluent Damaged(physobj)

fluent Destroyed(physobj)

; At any time, a physical object is either intact, damaged, or destroyed:
xor Intact, Damaged, Destroyed

event Damage(object,physobj)

event Destroy(object,physobj)

event Repair(object,physobj)

[object,physobj,time]
Happens(Damage(object,physobj),time) ->
HoldsAt(Intact(physobj),time).

[object,physobj,time]
Initiates(Damage(object,physobj),Damaged(physobj),time).

[object,physobj,time]
Terminates(Damage(object,physobj),Intact(physobj),time).

[object,physobj,time]
Happens(Destroy(object,physobj),time) ->
(HoldsAt(Intact(physobj),time)|
 HoldsAt(Damaged(physobj),time)).

[object,physobj,time]
Initiates(Destroy(object,physobj),Destroyed(physobj),time).

[object,physobj,time]
Terminates(Destroy(object,physobj),Intact(physobj),time).

[object,physobj,time]
Terminates(Destroy(object,physobj),Damaged(physobj),time).

[object,physobj,time]
Initiates(Repair(object,physobj),Intact(physobj),time).

; end of file.
