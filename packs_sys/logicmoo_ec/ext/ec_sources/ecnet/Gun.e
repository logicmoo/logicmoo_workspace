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

fluent Loaded(gun,bullet)
noninertial Loaded

[gun,bullet,time]
HoldsAt(Inside(bullet,gun),time) <->
HoldsAt(Loaded(gun,bullet),time).

event Shoot(agent,gun,object)

event ShootInjure(agent,gun,agent)

event ShootKill(agent,gun,agent)

event ShootDamage(agent,gun,physobj)

event ShootDestroy(agent,gun,physobj)

[agent,gun,bullet,object,time]
HoldsAt(Inside(bullet,gun),time) ->
Terminates(Shoot(agent,gun,object),
           Inside(bullet,gun),
           time).

[agent,gun,bullet,object,location1,location2,time]
HoldsAt(Inside(bullet,gun),time) &
HoldsAt(At(gun,location1),time) &
location1 != location2 ->
Terminates(Shoot(agent,gun,object),At(bullet,location2),time).

[agent,gun,bullet,object,location,time]
HoldsAt(At(object,location),time) &
HoldsAt(Inside(bullet,gun),time) ->
Initiates(Shoot(agent,gun,object),At(bullet,location),time).

[agent,gun,object,time]
Happens(Shoot(agent,gun,object),time) ->
HoldsAt(Holding(agent,gun),time) &
({bullet} HoldsAt(Loaded(gun,bullet),time)) &
({location} HoldsAt(At(agent,location),time) &
            HoldsAt(At(object,location),time)).

[agent1,gun,agent2,time]
Happens(Shoot(agent1,gun,agent2),time) ->
Happens(ShootInjure(agent1,gun,agent2),time) |
Happens(ShootKill(agent1,gun,agent2),time).

[agent1,gun,bullet,agent2,time]
HoldsAt(Inside(bullet,gun),time) &
Happens(ShootKill(agent1,gun,agent2),time) ->
Happens(Kill(bullet,agent2),time).

[agent1,gun,bullet,agent2,time]
HoldsAt(Inside(bullet,gun),time) &
Happens(ShootInjure(agent1,gun,agent2),time) ->
Happens(Injure(bullet,agent2),time).

[agent,gun,physobj,time]
Happens(Shoot(agent,gun,physobj),time) ->
Happens(ShootDamage(agent,gun,physobj),time) |
Happens(ShootDestroy(agent,gun,physobj),time).

[agent,gun,bullet,physobj,time]
HoldsAt(Inside(bullet,gun),time) &
Happens(ShootDamage(agent,gun,physobj),time) ->
Happens(Damage(bullet,physobj),time).

[agent,gun,bullet,physobj,time]
HoldsAt(Inside(bullet,gun),time) &
Happens(ShootDestroy(agent,gun,physobj),time) ->
Happens(Destroy(bullet,physobj),time).

; End of file.
