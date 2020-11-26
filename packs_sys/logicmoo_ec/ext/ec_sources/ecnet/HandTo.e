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

event HandTo(agent,agent,physobj)

[agent1,agent2,physobj,time]
Initiates(HandTo(agent1,agent2,physobj),
          Holding(agent2,physobj),
          time).

[agent1,agent2,physobj,time]
Terminates(HandTo(agent1,agent2,physobj),
           Holding(agent1,physobj),
           time).

[agent1,agent2,physobj,time]
Happens(HandTo(agent1,agent2,physobj),time) ->
HoldsAt(Holding(agent1,physobj),time).

event ShakeHands(agent,agent)

event WriteOn(agent,paper,pen)
