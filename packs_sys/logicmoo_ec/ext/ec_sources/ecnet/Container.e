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
;
; Container: container
;

; linkage to OTSpace(M):
[agent,container1,container2,time]
Happens(TakeOutOf(agent,container1,container2),time) ->
HoldsAt(ContainerIsOpen(container2),time).

[agent,container1,container2,time]
Happens(PutInside(agent,container1,container2),time) ->
HoldsAt(ContainerIsOpen(container2),time).

; agent opens container.
event ContainerOpen(agent,container)

; agent closes container.
event ContainerClose(agent,container)

; container is open.
fluent ContainerIsOpen(container)

fluent ContainerClosed(container)
noninertial ContainerClosed

[container,time]
HoldsAt(ContainerClosed(container),time) <->
!HoldsAt(ContainerIsOpen(container),time).

; A precondition axiom states that
; for an agent to open a container,
; the agent must be awake,
; the container must not already be open, and
; the agent must be holding the container.
[agent,container,time]
Happens(ContainerOpen(agent,container),time) ->
HoldsAt(Awake(agent),time) &
!HoldsAt(ContainerIsOpen(container),time) &
HoldsAt(Holding(agent,container),time).

; An effect axiom states that
; if an agent opens a container,
; the container will be open:
[agent,container,time]
Initiates(ContainerOpen(agent,container),ContainerIsOpen(container),time).

; A precondition axiom states that
; for an agent to close a container,
; the agent must be awake,
; the container must be open, and
; the agent must be holding the container.
[agent,container,time]
Happens(ContainerClose(agent,container),time) ->
HoldsAt(Awake(agent),time) &
HoldsAt(ContainerIsOpen(container),time) &
HoldsAt(Holding(agent,container),time).

; An effect axiom states that
; if an agent closes a container,
; the container will no longer be open:
[agent,container,time]
Terminates(ContainerClose(agent,container),ContainerIsOpen(container),time).

; End of file.
