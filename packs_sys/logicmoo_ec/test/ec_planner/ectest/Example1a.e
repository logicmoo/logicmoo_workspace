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
; deduction

option timediff off

load foundations/Root.e
load foundations/EC.e

sort agent

fluent Awake(agent)
;;   executable(wake_up(_X)).
event WakeUp(agent)

;;   axiom(initiates(wake_up(X),awake(X),T),[]).
[agent,time] Initiates(WakeUp(agent),Awake(agent),time).

agent James
;; axiom(initially(neg(awake(nathan))),[]). 
!HoldsAt(Awake(James),0).

Delta: Happens(WakeUp(James),0).

completion Delta Happens

range time 0 1
range offset 1 1

;;   axiom(terminates(fall_asleep(X),awake(Y),T),[]). 
;;  
;;   abducible(dummy).
;; executable(fall_asleep(_X)).

