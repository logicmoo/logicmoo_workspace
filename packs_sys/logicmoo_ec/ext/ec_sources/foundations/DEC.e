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
; Discrete Event Calculus (DEC)
;
; @article{Mueller:2004a,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "Event calculus reasoning through satisfiability",
;   journal = "Journal of Logic and Computation",
;   volume = "14",
;   number = "5",
;   pages = "703--730",
; }
;

sort time: integer
sort offset: integer

reified sort fluent
reified sort event

predicate Happens(event,time)
predicate HoldsAt(fluent,time)
predicate ReleasedAt(fluent,time)

predicate Initiates(event,fluent,time)
predicate Terminates(event,fluent,time)
predicate Releases(event,fluent,time)

[fluent,time]
(HoldsAt(fluent,time) &
 !ReleasedAt(fluent,time+1) &
 !({event} Happens(event,time) & Terminates(event,fluent,time))) ->
HoldsAt(fluent,time+1).

[fluent,time]
(!HoldsAt(fluent,time) &
 !ReleasedAt(fluent,time+1) &
 !({event} Happens(event,time) & Initiates(event,fluent,time))) ->
!HoldsAt(fluent,time+1).

[fluent,time]
(!ReleasedAt(fluent,time) &
 !({event} Happens(event,time) & Releases(event,fluent,time))) ->
!ReleasedAt(fluent,time+1).

[fluent,time]
(ReleasedAt(fluent,time) &
 !({event} Happens(event,time) &
   (Initiates(event,fluent,time) |
    Terminates(event,fluent,time)))) ->
ReleasedAt(fluent,time+1).

[event,fluent,time]
(Happens(event,time) & Initiates(event,fluent,time)) ->
(HoldsAt(fluent,time+1) & !ReleasedAt(fluent,time+1)).

[event,fluent,time]
(Happens(event,time) & Terminates(event,fluent,time)) ->
(!HoldsAt(fluent,time+1) & !ReleasedAt(fluent,time+1)).

[event,fluent,time]
(Happens(event,time) & Releases(event,fluent,time)) ->
ReleasedAt(fluent,time+1).

; End of file.
