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
; Causal Constraints
;
; @inproceedings{Shanahan:1999a,
;   author = "Murray Shanahan",
;   year = "1999",
;   title = "The ramification problem in the event calculus",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}ixteenth \uppercase{I}nternational \uppercase{J}oint \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
;   pages = "140--146",
;   address = "San Mateo, CA",
;   publisher = "Morgan Kaufmann",
; }
;

predicate Started(fluent,time)
predicate Stopped(fluent,time)

[fluent,time]
Started(fluent,time) <->
(HoldsAt(fluent,time) |
 ({event} Happens(event,time) & Initiates(event,fluent,time))).

[fluent,time]
Stopped(fluent,time) <->
(!HoldsAt(fluent,time) |
 ({event} Happens(event,time) & Terminates(event,fluent,time))).

predicate Initiated(fluent,time)
predicate Terminated(fluent,time)

[fluent,time]
Initiated(fluent,time) <->
(Started(fluent,time) &
 !({event} Happens(event,time) & Terminates(event,fluent,time))).

[fluent,time]
Terminated(fluent,time) <->
(Stopped(fluent,time) &
 !({event} Happens(event,time) & Initiates(event,fluent,time))).

; End of file.
