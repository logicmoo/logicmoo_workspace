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

option encoding 3

load foundations/Root.e
load foundations/EC.e

sort person
sort object

event Hold(person,object)
fluent Holding(person,object)

person P1
object O1

Happens(Hold(P1,O1),0).

[person,object,time]
Initiates(Hold(person,object),Holding(person,object),time).

!HoldsAt(Holding(P1,O1),0).
;;; AUTO !ReleasedAt(Holding(P1,O1),0).

completion Happens

range time 0 1
range offset 1 1

; End of file.
