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
; Method (D)
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object

object A,B

fluent P(object)
fluent Q(object)
fluent R(object)

predicate Ab1(object,time)
predicate Ab2(object,time)

[object,time]
HoldsAt(P(object),time) & !Ab1(object,time) ->
HoldsAt(Q(object),time).

[object,time]
HoldsAt(R(object),time) & !Ab2(object,time) ->
!HoldsAt(Q(object),time).

[object,time]
HoldsAt(R(object),time) -> HoldsAt(P(object),time).

HoldsAt(R(A),0).
HoldsAt(P(B),0).
!HoldsAt(R(B),0).

Theta: 
[object,time]
HoldsAt(R(object),time) -> Ab1(object,time).

range time 0 0
range offset 1 1

completion Theta Ab1
completion Theta Ab2

; End of file.
