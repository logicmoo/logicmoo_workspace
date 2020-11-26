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
; @inproceedings{ShanahanRandell:2004,
;   author = "Murray Shanahan and David A. Randell",
;   year = "2004",
;   title = "A logic-based formulation of active visual perception",
;   editor = "Didier Dubois and Christopher A. Welty and Mary-Anne Williams",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{N}inth \uppercase{I}nternational \uppercase{C}onference on \uppercase{P}rinciples of \uppercase{K}nowledge \uppercase{R}epresentation and \uppercase{R}easoning",
;   pages = "64--72",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort object
sort shape
sort aspect

object Object1
aspect Aspect1, Aspect2, Aspect3
shape Shape1, Shape2

predicate Shape(object,shape)
predicate Arc(shape,aspect,aspect)
fluent Aspect(object,aspect)
event Change(object,aspect,aspect)

; Sigma

[object,aspect1,aspect2,shape,time]
HoldsAt(Aspect(object,aspect1),time) &
Shape(object,shape) &
(Arc(shape,aspect1,aspect2) |
 Arc(shape,aspect2,aspect1)) ->
Initiates(Change(object,aspect1,aspect2),Aspect(object,aspect2),time).

[object,aspect1,aspect2,shape,time]
HoldsAt(Aspect(object,aspect1),time) &
Shape(object,shape) &
(Arc(shape,aspect1,aspect2) |
 Arc(shape,aspect2,aspect1)) ->
Terminates(Change(object,aspect1,aspect2),Aspect(object,aspect1),time).

; preconditions (added)

[object,aspect1,aspect2,time]
Happens(Change(object,aspect1,aspect2),time) ->
HoldsAt(Aspect(object,aspect1),time).

[object,aspect1,aspect2,aspect3,time]
Happens(Change(object,aspect1,aspect2),time) &
Happens(Change(object,aspect1,aspect3),time) ->
aspect2=aspect3.

; Psi

[object,shape1,shape2]
Shape(object,shape1) &
Shape(object,shape2) ->
shape1=shape2.

[object,aspect1,aspect2,time]
HoldsAt(Aspect(object,aspect1),time) &
HoldsAt(Aspect(object,aspect2),time) ->
aspect1=aspect2.

[aspect1,aspect2]
Arc(Shape1,aspect1,aspect2) <->
(aspect1=Aspect1 & aspect2=Aspect2).

[aspect1,aspect2]
Arc(Shape2,aspect1,aspect2) <->
((aspect1=Aspect1 & aspect2=Aspect3) |
 (aspect1=Aspect3 & aspect2=Aspect2)).

; Gamma

HoldsAt(Aspect(Object1,Aspect1),0).
HoldsAt(Aspect(Object1,Aspect2),1).

;completion Delta Happens

range time 0 1
range offset 1 1

; End of file.
