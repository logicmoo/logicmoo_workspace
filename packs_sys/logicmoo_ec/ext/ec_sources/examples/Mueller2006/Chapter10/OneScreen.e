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
; @phdthesis{Cassimatis:2002,
;   author = "Nicholas L. Cassimatis",
;   year = "2002",
;   title = "Polyscheme: A Cognitive Architecture for Integrating Multiple Representation and Inference Schemes",
;   address = "Cambridge, MA",
;   school = "Program in Media Arts and Sciences, School of Architecture and Planning, Massachusetts Institute of Technology",
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

load foundations/Root.e
load foundations/EC.e

sort object
sort location

object O1, O2
location L1, L2, L3

predicate Adjacent(location,location)
predicate Equal(object,object)

fluent At(object,location)
event Move(object,location,location)

; Sigma

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
Adjacent(location1,location2) ->
Initiates(Move(object,location1,location2),At(object,location2),time).

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
Adjacent(location1,location2) ->
Terminates(Move(object,location1,location2),At(object,location1),time).

; Psi

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
HoldsAt(At(object,location2),time) ->
location1=location2.

[object,time]
{location} HoldsAt(At(object,location),time).

[object1,object2,location,time]
HoldsAt(At(object1,location),time) &
HoldsAt(At(object2,location),time) ->
Equal(object1,object2).

[location1, location2]
Adjacent(location1,location2) <->
Adjacent(location2,location1).

[object1,object2]
Equal(object1,object2) <->
Equal(object2,object1).

; Gamma

[location1,location2]
Adjacent(location1,location2) <->
(location1=L1 & location2=L2) |
(location1=L2 & location2=L1) |
(location1=L2 & location2=L3) |
(location1=L3 & location2=L2).

HoldsAt(At(O1,L1),0).
[object] !HoldsAt(At(object,L3),0).

[object] !HoldsAt(At(object,L1),1).
[object] !HoldsAt(At(object,L3),1).

HoldsAt(At(O2,L3),2).
[object] !HoldsAt(At(object,L1),2).

; ADDED:
[object,location1,location2,time]
Happens(Move(object,location1,location2),time) ->
HoldsAt(At(object,location1),time) &
Adjacent(location1,location2).

[object1,object2,location1,location2,time]
Equal(object1,object2) &
Happens(Move(object1,location1,location2),time) ->
Happens(Move(object2,location1,location2),time).

range time 0 2
range offset 1 1

; End of file.
