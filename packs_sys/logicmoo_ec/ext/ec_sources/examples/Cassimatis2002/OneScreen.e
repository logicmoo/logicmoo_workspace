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

load foundations/Root.e
load foundations/EC.e
load examples/Cassimatis2002/PolySpace.e

grid G1
object X,Y,Screen

; perceptions:
Shape(X,Round).
Color(X,Red).
Shape(Y,Round).
Color(Y,Red).
Shape(Screen,Square).
Color(Screen,Green).
[time] HoldsAt(Location(G1,Screen,2,0),time).
HoldsAt(Location(G1,X,1,1),0).
HoldsAt(Location(G1,Y,3,1),2).

[xcoord,ycoord,time]
xcoord!=2 & !(xcoord=1 & ycoord=1 & time=0) ->
!HoldsAt(Location(G1,X,xcoord,ycoord),time) |
xcoord=3 & ycoord=1 & time=2 & Equal(X,Y).

[xcoord,ycoord,time]
xcoord!=2 & !(xcoord=3 & ycoord=1 & time=2) ->
!HoldsAt(Location(G1,Y,xcoord,ycoord),time) |
xcoord=1 & ycoord=1 & time=0 & Equal(X,Y).

range time 0 2
range xcoord 0 4
range ycoord 0 2
range offset 0 0

; End of file.
