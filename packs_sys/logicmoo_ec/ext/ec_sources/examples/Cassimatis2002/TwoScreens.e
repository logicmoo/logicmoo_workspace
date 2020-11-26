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
object X,Y,Screen1,Screen2

; perceptions:
Shape(X,Round).
Color(X,Red).
Shape(Y,Round).
Color(Y,Red).
Shape(Screen1,Square).
Color(Screen1,Green).
Shape(Screen2,Square).
Color(Screen2,Green).
[time] HoldsAt(Location(G1,Screen1,2,0),time).
[time] HoldsAt(Location(G1,Screen2,4,0),time).
HoldsAt(Location(G1,X,1,1),0).
HoldsAt(Location(G1,Y,5,1),4).

[xcoord,ycoord,time]
xcoord!=2 & xcoord!=4 & !(xcoord=1 & ycoord=1 & time=0) ->
!HoldsAt(Location(G1,X,xcoord,ycoord),time) |
xcoord=5 & ycoord=1 & time=4 & Equal(X,Y).

[xcoord,ycoord,time]
xcoord!=2 & xcoord!=4 & !(xcoord=5 & ycoord=1 & time=4) ->
!HoldsAt(Location(G1,Y,xcoord,ycoord),time) |
xcoord=1 & ycoord=1 & time=0 & Equal(X,Y).

range time 0 4
range xcoord 0 5
range ycoord 0 1
range offset 0 0

; End of file.
