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
; @article{Akman:2004,
;   author = "Varol Akman and Selim T. Erdogan and Joohyung Lee and Vladimir Lifschitz and Hudson Turner",
;   year = "2004",
;   title = "Representing the zoo world and the traffic world in the language of the causal calculator",
;   journal = "Artificial Intelligence",
;   volume = "153",
;   pages = "105--140",
; }
;

option encoding 3

load foundations/Root.e
load foundations/EC.e
load examples/AkmanEtAl2004/ZooWorld.e

human Homer
elephant Jumbo

Species(Homer)=HumanSpecies.
Adult(Homer).
Species(Jumbo)=ElephantSpecies.
Adult(Jumbo).

!HoldsAt(Opened(GateAO),0).
{position} HoldsAt(Pos(Homer,position),0) & Outside=Loc(position).
{position} HoldsAt(Pos(Jumbo,position),0) & CageA=Loc(position).

{position} HoldsAt(Pos(Homer,position),4) & CageA=Loc(position).
{position} HoldsAt(Pos(Jumbo,position),4) & Outside=Loc(position).

[human] HoldsAt(PosDeterminingFluent(human,1),4).
[event,animal] !HoldsAt(DoneBy(event,animal),4).

; ccalc.2.0b.8.3 single model
;HoldsAt(Pos(Homer,7),0).
;HoldsAt(Pos(Jumbo,2),0).
;Happens(Move(Jumbo,4),0).
;Happens(Open(Homer,GateAO),0).
;Happens(Mount(Homer,Jumbo),1).
;Happens(ThrowOff(Jumbo,Homer),2).
;HoldsAt(PosDeterminingFluent(Homer,1),2).
;Happens(Move(Jumbo,7),3).
;Happens(Mount(Homer,Jumbo),3).

range time 0 4
range position 1 8
range offset 0 0

; End of file.
