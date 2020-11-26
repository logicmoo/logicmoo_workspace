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
dog Snoopy

Species(Homer)=HumanSpecies.
Adult(Homer).
Species(Snoopy)=DogSpecies.
Adult(Snoopy).

!HoldsAt(Opened(GateAO),0).
{position} HoldsAt(Pos(Homer,position),0) & Outside=Loc(position).
{position} HoldsAt(Pos(Snoopy,position),0) & CageA=Loc(position).

{position} HoldsAt(Pos(Homer,position),2) & CageA=Loc(position).
{position} HoldsAt(Pos(Snoopy,position),2) & Outside=Loc(position).

[human] HoldsAt(PosDeterminingFluent(human,1),2).
[event,animal] !HoldsAt(DoneBy(event,animal),2).

range time 0 2
range position 1 8
range offset 0 0

; End of file.
