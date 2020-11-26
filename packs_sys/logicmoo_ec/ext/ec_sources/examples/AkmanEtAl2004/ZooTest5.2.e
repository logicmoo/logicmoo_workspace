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
horse Silver

Species(Homer)=HumanSpecies.
Adult(Homer).
Species(Jumbo)=ElephantSpecies.
Adult(Jumbo).
Species(Silver)=HorseSpecies.
Adult(Silver).

{position}
!HoldsAt(Pos(Homer,position),0) &
HoldsAt(Pos(Jumbo,position),0) &
HoldsAt(Pos(Homer,position),1) &
!HoldsAt(Pos(Jumbo,position),1).
[animal,time] !Happens(ThrowOff(animal,Homer),time).

[human] HoldsAt(PosDeterminingFluent(human,1),1).
[event,animal] !HoldsAt(DoneBy(event,animal),1).

;HoldsAt(Opened(GateAO),0).
;HoldsAt(Pos(Homer,3),0).
;HoldsAt(Pos(Jumbo,2),0).
;HoldsAt(Pos(Silver,7),0).
;Happens(Move(Jumbo,4),0).
;Happens(Move(Silver,8),0).
;Happens(Mount(Homer,Jumbo),0).

range time 0 1
range position 1 8
range offset 0 0

; End of file.
