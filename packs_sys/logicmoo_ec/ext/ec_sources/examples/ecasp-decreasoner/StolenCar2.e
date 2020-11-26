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
; @inproceedings{Kautz:1986,
;   author = "Henry A. Kautz",
;   year = "1986",
;   title = "The Logic of Persistence",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{F}ifth \uppercase{N}ational \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
;   pages = "401--405",
;   address = "Los Altos, CA",
;   publisher = "Morgan Kaufmann",
; }
;
; \fullciteA[p. 359]{Shanahan:1997}
;
; @book{Shanahan:1997,
;   author = "Murray Shanahan",
;   year = "1997",
;   title = "Solving the Frame Problem",
;   address = "Cambridge, MA",
;   publisher = "MIT Press",
; }
;
; abduction
;
; modifications from Shanahan's formulation:
; timestamps
; added !HoldsAt(CarParked(),0).
;

load foundations/Root.e
load foundations/EC.e

event Park()
event Steal()
fluent CarParked()

[time] Initiates(Park(),CarParked(),time).
[time] Terminates(Steal(),CarParked(),time).

!HoldsAt(CarParked(),0).
Happens(Park(),0).
; ABDUCED Happens(Steal(), 1).
!HoldsAt(CarParked(),2).

range time 0 2
range offset 1 1

; End of file.
