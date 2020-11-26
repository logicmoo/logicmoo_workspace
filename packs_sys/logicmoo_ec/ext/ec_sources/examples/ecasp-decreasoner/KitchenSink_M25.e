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
; @inproceedings{Shanahan:1990,
;   author = "Murray Shanahan",
;   year = "1990",
;   title = "Representing continuous change in the event calculus",
;   editor = "Luigia Carlucci Aiello",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{N}inth \uppercase{E}uropean \uppercase{C}onference on \uppercase{A}rtificial \uppercase{I}ntelligence",
;   pages = "598--603",
;   address = "London",
;   publisher = "Pitman",
; }
;
; \fullciteA[pp. 326--329]{Shanahan:1997}
;
; @book{Shanahan:1997,
;   author = "Murray Shanahan",
;   year = "1997",
;   title = "Solving the Frame Problem",
;   address = "Cambridge, MA",
;   publisher = "MIT Press",
; }
;
; deduction
;
; modifications from Shanahan's formulation:
; InitiallyP -> HoldsAt
; timestamps
; generalized (SkF6)
;

option trajectory on

load foundations/Root.e
load foundations/EC.e

sort height: integer

event TapOn()
event TapOff()
event Overflow()
fluent Filling()
fluent Height(height)
fluent Spilling()

[time] Initiates(TapOn(),Filling(),time).

[time] Terminates(TapOff(),Filling(),time).

; should probably add:
;[time] Terminates(TapOff(),Spilling(),time).

[height,time] Releases(TapOn(),Height(height),time).

[height,time]
HoldsAt(Height(height),time) ->
Initiates(TapOff(),Height(height),time).

[time] Terminates(Overflow(),Filling(),time).

[height,time]
HoldsAt(Height(height),time) ->
Initiates(Overflow(),Height(height),time).

[time] Initiates(Overflow(),Spilling(),time).

[height1,height2,offset,time]
HoldsAt(Height(height1),time) &
height2 = height1 + offset ->
Trajectory(Filling(),time,Height(height2),offset).

[height1,height2,time]
HoldsAt(Height(height1),time) &
HoldsAt(Height(height2),time) ->
height1 = height2.

[time] HoldsAt(Height(15),time) & HoldsAt(Filling(),time) ->
Happens(Overflow(),time).

HoldsAt(Height(0),0).
!HoldsAt(Filling(),0).
!HoldsAt(Spilling(),0).
Happens(TapOn(),5).

completion Happens

range time 0 25
range height 0 15
range offset 1 15

; End of file.
