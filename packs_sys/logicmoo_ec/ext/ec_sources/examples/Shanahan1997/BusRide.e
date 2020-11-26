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
; @article{Kartha:1994,
;   author = "G. Neelakantan Kartha",
;   year = "1994",
;   title = "Two counterexamples related to \uppercase{B}aker's approach to the frame problem",
;   journal = "Artificial Intelligence",
;   volume = "69",
;   number = "1--2",
;   pages = "379--391",
; }
;
; \fullciteA[pp. 359--361]{Shanahan:1997}
;
; @book{Shanahan:1997,
;   author = "Murray Shanahan",
;   year = "1997",
;   title = "Solving the Frame Problem",
;   address = "Cambridge, MA",
;   publisher = "MIT Press",
; }
;
; modifications from Shanahan's formulation:
; InitiallyN -> !HoldsAt
; timestamps
;

load foundations/Root.e
load foundations/EC.e

fluent HasTicket()
fluent OnRed()
fluent OnYellow()
event Buy()
event Board()
event BoardRed()
event BoardYellow()

[time] Happens(Board(),time) -> Happens(BoardRed(),time) | Happens(BoardYellow(),time).

[time] Initiates(Buy(),HasTicket(),time).
[time] HoldsAt(HasTicket(),time) -> Initiates(BoardRed(),OnRed(),time).
[time] HoldsAt(HasTicket(),time) -> Initiates(BoardYellow(),OnYellow(),time).

[time] !(HoldsAt(OnRed(),time) & HoldsAt(OnYellow(),time)).
[time] HoldsAt(OnRed(),time) -> HoldsAt(HasTicket(),time).
[time] HoldsAt(OnYellow(),time) -> HoldsAt(HasTicket(),time).

HoldsAt(OnRed(),2).

!HoldsAt(HasTicket(),0).
Happens(Buy(),0).
Happens(Board(),1).
; ABDUCED Happens(BoardRed(), 1).

completion Happens

range time 0 2
range offset 1 1

; End of file.
