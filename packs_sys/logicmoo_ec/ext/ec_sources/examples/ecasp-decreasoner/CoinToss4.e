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
; @incollection{Shanahan:1999,
;   author = "Shanahan, Murray",
;   year = "1999",
;   title = "The Event Calculus explained",
;   editor = "Michael J. Wooldridge and Manuela M. Veloso",
;   booktitle = "Artificial Intelligence Today: Recent Trends and Developments",
;   series = "Lecture Notes in Computer Science",
;   volume = "1600",
;   pages = "409--430",
;   address = "Berlin",
;   publisher = "Springer",
; }
;
; model finding
;
; modifications from Shanahan's formulation:
; InitiallyP -> HoldsAt
; pruning of models irrelevant to example
; timestamps
;

load foundations/Root.e
load foundations/EC.e

event Toss()
fluent ItsHeads()
fluent Heads()
noninertial ItsHeads

[time] HoldsAt(ItsHeads(),time) -> Initiates(Toss(),Heads(),time).
[time] !HoldsAt(ItsHeads(),time) -> Terminates(Toss(),Heads(),time).

HoldsAt(Heads(),0).
Happens(Toss(),1).
Happens(Toss(),2).
Happens(Toss(),3).

; prune models irrelevant to example:
HoldsAt(ItsHeads(),0).
HoldsAt(ItsHeads(),4).

completion Happens

range time 0 4
range offset 1 1

; End of file.
