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
; due to Raymond Reiter
;
; @inproceedings{KarthaLifschitz:1994,
;   author = "G. Neelakantan Kartha and Vladimir Lifschitz",
;   year = "1994",
;   title = "Actions with indirect effects (preliminary report)",
;   editor = "Jon Doyle and Erik Sandewall and Pietro Torasso",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{F}ourth \uppercase{I}nternational \uppercase{C}onference on \uppercase{P}rinciples of \uppercase{K}nowledge \uppercase{R}epresentation and \uppercase{R}easoning",
;   pages = "341--350",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann",
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
; InitiallyN -> !HoldsAt
; pruning of models irrelevant to example
; timestamps
;

load foundations/Root.e
load foundations/EC.e

event Throw()
fluent ItsBlack()
fluent ItsWhite()
fluent OnBlack()
fluent OnWhite()
noninertial ItsBlack, ItsWhite

[time]
HoldsAt(ItsWhite(),time) ->
Initiates(Throw(),OnWhite(),time).

[time]
HoldsAt(ItsBlack(),time) ->
Initiates(Throw(),OnBlack(),time).

[time] HoldsAt(ItsWhite(),time) | HoldsAt(ItsBlack(),time).

!HoldsAt(OnWhite(),0).
!HoldsAt(OnBlack(),0).
Happens(Throw(),1).

; prune models irrelevant to example:
HoldsAt(ItsWhite(),0).
HoldsAt(ItsBlack(),0).
HoldsAt(ItsWhite(),2).
HoldsAt(ItsBlack(),2).

completion Happens

range time 0 2
range offset 1 1

; End of file.
