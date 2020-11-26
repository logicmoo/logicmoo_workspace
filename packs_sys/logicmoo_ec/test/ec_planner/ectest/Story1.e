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
; @article{FrankEtAl:2003,
;   author = "Stefan L. Frank and Mathieu Koppen and Leo G. M. Noordman and Wietske Vonk",
;   year = "2003",
;   title = "Modeling knowledge-based inferences in story comprehension",
;   journal = "Cognitive Science",
;   volume = "27",
;   pages = "875--910",
; }
;

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort agent

include examples/FrankEtAl2003/FrankEtAl.e

agent Bob, Jilly

!HoldsAt(Raining(),0).
!HoldsAt(SunShining(),0).


;; must play same game
(HoldsAt(PlaySoccer(Bob),1) & HoldsAt(PlaySoccer(Jilly),1)) |
(HoldsAt(PlayHideAndSeek(Bob),1) & HoldsAt(PlayHideAndSeek(Jilly),1)) |
(HoldsAt(PlayComputerGame(Bob),1) & HoldsAt(PlayComputerGame(Jilly),1)).

;; one wins
HoldsAt(Win(Bob),1) | HoldsAt(Win(Jilly),1).

range time 0 1
range offset 0 0

; End of file.
