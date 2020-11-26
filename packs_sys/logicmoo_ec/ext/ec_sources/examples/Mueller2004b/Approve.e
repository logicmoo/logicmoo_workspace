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
; example of concurrent events with cumulative or canceling effects
;
; @inproceedings{Mueller:2004b,
;   author = "Erik T. Mueller",
;   year = "2004",
;   title = "A tool for satisfiability-based commonsense reasoning in the event calculus",
;   editor = "Valerie Barr and Zdravko Markov",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{S}eventeenth \uppercase{I}nternational \uppercase{F}lorida \uppercase{A}rtificial \uppercase{I}ntelligence \uppercase{R}esearch \uppercase{S}ociety \uppercase{C}onference",
;   pages = "147--152",
;   address = "Menlo Park, CA",
;   publisher = "AAAI Press",
; }
;

load foundations/Root.e
load foundations/EC.e

sort agent

event ApproveOf(agent,agent)
event DisapproveOf(agent,agent)
fluent Happy(agent)
fluent Confused(agent)

[agent1,agent2,time]
!Happens(DisapproveOf(agent1,agent2),time) ->
Initiates(ApproveOf(agent1,agent2),Happy(agent2),time).

[agent1,agent2,time]
!Happens(ApproveOf(agent1,agent2),time) ->
Terminates(DisapproveOf(agent1,agent2),Happy(agent2),time).

[agent1,agent2,time]
Happens(DisapproveOf(agent1,agent2),time) ->
Initiates(ApproveOf(agent1,agent2),Confused(agent2),time).

agent James, Peter

[agent] !HoldsAt(Happy(agent),0) & !HoldsAt(Confused(agent),0).

Happens(ApproveOf(Peter,James),0).
Happens(DisapproveOf(Peter,James),0).

completion Happens

range time 0 1
range offset 1 1

; End of file.
