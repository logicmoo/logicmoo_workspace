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

sort object
sort agent: object
sort physobj: object
sort location

fluent At(object,location)
fluent Holding(agent,physobj)
event PickUp(agent,physobj)
event SetDown(agent,physobj)
event Move(agent,location,location)

; state constraints

[agent,location,physobj,time]
HoldsAt(At(agent,location),time) &
HoldsAt(Holding(agent,physobj),time) ->
HoldsAt(At(physobj,location),time).

[object,location1,location2,time]
HoldsAt(At(object,location1),time) &
HoldsAt(At(object,location2),time) ->
location1=location2.

; effect axioms

[agent,location1,location2,time]
Initiates(Move(agent,location1,location2),At(agent,location2),time).

[agent,location1,location2,time]
Terminates(Move(agent,location1,location2),At(agent,location1),time).

[agent,physobj,time]
Initiates(PickUp(agent,physobj),Holding(agent,physobj),time).

[agent,physobj,time]
Terminates(SetDown(agent,physobj),Holding(agent,physobj),time).

; preconditions

[agent,location1,location2,time]
Happens(Move(agent,location1,location2),time) ->
HoldsAt(At(agent,location1),time).

[agent,physobj,time]
Happens(PickUp(agent,physobj),time) ->
{location} HoldsAt(At(agent,location),time) &
           HoldsAt(At(physobj,location),time).

; releases

[agent,physobj,location,time]
Releases(PickUp(agent,physobj),At(physobj,location),time).

[agent,physobj,location,time]
HoldsAt(At(agent,location),time) ->
Initiates(SetDown(agent,physobj),At(physobj,location),time).

;[agent,physobj,location1,location2,time]
;HoldsAt(At(agent,location1),time) &
;location1 != location2 ->
;Terminates(SetDown(agent,physobj),At(physobj,location2),time).

agent James
physobj Coin
location L1, L2, L3, L4

!HoldsAt(Holding(James,Coin),0).
HoldsAt(At(Coin,L4),0).
HoldsAt(At(James,L1),0).
Happens(Move(James,L1,L2),0).
Happens(Move(James,L2,L3),1).
Happens(Move(James,L3,L4),2).
Happens(PickUp(James,Coin),3).
Happens(Move(James,L4,L3),4).
Happens(Move(James,L3,L2),5).
Happens(SetDown(James,Coin),6).
Happens(Move(James,L2,L3),7).
Happens(Move(James,L3,L4),8).

completion Happens

range time 0 9
range offset 1 1

; End of file.
