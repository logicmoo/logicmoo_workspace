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
; @inproceedings{WinikoffEtAl:2002,
;   author = "Michael Winikoff and Lin Padgham and James Harland and John Thangarajah",
;   year = "2002",
;   title = "Declarative \& procedural goals in intelligent agent systems",
;   editor = "Dieter Fensel and Fausto Giunchiglia and Deborah McGuinness and Mary-Anne Williams",
;   booktitle = "\uppercase{P}roceedings of the \uppercase{E}ighth \uppercase{I}nternational \uppercase{C}onference on \uppercase{P}rinciples of \uppercase{K}nowledge \uppercase{R}epresentation and \uppercase{R}easoning",
;   pages = "470--481",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann",
; }
;
; @book{Mueller:2006,
;   author = "Erik T. Mueller",
;   year = "2006",
;   title = "Commonsense Reasoning",
;   address = "San Francisco",
;   publisher = "Morgan Kaufmann/Elsevier",
; }
;

load foundations/Root.e
load foundations/EC.e

sort object
sort agent: object
sort food: object
sort surface
sort plan

reified sort belief

agent Cat
surface Floor, Chair, Shelf, Table
food Food1, Food2
plan P1, P1a, P1b, P2, P2a

predicate SelectedPlan(agent,belief,plan,time)
predicate SoundPlan(agent,belief,plan,time)

fluent On(object,surface)
fluent Goal(agent,belief)
fluent CanJump(surface,surface)
fluent Plan(agent,belief,plan)
fluent Satiated(agent)
fluent Believe(agent,belief)

event AddPlan(agent,belief,plan)
event DropPlan(agent,belief,plan)
event Jump(agent,surface,surface)
event Move(surface,surface,surface)
event Eat(agent,food)
event Wait(agent)

belief BSatiated(agent)
belief BCanJump(surface,surface)
belief BOn(object,surface)

; Sigma

; A5
[agent,belief,plan,time]
Initiates(AddPlan(agent,belief,plan),Plan(agent,belief,plan),time).

; A6
[agent,belief,plan,time]
Terminates(DropPlan(agent,belief,plan),Plan(agent,belief,plan),time).

[agent,surface1,surface2,time]
HoldsAt(On(agent,surface1),time) &
HoldsAt(CanJump(surface1,surface2),time) ->
Initiates(Jump(agent,surface1,surface2),On(agent,surface2),time).

[agent,surface1,surface2,time]
HoldsAt(On(agent,surface1),time) &
HoldsAt(CanJump(surface1,surface2),time) ->
Terminates(Jump(agent,surface1,surface2),On(agent,surface1),time).

[surface1,surface2,surface3,time]
Initiates(Move(surface1,surface2,surface3),CanJump(surface1,surface3),time).

[surface1,surface2,surface3,time]
Terminates(Move(surface1,surface2,surface3),CanJump(surface1,surface2),time).

[agent,food,surface,time]
HoldsAt(On(agent,surface),time) &
HoldsAt(On(food,surface),time) ->
Initiates(Eat(agent,food),Satiated(agent),time).

[agent,food,surface,time]
HoldsAt(On(agent,surface),time) &
HoldsAt(On(food,surface),time) ->
Terminates(Eat(agent,food),On(food,surface),time).

[agent,surface1,surface2,belief,time]
HoldsAt(Believe(agent,BOn(agent,surface1)),time) &
HoldsAt(Believe(agent,BCanJump(surface1,surface2)),time) &
(belief = BOn(agent,surface2)) ->
Initiates(Jump(agent,surface1,surface2),
          Believe(agent,belief),
          time).

[agent,surface1,surface2,belief,time]
HoldsAt(Believe(agent,BOn(agent,surface1)),time) &
HoldsAt(Believe(agent,BCanJump(surface1,surface2)),time) &
(belief = BOn(agent,surface1)) ->
Terminates(Jump(agent,surface1,surface2),
           Believe(agent,belief),
           time).

[agent,surface1,surface2,surface3,belief,time]
(belief = BCanJump(surface1,surface3)) ->
Initiates(Move(surface1,surface2,surface3),
          Believe(agent,belief),
          time).

[agent,surface1,surface2,surface3,belief,time]
(belief = BCanJump(surface1,surface2)) ->
Terminates(Move(surface1,surface2,surface3),
           Believe(agent,belief),
           time).

[agent,food,surface,belief,time]
HoldsAt(Believe(agent,BOn(agent,surface)),time) &
HoldsAt(Believe(agent,BOn(food,surface)),time) &
(belief = BSatiated(agent)) ->
Initiates(Eat(agent,food),Believe(agent,belief),time).

[agent,food,surface,belief,time]
HoldsAt(Believe(agent,BOn(agent,surface)),time) &
HoldsAt(Believe(agent,BOn(food,surface)),time) &
(belief = BOn(food,surface)) ->
Terminates(Eat(agent,food),Believe(agent,belief),time).

; Delta

; A7
[agent,belief,plan,time]
HoldsAt(Goal(agent,belief),time) &
!HoldsAt(Believe(agent,belief),time) &
SelectedPlan(agent,belief,plan,time) &
(!{plan1} HoldsAt(Plan(agent,belief,plan1),time)) ->
Happens(AddPlan(agent,belief,plan),time).

; A8
[agent,belief,time]
HoldsAt(Plan(agent,belief,P1),time) &
!HoldsAt(Believe(agent,belief),time) &
SoundPlan(agent,belief,P1,time) ->
Happens(Jump(Cat,Floor,Chair),time).

[agent,belief,time]
HoldsAt(Plan(agent,belief,P1a),time) &
!HoldsAt(Believe(agent,belief),time) &
SoundPlan(agent,belief,P1a,time) ->
Happens(Wait(Cat),time).

[agent,belief,time]
HoldsAt(Plan(agent,belief,P2),time) &
!HoldsAt(Believe(agent,belief),time) &
SoundPlan(agent,belief,P2,time) ->
Happens(Jump(Cat,Chair,Shelf),time).

; A9
[agent,belief,plan,time]
HoldsAt(Plan(agent,belief,plan),time) ->
Happens(DropPlan(agent,belief,plan),time).

; A10
[agent,belief,time]
HoldsAt(Plan(agent,belief,P1),time) &
!HoldsAt(Believe(agent,belief),time) &
SoundPlan(agent,belief,P1,time) ->
Happens(AddPlan(agent,belief,P1a),time).

[agent,belief,time]
HoldsAt(Plan(agent,belief,P1a),time) &
!HoldsAt(Believe(agent,belief),time) &
SoundPlan(agent,belief,P1a,time) ->
Happens(AddPlan(agent,belief,P1b),time).

[agent,belief,time]
HoldsAt(Plan(agent,belief,P2),time) &
!HoldsAt(Believe(agent,belief),time) &
SoundPlan(agent,belief,P2,time) ->
Happens(AddPlan(agent,belief,P2a),time).

; reactive behavior
[agent,food,surface,time]
!HoldsAt(Satiated(agent),time) &
HoldsAt(On(agent,surface),time) &
HoldsAt(On(food,surface),time) ->
Happens(Eat(agent,food),time).

; narrative

Happens(Move(Chair,Table,Shelf),2).

; SelectedPlan - plan library

;[agent,belief,plan,time]
;SelectedPlan(agent,belief,plan,time) <->
;(agent=Cat & belief=BSatiated(Cat) & plan=P1 & time=0) |
;(agent=Cat & belief=BSatiated(Cat) & plan=P2 & time=4).

[agent,belief,plan,time]
SelectedPlan(agent,belief,plan,time) <->
({surface1,surface2,surface3,food}
 HoldsAt(Believe(agent,BOn(agent,surface1)),time) &
 HoldsAt(Believe(agent,BCanJump(surface1,surface2)),time) &
 HoldsAt(Believe(agent,BCanJump(surface2,surface3)),time) &
 HoldsAt(Believe(agent,BOn(food,surface3)),time) &
 belief=BSatiated(agent) &
 plan=P1 &
 time=0) |
({surface1,surface2,surface3,food}
 HoldsAt(Believe(agent,BOn(agent,surface1)),time) &
 HoldsAt(Believe(agent,BCanJump(surface1,surface2)),time) &
 HoldsAt(Believe(agent,BCanJump(surface2,surface3)),time) &
 HoldsAt(Believe(agent,BOn(food,surface3)),time) &
 belief=BSatiated(agent) &
 plan=P2 &
 time=4).


; SoundPlan

[agent,belief,plan,time]
SoundPlan(agent,belief,plan,time) <->
(plan=P1 ->
 HoldsAt(Believe(agent,BCanJump(Floor,Chair)),time) &
 HoldsAt(Believe(agent,BCanJump(Chair,Table)),time)) &
((plan=P1a | plan=P1b) ->
  HoldsAt(Believe(agent,BCanJump(Chair,Table)),time)).

; Gamma

[agent,belief]
HoldsAt(Goal(agent,belief),0) <->
(agent=Cat & belief=BSatiated(Cat)).

[agent,belief,plan] !HoldsAt(Plan(agent,belief,plan),0).

[object,surface] HoldsAt(On(object,surface),0) <->
(object=Cat & surface=Floor) |
(object=Food1 & surface=Table) |
(object=Food2 & surface=Shelf).

[surface1,surface2] HoldsAt(CanJump(surface1,surface2),0) <->
(surface1=Floor & surface2=Chair) |
(surface1=Chair & surface2=Table) |
(surface1=Shelf & surface2=Table).

[agent,object,surface]
HoldsAt(Believe(agent,BOn(object,surface)),0) <->
(agent=Cat & object=Cat & surface=Floor) |
(agent=Cat & object=Food1 & surface=Table).

[agent,surface1,surface2]
HoldsAt(Believe(agent,BCanJump(surface1,surface2)),0) <->
(agent=Cat & surface1=Floor & surface2=Chair) |
(agent=Cat & surface1=Chair & surface2=Table) |
(agent=Cat & surface1=Shelf & surface2=Table).

!HoldsAt(Believe(Cat,BSatiated(Cat)),0).

; ADDED:
!HoldsAt(Satiated(Cat),0).

completion Happens

range time 0 7
range offset 1 1

; End of file.
