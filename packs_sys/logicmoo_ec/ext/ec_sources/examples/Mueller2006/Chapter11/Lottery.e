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
; @book{OrtonyCloreCollins:1988,
;   author = "Andrew Ortony and Gerald L. Clore and Allan M. Collins",
;   year = "1988",
;   title = "The Cognitive Structure of Emotions",
;   address = "Cambridge",
;   publisher = "Cambridge University Press",
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

option modeldiff on

load foundations/Root.e
load foundations/EC.e

sort agent
sort aboutevent
sort desirability: integer

agent Kate, Lisa
aboutevent WinLotteryKate, WinLotteryLisa

fluent Joy(agent,aboutevent)
fluent Desirability(agent,agent,aboutevent,desirability)
fluent Believe(agent,aboutevent)
fluent Like(agent,agent)
fluent HappyFor(agent,agent,aboutevent)

event WinLottery(agent)
event AddJoy(agent,aboutevent)
event AddHappyFor(agent,agent,aboutevent)

; Sigma

[agent,aboutevent,time]
Initiates(AddJoy(agent,aboutevent),Joy(agent,aboutevent),time).

[agent1,agent2,aboutevent,time]
Initiates(AddHappyFor(agent1,agent2,aboutevent),
          HappyFor(agent1,agent2,aboutevent),
          time).

[agent1,agent2,aboutevent,time]
(agent1=Kate & aboutevent=WinLotteryKate) |
(agent1=Lisa & aboutevent=WinLotteryLisa) ->
Initiates(WinLottery(agent1),Believe(agent2,aboutevent),time).

; Delta

[agent,aboutevent,desirability,time]
!HoldsAt(Joy(agent,aboutevent),time) &
HoldsAt(Desirability(agent,agent,aboutevent,desirability),time) &
desirability=1 &
HoldsAt(Believe(agent,aboutevent),time) ->
Happens(AddJoy(agent,aboutevent),time).

[agent1,agent2,aboutevent,desirability1,desirability2,time]
!HoldsAt(HappyFor(agent1,agent2,aboutevent),time) &
HoldsAt(Desirability(agent1,agent2,aboutevent,desirability1),time) &
desirability1=1 &
HoldsAt(Desirability(agent1,agent1,aboutevent,desirability2),time) &
desirability2=1 &
HoldsAt(Like(agent1,agent2),time) &
HoldsAt(Believe(agent1,aboutevent),time) &
agent1 != agent2 ->
Happens(AddHappyFor(agent1,agent2,aboutevent),time).

Happens(WinLottery(Kate),0).

; Psi

[agent1,agent2,aboutevent,desirability1,desirability2,time]
HoldsAt(Desirability(agent1,agent2,aboutevent,desirability1),time) &
HoldsAt(Desirability(agent1,agent2,aboutevent,desirability2),time) ->
desirability1 = desirability2.

; Gamma

[agent,aboutevent] !HoldsAt(Joy(agent,aboutevent),0).
[agent1,agent2,aboutevent] !HoldsAt(HappyFor(agent1,agent2,aboutevent),0).
[aboutevent] !HoldsAt(Believe(Kate,aboutevent),0).
[aboutevent] !HoldsAt(Believe(Lisa,aboutevent),0).
[agent1,agent2,time] HoldsAt(Like(agent1,agent2),time).

[time] HoldsAt(Desirability(Lisa,Kate,WinLotteryKate,1),time).
[time] HoldsAt(Desirability(Kate,Kate,WinLotteryKate,1),time).
[time] HoldsAt(Desirability(Lisa,Lisa,WinLotteryKate,1),time).
[time] HoldsAt(Desirability(Kate,Kate,WinLotteryLisa,0),time).
[time] HoldsAt(Desirability(Kate,Lisa,WinLotteryLisa,0),time).
[time] HoldsAt(Desirability(Kate,Kate,WinLotteryLisa,0),time).
[time] HoldsAt(Desirability(Kate,Lisa,WinLotteryKate,0),time).
[time] HoldsAt(Desirability(Lisa,Lisa,WinLotteryLisa,0),time).
[time] HoldsAt(Desirability(Lisa,Kate,WinLotteryLisa,1),time).

completion Happens

range time 0 3
range desirability -1 1
range offset 1 1

; End of file.
