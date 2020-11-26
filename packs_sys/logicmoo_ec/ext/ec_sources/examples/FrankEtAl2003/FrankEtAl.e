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

fluent SunShining()
fluent Raining()
fluent Outside(agent)
fluent PlaySoccer(agent)
fluent PlayHideAndSeek(agent)
fluent PlayComputerGame(agent)
fluent PlayWithDog(agent)
fluent Win(agent)

noninertial Outside, PlaySoccer, PlayHideAndSeek, PlayComputerGame
noninertial PlayWithDog, Win

xor PlaySoccer, PlayHideAndSeek, PlayComputerGame, PlayWithDog

[agent,time]
HoldsAt(PlaySoccer(agent),time) ->
HoldsAt(Outside(agent),time).

[agent,time]
HoldsAt(PlaySoccer(agent),time) ->
({agent1} agent1!=agent & HoldsAt(PlaySoccer(agent1),time)).

[agent,time]
HoldsAt(PlayHideAndSeek(agent),time) ->
({agent1} agent1!=agent & HoldsAt(PlayHideAndSeek(agent1),time)).

[agent,time]
HoldsAt(PlayComputerGame(agent),time) ->
!HoldsAt(Outside(agent),time).

[agent,time]
HoldsAt(Win(agent),time) ->
(HoldsAt(PlaySoccer(agent),time) |
 HoldsAt(PlayHideAndSeek(agent),time) |
 (HoldsAt(PlayComputerGame(agent),time) &
  ({agent1} agent1!=agent & HoldsAt(PlayComputerGame(agent1),time)))).

[agent,time]
HoldsAt(PlaySoccer(agent),time) &
HoldsAt(Win(agent),time) ->
!HoldsAt(PlaySoccer(agent),time+1).

[agent,time]
HoldsAt(PlayHideAndSeek(agent),time) &
HoldsAt(Win(agent),time) ->
!HoldsAt(PlayHideAndSeek(agent),time+1).

[agent,time]
HoldsAt(PlayComputerGame(agent),time) &
HoldsAt(Win(agent),time) ->
!HoldsAt(PlayComputerGame(agent),time+1).

[agent,time]
HoldsAt(Win(agent),time) ->
HoldsAt(PlaySoccer(agent),time-1) |
HoldsAt(PlayHideAndSeek(agent),time-1) |
HoldsAt(PlayComputerGame(agent),time-1).

[agent,time]
HoldsAt(PlaySoccer(agent),time) ->
!HoldsAt(Raining(),time).

[agent,time]
HoldsAt(Win(agent),time) ->
!({agent1} agent1!=agent & HoldsAt(Win(agent1),time)).

[agent1,agent2,time]
HoldsAt(PlayHideAndSeek(agent1),time) &
HoldsAt(PlayHideAndSeek(agent2),time) ->
((HoldsAt(Outside(agent1),time) & HoldsAt(Outside(agent2),time)) |
 (!HoldsAt(Outside(agent1),time) & !HoldsAt(Outside(agent2),time))).

; End of file.
