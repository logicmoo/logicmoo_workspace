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
; @book{EthanAllen:1982,
;   author = "Ethan Allen",
;   year = "1982",
;   title = "Baseball Play and Strategy",
;   address = "Robert E. Krieger",
;   publisher = "Malabar, FL",
;   edition = "Third",
; }
;
; @book{Coombs:1967,
;   author = "Jack Coombs",
;   year = "1967",
;   title = "Baseball",
;   address = "Englewood Cliffs, NJ",
;   publisher = "Prentice-Hall",
;   edition = "4th",
;   howpublished = "revised by Danny Litwhiler",
; }
;

sort ballgame

sort hardball: ball

sort base: physobj
sort firstbase: base
;sort secondbase: base
;sort thirdbase: base
sort homeplate: base

sort mound: physobj
sort pitchermound: mound

;sort furniture: physobj
;sort bench: furniture
;sort playerbench: bench

sort field: physobj

;sort shortstoparea: field
;sort catcherarea: field

sort outfield: field
;sort leftfield: outfield
;sort centerfield: outfield
;sort rightfield: outfield

function BallOf(ballgame): hardball
function FirstBaseOf(ballgame): firstbase
;function SecondBaseOf(ballgame): secondbase
;function ThirdBaseOf(ballgame): thirdbase
function HomeplateOf(ballgame): homeplate
function OutfieldOf(ballgame): outfield
function PitchermoundOf(ballgame): pitchermound
function PlayerbenchOf(ballgame): playerbench

predicate HomeTeamPlayer(ballgame,agent)
predicate VisitingTeamPlayer(ballgame,agent)
predicate Player(ballgame,agent)
predicate OnOppositeTeams(ballgame,agent,agent)

event Pitch(ballgame,agent,hardball,agent)
event PitchInStrikeZone(ballgame,agent,hardball,agent)
event PitchOutOfStrikeZone(ballgame,agent,hardball,agent)
event Swing(ballgame,agent,hardball)
event SwingMiss(ballgame,agent,hardball)
event SwingHit(ballgame,agent,hardball)
event SwingHitFair(ballgame,agent,hardball)
event SwingHitFoul(ballgame,agent,hardball)
event SwingHitFairFly(ballgame,agent,hardball)
event SwingHitFairGround(ballgame,agent,hardball)

[ballgame,agent]
HomeTeamPlayer(ballgame,agent) ->
!VisitingTeamPlayer(ballgame,agent).

[ballgame,agent] HomeTeamPlayer(ballgame,agent) -> Player(ballgame,agent).

[ballgame,agent] VisitingTeamPlayer(ballgame,agent) -> Player(ballgame,agent).

[ballgame,agent1,agent2]
OnOppositeTeams(ballgame,agent1,agent2) <->
(HomeTeamPlayer(ballgame,agent1) &
 VisitingTeamPlayer(ballgame,agent2)) |
(HomeTeamPlayer(ballgame,agent2) &
 VisitingTeamPlayer(ballgame,agent1)).

[ballgame,agent1,hardball,agent2,pitchermound,homeplate,time]
Happens(Pitch(ballgame,agent1,hardball,agent2),time) &
PitchermoundOf(ballgame) = pitchermound &
HomeplateOf(ballgame) = homeplate ->
HoldsAt(Near(agent1,pitchermound),time) &
HoldsAt(Near(agent2,homeplate),time) &
OnOppositeTeams(ballgame,agent1,agent2).

[ballgame,agent1,agent2,hardball,time]
Happens(Pitch(ballgame,agent1,hardball,agent2),time) ->
Happens(PitchInStrikeZone(ballgame,agent1,hardball,agent2),time) |
Happens(PitchOutOfStrikeZone(ballgame,agent1,hardball,agent2),time).

[ballgame,agent1,agent2,hardball,time]
Happens(PitchInStrikeZone(ballgame,agent1,hardball,agent2),time) ->
!Happens(PitchOutOfStrikeZone(ballgame,agent1,hardball,agent2),time).

[ballgame,agent1,agent2,hardball,time]
Happens(PitchInStrikeZone(ballgame,agent1,hardball,agent2),time) ->
Happens(Swing(ballgame,agent2,hardball),time+1).

[ballgame,agent,hardball,time]
Happens(Swing(ballgame,agent,hardball),time) ->
Happens(SwingHit(ballgame,agent,hardball),time) |
Happens(SwingMiss(ballgame,agent,hardball),time).

[ballgame,agent,hardball,time]
Happens(SwingHit(ballgame,agent,hardball),time) ->
!Happens(SwingMiss(ballgame,agent,hardball),time).

[ballgame,agent,hardball,time]
Happens(SwingHit(ballgame,agent,hardball),time) ->
Happens(SwingHitFair(ballgame,agent,hardball),time) |
Happens(SwingHitFoul(ballgame,agent,hardball),time).

[ballgame,agent,hardball,time]
Happens(SwingHitFair(ballgame,agent,hardball),time) ->
!Happens(SwingHitFoul(ballgame,agent,hardball),time).

[ballgame,agent,hardball,time]
Happens(SwingHitFair(ballgame,agent,hardball),time) ->
Happens(SwingHitFairFly(ballgame,agent,hardball),time) |
Happens(SwingHitFairGround(ballgame,agent,hardball),time).

[ballgame,agent,hardball,time]
Happens(SwingHitFairFly(ballgame,agent,hardball),time) ->
Happens(SwingHitFairGround(ballgame,agent,hardball),time).

[ballgame,agent,hardball,homeplate,firstbase,time]
Happens(SwingHit(ballgame,agent,hardball),time) &
HomeplateOf(ballgame) = homeplate &
FirstBaseOf(ballgame) = firstbase ->
Happens(RunFromTo(agent,homeplate,firstbase),time).

[ballgame,agent,hardball,homeplate,outfield,time]
HomeplateOf(ballgame) = homeplate &
OutfieldOf(ballgame) = outfield &
Happens(SwingHitFairFly(ballgame,agent,hardball),time) ->
Happens(HitFromTo(agent,hardball,homeplate,outfield),time).

; End of file.
