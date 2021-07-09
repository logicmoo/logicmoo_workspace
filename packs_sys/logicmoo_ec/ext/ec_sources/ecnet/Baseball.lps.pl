% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Arson.e',87).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.lps.pl')).
% Fri, 26 Mar 2021 01:05:55 GMT File: <stream>(0x555567a68b00)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; @book{EthanAllen:1982,
%;   author = "Ethan Allen",
%;   year = "1982",
%;   title = "Baseball Play and Strategy",
%;   address = "Robert E. Krieger",
%;   publisher = "Malabar, FL",
%;   edition = "Third",
%; }
%;
%; @book{Coombs:1967,
%;   author = "Jack Coombs",
%;   year = "1967",
%;   title = "Baseball",
%;   address = "Englewood Cliffs, NJ",
%;   publisher = "Prentice-Hall",
%;   edition = "4th",
%;   howpublished = "revised by Danny Litwhiler",
%; }
%;

% sort ballgame
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',31).
% From E: 
% 
% sort(ballgame).
sort(ballgame).

% sort hardball: ball
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',33).
% From E: 
% 
% subsort(hardball,ball).
subsort(hardball, ball).

% sort base: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',35).
% From E: 
% 
% subsort(base,physobj).
subsort(base, physobj).

% sort firstbase: base
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',35).
% From E: 
% 
% subsort(firstbase,base).
subsort(firstbase, base).
%;sort secondbase: base
%;sort thirdbase: base

% sort homeplate: base
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',39).
% From E: 
% 
% subsort(homeplate,base).
subsort(homeplate, base).

% sort mound: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',41).
% From E: 
% 
% subsort(mound,physobj).
subsort(mound, physobj).

% sort pitchermound: mound
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',41).
% From E: 
% 
% subsort(pitchermound,mound).
subsort(pitchermound, mound).
%;sort furniture: physobj
%;sort bench: furniture
%;sort playerbench: bench

% sort field: physobj
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',48).
% From E: 
% 
% subsort(field,physobj).
subsort(field, physobj).
%;sort shortstoparea: field
%;sort catcherarea: field

% sort outfield: field
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',53).
% From E: 
% 
% subsort(outfield,field).
subsort(outfield, field).
%;sort leftfield: outfield
%;sort centerfield: outfield
%;sort rightfield: outfield

% function BallOf(ballgame): hardball
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',58).
% From E: 
% 
% function(
%    ballOf(ballgame), 
%    hardball).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',58).
function(ballOf(ballgame),hardball).

% function FirstBaseOf(ballgame): firstbase
% From E: 
% 
% function(
%    firstBaseOf(ballgame), 
%    firstbase).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',58).
function(firstBaseOf(ballgame),firstbase).
%;function SecondBaseOf(ballgame): secondbase
%;function ThirdBaseOf(ballgame): thirdbase

% function HomeplateOf(ballgame): homeplate
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',62).
% From E: 
% 
% function(
%    homeplateOf(ballgame), 
%    homeplate).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',62).
function(homeplateOf(ballgame),homeplate).

% function OutfieldOf(ballgame): outfield
% From E: 
% 
% function(
%    outfieldOf(ballgame), 
%    outfield).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',62).
function(outfieldOf(ballgame),outfield).

% function PitchermoundOf(ballgame): pitchermound
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',64).
% From E: 
% 
% function(
%    pitchermoundOf(ballgame), 
%    pitchermound).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',64).
function(pitchermoundOf(ballgame),pitchermound).

% function PlayerbenchOf(ballgame): playerbench
% From E: 
% 
% function(
%    playerbenchOf(ballgame), 
%    playerbench).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',64).
function(playerbenchOf(ballgame),playerbench).

% predicate HomeTeamPlayer(ballgame,agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',67).
% From E: 
% 
% predicate(homeTeamPlayer(ballgame,agent)).
mpred_prop(homeTeamPlayer(ballgame, agent), predicate).
predicates([homeTeamPlayer/2]).

% predicate VisitingTeamPlayer(ballgame,agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',67).
% From E: 
% 
% predicate(visitingTeamPlayer(ballgame,agent)).
mpred_prop(visitingTeamPlayer(ballgame, agent), predicate).
predicates([visitingTeamPlayer/2]).

% predicate Player(ballgame,agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',69).
% From E: 
% 
% predicate(player(ballgame,agent)).
mpred_prop(player(ballgame, agent), predicate).
predicates([player/2]).

% predicate OnOppositeTeams(ballgame,agent,agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',69).
% From E: 
% 
% predicate(onOppositeTeams(ballgame,agent,agent)).
mpred_prop(onOppositeTeams(ballgame, agent, agent), predicate).
predicates([onOppositeTeams/3]).

% event Pitch(ballgame,agent,hardball,agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',72).
% From E: 
% 
% event(pitch(ballgame, agent, hardball, 
%          agent)).
mpred_prop(pitch(ballgame, agent, hardball, agent), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',72).
events([pitch/4]).

% event PitchInStrikeZone(ballgame,agent,hardball,agent)
% From E: 
% 
% event(pitchInStrikeZone(ballgame, agent, hardball, 
%          agent)).
mpred_prop(pitchInStrikeZone(ballgame, agent, hardball, agent), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',72).
events([pitchInStrikeZone/4]).

% event PitchOutOfStrikeZone(ballgame,agent,hardball,agent)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',74).
% From E: 
% 
% event(pitchOutOfStrikeZone(ballgame, agent, hardball, 
%          agent)).
mpred_prop(pitchOutOfStrikeZone(ballgame, agent, hardball, agent), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',74).
events([pitchOutOfStrikeZone/4]).

% event Swing(ballgame,agent,hardball)
% From E: 
% 
% event(swing(ballgame,agent,hardball)).
mpred_prop(swing(ballgame, agent, hardball), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',74).
events([swing/3]).

% event SwingMiss(ballgame,agent,hardball)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',76).
% From E: 
% 
% event(swingMiss(ballgame,agent,hardball)).
mpred_prop(swingMiss(ballgame, agent, hardball), event).
events([swingMiss/3]).

% event SwingHit(ballgame,agent,hardball)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',76).
% From E: 
% 
% event(swingHit(ballgame,agent,hardball)).
mpred_prop(swingHit(ballgame, agent, hardball), event).
events([swingHit/3]).

% event SwingHitFair(ballgame,agent,hardball)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',78).
% From E: 
% 
% event(swingHitFair(ballgame,agent,hardball)).
mpred_prop(swingHitFair(ballgame, agent, hardball), event).
events([swingHitFair/3]).

% event SwingHitFoul(ballgame,agent,hardball)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',78).
% From E: 
% 
% event(swingHitFoul(ballgame,agent,hardball)).
mpred_prop(swingHitFoul(ballgame, agent, hardball), event).
events([swingHitFoul/3]).

% event SwingHitFairFly(ballgame,agent,hardball)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',80).
% From E: 
% 
% event(swingHitFairFly(ballgame,agent,hardball)).
mpred_prop(swingHitFairFly(ballgame, agent, hardball), event).
events([swingHitFairFly/3]).

% event SwingHitFairGround(ballgame,agent,hardball)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',80).
% From E: 
% 
% event(swingHitFairGround(ballgame,agent,hardball)).
mpred_prop(swingHitFairGround(ballgame, agent, hardball), event).
events([swingHitFairGround/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',83).
% [ballgame,agent]
% HomeTeamPlayer(ballgame,agent) ->
% !VisitingTeamPlayer(ballgame,agent).
% From E: 
% 
% '->'(
%    homeTeamPlayer(Ballgame,Agent), 
%    not(visitingTeamPlayer(Ballgame,Agent))).
visitingTeamPlayer(Ballgame, Agent)if not homeTeamPlayer(Ballgame, Agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',83).

 /*  if(visitingTeamPlayer(Ballgame,Agent),
        not(homeTeamPlayer(Ballgame,Agent))).
 */
 %  % =================================.


% [ballgame,agent]
 % HomeTeamPlayer(ballgame,agent) -> Player(ballgame,agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',86).
% From E: 
% 
% '->'(
%    homeTeamPlayer(Ballgame,Agent), 
%    player(Ballgame,Agent)).
(   player(Ballgame, Agent)
;   not homeTeamPlayer(Ballgame, Agent)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',86).

 /*   (   player(Ballgame, Agent)
        ;   not(homeTeamPlayer(Ballgame, Agent))
        ).
 */
 %  % =================================.


% [ballgame,agent]
 % VisitingTeamPlayer(ballgame,agent) -> Player(ballgame,agent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',88).
% From E: 
% 
% '->'(
%    visitingTeamPlayer(Ballgame,Agent), 
%    player(Ballgame,Agent)).
(   player(Ballgame, Agent)
;   not visitingTeamPlayer(Ballgame, Agent)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',88).

 /*   (   player(Ballgame, Agent)
        ;   not(visitingTeamPlayer(Ballgame, Agent))
        ).
 */
 %  % =================================.


% [ballgame,agent1,agent2]
% OnOppositeTeams(ballgame,agent1,agent2) <->
% (HomeTeamPlayer(ballgame,agent1) &
%  VisitingTeamPlayer(ballgame,agent2)) |
% (HomeTeamPlayer(ballgame,agent2) &
%  VisitingTeamPlayer(ballgame,agent1)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',92).
% From E: 
% 
% <->(
%    onOppositeTeams(Ballgame,Agent1,Agent2), 
%    ';'(
%       ','(
%          homeTeamPlayer(Ballgame,Agent1), 
%          visitingTeamPlayer(Ballgame,Agent2)), 
%       ','(
%          homeTeamPlayer(Ballgame,Agent2), 
%          visitingTeamPlayer(Ballgame,Agent1)))).
(   (   homeTeamPlayer(Ballgame, Agent1),
        visitingTeamPlayer(Ballgame, Agent2)
    ;   homeTeamPlayer(Ballgame, Agent2),
        visitingTeamPlayer(Ballgame, Agent1)
    )
;   not onOppositeTeams(Ballgame, Agent1, Agent2)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',92).

 /*  (   (   homeTeamPlayer(Ballgame, Agent1),
             visitingTeamPlayer(Ballgame, Agent2)
         ;   homeTeamPlayer(Ballgame, Agent2),
             visitingTeamPlayer(Ballgame, Agent1)
         )
     ;   not(onOppositeTeams(Ballgame, Agent1, Agent2))
     ).
 */
 %  % =================================.
(   onOppositeTeams(Ballgame, Agent1, Agent2)
;   (   not homeTeamPlayer(Ballgame, Agent1)
    ;   not visitingTeamPlayer(Ballgame, Agent2)
    ),
    (   not homeTeamPlayer(Ballgame, Agent2)
    ;   not visitingTeamPlayer(Ballgame, Agent1)
    )
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',92).

 /*  (   onOppositeTeams(Ballgame, Agent1, Agent2)
     ;   (   not(homeTeamPlayer(Ballgame, Agent1))
         ;   not(visitingTeamPlayer(Ballgame, Agent2))
         ),
         (   not(homeTeamPlayer(Ballgame, Agent2))
         ;   not(visitingTeamPlayer(Ballgame, Agent1))
         )
     ).
 */
 %  % =================================.


% [ballgame,agent1,hardball,agent2,pitchermound,homeplate,time]
% Happens(Pitch(ballgame,agent1,hardball,agent2),time) &
% PitchermoundOf(ballgame) = pitchermound &
% HomeplateOf(ballgame) = homeplate ->
% HoldsAt(Near(agent1,pitchermound),time) &
% HoldsAt(Near(agent2,homeplate),time) &
% OnOppositeTeams(ballgame,agent1,agent2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',99).
% From E: 
% 
% '->'(
%    ','(
%       happens(
%          pitch(Ballgame, Agent1, Hardball, 
%             Agent2), 
%          Time), 
%       ','(
%          '='(
%             pitchermoundOf(Ballgame), 
%             Pitchermound), 
%          '='(
%             homeplateOf(Ballgame), 
%             Homeplate))), 
%    ','(
%       holds(
%          near(Agent1,Pitchermound), 
%          Time), 
%       ','(
%          holds(
%             near(Agent2,Homeplate), 
%             Time), 
%          onOppositeTeams(Ballgame,Agent1,Agent2)))).
(   near(Agent1, Pitchermound)at Time,
    near(Agent2, Homeplate)at Time,
    onOppositeTeams(Ballgame, Agent1, Agent2)
;   not(happens(pitch(Ballgame, Agent1, Hardball, Agent2),
                Time))
;   not equals(pitchermoundOf(Ballgame), Pitchermound)
;   not equals(homeplateOf(Ballgame), Homeplate)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',99).

 /*  (   at(near(Agent1, Pitchermound), Time),
         at(near(Agent2, Homeplate), Time),
         onOppositeTeams(Ballgame, Agent1, Agent2)
     ;   not(happens(pitch(Ballgame, Agent1, Hardball, Agent2),
                     Time))
     ;   not(equals(pitchermoundOf(Ballgame), Pitchermound))
     ;   not(equals(homeplateOf(Ballgame), Homeplate))
     ).
 */
 %  % =================================.


% [ballgame,agent1,agent2,hardball,time]
% Happens(Pitch(ballgame,agent1,hardball,agent2),time) ->
% Happens(PitchInStrikeZone(ballgame,agent1,hardball,agent2),time) |
% Happens(PitchOutOfStrikeZone(ballgame,agent1,hardball,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',107).
% From E: 
% 
% '->'(
%    happens(
%       pitch(Ballgame, Agent1, Hardball, 
%          Agent2), 
%       Time), 
%    ';'(
%       happens(
%          pitchInStrikeZone(Ballgame, Agent1, Hardball, 
%             Agent2), 
%          Time), 
%       happens(
%          pitchOutOfStrikeZone(Ballgame, Agent1, Hardball, 
%             Agent2), 
%          Time))).
(   (   happens(pitchInStrikeZone(Ballgame,
                                  Agent1,
                                  Hardball,
                                  Agent2),
                Time)
    ;   happens(pitchOutOfStrikeZone(Ballgame,
                                     Agent1,
                                     Hardball,
                                     Agent2),
                Time)
    )
;   not(happens(pitch(Ballgame, Agent1, Hardball, Agent2),
                Time))
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',107).

 /*   (   (   happens(pitchInStrikeZone(Ballgame,
                                          Agent1,
                                          Hardball,
                                          Agent2),
                        Time)
            ;   happens(pitchOutOfStrikeZone(Ballgame,
                                             Agent1,
                                             Hardball,
                                             Agent2),
                        Time)
            )
        ;   not(happens(pitch(Ballgame,
                              Agent1,
                              Hardball,
                              Agent2),
                        Time))
        ).
 */
 %  % =================================.


% [ballgame,agent1,agent2,hardball,time]
% Happens(PitchInStrikeZone(ballgame,agent1,hardball,agent2),time) ->
% !Happens(PitchOutOfStrikeZone(ballgame,agent1,hardball,agent2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',112).
% From E: 
% 
% '->'(
%    happens(
%       pitchInStrikeZone(Ballgame, Agent1, Hardball, 
%          Agent2), 
%       Time), 
%    not(happens(
%           pitchOutOfStrikeZone(Ballgame, Agent1, Hardball, 
%              Agent2), 
%           Time))).
happens(pitchOutOfStrikeZone(Ballgame, Agent1, Hardball, Agent2), Time)if not happens(pitchInStrikeZone(Ballgame, Agent1, Hardball, Agent2), Time).

 /*  if(happens(pitchOutOfStrikeZone(Ballgame,
     				Agent1,
     				Hardball,
     				Agent2),
     	   Time),
        not(happens(pitchInStrikeZone(Ballgame,
     				 Agent1,
     				 Hardball,
     				 Agent2),
     	       Time))).
 */
 %  % =================================.


% [ballgame,agent1,agent2,hardball,time]
% Happens(PitchInStrikeZone(ballgame,agent1,hardball,agent2),time) ->
% Happens(Swing(ballgame,agent2,hardball),time+1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',116).
% From E: 
% 
% '->'(
%    happens(
%       pitchInStrikeZone(Ballgame, Agent1, Hardball, 
%          Agent2), 
%       Time), 
%    happens(
%       swing(Ballgame,Agent2,Hardball), 
%       Time+1)).
(   happens(swing(Ballgame, Agent2, Hardball), Time+1)
;   not(happens(pitchInStrikeZone(Ballgame,
                                  Agent1,
                                  Hardball,
                                  Agent2),
                Time))
).

 /*   (   happens(swing(Ballgame, Agent2, Hardball), Time+1)
        ;   not(happens(pitchInStrikeZone(Ballgame,
                                          Agent1,
                                          Hardball,
                                          Agent2),
                        Time))
        ).
 */
 %  % =================================.


% [ballgame,agent,hardball,time]
% Happens(Swing(ballgame,agent,hardball),time) ->
% Happens(SwingHit(ballgame,agent,hardball),time) |
% Happens(SwingMiss(ballgame,agent,hardball),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',120).
% From E: 
% 
% '->'(
%    happens(
%       swing(Ballgame,Agent,Hardball), 
%       Time), 
%    ';'(
%       happens(
%          swingHit(Ballgame,Agent,Hardball), 
%          Time), 
%       happens(
%          swingMiss(Ballgame,Agent,Hardball), 
%          Time))).
(   (   happens(swingHit(Ballgame, Agent, Hardball),
                Time)
    ;   happens(swingMiss(Ballgame, Agent, Hardball),
                Time)
    )
;   not happens(swing(Ballgame, Agent, Hardball), Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',120).

 /*   (   (   happens(swingHit(Ballgame, Agent, Hardball),
                        Time)
            ;   happens(swingMiss(Ballgame, Agent, Hardball),
                        Time)
            )
        ;   not(happens(swing(Ballgame, Agent, Hardball),
                        Time))
        ).
 */
 %  % =================================.


% [ballgame,agent,hardball,time]
% Happens(SwingHit(ballgame,agent,hardball),time) ->
% !Happens(SwingMiss(ballgame,agent,hardball),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',125).
% From E: 
% 
% '->'(
%    happens(
%       swingHit(Ballgame,Agent,Hardball), 
%       Time), 
%    not(happens(
%           swingMiss(Ballgame,Agent,Hardball), 
%           Time))).
happens(swingMiss(Ballgame, Agent, Hardball), Time)if not happens(swingHit(Ballgame, Agent, Hardball), Time).

 /*  if(happens(swingMiss(Ballgame,Agent,Hardball),
     	   Time),
        not(happens(swingHit(Ballgame,Agent,Hardball),
     	       Time))).
 */
 %  % =================================.


% [ballgame,agent,hardball,time]
% Happens(SwingHit(ballgame,agent,hardball),time) ->
% Happens(SwingHitFair(ballgame,agent,hardball),time) |
% Happens(SwingHitFoul(ballgame,agent,hardball),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',129).
% From E: 
% 
% '->'(
%    happens(
%       swingHit(Ballgame,Agent,Hardball), 
%       Time), 
%    ';'(
%       happens(
%          swingHitFair(Ballgame,Agent,Hardball), 
%          Time), 
%       happens(
%          swingHitFoul(Ballgame,Agent,Hardball), 
%          Time))).
(   (   happens(swingHitFair(Ballgame, Agent, Hardball),
                Time)
    ;   happens(swingHitFoul(Ballgame, Agent, Hardball),
                Time)
    )
;   not(happens(swingHit(Ballgame, Agent, Hardball),
                Time))
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',129).

 /*   (   (   happens(swingHitFair(Ballgame, Agent, Hardball),
                        Time)
            ;   happens(swingHitFoul(Ballgame, Agent, Hardball),
                        Time)
            )
        ;   not(happens(swingHit(Ballgame, Agent, Hardball),
                        Time))
        ).
 */
 %  % =================================.


% [ballgame,agent,hardball,time]
% Happens(SwingHitFair(ballgame,agent,hardball),time) ->
% !Happens(SwingHitFoul(ballgame,agent,hardball),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',134).
% From E: 
% 
% '->'(
%    happens(
%       swingHitFair(Ballgame,Agent,Hardball), 
%       Time), 
%    not(happens(
%           swingHitFoul(Ballgame,Agent,Hardball), 
%           Time))).
happens(swingHitFoul(Ballgame, Agent, Hardball), Time)if not happens(swingHitFair(Ballgame, Agent, Hardball), Time).

 /*  if(happens(swingHitFoul(Ballgame,Agent,Hardball),
     	   Time),
        not(happens(swingHitFair(Ballgame,Agent,Hardball),
     	       Time))).
 */
 %  % =================================.


% [ballgame,agent,hardball,time]
% Happens(SwingHitFair(ballgame,agent,hardball),time) ->
% Happens(SwingHitFairFly(ballgame,agent,hardball),time) |
% Happens(SwingHitFairGround(ballgame,agent,hardball),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',138).
% From E: 
% 
% '->'(
%    happens(
%       swingHitFair(Ballgame,Agent,Hardball), 
%       Time), 
%    ';'(
%       happens(
%          swingHitFairFly(Ballgame,Agent,Hardball), 
%          Time), 
%       happens(
%          swingHitFairGround(Ballgame,Agent,Hardball), 
%          Time))).
(   (   happens(swingHitFairFly(Ballgame, Agent, Hardball),
                Time)
    ;   happens(swingHitFairGround(Ballgame, Agent, Hardball),
                Time)
    )
;   not(happens(swingHitFair(Ballgame, Agent, Hardball),
                Time))
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',138).

 /*   (   (   happens(swingHitFairFly(Ballgame, Agent, Hardball),
                        Time)
            ;   happens(swingHitFairGround(Ballgame,
                                           Agent,
                                           Hardball),
                        Time)
            )
        ;   not(happens(swingHitFair(Ballgame, Agent, Hardball),
                        Time))
        ).
 */
 %  % =================================.


% [ballgame,agent,hardball,time]
% Happens(SwingHitFairFly(ballgame,agent,hardball),time) ->
% Happens(SwingHitFairGround(ballgame,agent,hardball),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',143).
% From E: 
% 
% '->'(
%    happens(
%       swingHitFairFly(Ballgame,Agent,Hardball), 
%       Time), 
%    happens(
%       swingHitFairGround(Ballgame,Agent,Hardball), 
%       Time)).
(   happens(swingHitFairGround(Ballgame, Agent, Hardball),
            Time)
;   not(happens(swingHitFairFly(Ballgame, Agent, Hardball),
                Time))
).

 /*   (   happens(swingHitFairGround(Ballgame, Agent, Hardball),
                    Time)
        ;   not(happens(swingHitFairFly(Ballgame, Agent, Hardball),
                        Time))
        ).
 */
 %  % =================================.


% [ballgame,agent,hardball,homeplate,firstbase,time]
% Happens(SwingHit(ballgame,agent,hardball),time) &
% HomeplateOf(ballgame) = homeplate &
% FirstBaseOf(ballgame) = firstbase ->
% Happens(RunFromTo(agent,homeplate,firstbase),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',147).
% From E: 
% 
% '->'(
%    ','(
%       happens(
%          swingHit(Ballgame,Agent,Hardball), 
%          Time), 
%       ','(
%          '='(
%             homeplateOf(Ballgame), 
%             Homeplate), 
%          '='(
%             firstBaseOf(Ballgame), 
%             Firstbase))), 
%    happens(
%       runFromTo(Agent,Homeplate,Firstbase), 
%       Time)).
(   happens(runFromTo(Agent, Homeplate, Firstbase), Time)
;   not(happens(swingHit(Ballgame, Agent, Hardball),
                Time))
;   not equals(homeplateOf(Ballgame), Homeplate)
;   not equals(firstBaseOf(Ballgame), Firstbase)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',147).

 /*   (   happens(runFromTo(Agent, Homeplate, Firstbase),
                    Time)
        ;   not(happens(swingHit(Ballgame, Agent, Hardball),
                        Time))
        ;   not(equals(homeplateOf(Ballgame), Homeplate))
        ;   not(equals(firstBaseOf(Ballgame), Firstbase))
        ).
 */
 %  % =================================.


% [ballgame,agent,hardball,homeplate,outfield,time]
% HomeplateOf(ballgame) = homeplate &
% OutfieldOf(ballgame) = outfield &
% Happens(SwingHitFairFly(ballgame,agent,hardball),time) ->
% Happens(HitFromTo(agent,hardball,homeplate,outfield),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',153).
% From E: 
% 
% '->'(
%    ','(
%       '='(
%          homeplateOf(Ballgame), 
%          Homeplate), 
%       ','(
%          '='(
%             outfieldOf(Ballgame), 
%             Outfield), 
%          happens(
%             swingHitFairFly(Ballgame,Agent,Hardball), 
%             Time))), 
%    happens(
%       hitFromTo(Agent, Hardball, Homeplate, 
%          Outfield), 
%       Time)).
(   happens(hitFromTo(Agent,
                      Hardball,
                      Homeplate,
                      Outfield),
            Time)
;   not equals(homeplateOf(Ballgame), Homeplate)
;   not equals(outfieldOf(Ballgame), Outfield)
;   not(happens(swingHitFairFly(Ballgame, Agent, Hardball),
                Time))
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',153).

 /*   (   happens(hitFromTo(Agent,
                              Hardball,
                              Homeplate,
                              Outfield),
                    Time)
        ;   not(equals(homeplateOf(Ballgame), Homeplate))
        ;   not(equals(outfieldOf(Ballgame), Outfield))
        ;   not(happens(swingHitFairFly(Ballgame, Agent, Hardball),
                        Time))
        ).
 */
 %  % =================================.


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.e',157).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Baseball.lps.pl')).
