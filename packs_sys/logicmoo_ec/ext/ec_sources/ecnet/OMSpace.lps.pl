% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Money.e',12).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.lps.pl')).
% Fri, 26 Mar 2021 01:06:02 GMT File: <stream>(0x555567d93f00)


%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; OMSpace: object-scale metric space
%;
%; The OMSpace representation deals with metric space at
%; the scale of objects.
%;
%; @article{Morgenstern:2001,
%;   author = "Morgenstern, Leora",
%;   year = "2001",
%;   title = "Mid-sized axiomatizations of commonsense problems: A case study in egg cracking",
%;   journal = "Studia Logica",
%;   volume = "67",
%;   pages = "333--384",
%; }
%;
%; @article{Shanahan:2003,
%;   author = "Shanahan, Murray",
%;   year = "2004",
%;   title = "An attempt to formalise a non-trivial benchmark problem in common sense reasoning",
%;   journal = "Artificial Intelligence",
%;   volume = "153",
%;   pages = "141--165",
%; }
%;

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',35).
% sort height: integer
% From E: 
% 
% subsort(height,integer).
subsort(height, integer).

% sort distance: integer
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',35).
% From E: 
% 
% subsort(distance,integer).
subsort(distance, integer).
%; Height
%; The height of object is height.

% fluent Height(object,height)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',40).
% From E: 
% 
% fluent(height(object,height)).
mpred_prop(height(object, height), fluent).
fluents([height/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',43).
%; State constraint represent the fact that each
%; object has a unique height:
% [object,height1,height2,time]
% HoldsAt(Height(object,height1),time) &
% HoldsAt(Height(object,height2),time) ->
% height1=height2.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',45).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          height(Object,Height1), 
%          Time), 
%       holds(
%          height(Object,Height2), 
%          Time)), 
%    Height1=Height2).
(   equals(Height1, Height2)
;   not height(Object, Height1)at Time
;   not height(Object, Height2)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',45).

 /*   (   equals(Height1, Height2)
        ;   at(not(height(Object, Height1)), Time)
        ;   at(not(height(Object, Height2)), Time)
        ).
 */
 %  % =================================.


% [object,time]
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',51).
% {height}% HoldsAt(Height(object,height),time).
% From E: 
% 
% exists(Height, 
%    holds(
%       height(Object,Height), 
%       Time)).
exists(Height, height(Object, Height)at Time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',51).

 /*  exists(Height,
      at(height(Object,Height),Time)).
 */
 %  % =================================.


%; falling
%; physobj1 is falling from physobj2 to physobj3.

% fluent FallingFromTo(physobj,physobj,physobj)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',56).
% From E: 
% 
% fluent(fallingFromTo(physobj,physobj,physobj)).
mpred_prop(fallingFromTo(physobj, physobj, physobj), fluent).
fluents([fallingFromTo/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',56).
%; physobj1 starts falling from physobj2 to physobj3.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',59).
% event StartFallingFromTo(physobj,physobj,physobj)
% From E: 
% 
% event(startFallingFromTo(physobj,physobj,physobj)).
mpred_prop(startFallingFromTo(physobj, physobj, physobj), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',59).
events([startFallingFromTo/3]).


%; physobj1 collides with physobj2.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',61).
% event CollideWith(physobj,physobj)
% From E: 
% 
% event(collideWith(physobj,physobj)).
mpred_prop(collideWith(physobj, physobj), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',61).
events([collideWith/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',63).
%; An effect axiom states that if a first physical object starts
%; falling from a second physical object to a third physical
%; object, the first physical object will be falling from the
%; second physical object to the third physical object:
% [physobj1,physobj2,physobj3,time]
% Initiates(StartFallingFromTo(physobj1,physobj2,physobj3),
%           FallingFromTo(physobj1,physobj2,physobj3),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',67).
% From E: 
% 
% initiates_at(
%    startFallingFromTo(Physobj1,Physobj2,Physobj3), 
%    fallingFromTo(Physobj1,Physobj2,Physobj3), 
%    Time).
startFallingFromTo(Physobj1, Physobj2, Physobj3)initiates fallingFromTo(Physobj1, Physobj2, Physobj3).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',67).

 /*  initiated(happens(startFallingFromTo(Physobj1,
     				     Physobj2,
     				     Physobj3),
     		  Time_from,
     		  Time_until),
     	  fallingFromTo(Physobj1,Physobj2,Physobj3),
     	  []).
 */
 %  % =================================.


%; A precondition axiom states that for
%; a first physical object to start
%; falling from a second physical object to a third physical
%; object,
%; the height of the first physical object and the
%; second physical object must be the same.
% [physobj1,physobj2,physobj3,height1,height2,time]
% Happens(StartFallingFromTo(physobj1,physobj2,physobj3),time) &
% HoldsAt(Height(physobj1,height1),time) &
% HoldsAt(Height(physobj2,height2),time) ->
% height1=height2.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',78).
% From E: 
% 
% '->'(
%    ','(
%       happens(
%          startFallingFromTo(Physobj1,Physobj2,Physobj3), 
%          Time), 
%       ','(
%          holds(
%             height(Physobj1,Height1), 
%             Time), 
%          holds(
%             height(Physobj2,Height2), 
%             Time))), 
%    Height1=Height2).
(   equals(Height1, Height2)
;   not(happens(startFallingFromTo(Physobj1,
                                   Physobj2,
                                   Physobj3),
                Time))
;   not height(Physobj1, Height1)at Time
;   not height(Physobj2, Height2)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',78).

 /*   (   equals(Height1, Height2)
        ;   not(happens(startFallingFromTo(Physobj1,
                                           Physobj2,
                                           Physobj3),
                        Time))
        ;   at(not(height(Physobj1, Height1)), Time)
        ;   at(not(height(Physobj2, Height2)), Time)
        ).
 */
 %  % =================================.


%; A state constraint says that a physical object
%; cannot fall from itself, cannot fall to itself,
%; and cannot fall from and to the same physical object:
% [physobj1,physobj2,physobj3,time]
% HoldsAt(FallingFromTo(physobj1,physobj2,physobj3),time) ->
% physobj1!=physobj2 &
% physobj1!=physobj3 &
% physobj2!=physobj3.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',87).
% From E: 
% 
% '->'(
%    holds(
%       fallingFromTo(Physobj1,Physobj2,Physobj3), 
%       Time), 
%    ','(
%       Physobj1\=Physobj2, 
%       ','(
%          Physobj1\=Physobj3, 
%          Physobj2\=Physobj3))).
(   { dif(Physobj1, Physobj2)
    },
    { dif(Physobj1, Physobj3)
    },
    { dif(Physobj2, Physobj3)
    }
;   at(not fallingFromTo(Physobj1, Physobj2, Physobj3),
       Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',87).

 /*  (   { dif(Physobj1, Physobj2)
         },
         { dif(Physobj1, Physobj3)
         },
         { dif(Physobj2, Physobj3)
         }
     ;   at(not(fallingFromTo(Physobj1, Physobj2, Physobj3)),
            Time)
     ).
 */
 %  % =================================.


%; A state constraint says that the sky cannot fall:
% [sky,physobj1,physobj2,time]
% !HoldsAt(FallingFromTo(sky,physobj1,physobj2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',94).
% From E: 
% 
% holds(
%    not(fallingFromTo(Sky,Physobj1,Physobj2)), 
%    Time).
not fallingFromTo(Sky, Physobj1, Physobj2)at Time.

 /*  l_int(holds(not(fallingFromTo(Sky,Physobj1,Physobj2)),
     	    Time),
           []).
 */
 %  % =================================.


%; A releases axiom states that if
%; if a first physical object starts
%; falling from a second physical object to a third physical
%; object, the height of the first physical object
%; will be released from inertia:
% [physobj1,physobj2,physobj3,height,time]
% Releases(StartFallingFromTo(physobj1,physobj2,physobj3),
%          Height(physobj1,height),
%          time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',102).
% From E: 
% 
% releases_at(
%    startFallingFromTo(Physobj1,Physobj2,Physobj3), 
%    height(Physobj1,Height), 
%    Time).
releases(startFallingFromTo(Physobj1, Physobj2, Physobj3), height(Physobj1, Height)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',102).

 /*  releases(startFallingFromTo(Physobj1,
     			    Physobj2,
     			    Physobj3),
     	 height(Physobj1,Height)).
 */
 %  % =================================.


%; A trajectory axiom states that
%; if a first physical object starts falling
%; from a second physical object
%; to a third physical object
%; at a time and
%; the first physical object has a height at the time,
%; then the first physical object will have a height
%; equal to the height minus an offset
%; at a time equal to the time plus the offset:
% [physobj1,physobj2,physobj3,height1,height2,offset,time]
% HoldsAt(Height(physobj1,height1),time) &
% height2=height1-offset ->
% Trajectory(FallingFromTo(physobj1,physobj2,physobj3),time,
%            Height(physobj1,height2),offset).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',116).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          height(Physobj1,Height1), 
%          Time), 
%       Height2=Height1-Offset), 
%    trajectory(
%       fallingFromTo(Physobj1,Physobj2,Physobj3), 
%       Time, 
%       height(Physobj1,Height2), 
%       Offset)).
(   trajectory(fallingFromTo(Physobj1, Physobj2, Physobj3),
               Time,
               height(Physobj1, Height2),
               Offset)
;   not height(Physobj1, Height1)at Time
;   not equals(Height2, Height1-Offset)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',116).

 /*   (   trajectory(fallingFromTo(Physobj1, Physobj2, Physobj3),
                       Time,
                       height(Physobj1, Height2),
                       Offset)
        ;   at(not(height(Physobj1, Height1)), Time)
        ;   not(equals(Height2, Height1-Offset))
        ).
 */
 %  % =================================.


%; A trigger axiom states that
%; if a first physical object is falling
%; from a second physical object
%; to a third physical object and
%; the height of the first physical object
%; is the same as the height of the third physical object,
%; the first physical object collides with the
%; third physical object:
% [physobj1,physobj2,physobj3,height,time]
% HoldsAt(FallingFromTo(physobj1,physobj2,physobj3),time) &
% HoldsAt(Height(physobj1,height),time) &
% HoldsAt(Height(physobj3,height),time) ->
% Happens(CollideWith(physobj1,physobj3),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',130).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          fallingFromTo(Physobj1,Physobj2,Physobj3), 
%          Time), 
%       ','(
%          holds(
%             height(Physobj1,Height), 
%             Time), 
%          holds(
%             height(Physobj3,Height), 
%             Time))), 
%    happens(
%       collideWith(Physobj1,Physobj3), 
%       Time)).
(   happens(collideWith(Physobj1, Physobj3), Time)
;   at(not fallingFromTo(Physobj1, Physobj2, Physobj3),
       Time)
;   not height(Physobj1, Height)at Time
;   not height(Physobj3, Height)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',130).

 /*   (   happens(collideWith(Physobj1, Physobj3), Time)
        ;   at(not(fallingFromTo(Physobj1, Physobj2, Physobj3)),
               Time)
        ;   at(not(height(Physobj1, Height)), Time)
        ;   at(not(height(Physobj3, Height)), Time)
        ).
 */
 %  % =================================.


%; An effect axiom states that
%; if a first physical object is falling
%; from a second physical object
%; to a third physical object and
%; the first physical object collides with
%; the third physical object,
%; the first physical object will be on the third physical object:
% [physobj1,physobj2,physobj3,time]
% HoldsAt(FallingFromTo(physobj1,physobj2,physobj3),time) ->
% Initiates(CollideWith(physobj1,physobj3),
%           On(physobj1,physobj3),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',143).
% From E: 
% 
% '->'(
%    holds(
%       fallingFromTo(Physobj1,Physobj2,Physobj3), 
%       Time), 
%    initiates_at(
%       collideWith(Physobj1,Physobj3), 
%       on(Physobj1,Physobj3), 
%       Time)).
(   initiates(collideWith(Physobj1, Physobj3),
              on(Physobj1, Physobj3)at Time)
;   at(not fallingFromTo(Physobj1, Physobj2, Physobj3),
       Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',143).

 /*   (   initiates(collideWith(Physobj1, Physobj3),
                      at(on(Physobj1, Physobj3), Time))
        ;   at(not(fallingFromTo(Physobj1, Physobj2, Physobj3)),
               Time)
        ).
 */
 %  % =================================.


%; An effect axiom states that
%; if a physical object collides with another
%; physical object,
%; the height of the first physical object will
%; be the height of the second physical object:
% [physobj1,physobj2,height,time]
% HoldsAt(Height(physobj2,height),time) ->
% Initiates(CollideWith(physobj1,physobj2),
%           Height(physobj1,height),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',154).
% From E: 
% 
% '->'(
%    holds(
%       height(Physobj2,Height), 
%       Time), 
%    initiates_at(
%       collideWith(Physobj1,Physobj2), 
%       height(Physobj1,Height), 
%       Time)).
(   initiates(collideWith(Physobj1, Physobj2),
              height(Physobj1, Height)at Time)
;   not height(Physobj2, Height)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',154).

 /*   (   initiates(collideWith(Physobj1, Physobj2),
                      at(height(Physobj1, Height), Time))
        ;   at(not(height(Physobj2, Height)), Time)
        ).
 */
 %  % =================================.


%;[physobj1,physobj2,height1,height2,time]
%;HoldsAt(Height(physobj2,height1),time) &
%;height1 != height2 ->
%;Terminates(CollideWith(physobj1,physobj2),
%;           Height(physobj1,height2),
%;           time).
%; An effect axiom states that
%; if a first physical object is falling
%; from a second physical object
%; to a third physical object and
%; the first physical object collides with
%; the third physical object,
%; the first physical object will no longer be
%; falling from the second physical object to the
%; third physical object:
% [physobj1,physobj2,physobj3,time]
% HoldsAt(FallingFromTo(physobj1,physobj2,physobj3),time) ->
% Terminates(CollideWith(physobj1,physobj3),
%            FallingFromTo(physobj1,physobj2,physobj3),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',176).
% From E: 
% 
% '->'(
%    holds(
%       fallingFromTo(Physobj1,Physobj2,Physobj3), 
%       Time), 
%    terminates_at(
%       collideWith(Physobj1,Physobj3), 
%       fallingFromTo(Physobj1,Physobj2,Physobj3), 
%       Time)).
(   terminates(collideWith(Physobj1, Physobj3),
               at(fallingFromTo(Physobj1, Physobj2, Physobj3),
                  Time))
;   at(not fallingFromTo(Physobj1, Physobj2, Physobj3),
       Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',176).

 /*   (   terminates(collideWith(Physobj1, Physobj3),
                       at(fallingFromTo(Physobj1,
                                        Physobj2,
                                        Physobj3),
                          Time))
        ;   at(not(fallingFromTo(Physobj1, Physobj2, Physobj3)),
               Time)
        ).
 */
 %  % =================================.


%; flying
%; agent is flying from physobj1 to physobj2.

% fluent FlyingFromTo(agent,physobj,physobj)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',184).
% From E: 
% 
% fluent(flyingFromTo(agent,physobj,physobj)).
mpred_prop(flyingFromTo(agent, physobj, physobj), fluent).
fluents([flyingFromTo/3]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',184).
%; agent starts flying from physobj1 to physobj2.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',187).
% event StartFlyingFromTo(agent,physobj,physobj)
% From E: 
% 
% event(startFlyingFromTo(agent,physobj,physobj)).
events([startFlyingFromTo/3]).
mpred_prop(startFlyingFromTo(agent, physobj, physobj), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',187).
actions([startFlyingFromTo/3]).


%; agent reaches physobj.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',189).
% event Reach(agent,physobj)
% From E: 
% 
% event(reach(agent,physobj)).
events([reach/2]).
mpred_prop(reach(agent, physobj), action).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',189).
actions([reach/2]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',191).
%; An effect axiom states that if an agent starts
%; flying from a physical object to another physical object,
%; the agent will be flying from the first physical object
%; to the second physical object:
% [agent,physobj1,physobj2,time]
% Initiates(StartFlyingFromTo(agent,physobj1,physobj2),
%           FlyingFromTo(agent,physobj1,physobj2),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',195).
% From E: 
% 
% initiates_at(
%    startFlyingFromTo(Agent,Physobj1,Physobj2), 
%    flyingFromTo(Agent,Physobj1,Physobj2), 
%    Time).
startFlyingFromTo(Agent, Physobj1, Physobj2)initiates flyingFromTo(Agent, Physobj1, Physobj2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',195).

 /*  initiated(happens(startFlyingFromTo(Agent,
     				    Physobj1,
     				    Physobj2),
     		  Time_from,
     		  Time_until),
     	  flyingFromTo(Agent,Physobj1,Physobj2),
     	  []).
 */
 %  % =================================.


%; A precondition axiom states that for
%; an agent to start flying from a physical object to
%; another physical object,
%; the height of the agent and
%; the first physical object must be the same:
% [agent,physobj1,physobj2,height1,height2,time]
% Happens(StartFlyingFromTo(agent,physobj1,physobj2),time) &
% HoldsAt(Height(agent,height1),time) &
% HoldsAt(Height(physobj1,height2),time) ->
% height1=height2.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',205).
% From E: 
% 
% '->'(
%    ','(
%       happens(
%          startFlyingFromTo(Agent,Physobj1,Physobj2), 
%          Time), 
%       ','(
%          holds(
%             height(Agent,Height1), 
%             Time), 
%          holds(
%             height(Physobj1,Height2), 
%             Time))), 
%    Height1=Height2).
(   equals(Height1, Height2)
;   not(happens(startFlyingFromTo(Agent, Physobj1, Physobj2),
                Time))
;   not height(Agent, Height1)at Time
;   not height(Physobj1, Height2)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',205).

 /*   (   equals(Height1, Height2)
        ;   not(happens(startFlyingFromTo(Agent,
                                          Physobj1,
                                          Physobj2),
                        Time))
        ;   at(not(height(Agent, Height1)), Time)
        ;   at(not(height(Physobj1, Height2)), Time)
        ).
 */
 %  % =================================.


%; A state constraint says that an agent
%; cannot fly from and to the same physical object:
% [agent,physobj1,physobj2,time]
% HoldsAt(FlyingFromTo(agent,physobj1,physobj2),time) ->
% physobj1!=physobj2.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',213).
% From E: 
% 
% '->'(
%    holds(
%       flyingFromTo(Agent,Physobj1,Physobj2), 
%       Time), 
%    Physobj1\=Physobj2).
(   { dif(Physobj1, Physobj2)
    }
;   not flyingFromTo(Agent, Physobj1, Physobj2)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',213).

 /*   (   { dif(Physobj1, Physobj2)
            }
        ;   at(not(flyingFromTo(Agent, Physobj1, Physobj2)),
               Time)
        ).
 */
 %  % =================================.


%; A releases axiom states that if an agent
%; starts flying from a physical object to another
%; physical object, the height of the agent will
%; be released from inertia:
% [agent,physobj1,physobj2,height,time]
% Releases(StartFlyingFromTo(agent,physobj1,physobj2),
%          Height(agent,height),
%          time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',221).
% From E: 
% 
% releases_at(
%    startFlyingFromTo(Agent,Physobj1,Physobj2), 
%    height(Agent,Height), 
%    Time).
releases(startFlyingFromTo(Agent, Physobj1, Physobj2), height(Agent, Height)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',221).

 /*  releases(startFlyingFromTo(Agent,Physobj1,Physobj2),
     	 height(Agent,Height)).
 */
 %  % =================================.


%; A trajectory axiom states that
%; if an agent starts flying from
%; from a physical object
%; to another physical object
%; at a time and
%; the agent has a height at the time,
%; then the agent will have a height
%; equal to the height plus an offset
%; at a time equal to the time plus the offset:
% [agent,physobj1,physobj2,height1,height2,offset,time]
% HoldsAt(Height(agent,height1),time) &
% height2=height1+offset ->
% Trajectory(FlyingFromTo(agent,physobj1,physobj2),time,
%            Height(agent,height2),offset).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',235).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          height(Agent,Height1), 
%          Time), 
%       Height2=Height1+Offset), 
%    trajectory(
%       flyingFromTo(Agent,Physobj1,Physobj2), 
%       Time, 
%       height(Agent,Height2), 
%       Offset)).
(   trajectory(flyingFromTo(Agent, Physobj1, Physobj2),
               Time,
               height(Agent, Height2),
               Offset)
;   not height(Agent, Height1)at Time
;   not equals(Height2, Height1+Offset)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',235).

 /*   (   trajectory(flyingFromTo(Agent, Physobj1, Physobj2),
                       Time,
                       height(Agent, Height2),
                       Offset)
        ;   at(not(height(Agent, Height1)), Time)
        ;   not(equals(Height2, Height1+Offset))
        ).
 */
 %  % =================================.


%; A trigger axiom states that
%; if an agent is flying
%; from a physical object
%; to another physical object and
%; the height of the agent
%; is the same as the height of the second physical object,
%; the agent reaches the second physical object:
% [agent,physobj1,physobj2,height,time]
% HoldsAt(FlyingFromTo(agent,physobj1,physobj2),time) &
% HoldsAt(Height(agent,height),time) &
% HoldsAt(Height(physobj2,height),time) ->
% Happens(Reach(agent,physobj2),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',248).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          flyingFromTo(Agent,Physobj1,Physobj2), 
%          Time), 
%       ','(
%          holds(
%             height(Agent,Height), 
%             Time), 
%          holds(
%             height(Physobj2,Height), 
%             Time))), 
%    happens(
%       reach(Agent,Physobj2), 
%       Time)).
(   happens(reach(Agent, Physobj2), Time)
;   not flyingFromTo(Agent, Physobj1, Physobj2)at Time
;   not height(Agent, Height)at Time
;   not height(Physobj2, Height)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',248).

 /*   (   happens(reach(Agent, Physobj2), Time)
        ;   at(not(flyingFromTo(Agent, Physobj1, Physobj2)),
               Time)
        ;   at(not(height(Agent, Height)), Time)
        ;   at(not(height(Physobj2, Height)), Time)
        ).
 */
 %  % =================================.


%; An effect axiom states that
%; if an agent reaches a physical object,
%; the height of the agent will be the
%; height of the physical object:
% [agent,physobj,height,time]
% HoldsAt(Height(physobj,height),time) ->
% Initiates(Reach(agent,physobj),Height(agent,height),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',258).
% From E: 
% 
% '->'(
%    holds(
%       height(Physobj,Height), 
%       Time), 
%    initiates_at(
%       reach(Agent,Physobj), 
%       height(Agent,Height), 
%       Time)).
(   initiates(reach(Agent, Physobj),
              height(Agent, Height)at Time)
;   not height(Physobj, Height)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',258).

 /*   (   initiates(reach(Agent, Physobj),
                      at(height(Agent, Height), Time))
        ;   at(not(height(Physobj, Height)), Time)
        ).
 */
 %  % =================================.


%;[agent,physobj,height1,height2,time]
%;HoldsAt(Height(physobj,height1),time) &
%;height1!=height2 ->
%;Terminates(Reach(agent,physobj),Height(agent,height2),time).
%; An effect axiom states that
%; if an agent is flying
%; from a physical object
%; to another physical object and
%; the agent reaches the second physical object,
%; the agent will no longer be
%; flying from the first physical object
%; to the second physical object:
% [agent,physobj1,physobj2,time]
% HoldsAt(FlyingFromTo(agent,physobj1,physobj2),time) ->
% Terminates(Reach(agent,physobj2),
%            FlyingFromTo(agent,physobj1,physobj2),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',275).
% From E: 
% 
% '->'(
%    holds(
%       flyingFromTo(Agent,Physobj1,Physobj2), 
%       Time), 
%    terminates_at(
%       reach(Agent,Physobj2), 
%       flyingFromTo(Agent,Physobj1,Physobj2), 
%       Time)).
(   terminates(reach(Agent, Physobj2),
               at(flyingFromTo(Agent, Physobj1, Physobj2),
                  Time))
;   not flyingFromTo(Agent, Physobj1, Physobj2)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',275).

 /*   (   terminates(reach(Agent, Physobj2),
                       at(flyingFromTo(Agent, Physobj1, Physobj2),
                          Time))
        ;   at(not(flyingFromTo(Agent, Physobj1, Physobj2)),
               Time)
        ).
 */
 %  % =================================.


%; A releases axiom states that
%; if an agent holds a physical object,
%; the height of the physical object is released from inertia:
% [agent,physobj,height,time]
% Releases(Hold(agent,physobj),Height(physobj,height),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',284).
% From E: 
% 
% releases_at(
%    hold(Agent,Physobj), 
%    height(Physobj,Height), 
%    Time).
releases(hold(Agent, Physobj), height(Physobj, Height)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',284).

 /*  releases(hold(Agent,Physobj),
     	 height(Physobj,Height)).
 */
 %  % =================================.


%;[agent,physobj,height1,height2,time]
%;(!{object} PartOf(physobj,object)) &
%;HoldsAt(Height(physobj,height1),time) &
%;height1 != height2 ->
%;Terminates(LetGoOf(agent,physobj),Height(physobj,height2),time).
% [agent,physobj,height,time]
% (!{object} PartOf(physobj,object)) &
% HoldsAt(Height(physobj,height),time) ->
% Initiates(LetGoOf(agent,physobj),Height(physobj,height),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',293).
% From E: 
% 
% '->'(
%    ','(
%       not(thereExists(Object, 
%              partOf(Physobj,Object))), 
%       holds(
%          height(Physobj,Height), 
%          Time)), 
%    initiates_at(
%       letGoOf(Agent,Physobj), 
%       height(Physobj,Height), 
%       Time)).
(   initiates(letGoOf(Agent, Physobj),
              height(Physobj, Height)at Time)
;   thereExists(Object, partOf(Physobj, Object))
;   not height(Physobj, Height)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',293).

 /*   (   initiates(letGoOf(Agent, Physobj),
                      at(height(Physobj, Height), Time))
        ;   thereExists(Object, partOf(Physobj, Object))
        ;   at(not(height(Physobj, Height)), Time)
        ).
 */
 %  % =================================.


%; A state constraint says that
%; if an agent is holding a physical object and
%; the height of the agent is height,
%; the height of the physical object is height:
% [agent,physobj,height,time]
% HoldsAt(Holding(agent,physobj),time) &
% HoldsAt(Height(agent,height),time) ->
% HoldsAt(Height(physobj,height),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',302).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          holding(Agent,Physobj), 
%          Time), 
%       holds(
%          height(Agent,Height), 
%          Time)), 
%    holds(
%       height(Physobj,Height), 
%       Time)).
(   height(Physobj, Height)at Time
;   not holding(Agent, Physobj)at Time
;   not height(Agent, Height)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',302).

 /*   (   at(height(Physobj, Height), Time)
        ;   at(not(holding(Agent, Physobj)), Time)
        ;   at(not(height(Agent, Height)), Time)
        ).
 */
 %  % =================================.


%; A state constraint says that if a physical object
%; is part of an object,
%; the height of the physical object
%; is the same as the height of the object:
% [physobj,object,height,time]
% PartOf(physobj,object) &
% HoldsAt(Height(object,height),time) ->
% HoldsAt(Height(physobj,height),time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',311).
% From E: 
% 
% '->'(
%    ','(
%       partOf(Physobj,Object), 
%       holds(
%          height(Object,Height), 
%          Time)), 
%    holds(
%       height(Physobj,Height), 
%       Time)).
(   height(Physobj, Height)at Time
;   not partOf(Physobj, Object)
;   not height(Object, Height)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',311).

 /*   (   at(height(Physobj, Height), Time)
        ;   not(partOf(Physobj, Object))
        ;   at(not(height(Object, Height)), Time)
        ).
 */
 %  % =================================.


%;event Catch(agent,physobj)
%;event HitFromTo(agent,physobj,object,object)
%;fluent Distance(physobj,physobj,distance)
%;fluent FlyingAcrossFromTo(physobj,object,object)
%;[agent,physobj1,physobj2,physobj3,time]
%;Initiates(HitFromTo(agent,physobj1,physobj2,physobj3),
%;          FlyingAcrossFromTo(physobj1,physobj2,physobj3),
%;          time).
%;[agent,physobj1,physobj2,physobj3,distance,time]
%;Releases(HitFromTo(agent,physobj1,physobj2,physobj3),
%;         Distance(physobj1,physobj2,distance),
%;         time).
%;[agent,physobj1,physobj2,physobj3,distance,time]
%;Releases(HitFromTo(agent,physobj1,physobj2,physobj3),
%;         Distance(physobj1,physobj3,distance),
%;         time).
%;[physobj1,physobj2,physobj3,offset,time]
%;Trajectory(FlyingAcrossFromTo(physobj1,physobj2,physobj3),time,
%;           Distance(physobj1,physobj2,offset),offset).
%;[physobj1,physobj2,physobj3,distance1,distance2,offset,time]
%;HoldsAt(Distance(physobj2,physobj3,distance1),time) &
%;distance2 = distance1 - time ->
%;Trajectory(FlyingAcrossFromTo(physobj1,physobj2,physobj3),time,
%;           Distance(physobj1,physobj3,distance2),offset).
%;[agent,physobj1,physobj2,physobj3,time]
%;HoldsAt(FlyingAcrossFromTo(physobj1,physobj2,physobj3),time) ->
%;Initiates(Catch(agent,physobj1),
%;          Holding(agent,physobj1),
%;          time).
%;[agent,physobj1,physobj2,physobj3,time]
%;HoldsAt(FlyingAcrossFromTo(physobj1,physobj2,physobj3),time) ->
%;Terminates(Catch(agent,physobj1),
%;           FlyingAcrossFromTo(physobj1,physobj2,physobj3),
%;           time).
%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.e',358).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/OMSpace.lps.pl')).
