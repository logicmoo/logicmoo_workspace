% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/Fire.e',124).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.lps.pl')).
% Fri, 26 Mar 2021 01:05:59 GMT File: <stream>(0x555566f09d00)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; GSpace: grid space
%;
%; @book{Mueller:1998,
%;   author = "Erik T. Mueller",
%;   year = "1998",
%;   title = "Natural Language Processing with \uppercase{T}hought\uppercase{T}reasure",
%;   address = "New York",
%;   publisher = "Signiform",
%; }
%;

% sort coord: integer
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.e',22).
% From E: 
% 
% subsort(coord,integer).
subsort(coord, integer).

% sort grid
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.e',22).
% From E: 
% 
% sort(grid).
sort(grid).
%; object is at (coord1, coord2) in grid.

% fluent GridAt(grid,object,coord,coord)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.e',25).
% From E: 
% 
% fluent(gridAt(grid, object, coord, 
%           coord)).
mpred_prop(gridAt(grid, object, coord, coord), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.e',25).
fluents([gridAt/4]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.e',28).
%; agent walks from (coord1, coord2)
%; to (coord3, coord4) in grid.

:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.e',30).
% event GridWalk(grid,agent,coord,coord,coord,coord)
% From E: 
% 
% event(gridWalk(grid, agent, coord, coord, coord, 
%          coord)).
mpred_prop(gridWalk(grid, agent, coord, coord, coord, coord), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.e',30).
events([gridWalk/6]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.e',32).
%; A state constraint says that for a given grid an
%; object is at one cell in that grid at a time:
% [grid,object,coord1,coord2,coord3,coord4,time]
% HoldsAt(GridAt(grid,object,coord1,coord2),time) &
% HoldsAt(GridAt(grid,object,coord3,coord4),time) ->
% coord1=coord3 & coord2=coord4.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.e',34).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          gridAt(Grid, Object, Coord1, 
%             Coord2), 
%          Time), 
%       holds(
%          gridAt(Grid, Object, Coord3, 
%             Coord4), 
%          Time)), 
%    ','(
%       Coord1=Coord3, 
%       Coord2=Coord4)).
(   equals(Coord1, Coord3),
    equals(Coord2, Coord4)
;   not gridAt(Grid, Object, Coord1, Coord2)at Time
;   not gridAt(Grid, Object, Coord3, Coord4)at Time
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.e',34).

 /*  (   equals(Coord1, Coord3),
         equals(Coord2, Coord4)
     ;   at(not(gridAt(Grid, Object, Coord1, Coord2)),
            Time)
     ;   at(not(gridAt(Grid, Object, Coord3, Coord4)),
            Time)
     ).
 */
 %  % =================================.


%; An effect axiom states that
%; if an agent walks from one cell in a grid to another cell,
%; the agent will be at second cell:
% [grid,agent,coord1,coord2,coord3,coord4,time]
% Initiates(GridWalk(grid,agent,coord1,coord2,coord3,coord4),
%           GridAt(grid,agent,coord3,coord4),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.e',42).
% From E: 
% 
% initiates_at(
%    gridWalk(Grid, Agent, Coord1, Coord2, Coord3, 
%       Coord4), 
%    gridAt(Grid, Agent, Coord3, 
%       Coord4), 
%    Time).
gridWalk(Grid, Agent, Coord1, Coord2, Coord3, Coord4)initiates gridAt(Grid, Agent, Coord3, Coord4).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.e',42).

 /*  initiated(happens(gridWalk(Grid,
     			   Agent,
     			   Coord1,
     			   Coord2,
     			   Coord3,
     			   Coord4),
     		  Time_from,
     		  Time_until),
     	  gridAt(Grid,Agent,Coord3,Coord4),
     	  []).
 */
 %  % =================================.


%; An effect axiom states that
%; if an agent walks from one cell in a grid to another cell,
%; the agent will no longer be at the first cell:
% [grid,agent,coord1,coord2,coord3,coord4,time]
% Terminates(GridWalk(grid,agent,coord1,coord2,coord3,coord4),
%            GridAt(grid,agent,coord1,coord2),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.e',50).
% From E: 
% 
% terminates_at(
%    gridWalk(Grid, Agent, Coord1, Coord2, Coord3, 
%       Coord4), 
%    gridAt(Grid, Agent, Coord1, 
%       Coord2), 
%    Time).
gridWalk(Grid, Agent, Coord1, Coord2, Coord3, Coord4)terminates gridAt(Grid, Agent, Coord1, Coord2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.e',50).

 /*  terminated(happens(gridWalk(Grid,
     			    Agent,
     			    Coord1,
     			    Coord2,
     			    Coord3,
     			    Coord4),
     		   Time_from,
     		   Time_until),
     	   gridAt(Grid,Agent,Coord1,Coord2),
     	   []).
 */
 %  % =================================.


%; A precondition axiom states that for an agent to walk
%; from one cell in a grid to another cell, the agent
%; must be at the first cell, the second cell must not
%; be occupied, and the first cell must be adjacent to
%; the second cell:
% [grid,agent,coord1,coord2,coord3,coord4,time]
% Happens(GridWalk(grid,agent,coord1,coord2,coord3,coord4),time) ->
% HoldsAt(GridAt(grid,agent,coord1,coord2),time) &
% (!{object} HoldsAt(GridAt(grid,object,coord3,coord4),time)) &
% (coord1=coord3 |
%  coord1=coord3+1 |
%  coord1=coord3-1) &
% (coord2=coord4 |
%  coord2=coord4+1 |
%  coord2=coord4-1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.e',60).
% From E: 
% 
% '->'(
%    happens(
%       gridWalk(Grid, Agent, Coord1, Coord2, Coord3, 
%          Coord4), 
%       Time), 
%    ','(
%       holds(
%          gridAt(Grid, Agent, Coord1, 
%             Coord2), 
%          Time), 
%       ','(
%          not(thereExists(Object, 
%                 holds(
%                    gridAt(Grid, Object, Coord3, 
%                       Coord4), 
%                    Time))), 
%          ','(
%             ';'(
%                Coord1=Coord3, 
%                ';'(
%                   Coord1=Coord3+1, 
%                   Coord1=Coord3-1)), 
%             ';'(
%                Coord2=Coord4, 
%                ';'(
%                   Coord2=Coord4+1, 
%                   Coord2=Coord4-1)))))).
(   gridAt(Grid, Agent, Coord1, Coord2)at Time,
    not(thereExists(Object,
                    at(gridAt(Grid,
                              Object,
                              Coord3,
                              Coord4),
                       Time))),
    (   equals(Coord1, Coord3)
    ;   equals(Coord1, Coord3+1)
    ;   equals(Coord1, Coord3-1)
    ),
    (   equals(Coord2, Coord4)
    ;   equals(Coord2, Coord4+1)
    ;   equals(Coord2, Coord4-1)
    )
;   not(happens(gridWalk(Grid,
                         Agent,
                         Coord1,
                         Coord2,
                         Coord3,
                         Coord4),
                Time))
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.e',60).

 /*  (   at(gridAt(Grid, Agent, Coord1, Coord2), Time),
         not(thereExists(Object,
                         at(gridAt(Grid,
                                   Object,
                                   Coord3,
                                   Coord4),
                            Time))),
         (   equals(Coord1, Coord3)
         ;   equals(Coord1, Coord3+1)
         ;   equals(Coord1, Coord3-1)
         ),
         (   equals(Coord2, Coord4)
         ;   equals(Coord2, Coord4+1)
         ;   equals(Coord2, Coord4-1)
         )
     ;   not(happens(gridWalk(Grid,
                              Agent,
                              Coord1,
                              Coord2,
                              Coord3,
                              Coord4),
                     Time))
     ).
 */
 %  % =================================.


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.e',70).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/GSpace.lps.pl')).
