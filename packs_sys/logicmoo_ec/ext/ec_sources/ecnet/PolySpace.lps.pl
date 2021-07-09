% From E: 
% 
% ':-'(call_pel_directive(translate(unskipped,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.lps.pl'))).
:- call_pel_directive(translate(unskipped,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.lps.pl')).
:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(lps).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PlayNeed.e',131).
% From E: 
% 
% ':-'(call_pel_directive(translate(begining,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.lps.pl'))).
:- call_pel_directive(translate(begining,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.lps.pl')).
% Fri, 26 Mar 2021 01:06:03 GMT File: <stream>(0x555567c94300)%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available at
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; @phdthesis{Cassimatis:2002,
%;   author = "Nicholas L. Cassimatis",
%;   year = "2002",
%;   title = "Polyscheme: A Cognitive Architecture for Integrating Multiple Representation and Inference Schemes",
%;   address = "Cambridge, MA",
%;   school = "Program in Media Arts and Sciences, School of Architecture and Planning, Massachusetts Institute of Technology",
%; }
%;
%; sorts

% sort object
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',20).
% From E: 
% 
% sort(object).
sort(object).

% sort xcoord: integer
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',22).
% From E: 
% 
% subsort(xcoord,integer).
subsort(xcoord, integer).

% sort ycoord: integer
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',22).
% From E: 
% 
% subsort(ycoord,integer).
subsort(ycoord, integer).

% sort grid
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',24).
% From E: 
% 
% sort(grid).
sort(grid).

% sort shape
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',24).
% From E: 
% 
% sort(shape).
sort(shape).

% sort color
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',26).
% From E: 
% 
% sort(color).
sort(color).
%; constants

% shape Round,Square
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',28).
% From E: 
% 
% t(shape,round).
isa(round, shape).
% From E: 
% 
% t(shape,square).
isa(square, shape).

% color Red,Green
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',30).
% From E: 
% 
% t(color,red).
isa(red, color).
% From E: 
% 
% t(color,green).
isa(green, color).
%; predicates, fluents, and events

% predicate Equal(object,object)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',32).
% From E: 
% 
% predicate(equal(object,object)).
mpred_prop(equal(object, object), predicate).
predicates([equal/2]).

% predicate Shape(object,shape)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',34).
% From E: 
% 
% predicate(shape(object,shape)).
mpred_prop(shape(object, shape), predicate).
predicates([shape/2]).

% predicate Color(object,color)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',34).
% From E: 
% 
% predicate(color(object,color)).
mpred_prop(color(object, color), predicate).
predicates([color/2]).

% fluent Location(grid,object,xcoord,ycoord)
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',36).
% From E: 
% 
% fluent(location(grid, object, xcoord, 
%           ycoord)).
mpred_prop(location(grid, object, xcoord, ycoord), fluent).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',36).
fluents([location/4]).

% event Move(grid,object,xcoord,ycoord,xcoord,ycoord)
% From E: 
% 
% event(move(grid, object, xcoord, ycoord, xcoord, 
%          ycoord)).
mpred_prop(move(grid, object, xcoord, ycoord, xcoord, ycoord), event).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',36).
events([move/6]).


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',39).
%; axioms
% [object1,object2]
 % Equal(object1,object2) -> Equal(object2,object1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',41).
% From E: 
% 
% '->'(
%    equal(Object1,Object2), 
%    equal(Object2,Object1)).
(   equal(Object2, Object1)
;   not equal(Object1, Object2)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',41).

 /*   (   equal(Object2, Object1)
        ;   not(equal(Object1, Object2))
        ).
 */
 %  % =================================.


:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',43).
%; objects have unique shape
% [object,shape1,shape2]
% Shape(object,shape1) & Shape(object,shape2) ->
% shape1=shape2.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',45).
% From E: 
% 
% '->'(
%    ','(
%       shape(Object,Shape1), 
%       shape(Object,Shape2)), 
%    Shape1=Shape2).
(   equals(Shape1, Shape2)
;   not shape(Object, Shape1)
;   not shape(Object, Shape2)
).

 /*   (   equals(Shape1, Shape2)
        ;   not(shape(Object, Shape1))
        ;   not(shape(Object, Shape2))
        ).
 */
 %  % =================================.


%; objects have unique color
% [object,color1,color2]
% Color(object,color1) & Color(object,color2) ->
% color1=color2.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',49).
% From E: 
% 
% '->'(
%    ','(
%       color(Object,Color1), 
%       color(Object,Color2)), 
%    Color1=Color2).
(   equals(Color1, Color2)
;   not color(Object, Color1)
;   not color(Object, Color2)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',49).

 /*   (   equals(Color1, Color2)
        ;   not(color(Object, Color1))
        ;   not(color(Object, Color2))
        ).
 */
 %  % =================================.


%; if objects are the same, they have the same shape
% [object1,object2]
% Equal(object1,object2) ->
% ({shape} Shape(object1,shape) & Shape(object2,shape)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',54).
% From E: 
% 
% '->'(
%    equal(Object1,Object2), 
%    thereExists(Shape, 
%       ','(
%          shape(Object1,Shape), 
%          shape(Object2,Shape)))).
(   thereExists(Shape,
                 (shape(Object1, Shape), shape(Object2, Shape)))
;   not equal(Object1, Object2)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',54).

 /*  (   thereExists(Shape,
                      (shape(Object1, Shape), shape(Object2, Shape)))
     ;   not(equal(Object1, Object2))
     ).
 */
 %  % =================================.


%; if objects are the same, they have the same color
% [object1,object2]
% Equal(object1,object2) ->
% ({color} Color(object1,color) & Color(object2,color)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',59).
% From E: 
% 
% '->'(
%    equal(Object1,Object2), 
%    thereExists(Color, 
%       ','(
%          color(Object1,Color), 
%          color(Object2,Color)))).
(   thereExists(Color,
                 (color(Object1, Color), color(Object2, Color)))
;   not equal(Object1, Object2)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',59).

 /*  (   thereExists(Color,
                      (color(Object1, Color), color(Object2, Color)))
     ;   not(equal(Object1, Object2))
     ).
 */
 %  % =================================.


%; if objects are the same, they have the same location
% [grid,object1,object2,xcoord1,ycoord1,xcoord2,ycoord2,time]
% Equal(object1,object2) ->
% (HoldsAt(Location(grid,object1,xcoord1,ycoord1),time) &
%  HoldsAt(Location(grid,object2,xcoord2,ycoord2),time) ->
%  xcoord1=xcoord2 & ycoord1=ycoord2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',64).
% From E: 
% 
% '->'(
%    equal(Object1,Object2), 
%    '->'(
%       ','(
%          holds(
%             location(Grid, Object1, Xcoord1, 
%                Ycoord1), 
%             Time), 
%          holds(
%             location(Grid, Object2, Xcoord2, 
%                Ycoord2), 
%             Time)), 
%       ','(
%          Xcoord1=Xcoord2, 
%          Ycoord1=Ycoord2))).
(   (   equals(Xcoord1, Xcoord2),
        equals(Ycoord1, Ycoord2)
    ;   at(not location(Grid, Object1, Xcoord1, Ycoord1),
           Time)
    ;   at(not location(Grid, Object2, Xcoord2, Ycoord2),
           Time)
    )
;   not equal(Object1, Object2)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',64).

 /*  (   (   equals(Xcoord1, Xcoord2),
             equals(Ycoord1, Ycoord2)
         ;   at(not(location(Grid, Object1, Xcoord1, Ycoord1)),
                Time)
         ;   at(not(location(Grid, Object2, Xcoord2, Ycoord2)),
                Time)
         )
     ;   not(equal(Object1, Object2))
     ).
 */
 %  % =================================.


%; object in one location at a time
% [grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]
% HoldsAt(Location(grid,object,xcoord1,ycoord1),time) &
% HoldsAt(Location(grid,object,xcoord2,ycoord2),time) ->
% xcoord1=xcoord2 & ycoord1=ycoord2.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',71).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          location(Grid, Object, Xcoord1, 
%             Ycoord1), 
%          Time), 
%       holds(
%          location(Grid, Object, Xcoord2, 
%             Ycoord2), 
%          Time)), 
%    ','(
%       Xcoord1=Xcoord2, 
%       Ycoord1=Ycoord2)).
(   equals(Xcoord1, Xcoord2),
    equals(Ycoord1, Ycoord2)
;   at(not location(Grid, Object, Xcoord1, Ycoord1),
       Time)
;   at(not location(Grid, Object, Xcoord2, Ycoord2),
       Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',71).

 /*  (   equals(Xcoord1, Xcoord2),
         equals(Ycoord1, Ycoord2)
     ;   at(not(location(Grid, Object, Xcoord1, Ycoord1)),
            Time)
     ;   at(not(location(Grid, Object, Xcoord2, Ycoord2)),
            Time)
     ).
 */
 %  % =================================.


%; objects have locations
% [grid,object,time]
% (% {xcoord,ycoord} HoldsAt(Location(grid,object,xcoord,ycoord),time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',77).
% From E: 
% 
% exists(
%    [Xcoord,Ycoord], 
%    holds(
%       location(Grid, Object, Xcoord, 
%          Ycoord), 
%       Time)).
exists(Xcoord, exists(Ycoord, location(Grid, Object, Xcoord, Ycoord)at Time)).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',77).

 /*  exists(Xcoord,
      exists(Ycoord,
      at(location(Grid,Object,Xcoord,Ycoord),
        Time))).
 */
 %  % =================================.


%; different objects are not at same location
% [grid,object1,object2,xcoord1,ycoord1,time]
% HoldsAt(Location(grid,object1,xcoord1,ycoord1),time) &
% HoldsAt(Location(grid,object2,xcoord1,ycoord1),time) ->
% Equal(object1,object2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',81).
% From E: 
% 
% '->'(
%    ','(
%       holds(
%          location(Grid, Object1, Xcoord1, 
%             Ycoord1), 
%          Time), 
%       holds(
%          location(Grid, Object2, Xcoord1, 
%             Ycoord1), 
%          Time)), 
%    equal(Object1,Object2)).
(   equal(Object1, Object2)
;   at(not location(Grid, Object1, Xcoord1, Ycoord1),
       Time)
;   at(not location(Grid, Object2, Xcoord1, Ycoord1),
       Time)
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',81).

 /*   (   equal(Object1, Object2)
        ;   at(not(location(Grid, Object1, Xcoord1, Ycoord1)),
               Time)
        ;   at(not(location(Grid, Object2, Xcoord1, Ycoord1)),
               Time)
        ).
 */
 %  % =================================.


%; moving to a location causes an object to be at that location
% [grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]
% Initiates(Move(grid,object,xcoord1,ycoord1,xcoord2,ycoord2),
%           Location(grid,object,xcoord2,ycoord2),
%           time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',87).
% From E: 
% 
% initiates_at(
%    move(Grid, Object, Xcoord1, Ycoord1, Xcoord2, 
%       Ycoord2), 
%    location(Grid, Object, Xcoord2, 
%       Ycoord2), 
%    Time).
move(Grid, Object, Xcoord1, Ycoord1, Xcoord2, Ycoord2)initiates location(Grid, Object, Xcoord2, Ycoord2).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',87).

 /*  initiated(happens(move(Grid,
     		       Object,
     		       Xcoord1,
     		       Ycoord1,
     		       Xcoord2,
     		       Ycoord2),
     		  Time_from,
     		  Time_until),
     	  location(Grid,Object,Xcoord2,Ycoord2),
     	  []).
 */
 %  % =================================.


%; moving to a location causes the object no longer to be at its previous
%; location
% [grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]
% Terminates(Move(grid,object,xcoord1,ycoord1,xcoord2,ycoord2),
%            Location(grid,object,xcoord1,ycoord1),
%            time).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',94).
% From E: 
% 
% terminates_at(
%    move(Grid, Object, Xcoord1, Ycoord1, Xcoord2, 
%       Ycoord2), 
%    location(Grid, Object, Xcoord1, 
%       Ycoord1), 
%    Time).
move(Grid, Object, Xcoord1, Ycoord1, Xcoord2, Ycoord2)terminates location(Grid, Object, Xcoord1, Ycoord1).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',94).

 /*  terminated(happens(move(Grid,
     			Object,
     			Xcoord1,
     			Ycoord1,
     			Xcoord2,
     			Ycoord2),
     		   Time_from,
     		   Time_until),
     	   location(Grid,
     		    Object,
     		    Xcoord1,
     		    Ycoord1),
     	   []).
 */
 %  % =================================.


%;; allow diagonal movements
%;[grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]
%;Happens(Move(grid,object,xcoord1,ycoord1,xcoord2,ycoord2),time) ->
%;HoldsAt(Location(grid,object,xcoord1,ycoord1),time) &
%;(xcoord1=xcoord2 |
%; xcoord1=xcoord2+1 |
%; xcoord1=xcoord2-1) &
%;(ycoord1=ycoord2 |
%; ycoord1=ycoord2+1 |
%; ycoord1=ycoord2-1).
%; only allow right angle movements
% [grid,object,xcoord1,ycoord1,xcoord2,ycoord2,time]
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',112).
% Happens(Move(grid,object,xcoord1,ycoord1,xcoord2,ycoord2),time) ->
% HoldsAt(Location(grid,object,xcoord1,ycoord1),time) &
% ((xcoord1=xcoord2 & (ycoord1=ycoord2+1 | ycoord1=ycoord2-1)) |
%  (ycoord1=ycoord2 & (xcoord1=xcoord2+1 | xcoord1=xcoord2-1))).
% From E: 
% 
% '->'(
%    happens(
%       move(Grid, Object, Xcoord1, Ycoord1, Xcoord2, 
%          Ycoord2), 
%       Time), 
%    ','(
%       holds(
%          location(Grid, Object, Xcoord1, 
%             Ycoord1), 
%          Time), 
%       ';'(
%          ','(
%             Xcoord1=Xcoord2, 
%             ';'(
%                Ycoord1=Ycoord2+1, 
%                Ycoord1=Ycoord2-1)), 
%          ','(
%             Ycoord1=Ycoord2, 
%             ';'(
%                Xcoord1=Xcoord2+1, 
%                Xcoord1=Xcoord2-1))))).
(   location(Grid, Object, Xcoord1, Ycoord1)at Time,
    (   equals(Xcoord1, Xcoord2),
        (   equals(Ycoord1, Ycoord2+1)
        ;   equals(Ycoord1, Ycoord2-1)
        )
    ;   equals(Ycoord1, Ycoord2),
        (   equals(Xcoord1, Xcoord2+1)
        ;   equals(Xcoord1, Xcoord2-1)
        )
    )
;   not(happens(move(Grid,
                     Object,
                     Xcoord1,
                     Ycoord1,
                     Xcoord2,
                     Ycoord2),
                Time))
).
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',112).

 /*  (   at(location(Grid, Object, Xcoord1, Ycoord1),
            Time),
         (   equals(Xcoord1, Xcoord2),
             (   equals(Ycoord1, Ycoord2+1)
             ;   equals(Ycoord1, Ycoord2-1)
             )
         ;   equals(Ycoord1, Ycoord2),
             (   equals(Xcoord1, Xcoord2+1)
             ;   equals(Xcoord1, Xcoord2-1)
             )
         )
     ;   not(happens(move(Grid,
                          Object,
                          Xcoord1,
                          Ycoord1,
                          Xcoord2,
                          Ycoord2),
                     Time))
     ).
 */
 %  % =================================.


%; End of file.
:-was_s_l('/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.e',116).
% From E: 
% 
% ':-'(call_pel_directive(translate(ending,'/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.lps.pl'))).
:- call_pel_directive(translate(ending,
                                '/mnt/sdc1/logicmoo_workspace.1/packs_sys/logicmoo_ec/ext/ec_sources/ecnet/PolySpace.lps.pl')).
