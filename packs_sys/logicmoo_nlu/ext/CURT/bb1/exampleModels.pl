/*************************************************************************

    File: exampleModels.pl
    Copyright (C) 2004 Patrick Blackburn & Johan Bos

    This file is part of BB1, version 1.2 (August 2005).

    BB1 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB1 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB1; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(exampleModels,[example/2]).


/*========================================================================
   Example Models
========================================================================*/

example(1,
        model([d1,d2,d3,d4,d5],
              [f(0,jules,d1),
               f(0,vincent,d2),
               f(0,pumpkin,d3),
               f(0,honey_bunny,d4),
               f(0,yolanda,d5),
               f(1,customer,[d1,d2]),
               f(1,robber,[d3,d4]),
               f(2,love,[(d3,d4)])])).


example(2,
        model([d1,d2,d3,d4,d5,d6],
              [f(0,jules,d1),
               f(0,vincent,d2),
               f(0,pumpkin,d3),
               f(0,honey_bunny,d4),
               f(0,yolanda,d4),
               f(1,customer,[d1,d2,d5,d6]),
               f(1,robber,[d3,d4]),
               f(2,love,[])])).


example(3,
        model([d1,d2,d3,d4,d5,d6,d7,d8],
              [f(0,mia,d1),
               f(0,jody,d2),
               f(0,jules,d3),
               f(0,vincent,d4),
               f(1,woman,[d1,d2]),
               f(1,man,[d3,d4]),
               f(1,joke,[d5,d6]),
               f(1,episode,[d7,d8]),
               f(2,in,[(d5,d7),(d5,d8)]),
               f(2,tell,[(d1,d5),(d2,d6)])])).

example(4,model([d1,d7,d9],[f(2,accessible_world,[(d9,d9),(d9,d7),(d1,d9),(d1,d7),(d1,d1)])])).
example(5,model([d1,d7,d9],[f(2,accessible_world,[(d9,d9),(d9,d7),(d1,d9),(d1,d1)])])).
example(6,model([d9,d7,d1],[f(2,accessible_world,[(d9,d9),(d9,d7),(d1,d9),(d1,d7),(d1,d1)])])).
