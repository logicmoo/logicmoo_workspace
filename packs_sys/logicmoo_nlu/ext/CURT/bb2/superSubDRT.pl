/*************************************************************************

    File: superSubDRT.pl
    Copyright (C) 2004 Patrick Blackburn & Johan Bos

    This file is part of BB2, version 1.0 (June 2004).

    BB2 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB2 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB2; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(superSubDRT,[superSubDrs/3]).

:- use_module(mergeDRT,[mergeDrs/2]).


/*========================================================================
   Compute Super- and SubDRS pairs
========================================================================*/

superSubDrs(drs(D,[imp(Sub,_)|C]),Drs-Super,Sub):-
   mergeDrs(merge(Drs,drs(D,C)),Super).

superSubDrs(drs(D,[imp(B,Sub)|C]),Drs-Super,Sub):-
   mergeDrs(merge(merge(drs(D,C),B),Drs),Super).

superSubDrs(drs(D,[imp(B,_)|C]),Drs-Super,Sub):-
   superSubDrs(B,merge(Drs,drs(D,C))-Super,Sub).

superSubDrs(drs(D,[imp(B1,B2)|C]),Drs-Super,Sub):-
   superSubDrs(B2,merge(Drs,merge(drs(D,C),B1))-Super,Sub).

superSubDrs(drs(D,[or(Sub,_)|C]),Drs-Super,Sub):-
   mergeDrs(merge(Drs,drs(D,C)),Super).

superSubDrs(drs(D,[or(_,Sub)|C]),Drs-Super,Sub):-
   mergeDrs(merge(Drs,drs(D,C)),Super).

superSubDrs(drs(D,[or(B,_)|C]),Drs-Super,Sub):-
   superSubDrs(B,merge(Drs,drs(D,C))-Super,Sub).

superSubDrs(drs(D,[or(_,B)|C]),Drs-Super,Sub):-
   superSubDrs(B,merge(Drs,drs(D,C))-Super,Sub).

superSubDrs(drs(D,[not(Sub)|C]),Drs-Super,Sub):-
   mergeDrs(merge(Drs,drs(D,C)),Super).

superSubDrs(drs(D,[not(B)|C]),Drs-Super,Sub):-
   superSubDrs(B,merge(Drs,drs(D,C))-Super,Sub).

superSubDrs(drs(D,[Cond|C]),Drs-Super,Sub):-
   superSubDrs(drs([],C),Drs-B,Sub),
   mergeDrs(merge(drs(D,[Cond]),B),Super).

