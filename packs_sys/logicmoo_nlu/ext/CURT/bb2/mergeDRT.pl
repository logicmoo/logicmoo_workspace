/*************************************************************************

    File: mergeDRT.pl
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

:- module(mergeDRT,[mergeDrs/2]).

:- use_module(comsemPredicates,[appendLists/3,
                                basicCondition/3]).


/*========================================================================
   DRS-merge
========================================================================*/

mergeDrs(drs(D,C1),drs(D,C2)):-
   mergeDrs(C1,C2).

mergeDrs(lam(X,B1),lam(X,B2)):-
   mergeDrs(B1,B2).

mergeDrs(merge(B1,B2),drs(D3,C3)):-
   mergeDrs(B1,drs(D1,C1)),
   mergeDrs(B2,drs(D2,C2)),
   appendLists(D1,D2,D3),
   appendLists(C1,C2,C3).

mergeDrs([Cond1:F|C1],[Cond2:F|C2]):-
   mergeDrs([Cond1|C1],[Cond2|C2]).

mergeDrs([imp(B1,B2)|C1],[imp(B3,B4)|C2]):-
   mergeDrs(B1,B3),
   mergeDrs(B2,B4),
   mergeDrs(C1,C2).

mergeDrs([or(B1,B2)|C1],[or(B3,B4)|C2]):-
   mergeDrs(B1,B3),
   mergeDrs(B2,B4),
   mergeDrs(C1,C2).

mergeDrs([not(B1)|C1],[not(B2)|C2]):-
   mergeDrs(B1,B2),
   mergeDrs(C1,C2).

mergeDrs([Basic|C1],[Basic|C2]):-
   basicCondition(Basic,_,_),
   mergeDrs(C1,C2).

mergeDrs([],[]).

