/*************************************************************************

    File: worldKnowledge.pl
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

:- module(worldKnowledge,[worldKnowledge/3]).

/*========================================================================
   Axioms for World Knowledge
========================================================================*/


%% Nothing can have itself, and no object can be part of two different objects
%%
worldKnowledge(have,2,Axiom):- 
   Axiom = and(not(some(X,have(X,X))),all(X,all(Y,
               imp(some(Z,and(object(X),and(object(Y),and(object(Z),
                   and(have(X,Z),have(Y,Z)))))),eq(X,Y))))).



worldKnowledge(husband,1,Axiom):- 
   Axiom = all(A,imp(husband(A),married(A))).

worldKnowledge(wife,1,Axiom):- 
   Axiom = all(A,imp(wife(A),married(A))).

worldKnowledge(of,2,Axiom):- 
   Axiom = all(A,all(B,imp(of(A,B),have(B,A)))).
