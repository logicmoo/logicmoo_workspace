/*************************************************************************

    File: situationalKnowledge.pl
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

:- module(situationalKnowledge,[situationalKnowledge/1]).

:- dynamic situationalKnowledge/1.

/*========================================================================
   Axioms for Situational Knowledge
========================================================================*/

%% There are at least two different cars
%%
%situationalKnowledge(Axiom):-
%   Axiom = some(X,some(Y,and(car(X),and(car(Y),not(eq(X,Y)))))).
