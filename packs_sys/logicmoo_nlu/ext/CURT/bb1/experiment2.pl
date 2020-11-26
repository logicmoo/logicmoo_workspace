/*************************************************************************

    File: experiment2.pl
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

:- use_module(comsemPredicates,[infix/0,prefix/0]).


/*========================================================================
   Syntax-semantics rules 
========================================================================*/

s(Sem)--> np(X,SemVP,Sem), vp(X,SemVP).

np(X,Scope,Sem)--> det(X,Restr,Scope,Sem), noun(X,Restr). 

np(SemPN,Sem,Sem)--> pn(SemPN).

vp(X,Sem)--> iv(X,Sem).

vp(X,Sem)--> tv(X,Y,SemTV), np(Y,SemTV,Sem). 


/*========================================================================
   Proper Names
========================================================================*/

pn(vincent)--> [vincent].
pn(mia)--> [mia].


/*========================================================================
   Transitive Verbs
========================================================================*/

tv(Y,Z,love(Y,Z))--> [loves].
tv(Y,Z,like(Y,Z))--> [likes].


/*========================================================================
   Intransitive Verbs
========================================================================*/

iv(Y,snort(Y))--> [snorts].
iv(Y,walk(Y))--> [walks].


/*========================================================================
   Determiners
========================================================================*/

det(X,Restr,Scope,some(X,and(Restr,Scope)))--> [a].
det(X,Restr,Scope,all(X,imp(Restr,Scope)))--> [every].


/*========================================================================
   Nouns
========================================================================*/

noun(X,woman(X))--> [woman].
noun(X,footmassage(X))--> [foot,massage].
