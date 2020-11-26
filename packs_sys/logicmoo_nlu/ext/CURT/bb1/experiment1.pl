/*************************************************************************

    File: experiment1.pl
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
   Syntax-Semantics Rules 
========================================================================*/

s(Sem)--> np(Sem), vp(SemVP), 
   {
    arg(1,SemVP,X),
    arg(1,Sem,X),
    arg(2,Sem,Matrix),
    arg(2,Matrix,SemVP)
   }.
            
s(Sem)--> np(SemNP), vp(Sem), 
   {
    arg(1,Sem,SemNP)
   }.

np(Sem)--> pn(Sem).

np(Sem)--> det(Sem), noun(SemNoun), 
   {
    arg(1,SemNoun,X),
    arg(1,Sem,X),
    arg(2,Sem,Matrix),
    arg(1,Matrix,SemNoun)
   }.

vp(Sem)--> iv(Sem).

vp(Sem)--> tv(SemTV), np(Sem), 
   {
    arg(2,SemTV,X),
    arg(1,Sem,X),
    arg(2,Sem,Matrix),
    arg(2,Matrix,SemTV)
   }.

vp(Sem)--> tv(Sem), np(SemNP), 
   { 
    arg(2,Sem,SemNP)
   }.


/*========================================================================
   Proper Names
========================================================================*/

pn(vincent)--> [vincent].
pn(mia)--> [mia].


/*========================================================================
   Transitive Verbs
========================================================================*/

tv(love(_,_))--> [loves].
tv(like(_,_))--> [likes].


/*========================================================================
   Intransitive Verbs
========================================================================*/

iv(snort(_))--> [snorts].
iv(walk(_))--> [walks].


/*========================================================================
   Determiners
========================================================================*/

det(some(_,and(_,_)))--> [a].
det(all(_,imp(_,_)))--> [every].


/*========================================================================
   Nouns
========================================================================*/

noun(woman(_))--> [woman].
noun(footmassage(_))--> [foot,massage].





