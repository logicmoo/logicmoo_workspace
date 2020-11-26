/*************************************************************************

    File: experiment3.pl
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

:- use_module(betaConversion,[betaConvert/2]).

:- use_module(comsemPredicates,[infix/0,prefix/0]).


/*========================================================================
   Syntax-semantics rules 
========================================================================*/

s(app(NP,VP))--> np(NP), vp(VP).

np(app(Det,Noun))--> det(Det), noun(Noun). 

np(PN)--> pn(PN).

vp(IV)--> iv(IV).

vp(app(TV,NP))--> tv(TV), np(NP). 


/*========================================================================
   Proper Names
========================================================================*/

pn(lam(P,app(P,vincent)))--> [vincent].

pn(lam(P,app(P,mia)))--> [mia].


/*========================================================================
   Transitive Verbs
========================================================================*/

tv(lam(X,lam(Y,app(X,lam(Z,love(Y,Z))))))--> [loves].
tv(lam(X,lam(Y,app(X,lam(Z,like(Y,Z))))))--> [likes].


/*========================================================================
   Intransitive Verbs
========================================================================*/

iv(lam(Y,snort(Y)))--> [snorts].
iv(lam(Y,walk(Y)))--> [walks].


/*========================================================================
   Determiners
========================================================================*/

det(lam(P,lam(Q,all(X,imp(app(P,X),app(Q,X))))))--> [every].

det(lam(P,lam(Q,some(X,and(app(P,X),app(Q,X))))))--> [a].


/*========================================================================
   Nouns
========================================================================*/

noun(lam(X,woman(X)))--> [woman].

noun(lam(X,footmassage(X)))--> [foot,massage].


