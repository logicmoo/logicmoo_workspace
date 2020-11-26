/*************************************************************************

    File: pluggingAlgorithm.pl
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

/*========================================================================
     Module Declaration
========================================================================*/

:- module(pluggingAlgorithm,[plugUSR/2]).

:- use_module(comsemPredicates,[compose/3,
                                memberList/2,
                                selectFromList/3]).


/*========================================================================
     Declaration of dynamic predicates
========================================================================*/

:- dynamic plug/2, leq/2, hole/1, label/1.
:- dynamic some/3, all/3, que/4.
:- dynamic not/2, or/3, imp/3, and/3.
:- dynamic pred1/3, pred2/4, eq/3.


/*========================================================================
   Main Plugging Predicate (all pluggings)
========================================================================*/

plugUSR(USR,Sem):-
   numbervars(USR,0,_),          % 1 Skolemise USR
   initUSR, 
   assertUSR(USR),               % 2 Break down and assert USR
   top(Top),  
   findall(H,hole(H),Holes),
   findall(L,
           (label(L),\+ parent(_,L)),
           Labels),
   plugHoles(Holes,Labels,[]),   % 3 Calculate a plugging
   url2srl(Top,Sem).             % 4 Construct SRL formula


/*========================================================================
   Asserting immediate dominance relations to the Prolog database
========================================================================*/
    
parent(A,B):- imp(A,B,_).
parent(A,B):- imp(A,_,B).
parent(A,B):- or(A,B,_).
parent(A,B):- or(A,_,B).
parent(A,B):- and(A,B,_).
parent(A,B):- and(A,_,B).
parent(A,B):- not(A,B).
parent(A,B):- all(A,_,B).
parent(A,B):- some(A,_,B).
parent(A,B):- que(A,_,B,_).
parent(A,B):- que(A,_,_,B).
parent(A,B):- plug(A,B).


/*========================================================================
   Transitive Closure of Dominance
========================================================================*/

dom(X,Y):- dom([],X,Y).

dom(L,X,Y):- 
   parent(X,Y), 
   \+ memberList(parent(X,Y),L).

dom(L,X,Y):- 
   leq(Y,X), 
   \+ memberList(leq(Y,X),L).

dom(L,X,Z):- 
   parent(X,Y), 
   \+ memberList(parent(X,Y),L),
   dom([parent(X,Y)|L],Y,Z).

dom(L,X,Z):- 
   leq(Y,X), 
   \+ memberList(leq(Y,X),L),
   dom([leq(Y,X)|L],Y,Z).


/*========================================================================
   Plugging Holes with Labels
========================================================================*/

plugHoles([],_,Plugs):-
   admissiblePlugging(Plugs).

plugHoles([H|Holes],Labels1,Plugs):-
   admissiblePlugging(Plugs),
   selectFromList(L,Labels1,Labels2),
   plugHoles(Holes,Labels2,[plug(H,L)|Plugs]).


/*========================================================================
   Check whether plugging is propers
========================================================================*/

admissiblePlugging(Plugs):-
   retractall(plug(_,_)), 
   findall(X,(memberList(X,Plugs),assert(X)),_),
   \+ dom(A,A),
   \+ ( parent(A,B), parent(A,C), \+ B=C, dom(B,D), dom(C,D)).
   
 
/*========================================================================
    Top of a USR
========================================================================*/

top(X):- dom(X,_), \+ dom(_,X), !.


/*========================================================================
   From USRs to FOLs
========================================================================*/

url2srl(H,F):-
   hole(H),
   plug(H,L),
   url2srl(L,F).

url2srl(L,all(X,F)):-
   all(L,X,H),
   url2srl(H,F).

url2srl(L,some(X,F)):-
   some(L,X,H),
   url2srl(H,F).

url2srl(L,que(X,F1,F2)):-
   que(L,X,H1,H2),
   url2srl(H1,F1),
   url2srl(H2,F2).

url2srl(L,imp(F1,F2)):-
   imp(L,H1,H2),
   url2srl(H1,F1),
   url2srl(H2,F2).

url2srl(L,and(F1,F2)):-
   and(L,H1,H2),
   url2srl(H1,F1),
   url2srl(H2,F2).

url2srl(L,or(F1,F2)):-
   or(L,H1,H2),
   url2srl(H1,F1),
   url2srl(H2,F2).

url2srl(L,not(F)):-
   not(L,H),
   url2srl(H,F).

url2srl(L,eq(X,Y)):-
   eq(L,X,Y).

url2srl(L,F):-
   pred1(L,Symbol,Arg),
   compose(F,Symbol,[Arg]).

url2srl(L,F):-
   pred2(L,Symbol,Arg1,Arg2),
   compose(F,Symbol,[Arg1,Arg2]).


/*========================================================================
   Assert USR to Prolog database
========================================================================*/

assertUSR(some(_,F)):-   
    assertUSR(F).

assertUSR(and(F1,F2)):-
    assertUSR(F1),
    assertUSR(F2).

assertUSR(F):- 
    \+ F=and(_,_),
    \+ F=some(_,_),
    assert(F).


/*========================================================================
   Initialisation 
========================================================================*/

initUSR:-
   retractall(hole(_)), retractall(label(_)), retractall(leq(_,_)), 
   retractall(some(_,_,_)), retractall(all(_,_,_)), retractall(que(_,_,_,_)), 
   retractall(pred1(_,_,_)), retractall(pred2(_,_,_,_)), retractall(eq(_,_,_)), 
   retractall(and(_,_,_)), retractall(or(_,_,_)), retractall(not(_,_)), 
   retractall(plug(_,_)), retractall(imp(_,_,_)).

