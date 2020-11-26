/*************************************************************************

    File: backgroundKnowledge.pl
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

:- module(backgroundKnowledge,[backgroundKnowledge/2]).

:- use_module(comsemPredicates,[memberList/2,
				basicFormula/1,
				compose/3]).

:- use_module(lexicalKnowledge,[lexicalKnowledge/3]).

:- use_module(worldKnowledge,[worldKnowledge/3]).

:- use_module(situationalKnowledge,[situationalKnowledge/1]).


/*========================================================================
   Declare Dynamic Predicates
========================================================================*/

:- dynamic knowledge/1.


/*========================================================================
   Add Background Knowledge to Formula
========================================================================*/

backgroundKnowledge(Formula1,Formula2):-
   formula2symbols(Formula1,Symbols),
   backgroundKnowledge(Formula1,Symbols,Formula2).


/*========================================================================
   Add Background Knowledge until fixed point is reached
========================================================================*/

backgroundKnowledge(Formula1,Symbols1,Formula3):-
   computeBackgroundKnowledge(Symbols1,Formula2),
   formula2symbols(and(Formula1,Formula2),Symbols2),
   (
      sort(Symbols1,Sorted), %%% No new symbols, hence fixed point
      sort(Symbols2,Sorted), %%% is reached!
      !,
      Formula3=Formula2
   ;
      backgroundKnowledge(and(Formula1,Formula2),Symbols2,Formula3)
   ).


/*========================================================================
   Computing World Knowledge
========================================================================*/

computeBackgroundKnowledge(Symbols,Formula):-
   retractall(knowledge(_)),
   findall(_,(lexicalKnowledge(Symbol,Arity,F),
              memberList(symbol(Symbol,Arity),Symbols),
              assert(knowledge(F))),_),
   findall(_,(worldKnowledge(Symbol,Arity,F),
              memberList(symbol(Symbol,Arity),Symbols),
              assert(knowledge(F))),_),
   findall(_,(situationalKnowledge(F),
              assert(knowledge(F))),_),
   knowledge2formula(Formula).


/*========================================================================
   Put all selected knowledge in one big formula
========================================================================*/

knowledge2formula(F):-
   knowledge(F1),
   retract(knowledge(F1)),
   (
      knowledge(_), !,
      knowledge2formula(F2),
      F=and(F1,F2)
   ; 
      F=F1
   ).
   

/*========================================================================
   Derive all Symbols from a Formula
========================================================================*/

formula2symbols(F,S):-
   formula2symbols(F,[],S).

formula2symbols(X,S,S):-
   var(X), !.

formula2symbols(X,S,[symbol(X,0)|S]):-
   atom(X),
   \+ memberList(symbol(X,0),S).

formula2symbols(X,S,S):-
   atom(X),
   memberList(symbol(X,0),S).

formula2symbols(some(_,F),S1,S2):-
   formula2symbols(F,S1,S2).

formula2symbols(all(_,F),S1,S2):-
   formula2symbols(F,S1,S2).

formula2symbols(not(F),S1,S2):-
   formula2symbols(F,S1,S2).

formula2symbols(and(F1,F2),S1,S3):-
   formula2symbols(F1,S1,S2),
   formula2symbols(F2,S2,S3).

formula2symbols(or(F1,F2),S1,S3):-
   formula2symbols(F1,S1,S2),
   formula2symbols(F2,S2,S3).

formula2symbols(imp(F1,F2),S1,S3):-
   formula2symbols(F1,S1,S2),
   formula2symbols(F2,S2,S3).

formula2symbols(eq(X,Y),S1,S3):-
   formula2symbols(X,S1,S2),
   formula2symbols(Y,S2,S3).

formula2symbols(F,S1,[symbol(Symbol,1)|S2]):- 
   basicFormula(F),
   compose(F,Symbol,[Arg]),
   \+ memberList(symbol(Symbol,1),S1),
   formula2symbols(Arg,S1,S2).

formula2symbols(F,S1,S2):- 
   basicFormula(F),
   compose(F,Symbol,[Arg]),
   memberList(symbol(Symbol,1),S1),
   formula2symbols(Arg,S1,S2).

formula2symbols(F,S1,[symbol(Symbol,2)|S3]):- 
   basicFormula(F),
   compose(F,Symbol,[Arg1,Arg2]),
   \+ memberList(symbol(Symbol,2),S1),
   formula2symbols(Arg1,S1,S2),
   formula2symbols(Arg2,S2,S3).

formula2symbols(F,S1,S3):- 
   basicFormula(F),
   compose(F,Symbol,[Arg1,Arg2]),
   memberList(symbol(Symbol,2),S1),
   formula2symbols(Arg1,S1,S2),
   formula2symbols(Arg2,S2,S3).
