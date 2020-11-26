/*************************************************************************

    File: sortalCheck.pl
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

:- module(sortalCheck,[sortalCheckDrs/1]).

:- use_module(comsemPredicates,[compose/3,
                                memberList/2,
                                basicCondition/3]).

:- use_module(lexicalKnowledge,[lexicalKnowledge/3]).


/*========================================================================
   Sortal Check (DRSs)
========================================================================*/

sortalCheckDrs(drs([],C)):-
   sortalCheckConds(C).

sortalCheckDrs(drs([X|D],C)):-
   sortalCheckRef(C,X,[]),
   sortalCheckDrs(drs(D,C)).

sortalCheckDrs(merge(B1,B2)):-
   sortalCheckDrs(B1),
   sortalCheckDrs(B2).

sortalCheckDrs(alfa(_,B1,B2)):-
   sortalCheckDrs(B1),
   sortalCheckDrs(B2).


/*========================================================================
   Sortal Check (DRS-Conditions)
========================================================================*/

sortalCheckConds([]).

sortalCheckConds([Cond:_|C]):-
   sortalCheckConds([Cond|C]).

sortalCheckConds([not(B)|C]):-
   sortalCheckDrs(B),
   sortalCheckConds(C).

sortalCheckConds([imp(B1,B2)|C]):-
   sortalCheckDrs(B1),
   sortalCheckDrs(B2),
   sortalCheckConds(C).

sortalCheckConds([or(B1,B2)|C]):-
   sortalCheckDrs(B1),
   sortalCheckDrs(B2),
   sortalCheckConds(C).

sortalCheckConds([Basic|C]):-
   basicCondition(Basic,_,_),
   sortalCheckConds(C).


/*========================================================================
   Sortal Check (referents)
========================================================================*/

sortalCheckRef([],_,L):-
   consistent(L).

sortalCheckRef([Cond:_|C],Ref,L):-
   sortalCheckRef([Cond|C],Ref,L).

sortalCheckRef([not(_)|C],Ref,L):-
   sortalCheckRef(C,Ref,L).

sortalCheckRef([or(_,_)|C],Ref,L):-
   sortalCheckRef(C,Ref,L).

sortalCheckRef([imp(_,_)|C],Ref,L):-
   sortalCheckRef(C,Ref,L).

sortalCheckRef([Basic|C],Ref,L):-
   basicCondition(Basic,Symbol,[X]),
   (
      X==Ref, 
      sortalCheckRef(C,Ref,[Symbol:1|L])
   ;
      \+ X==Ref,
      sortalCheckRef(C,Ref,L)
   ).

sortalCheckRef([eq(X,Y)|C],Ref,L):-
   (
      var(X), 
      var(Y),
      sortalCheckRef(C,Ref,L)
   ;  
      var(X),
      atom(Y),
      X==Ref, 
      sortalCheckRef(C,Ref,[Y:0|L])
   ;  
      var(X),
      atom(Y),
      \+ X==Ref, 
      sortalCheckRef(C,Ref,L)
   ;
      atom(X),
      var(Y),
      Y==Ref, 
      sortalCheckRef(C,Ref,[X:0|L])
   ;
      atom(X),
      var(Y),
      \+ Y==Ref, 
      sortalCheckRef(C,Ref,L)
   ;
      atom(X),
      atom(Y),
      sortalCheckRef(C,Ref,L)
   ).

sortalCheckRef([Basic|C],Ref,L):-
   basicCondition(Basic,Sym,Args),
   \+ Sym = eq,
   \+ Args = [_],
   sortalCheckRef(C,Ref,L).



/*========================================================================
   Consistency Check
========================================================================*/

consistent(L1):- 
   addSuperConcepts(L1,L2),
   \+ conflict(L2).


addSuperConcepts(C1,C2):-
   addSuperConcepts(C1,[],C3),
   (
      length(C1,Len),
      length(C3,Len), !,
      C3=C2
   ;
      addSuperConcepts(C3,C2)
   ).
   

addSuperConcepts([],L,L).

addSuperConcepts([X:1|L1],Accu,L2):-
   lexicalKnowledge(X,1,Axiom),
   Axiom = all(A,imp(Concept,SuperConcept)),
   basicCondition(Concept,X,[A]),
   basicCondition(SuperConcept,Y,[A]), 
   \+ memberList(Y:1,L1),
   \+ memberList(Y:1,Accu),
   !,
   addSuperConcepts(L1,[X:1,Y:1|Accu],L2).

addSuperConcepts([X:0|L1],Accu,L2):-
   lexicalKnowledge(X,0,Axiom),
   Axiom = all(A,imp(eq(A,X),SuperConcept)),
   basicCondition(SuperConcept,Y,[A]), 
   \+ memberList(Y:1,L1),
   \+ memberList(Y:1,Accu),
   !,
   addSuperConcepts(L1,[X:0,Y:1|Accu],L2).

addSuperConcepts([X|L1],Accu,L2):-
   addSuperConcepts(L1,[X|Accu],L2).


conflict(L):-
   memberList(X:1,L),
   lexicalKnowledge(X,1,Axiom),
   Axiom = all(A,imp(Concept,not(SuperConcept))),
   basicCondition(Concept,X,[A]),
   basicCondition(SuperConcept,Y,[A]),
   memberList(Y:1,L). 
   

