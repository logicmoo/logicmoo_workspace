/*************************************************************************

    File: presupDRT.pl
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

:- module(presupDRT,[presupDRT/0,
                     presupDRT/3,
                     presupDRTTestSuite/0,
                     infix/0,
                     prefix/0]).

:- use_module(readLine,[readLine/1]).

:- use_module(comsemPredicates,[prefix/0,
                                infix/0,
                                compose/3,
                                memberList/2,
                                reverseList/2,
                                removeFirst/3,
                                printRepresentations/1]).

:- use_module(betaConversionDRT,[betaConvert/2]).

:- use_module(mergeDRT,[mergeDrs/2]).

:- use_module(presupTestSuite,[discourse/2]).

:- use_module(bindingViolation,[bindingViolationDrs/1]).

:- use_module(freeVarCheck,[freeVarCheckDrs/1]).

:- use_module(sortalCheck,[sortalCheckDrs/1]).

:- [englishGrammar].

:- [englishLexicon].

:- [semLexPresupDRT].

:- [semRulesDRT].

'^'(V,G):-forall((G),writeln(V)).

/*========================================================================
   Driver Predicates
========================================================================*/

presupDRT:-
   readLine(Discourse),
   presupDRT(Discourse,drs([],[]),DRSs),
   printRepresentations(DRSs).

presupDRT(Text,Old,New):-
   findall(Sem,Drs^(t([sem:Drs],Text,[]),resolveDrs(merge(Old,Drs),Sem)),New),
   \+ New=[].


/*========================================================================
   Test Suite Predicates
========================================================================*/

presupDRTTestSuite:-
   nl, write('>>>>> PRESUP-DRT ON TEST SUITE <<<<<'), nl,
   discourse(Discourse,Readings),
   format('~nDiscourse: ~p (~p readings)',[Discourse,Readings]),
   presupDRT(Discourse,drs([],[]),DRSs),
   printRepresentations(DRSs),
   fail.

presupDRTTestSuite.


/*========================================================================
   Pronoun Resolution
========================================================================*/

resolveDrs(B,R):-
   \+ findAlfaDrs(B,_,_,_,[]-_),
   \+ bindingViolationDrs(B),
   mergeDrs(B,R).

resolveDrs(ADRS,DRS):-
   findAlfaDrs(ADRS,RDRS,alfa(Type,Alfa),Ac,[]-Bi),
   resolveAlfa(Alfa,Type,Ac,Bi,RDRS),
   resolveDrs(RDRS,DRS).


/*========================================================================
   Find First Alfa-DRS (DRSs)
========================================================================*/

findAlfaDrs(alfa(T,B1,B2),alfa(T,R1,B2),Alfa,Ac,Bi1-Bi2):-
   findAlfaDrs(B1,R1,Alfa,Ac,Bi1-Bi2), !. 

findAlfaDrs(alfa(T,B1,B2),merge(A,B2),alfa(T,M1),[a(A)],Bi-Bi):-
   mergeDrs(B1,M1). 

findAlfaDrs(merge(B1,B),merge(R1,B),Alfa,Ac,Bi1-Bi2):-
   findAlfaDrs(B1,R1,Alfa,Ac,Bi1-Bi2), !.

findAlfaDrs(merge(B1,B2),merge(R1,R2),Alfa,Ac,Bi1-Bi2):-
   findAlfaDrs(B2,R2,Alfa,Ac,[r(M1,R1)|Bi1]-Bi2),
   mergeDrs(B1,M1). 

findAlfaDrs(drs(D,C1),merge(A,R),Alfa,[a(A)|Ac],Bi1-Bi2):-
   findAlfaConds(C1,C2,Alfa,Ac,[r(drs(D,C2),R)|Bi1]-Bi2).


/*========================================================================
   Find First Alfa-DRS (DRS-Conditions)
========================================================================*/

findAlfaConds([imp(B1,B)|C],[imp(B2,B)|C],Alfa,Ac,Bi1-Bi2):-
   findAlfaDrs(B1,B2,Alfa,Ac,Bi1-Bi2), !.

findAlfaConds([imp(B1,B2)|C],[imp(merge(R1,A),R2)|C],Alfa,[a(A)|Ac],Bi1-Bi2):-
   findAlfaDrs(B2,R2,Alfa,Ac,[r(M1,R1)|Bi1]-Bi2), !,
   mergeDrs(B1,M1).

findAlfaConds([or(B1,B)|C],[or(B2,B)|C],Alfa,Ac,Bi1-Bi2):-
   findAlfaDrs(B1,B2,Alfa,Ac,Bi1-Bi2), !.

findAlfaConds([or(B,B1)|C],[or(B,B2)|C],Alfa,Ac,Bi1-Bi2):-
   findAlfaDrs(B1,B2,Alfa,Ac,Bi1-Bi2), !.

findAlfaConds([not(B1)|C],[not(B2)|C],Alfa,Ac,Bi1-Bi2):-
   findAlfaDrs(B1,B2,Alfa,Ac,Bi1-Bi2), !.

findAlfaConds([X|C1],[X|C2],Alfa,Ac,Bi1-Bi2):-
   findAlfaConds(C1,C2,Alfa,Ac,Bi1-Bi2).


/*========================================================================
   Resolve Alfa
========================================================================*/

resolveAlfa(Alfa,_,Ac,Bi,B):-
   bindAlfa(Bi,Alfa),
   dontResolve(Ac),
   sortalCheckDrs(B).

resolveAlfa(Alfa,Type,Ac,Bi,B):-
   accommodateAlfa(Ac,Type,Alfa),
   dontResolve(Bi),
   freeVarCheckDrs(B).


/*------------------------------------------------------------------------
   Binding
------------------------------------------------------------------------*/

bindAlfa([r(drs(D2,C2),drs(D3,C3))|P],drs([X|D1],C1)):-
   memberList(X,D2),
   mergeLists(D1,D2,D3),
   mergeLists(C1,C2,C3),
   dontResolve(P).

bindAlfa([r(R,R)|P],Alfa):-
   bindAlfa(P,Alfa).

bindAlfa([a(drs([],[]))|P],Alfa):-
   bindAlfa(P,Alfa).


/*------------------------------------------------------------------------
   Accommodation
------------------------------------------------------------------------*/

accommodateAlfa(P1,nam,Alfa):-
   removeFirst(a(Alfa),P1,P2),
   dontResolve(P2).

accommodateAlfa([a(Alfa)|P],def,Alfa):-
   dontResolve(P).

accommodateAlfa([r(R,R)|P],Type,Alfa):-
   accommodateAlfa(P,Type,Alfa).

accommodateAlfa([a(drs([],[]))|P],def,Alfa):-
   accommodateAlfa(P,def,Alfa).


/*========================================================================
    Do not resolve remaining of projection path
========================================================================*/

dontResolve([]).

dontResolve([a(drs([],[]))|L]):- 
   dontResolve(L).

dontResolve([r(X,X)|L]):- 
   dontResolve(L).


/*========================================================================
   Merge Lists - Check for Duplicates
========================================================================*/

mergeLists([],L,L).

mergeLists([X|R],L1,L2):-
   memberList(Y,L1), X==Y, !,
   mergeLists(R,L1,L2).

mergeLists([X|R],L1,[X|L2]):-
   mergeLists(R,L1,L2).



/*========================================================================
   Info
========================================================================*/

info:-
   format('~n> ------------------------------------------------------------------ <',[]),
   format('~n> presupDRT.pl, by Patrick Blackburn and Johan Bos                   <',[]),
   format('~n>                                                                    <',[]),
   format('~n> ?- presupDRT.              - parse a typed-in sentence             <',[]),
   format('~n> ?- presupDRT(S,Old,New).   - parse a sentence and return DRS       <',[]),
   format('~n> ?- presupDRTTestSuite.     - run the test suite                    <',[]),
   format('~n> ------------------------------------------------------------------ <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info.
