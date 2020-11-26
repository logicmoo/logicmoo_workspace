/*************************************************************************

    File: pronounDRT.pl
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

:- module(pronounDRT,[pronounDRT/0,
                      pronounDRT/3,
                      pronounDRTTestSuite/0,
                      infix/0,
                      prefix/0]).

:- use_module(readLine,[readLine/1]).

:- use_module(comsemPredicates,[prefix/0,
                                infix/0,
                                compose/3,
                                memberList/2,
                                printRepresentations/1]).

:- use_module(betaConversionDRT,[betaConvert/2]).

:- use_module(mergeDRT,[mergeDrs/2]).

:- use_module(pronounTestSuite,[discourse/2]).

:- use_module(bindingViolation,[bindingViolationDrs/1]).

:- use_module(sortalCheck,[sortalCheckDrs/1]).

:- [englishGrammar].

:- [englishLexicon].

:- [semLexPronounDRT].

:- [semRulesDRT].


/*========================================================================
   Driver Predicates
========================================================================*/

pronounDRT:-
   readLine(Discourse),
   pronounDRT(Discourse,drs([],[]),DRSs),
   printRepresentations(DRSs).

pronounDRT(Text,Old,New):-
   findall(Sem,Drs^(t([sem:Drs],Text,[]),resolveDrs(merge(Old,Drs),Sem)),New),
   \+ New=[].


/*========================================================================
   Test Suite Predicates
========================================================================*/

pronounDRTTestSuite:-
   nl, write('>>>>> PRONOUN-DRT ON TEST SUITE <<<<< '), nl,
   discourse(Discourse,Readings),
   format('~nDiscourse: ~p (~p readings)',[Discourse,Readings]),
   pronounDRT(Discourse,drs([],[]),DRSs),
   printRepresentations(DRSs),
   fail.

pronounDRTTestSuite.


/*========================================================================
   Pronoun Resolution
========================================================================*/

resolveDrs(B,R):-
   \+ findAlfaDrs(B,_,_,[]-_),
   \+ bindingViolationDrs(B),
   mergeDrs(B,R).

resolveDrs(ADRS,DRS):-
   findAlfaDrs(ADRS,RDRS,Alfa,[]-Re),
   bindAlfa(Re,Alfa,RDRS),
   resolveDrs(RDRS,DRS).


/*========================================================================
   Find First Alfa-DRS (DRSs)
========================================================================*/

findAlfaDrs(alfa(T,B1,B2),alfa(T,R1,B2),Alfa,Re1-Re2):-
   findAlfaDrs(B1,R1,Alfa,Re1-Re2), !. 

findAlfaDrs(alfa(T,B1,B2),B2,alfa(T,M1),Re-Re):-
   mergeDrs(B1,M1).

findAlfaDrs(merge(B1,B),merge(R1,B),Alfa,Re1-Re2):-
   findAlfaDrs(B1,R1,Alfa,Re1-Re2), !.

findAlfaDrs(merge(B1,B2),merge(R1,R2),Alfa,Re1-Re2):-
   findAlfaDrs(B2,R2,Alfa,[r(M1,R1)|Re1]-Re2),
   mergeDrs(B1,M1). 

findAlfaDrs(drs(D,C1),R,Alfa,Re1-Re2):-
   findAlfaConds(C1,C2,Alfa,[r(drs(D,C2),R)|Re1]-Re2).


/*========================================================================
   Find First Alfa-DRS (DRS-Conditions)
========================================================================*/

findAlfaConds([imp(B1,B)|C],[imp(B2,B)|C],Alfa,Re1-Re2):-
   findAlfaDrs(B1,B2,Alfa,Re1-Re2), !.

findAlfaConds([imp(B1,B2)|C],[imp(R1,R2)|C],Alfa,Re1-Re2):-
   findAlfaDrs(B2,R2,Alfa,[r(M1,R1)|Re1]-Re2), !,
   mergeDrs(B1,M1).

findAlfaConds([or(B1,B)|C],[or(B2,B)|C],Alfa,Re1-Re2):-
   findAlfaDrs(B1,B2,Alfa,Re1-Re2), !.

findAlfaConds([or(B,B1)|C],[or(B,B2)|C],Alfa,Re1-Re2):-
   findAlfaDrs(B1,B2,Alfa,Re1-Re2), !.

findAlfaConds([not(B1)|C],[not(B2)|C],Alfa,Re1-Re2):-
   findAlfaDrs(B1,B2,Alfa,Re1-Re2), !.

findAlfaConds([X|C1],[X|C2],Alfa,Re1-Re2):-
   findAlfaConds(C1,C2,Alfa,Re1-Re2).


/*========================================================================
   Resolve Alfa
========================================================================*/

bindAlfa([r(drs(D2,C2),drs(D3,C3))|L],alfa(_,drs([X|D1],C1)),B):-
   memberList(X,D2),
   mergeLists(D1,D2,D3),
   mergeLists(C1,C2,C3),
   dontResolve(L),
   sortalCheckDrs(B).

bindAlfa([r(R,R)|L],Alfa,B):-
   bindAlfa(L,Alfa,B).


/*========================================================================
    Do not resolve remaining of projection path
========================================================================*/

dontResolve([]).

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
   format('~n> pronounDRT.pl, by Patrick Blackburn and Johan Bos                  <',[]),
   format('~n>                                                                    <',[]),
   format('~n> ?- pronounDRT.              - parse a typed-in sentence            <',[]),
   format('~n> ?- pronounDRT(S,Old,New).   - parse a sentence and return DRS      <',[]),
   format('~n> ?- pronounDRTTestSuite.     - run the test suite                   <',[]),
   format('~n> ------------------------------------------------------------------ <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info.
