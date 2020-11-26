/*************************************************************************

    File: kellerStorage.pl
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

:- module(kellerStorage,[infix/0,
                         prefix/0,
                         info/0,
                         kellerStorage/0,
			 kellerStorage/2,
			 kellerStorageTestSuite/0]).

:- use_module(readLine,[readLine/1]).

:- use_module(comsemPredicates,[memberList/2,
				appendLists/3,
				selectFromList/3,
				compose/3,
                                infix/0,
                                prefix/0,
				printRepresentations/1]).

:- use_module(alphaConversion,[alphabeticVariants/2]).

:- use_module(betaConversion,[betaConvert/2]).

:- [englishGrammar].

:- [englishLexicon].

:- use_module(sentenceTestSuite,[sentence/2]).

:- [semLexStorage].

:- [semRulesKeller].


/*========================================================================
   Driver Predicates
========================================================================*/

kellerStorage:-
   readLine(Sentence),
   setof(Sem,t([sem:Sem],Sentence,[]),Sems1),
   filterAlphabeticVariants(Sems1,Sems2),
   printRepresentations(Sems2).

kellerStorage(Sentence,Sems2):-
   setof(Sem,t([sem:Sem],Sentence,[]),Sems1),
   filterAlphabeticVariants(Sems1,Sems2).


/*========================================================================
   Test Suite Predicates
========================================================================*/

kellerStorageTestSuite:-
   nl, write('>>>>> KELLER STORAGE ON SENTENCE TEST SUITE <<<<< '), nl,
   sentence(Sentence,Readings),
   format('~nSentence: ~p (~p readings)',[Sentence,Readings]),
   kellerStorage(Sentence,Sems),
   printRepresentations(Sems),
   fail.

kellerStorageTestSuite.


/*========================================================================
   Filter Alphabetic Variants
========================================================================*/

filterAlphabeticVariants(L1,L2):-
   selectFromList(X,L1,L3),
   memberList(Y,L3),
   alphabeticVariants(X,Y), !,
   filterAlphabeticVariants(L3,L2).

filterAlphabeticVariants(L,L).


/*========================================================================
   Quantifier Retrieval
========================================================================*/

sRetrieval([S],S).

sRetrieval([Sem|Store],S):-
   selectFromList(bo([Q|NestedStore],X),Store,TempStore),
   appendLists(NestedStore,TempStore,NewStore),
   sRetrieval([app(Q,lam(X,Sem))|NewStore],S).


/*========================================================================
   Info
========================================================================*/

info:-
   format('~n> ------------------------------------------------------------------- <',[]),
   format('~n> kellerStorage.pl, by Patrick Blackburn and Johan Bos                <',[]),
   format('~n>                                                                     <',[]),
   format('~n> ?- kellerStorage.            - parse a typed-in sentence            <',[]),
   format('~n> ?- kellerStorage(S,F).       - parse a sentence and return formulas <',[]),
   format('~n> ?- kellerStorageTestSuite.   - run the test suite                   <',[]),
   format('~n> ?- infix.                    - switches to infix display mode       <',[]),
   format('~n> ?- prefix.                   - switches to prefix display mode      <',[]),
   format('~n> ?- info.                     - show this information                <',[]),
   format('~n> ------------------------------------------------------------------- <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info.
