/*************************************************************************

    File: holeSemantics.pl
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

:- module(holeSemantics,[info/0,
                         prefix/0,
                         infix/0,
                         holeSemantics/0,
			 holeSemantics/2,
			 holeSemanticsTestSuite/0]).

:- use_module(readLine,[readLine/1]).

:- use_module(comsemPredicates,[infix/0,
                                prefix/0,
                                printRepresentations/1]).

:- use_module(sentenceTestSuite,[sentence/2]).

:- use_module(pluggingAlgorithm,[plugUSR/2]).

:- use_module(betaConversion,[betaConvert/2]).

:- [englishGrammar].

:- [englishLexicon].

:- [semLexHole].

:- [semRulesHole].


/*========================================================================
   Driver Predicates
========================================================================*/

holeSemantics:-
   readLine(Sentence),
   t([sem:USR],Sentence,[]),   
   printRepresentations([USR]),
   setof(Sem,plugUSR(USR,Sem),Sems),
   printRepresentations(Sems).

holeSemantics(Sentence,Sems):-
   t([sem:USR],Sentence,[]),   
   setof(Sem,plugUSR(USR,Sem),Sems).


/*========================================================================
   Testsuite Predicates
========================================================================*/

holeSemanticsTestSuite:-
   nl, write('>>>>> HOLE SEMANTICS ON SENTENCE TEST SUITE <<<<< '), nl,
   sentence(Sentence,Readings),
   format('~nSentence: ~p (~p readings)',[Sentence,Readings]),
   holeSemantics(Sentence,Sems),
   printRepresentations(Sems),
   fail.

holeSemanticsTestSuite.


/*========================================================================
   Info
========================================================================*/

info:-
   format('~n> ------------------------------------------------------------------- <',[]),
   format('~n> holeSemantics.pl, by Patrick Blackburn and Johan Bos                <',[]),
   format('~n>                                                                     <',[]),
   format('~n> ?- holeSemantics.            - parse a typed-in sentence            <',[]),
   format('~n> ?- holeSemantics(S,F).       - parse a sentence and return formulas <',[]),
   format('~n> ?- holeSemanticsTestSuite.   - run the test suite                   <',[]),
   format('~n> ?- infix.                    - switches to infix display mode       <',[]),
   format('~n> ?- prefix.                   - switches to prefix display mode      <',[]),
   format('~n> ?- info.                     - show this information                <',[]),
   format('~n> ------------------------------------------------------------------- <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info.
