/*************************************************************************

    File: lambdaDRT.pl
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

:- module(lambdaDRT,[lambdaDRT/0,
                     lambdaDRT/3,
                     lambdaDRTTestSuite/0,
                     infix/0,
                     prefix/0]).

:- use_module(readLine,[readLine/1]).

:- use_module(comsemPredicates,[prefix/0,
                                infix/0,
                                compose/3,
                                printRepresentations/1]).

:- use_module(betaConversionDRT,[betaConvert/2]).

:- use_module(mergeDRT,[mergeDrs/2]).

:- use_module(lambdaTestSuite,[discourse/2]).

:- [englishGrammar].

:- [englishLexicon].

:- [semLexLambdaDRT].

:- [semRulesDRT].


/*========================================================================
   Driver Predicates
========================================================================*/

lambdaDRT:-
   readLine(Discourse),
   lambdaDRT(Discourse,DRSs),
   printRepresentations(DRSs).

lambdaDRT(Discourse,Sems):-
   findall(Sem,(t([sem:Sem],Discourse,[])),Sems),
   \+ Sems=[].

lambdaDRT(Discourse,Old,Sems):-
   findall(Sem,(t([sem:Drs],Discourse,[]),mergeDrs(merge(Old,Drs),Sem)),Sems),
   \+ Sems=[].


/*========================================================================
   Test Suite Predicates
========================================================================*/

lambdaDRTTestSuite:-
   nl, write('>>>>> LAMBDA-DRT ON TEST SUITE <<<<< '), nl,
   discourse(Discourse,Readings),
   format('~nDiscourse: ~p (~p readings)',[Discourse,Readings]),
   lambdaDRT(Discourse,DRSs),
   printRepresentations(DRSs),
   fail.     

lambdaDRTTestSuite.


/*========================================================================
   Info
========================================================================*/

info:-
   format('~n> ------------------------------------------------------------------ <',[]),
   format('~n> lambdaDRT.pl, by Patrick Blackburn and Johan Bos                   <',[]),
   format('~n>                                                                    <',[]),
   format('~n> ?- lambdaDRT.              - parse a typed-in sentence             <',[]),
   format('~n> ?- lambdaDRT(S,D).         - parse a sentence and return DRS       <',[]),
   format('~n> ?- lambdaDRTTestSuite.     - run the test suite                    <',[]),
   format('~n> ------------------------------------------------------------------ <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info.
