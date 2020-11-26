/*************************************************************************

    File: menu.pl
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



book1:-

   format('~n~`*t~70|~n', []),
   format('*~t*~70|~n', []),
   format('*~t Representation and Inference for Natural Language ~t*~70|~n',[]),
   format('*~t A First Course in Computational Semantics ~t*~70|~n',[]),
   format('*~t*~70|~n', []),
   format('*~t By Patrick Blackburn & Johan Bos ~t*~70|~n',[]),
   format('*~t*~70|~n', []),
   format('~`*t~70|~n', []),
   format('*~t*~70|~n', []),
   format('* ~a ~t*~70|~n', [' <1> Chapter 1: First-Order Logic']),
   format('* ~a ~t*~70|~n', [' <2> Chapter 2: Lambda Calculus']),
   format('* ~a ~t*~70|~n', [' <3> Chapter 3: Underspecified Representations']),
   format('* ~a ~t*~70|~n', [' <4> Chapter 4: Propositional Inference']),
   format('* ~a ~t*~70|~n', [' <5> Chapter 5: First-Order Inference']),
   format('* ~a ~t*~70|~n', [' <6> Chapter 6: Putting Everything Together']),
   format('*~t*~70|~n', []),
   format('~`*t~70|~n', []),
   format('Enter your choice (1-6) and hit <Return>~n',[]).


chapter1:-

   format('~n~`*t Chapter 1: First-Order Logic ~`*t~70|~n',[]),
   format('*~t*~70|~n', []),
   format('* ~a ~t*~70|~n', [' <1> Model Checker 1']),
   format('* ~a ~t*~70|~n', [' <2> Model Checker 2']),
   format('*~t*~70|~n', []),
   format('* ~a ~t*~70|~n', [' <0> Main Menu']),
   format('*~t*~70|~n', []),
   format('~`*t~70|~n', []),
   format('Enter your choice (1-2) and hit <Return>~n',[]).

chapter2:-

   format('~n~`*t Chapter 2: Lambda Calculus ~`*t~70|~n',[]),
   format('*~t*~70|~n', []),
   format('* ~a ~t*~70|~n', [' <1> Experiment 1 (experiment1.pl)']),
   format('* ~a ~t*~70|~n', [' <2> Experiment 2 (experiment2.pl)']),
   format('* ~a ~t*~70|~n', [' <3> Experiment 3 (experiment3.pl)']),
   format('* ~a ~t*~70|~n', [' <4> Beta Conversion (betaConversion.pl)']),
   format('* ~a ~t*~70|~n', [' <5> Lambda Calculus (lambda.pl)']),
   format('*~t*~70|~n', []),
   format('* ~a ~t*~70|~n', [' <0> Main Menu']),
   format('*~t*~70|~n', []),
   format('~`*t~70|~n', []),
   format('Enter your choice (1-5) and hit <Return>~n',[]).

chapter3:-

   format('~n~`*t Chapter 3: Underspecified Representations ~`*t~70|~n',[]),
   format('*~t*~70|~n', []),
   format('* ~a ~t*~70|~n', [' <1> Cooper Storage (cooperStorage.pl)']),
   format('* ~a ~t*~70|~n', [' <2> Keller Storage (kellerStorage.pl)']),
   format('* ~a ~t*~70|~n', [' <3> Hole Semantics (holeSemantics.pl)']),
   format('*~t*~70|~n', []),
   format('* ~a ~t*~70|~n', [' <0> Main Menu']),
   format('*~t*~70|~n', []),
   format('~`*t~70|~n', []),
   format('Enter your choice (1-3) and hit <Return>~n',[]).

chapter4:-

   format('~n~`*t Chapter 4: Propositional Inference ~`*t~70|~n',[]),
   format('*~t*~70|~n', []),
   format('* ~a ~t*~70|~n', [' <1> Propositional Tableau (propTableau.pl)']),
   format('* ~a ~t*~70|~n', [' <2> Propositional Resolution (propResolution.pl)']),
   format('* ~a ~t*~70|~n', [' <3> Conjunctive Normal Form (cnf.pl)']),
   format('*~t*~70|~n', []),
   format('* ~a ~t*~70|~n', [' <0> Main Menu']),
   format('*~t*~70|~n', []),
   format('~`*t~70|~n', []),
   format('Enter your choice (1-3) and hit <Return>~n',[]).

chapter5:-

   format('~n~`*t Chapter 5: First-Order Inference ~`*t~70|~n',[]),
   format('*~t*~70|~n', []),
   format('* ~a ~t*~70|~n', [' <1> Free-Variable Tableau (freeVarTabl.pl)']),
   format('* ~a ~t*~70|~n', [' <2> First-Order Resolution (foResolution.pl)']),
   format('* ~a ~t*~70|~n', [' <3> Theorem Provers and Model Builders (callInference.pl)']),
   format('*~t*~70|~n', []),
   format('* ~a ~t*~70|~n', [' <0> Main Menu']),
   format('*~t*~70|~n', []),
   format('~`*t~70|~n', []),
   format('Enter your choice (1-3) and hit <Return>~n',[]).

chapter6:-

   format('~n~`*t Chapter 6: Putting Everything Together ~`*t~70|~n',[]),
   format('*~t*~70|~n', []),
   format('* ~a ~t*~70|~n', [' <1> Baby Curt (babyCurt.pl)']),
   format('* ~a ~t*~70|~n', [' <2> Rugrat Curt (rugratCurt.pl)']),
   format('* ~a ~t*~70|~n', [' <3> Clever Curt (cleverCurt.pl)']),
   format('* ~a ~t*~70|~n', [' <4> Sensitive Curt (sensitiveCurt.pl)']),
   format('* ~a ~t*~70|~n', [' <5> Scrupulous Curt (scrupulousCurt.pl)']),
   format('* ~a ~t*~70|~n', [' <6> Knowledgeable Curt (knowledgeableCurt.pl)']),
   format('* ~a ~t*~70|~n', [' <7> Helpful Curt (helpfulCurt.pl)']),
   format('*~t*~70|~n', []),
   format('* ~a ~t*~70|~n', [' <0> Main Menu']),
   format('*~t*~70|~n', []),
   format('~`*t~70|~n', []),
   format('Enter your choice (1-7) and hit <Return>~n',[]).



menu:-
   book1,
   getChar(X),
   ( X=1, !, loop1(S), loop(S)
   ; X=2, !, loop2(S), loop(S)
   ; X=3, !, loop3(S), loop(S)
   ; X=4, !, loop4(S), loop(S)
   ; X=5, !, loop5(S), loop(S)
   ; X=6, !, loop6(S), loop(S)
   ; menu ).

loop(S):-
   ( S=run, !, menu
   ; S=quit, ! ).

loop1(S):-
   chapter1,
   getChar(X),
   ( X=1, !, S=quit, [modelChecker1]
   ; X=2, !, S=quit, [modelChecker2]
   ; S=run ).

loop2(S):-
   chapter2,
   getChar(X),
   ( X=1, !, S=quit, [experiment1]
   ; X=2, !, S=quit, [experiment2]
   ; X=3, !, S=quit, [experiment3]
   ; X=4, !, S=quit, [betaConversion]
   ; X=5, !, S=quit, [lambda]
   ; S=run ).

loop3(S):-
   chapter3,
   getChar(X),
   ( X=1, !, S=quit, [cooperStorage]
   ; X=2, !, S=quit, [kellerStorage]
   ; X=3, !, S=quit, [holeSemantics]
   ; S=run ).

loop4(S):-
   chapter4,
   getChar(X),
   ( X=1, !, S=quit, [propTableau]
   ; X=2, !, S=quit, [propResolution]
   ; X=3, !, S=quit, [cnf]
   ; S=run ).

loop5(S):-
   chapter5,
   getChar(X),
   ( X=1, !, S=quit, [freeVarTabl]
   ; X=2, !, S=quit, [foResolution]
   ; X=3, !, S=quit, [callInference]
   ; S=run ).

loop6(S):-
   chapter6,
   getChar(X),
   ( X=1, !, S=quit, [babyCurt]
   ; X=2, !, S=quit, [rugratCurt]
   ; X=3, !, S=quit, [cleverCurt]
   ; X=4, !, S=quit, [sensitiveCurt]
   ; X=5, !, S=quit, [scrupulousCurt]
   ; X=6, !, S=quit, [knowledgeableCurt]
   ; X=7, !, S=quit, [helpfulCurt]
   ; S=run ).

getChar(Y):-
   get0(X),
   ( X=10, !, getChar(Y)
   ; name(Y,[X]), peek ).

peek:-
   peek_code(X),
   ( X=10, !, get0(_)
   ; get0(_), peek ).


:- menu.
