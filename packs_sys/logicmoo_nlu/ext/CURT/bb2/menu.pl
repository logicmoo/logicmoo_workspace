/*************************************************************************

    File: menu.pl
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



book2:-

   format('~n~`*t~70|~n', []),
   format('*~t*~70|~n', []),
   format('*~t Working with  Discourse Representation Theory ~t*~70|~n',[]),
   format('*~t An Advanced Course in Computational Semantics ~t*~70|~n',[]),
   format('*~t*~70|~n', []),
   format('*~t By Patrick Blackburn & Johan Bos ~t*~70|~n',[]),
   format('*~t*~70|~n', []),
   format('~`*t~70|~n', []),
   format('*~t*~70|~n', []),
   format('* ~a ~t*~70|~n', [' <1> Chapter 1: Discourse Representation Theory']),
   format('* ~a ~t*~70|~n', [' <2> Chapter 2: Building Discourse Representations']),
   format('* ~a ~t*~70|~n', [' <3> Chapter 3: Pronoun Resolution']),
   format('* ~a ~t*~70|~n', [' <4> Chapter 4: Presupposition Projection']),
%   format('* ~a ~t*~70|~n', [' <5> Chapter 5: Centering and Preferences']),
%   format('* ~a ~t*~70|~n', [' <6> Chapter 6: Optimizing Inference']),
   format('*~t*~70|~n', []),
   format('~`*t~70|~n', []),
   format('Enter your choice (1-4) and hit <Return>~n',[]).


chapter1:-

   format('~n~`*t Chapter 1: Discourse Representation Theory ~`*t~70|~n',[]),
   format('*~t*~70|~n', []),
   format('* ~a ~t*~70|~n', [' <1> From DRT to First-Order Logic (drs2fol.pl)']),
   format('*~t*~70|~n', []),
   format('* ~a ~t*~70|~n', [' <0> Main Menu']),
   format('*~t*~70|~n', []),
   format('~`*t~70|~n', []),
   format('Enter your choice (0-1) and hit <Return>~n',[]).

chapter2:-

   format('~n~`*t Chapter 2: Building Discourse Representations ~`*t~70|~n',[]),
   format('*~t*~70|~n', []),
   format('* ~a ~t*~70|~n', [' <1> Lambda DRT (lambdaDRT.pl)']),
   format('* ~a ~t*~70|~n', [' <2> CURT (curtDRT.pl)']),
   format('*~t*~70|~n', []),
   format('* ~a ~t*~70|~n', [' <0> Main Menu']),
   format('*~t*~70|~n', []),
   format('~`*t~70|~n', []),
   format('Enter your choice (0-2) and hit <Return>~n',[]).

chapter3:-

   format('~n~`*t Chapter 3: Pronoun Resolution ~`*t~70|~n',[]),
   format('*~t*~70|~n', []),
   format('* ~a ~t*~70|~n', [' <1> Pronoun Resolution (pronounDRT.pl)']),
   format('* ~a ~t*~70|~n', [' <2> CURT (curtPDRT.pl)']),
   format('*~t*~70|~n', []),
   format('* ~a ~t*~70|~n', [' <0> Main Menu']),
   format('*~t*~70|~n', []),
   format('~`*t~70|~n', []),
   format('Enter your choice (0-2) and hit <Return>~n',[]).

chapter4:-

   format('~n~`*t Chapter 4: Presupposition Projection ~`*t~70|~n',[]),
   format('*~t*~70|~n', []),
   format('* ~a ~t*~70|~n', [' <1> Presupposition Projection (presupDRT.pl)']),
   format('* ~a ~t*~70|~n', [' <2> CURT (curtPPDRT.pl)']),
   format('*~t*~70|~n', []),
   format('* ~a ~t*~70|~n', [' <0> Main Menu']),
   format('*~t*~70|~n', []),
   format('~`*t~70|~n', []),
   format('Enter your choice (1-3) and hit <Return>~n',[]).

menu:-
   book2,
   getChar(X),
   ( X=1, !, loop1(S), loop(S)
   ; X=2, !, loop2(S), loop(S)
   ; X=3, !, loop3(S), loop(S)
   ; X=4, !, loop4(S), loop(S)
%   ; X=5, !, loop5(S), loop(S)
%   ; X=6, !, loop6(S), loop(S)
   ; menu ).

loop(S):-
   ( S=run, !, menu
   ; S=quit, ! ).

loop1(S):-
   chapter1,
   getChar(X),
   ( X=1, !, S=quit, [drs2fol]
   ; S=run ).

loop2(S):-
   chapter2,
   getChar(X),
   ( X=1, !, S=quit, [lambdaDRT]
   ; X=2, !, S=quit, [curtDRT]
   ; S=run ).

loop3(S):-
   chapter3,
   getChar(X),
   ( X=1, !, S=quit, [pronounDRT]
   ; X=2, !, S=quit, [curtPDRT]
   ; S=run ).

loop4(S):-
   chapter4,
   getChar(X),
   ( X=1, !, S=quit, [presupDRT]
   ; X=2, !, S=quit, [curtPPDRT]
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
