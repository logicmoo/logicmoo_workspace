/*************************************************************************

    File: betaConversion.pl
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

:- module(betaConversion,[info/0,
                          infix/0,
                          prefix/0, 
                          betaConvertTestSuite/0,
			  betaConvert/2]).

:- use_module(comsemPredicates,[infix/0,
                                prefix/0,
                                compose/3]).

:- use_module(alphaConversion,[alphaConvert/2,
                               alphabeticVariants/2]).

:- use_module(betaConversionTestSuite,[expression/2]).


/*========================================================================
   Beta-Conversion (introducing stack)
========================================================================*/

betaConvert(X,Y):-
   betaConvert(X,Y,[]).


/*========================================================================
   Beta-Conversion (comment-in for tracing)
========================================================================*/

%betaConvert(X,_,S):-
%   nl, write(expre), tab(1), print(X),
%   nl, write(stack), tab(1), print(S),
%   fail.


/*========================================================================
   Beta-Conversion (core stuff)
========================================================================*/

betaConvert(X,Y,[]):-
   var(X),
   Y=X.

betaConvert(Expression,Result,Stack):- 
   nonvar(Expression),
   Expression = app(Functor,Argument),
   nonvar(Functor),
                                     % to suppress alpha-conversion:
   alphaConvert(Functor,Converted),  %  comment-out this line 
%   Functor=Converted,               %  comment-in this line 

   betaConvert(Converted,Result,[Argument|Stack]).

betaConvert(Expression,Result,[X|Stack]):-
   nonvar(Expression),
   Expression = lam(X,Formula),
   betaConvert(Formula,Result,Stack).

betaConvert(Formula,Result,[]):-
   nonvar(Formula),
   \+ (Formula = app(X,_), nonvar(X)),
   compose(Formula,Functor,Formulas),
   betaConvertList(Formulas,ResultFormulas),
   compose(Result,Functor,ResultFormulas).


/*========================================================================
   Beta-Convert a list
========================================================================*/

betaConvertList([],[]).

betaConvertList([Formula|Others],[Result|ResultOthers]):-
   betaConvert(Formula,Result),
   betaConvertList(Others,ResultOthers).


/*========================================================================
   Prove all formulas from the test suite
========================================================================*/

betaConvertTestSuite:-
   format('~n>>>>> BETA CONVERSION ON TEST SUITE <<<<<~n',[]), 
   expression(Expression,Expected),
   format('~n~nExpression: ~p~nExpected: ~p',[Expression,Expected]),
   betaConvert(Expression,Converted,[]),
   format('~nConverted: ~p',[Converted]),
   compareResults(Expected,Converted,Result),
   format('~nResult: ~p',[Result]),
   fail.

betaConvertTestSuite.


/*========================================================================
   Compare Results of the Test Suite
========================================================================*/

compareResults(A,B,Result):-
    (
     alphabeticVariants(A,B),
     !,
     Result=ok
    ;
     Result=error
    ).


/*========================================================================
   Info
========================================================================*/

info:-
   format('~n> ------------------------------------------------------------------- <',[]),
   format('~n> betaConversion.pl, by Patrick Blackburn and Johan Bos               <',[]),
   format('~n>                                                                     <',[]),
   format('~n> ?- betaConvert(F,C).         - beta-convert a formula               <',[]),
   format('~n> ?- betaConvertTestSuite.     - run the test suite                   <',[]),
   format('~n> ?- infix.                    - switches to infix display mode       <',[]),
   format('~n> ?- prefix.                   - switches to prefix display mode      <',[]),
   format('~n> ?- info.                     - shows this information               <',[]),
   format('~n> ------------------------------------------------------------------- <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info.
