/*************************************************************************

    File: betaConversionDRT.pl
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

:- module(betaConversionDRT,[betaConvert/2]).

:- use_module(comsemPredicates,[compose/3,substitute/4]).

:- use_module(alphaConversionDRT,[alphaConvertDRS/2]).


/*========================================================================
   Beta-Conversion (introducing stack)
========================================================================*/

betaConvert(X,Y):-
   betaConvert(X,Y,[]).


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
   alphaConvertDRS(Functor,Converted),
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
   Info
========================================================================*/

info:-
   format('~n> ------------------------------------------------------------------- <',[]),
   format('~n> betaConversion.pl, by Patrick Blackburn and Johan Bos               <',[]),
   format('~n>                                                                     <',[]),
   format('~n> ?- betaConvert(F,C).         - beta-convert a formula               <',[]),
   format('~n> ------------------------------------------------------------------- <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info.
