/*************************************************************************

         name: betaConversion.pl (Chapter 2)
      version: July 13, 2001
  description: Implementation of Beta-Conversion
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(betaConversion,[betaConvertTestSuite/0,
			  betaConvert/2]).

:- ensure_loaded(comsemOperators).

:- use_module(comsemPredicates,[compose/3,substitute/4]).

:- use_module(alphaConversion,[alphaConvert/2]).

:- use_module(betaConversionTestSuite,[expression/2]).


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

betaConvert(Expression,Z,Stack):- 
   nonvar(Expression),
   Expression = (X@Y),
   nonvar(X),
   alphaConvert(X,C),
   betaConvert(C,Z,[Y|Stack]).

betaConvert(Expression,Result,[A|Stack]):-
   nonvar(Expression),
   Expression = lambda(X,Formula),
   substitute(A,X,Formula,New), %%% Substitution-Based
%  A=X, New=Formula,            %%% Unification-Based
   betaConvert(New,Result,Stack).

betaConvert(Formula,Result,[]):-
   nonvar(Formula),
   \+ (Formula = (X@_), nonvar(X)),
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
   format('~n~nExpression: ~p~n  Expected: ~p',[Expression,Expected]),
   betaConvert(Expression,Converted),
   format('~n Converted: ~p',[Converted]),
   fail.

betaConvertTestSuite.
