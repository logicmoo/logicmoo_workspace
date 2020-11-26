/*************************************************************************

         name: betaConversion2.pl (Chapter 2)
      version: July 13, 2001
  description: Implementation of Beta-Conversion
      authors: Patrick Blackburn & Johan Bos
  modified by: Christof Rumpf, November 14, 2002
 
*************************************************************************/

:- module(betaConversion2,[betaConvertTestSuite/0,
			  betaConvert/2]).

:- ensure_loaded(comsemOperators).

:- use_module(comsemPredicates,[compose/3,substitute/4]).

:- use_module(alphaConversion2,[alphaConvert/2]).

:- use_module(betaConversionTestSuite,[expression/2]).

:- use_module(auxlib,[myformat/2]).

/*========================================================================
   Beta-Conversion (introducing stack)
========================================================================*/

betaConvert(X,Y):-
   betaConvert(X,Y,[]).


/*========================================================================
   Beta-Conversion (core stuff)
========================================================================*/

betaConvert(X,X,[]):-
   var(X), !.

betaConvert((X@Y),Result,Stack):-
   nonvar(X), !,
   alphaConvert(X,C),
   betaConvert(C,Result,[Y|Stack]).

betaConvert(lambda(X,Formula),Result,[A|Stack]):- !,
   substitute(A,X,Formula,New), %%% Substitution-Based
%  A=X, New=Formula,            %%% Unification-Based
   betaConvert(New,Result,Stack).

betaConvert(Formula,Result,[]):-
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
   myformat('~n~n Expression: ~p',[Expression]),
   myformat('~n   Expected: ~p',[Expected]),
   betaConvert(Expression,Converted),
   myformat('~n  Converted: ~p',[Converted]),
   fail.

betaConvertTestSuite.
