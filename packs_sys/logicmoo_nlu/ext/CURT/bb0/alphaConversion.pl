/*************************************************************************

         name: alphaConversion.pl (Chapter 2)
      version: April 25, 2001
  description: Implementation of Alpha-Conversion
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(alphaConversion,[alphaConvert/2,
                           alphabeticVariants/2]).

:- use_module(comsemPredicates,[compose/3,
				memberList/2]).

:- ensure_loaded(comsemOperators).


/*========================================================================
   Alpha Conversion (introducing substitutions)
========================================================================*/

alphaConvert(F1,F2):-
	alphaConvert(F1,[],F2).


/*========================================================================
   Alpha Conversion 
========================================================================*/

alphaConvert(X,Subs,Y):-
        var(X), 
        (
         memberList(sub(Z,Y),Subs),
         X==Z, !
        ;
         Y=X
        ).

alphaConvert(Expression,Subs,exists(Y,F2)):-
	nonvar(Expression),
	Expression = exists(X,F1),
        alphaConvert(F1,[sub(X,Y)|Subs],F2).

alphaConvert(Expression,Subs,forall(Y,F2)):- 
	nonvar(Expression),
	Expression = forall(X,F1),
        alphaConvert(F1,[sub(X,Y)|Subs],F2).

alphaConvert(Expression,Subs,lambda(Y,F2)):-
	nonvar(Expression),
	Expression = lambda(X,F1),
        alphaConvert(F1,[sub(X,Y)|Subs],F2).

alphaConvert(F1,Subs,F2):-
	nonvar(F1),
	\+ F1 = exists(_,_),
	\+ F1 = forall(_,_),
	\+ F1 = lambda(_,_),
        compose(F1,Symbol,Args1),
        alphaConvertList(Args1,Subs,Args2),
        compose(F2,Symbol,Args2).


/*========================================================================
   Alpha Conversion (listwise)
========================================================================*/

alphaConvertList([],_,[]).

alphaConvertList([X|L1],Subs,[Y|L2]):-
        alphaConvert(X,Subs,Y),
        alphaConvertList(L1,Subs,L2).


/*========================================================================
   Alphabetic Variants
========================================================================*/

alphabeticVariants(Term1,Term2):-
	alphaConvert(Term1,Term3),
	alphaConvert(Term2,Term4),
	numbervars(Term3,0,_),
	numbervars(Term4,0,_),
	Term3=Term4.
