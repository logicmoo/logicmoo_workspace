/*************************************************************************

         name: alphaConversion2.pl (Chapter 2)
      version: April 25, 2001
  description: Implementation of Alpha-Conversion
      authors: Patrick Blackburn & Johan Bos
  modified by: Christof Rumpf, November 14, 2002
 
*************************************************************************/

:- module(alphaConversion2,[alphaConvert/2,
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
         X==Z
        ;
         Y=X
        ), !.

alphaConvert(exists(X,F1),Subs,exists(Y,F2)):- !,
        alphaConvert(F1,[sub(X,Y)|Subs],F2).

alphaConvert(forall(X,F1),Subs,forall(Y,F2)):- !, 
        alphaConvert(F1,[sub(X,Y)|Subs],F2).

alphaConvert(lambda(X,F1),Subs,lambda(Y,F2)):- !,
        alphaConvert(F1,[sub(X,Y)|Subs],F2).

alphaConvert(F1,Subs,F2):-
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
