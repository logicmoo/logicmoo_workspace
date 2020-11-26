/*************************************************************************

         name: usrPredicates.pl (Chapter 3)
      version: July 23, 2001
  description: Additional predicates for working with USRs
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(usrPredicates,[mergeUSR/2,
			 betaConvertUSR/2,
			 alphaConvertUSR/2]).

:- ensure_loaded(comsemOperators).

:- use_module(comsemPredicates,[memberList/2,
				appendLists/3,
				compose/3,
				substitute/4]).


/*========================================================================
   Merge for Underspecified Semantic Representations
========================================================================*/

mergeUSR(usr(D,L,C),usr(D,L,C)).

mergeUSR(merge(U1,U2),usr(D3,L3,C3)):-
   mergeUSR(U1,usr(D1,L1,C1)),
   mergeUSR(U2,usr(D2,L2,C2)),
   appendLists(D1,D2,D3),
   appendLists(L1,L2,L3),
   appendLists(C1,C2,C3).


/*========================================================================
   Beta-Conversion (introducing stack)
========================================================================*/

betaConvertUSR(X,Y):-
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
   alphaConvertUSR(X,C),
   betaConvert(C,Z,[Y|Stack]).

betaConvert(Expression,Result,[A|Stack]):-
   nonvar(Expression),
   Expression = lambda(X,Formula),
   substitute(A,X,Formula,New),
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
   betaConvertUSR(Formula,Result),
   betaConvertList(Others,ResultOthers).


/*========================================================================
   Alpha Conversion (introducing substitutions)
========================================================================*/

alphaConvertUSR(F1,F2):-
	alphaConvert(F1,[]-_,F2).


/*========================================================================
   Alpha Conversion 
========================================================================*/

alphaConvert(X,Subs-Subs,Y):-
        var(X), !,
        (
         memberList(sub(Z,Y),Subs),
         X==Z, !
        ;
         Y=X
        ).

alphaConvert(lambda(X,F1),Subs1-Subs2,lambda(Y,F2)):- !,
        alphaConvert(F1,[sub(X,Y)|Subs1]-Subs2,F2).

alphaConvert(usr([X|L1],F1,C1),Subs1-Subs2,usr([Y|L2],F2,C2)):- !,
        alphaConvert(usr(L1,F1,C1),[sub(X,Y)|Subs1]-Subs2,usr(L2,F2,C2)).

alphaConvert(usr([],F1,C1),Subs1-Subs3,usr([],F2,C2)):- !,
	alphaConvertList(F1,Subs1-Subs2,F2),
	alphaConvertList(C1,Subs2-Subs3,C2).

alphaConvert(merge(U1,U2),Subs1-Subs3,merge(U3,U4)):- !,
	alphaConvert(U1,Subs1-Subs2,U3),
	alphaConvert(U2,Subs2-Subs3,U4).

alphaConvert(F1,Subs1-Subs2,F2):- 
        compose(F1,Symbol,Args1),
        alphaConvertList(Args1,Subs1-Subs2,Args2),
        compose(F2,Symbol,Args2).


/*========================================================================
   Alpha Conversion (listwise)
========================================================================*/

alphaConvertList([],Subs-Subs,[]).

alphaConvertList([X|L1],Subs1-Subs3,[Y|L2]):-
        alphaConvert(X,Subs1-Subs2,Y),
        alphaConvertList(L1,Subs2-Subs3,L2).

