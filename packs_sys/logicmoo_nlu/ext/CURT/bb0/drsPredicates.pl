/*************************************************************************

         name: drsPredicates.pl (Volume 2, Chapter 2)
      version: July 23, 2001
  description: Additional predicates for working with DRSs
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(drsPredicates,[mergeDrs/2,
			 betaConvertDrs/2,
			 alphaConvertDrs/2,
			 superSubDrs/3]).

:- ensure_loaded(comsemOperators).

:- use_module(comsemPredicates,[memberList/2,
				appendLists/3,
				compose/3,
				basicFormula/1,
				substitute/4]).

/*========================================================================
   DRS-merge
========================================================================*/

mergeDrs(drs(D,C1),drs(D,C2)):-
   mergeDrs(C1,C2).

mergeDrs(merge(B1,B2),drs(D3,C3)):-
   mergeDrs(B1,drs(D1,C1)),
   mergeDrs(B2,drs(D2,C2)),
   appendLists(D1,D2,D3),
   appendLists(C1,C2,C3).

mergeDrs([B1 > B2|C1],[B3 > B4|C2]):- !,
   mergeDrs(B1,B3),
   mergeDrs(B2,B4),
   mergeDrs(C1,C2).

mergeDrs([B1 v B2|C1],[B3 v B4|C2]):- !,
   mergeDrs(B1,B3),
   mergeDrs(B2,B4),
   mergeDrs(C1,C2).

mergeDrs([~ B1|C1],[~ B2|C2]):- !,
   mergeDrs(B1,B2),
   mergeDrs(C1,C2).

mergeDrs([question(X,B1,B2)|C1],[question(X,B3,B4)|C2]):- !,
   mergeDrs(B1,B3),
   mergeDrs(B2,B4),
   mergeDrs(C1,C2).

mergeDrs([C|C1],[C|C2]):-
   mergeDrs(C1,C2).

mergeDrs([],[]).




/*========================================================================
   Beta-Conversion (introducing stack)
========================================================================*/

betaConvertDrs(X,Y):-
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
   alphaConvertDrs(X,C),
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
   betaConvertDrs(Formula,Result),
   betaConvertList(Others,ResultOthers).


/*========================================================================
   Alpha Conversion (introducing substitutions)
========================================================================*/

alphaConvertDrs(F1,F2):-
	alphaConvertDrs(F1,[]-_,F2).


/*========================================================================
   Alpha Conversion 
========================================================================*/

alphaConvertDrs(X,Subs-Subs,Y):-
        var(X), 
        (
         memberList(sub(Z,Y),Subs),
         X==Z, !
        ;
         Y=X
        ).

alphaConvertDrs(X,Subs-Subs,Y):-
	atom(X),
	Y=X.

alphaConvertDrs(Expression,Subs1-Subs2,lambda(Y,B2)):-
	nonvar(Expression),
	Expression = lambda(X,B1),
        alphaConvertDrs(B1,[sub(X,Y)|Subs1]-Subs2,B2).

alphaConvertDrs(Expression,Subs1-Subs2,drs([Y|L2],C2)):-
	nonvar(Expression),
	Expression = drs([X|L1],C1),
        alphaConvertDrs(drs(L1,C1),[sub(X,Y)|Subs1]-Subs2,drs(L2,C2)).

alphaConvertDrs(Expression,Subs-Subs,drs([],C2)):-
	nonvar(Expression),
	Expression = drs([],C1),
	alphaConvertConditions(C1,Subs,C2).

alphaConvertDrs(Expression,Subs1-Subs3,merge(B3,B4)):-
	nonvar(Expression),
	Expression = merge(B1,B2),
	alphaConvertDrs(B1,Subs1-Subs2,B3),
	alphaConvertDrs(B2,Subs2-Subs3,B4).

alphaConvertDrs(Expression,Subs1-Subs4,alfa(Y,Type,B3,B4)):-
	nonvar(Expression),
	Expression = alfa(X,Type,B1,B2),
	alphaConvertDrs(B1,Subs1-Subs2,B3),
	alphaConvertDrs(X,Subs2-Subs3,Y),
	alphaConvertDrs(B2,Subs3-Subs4,B4).

alphaConvertDrs(Expression,Subs1-Subs1,B3 @ B4):-
	nonvar(Expression),
	Expression = (B1 @ B2),
	alphaConvertDrs(B1,Subs1-_,B3),
	alphaConvertDrs(B2,Subs1-_,B4).


/*========================================================================
   Alpha Conversion Conditions
========================================================================*/

alphaConvertConditions([],_,[]).

alphaConvertConditions([X|L1],Subs,[Y|L2]):-
        alphaConvertCondition(X,Subs,Y),
        alphaConvertConditions(L1,Subs,L2).

alphaConvertCondition(question(X,B1,B2),Subs1,question(Y,B3,B4)):- 
	alphaConvertDrs(X,[sub(X,Y)|Subs1]-Subs2,Y),
	alphaConvertDrs(B1,Subs2-Subs3,B3),
	alphaConvertDrs(B2,Subs3-_,B4).

alphaConvertCondition(B1 > B2,Subs1,B3 > B4):- 
	alphaConvertDrs(B1,Subs1-Subs2,B3),
	alphaConvertDrs(B2,Subs2-_,B4).

alphaConvertCondition(B1 v B2,Subs1,B3 v B4):- 
	alphaConvertDrs(B1,Subs1-_,B3),
	alphaConvertDrs(B2,Subs1-_,B4).

alphaConvertCondition(~ B1,Subs1,~ B2):- 
	alphaConvertDrs(B1,Subs1-_,B2).

alphaConvertCondition(F1,Subs1,F2):-
	basicFormula(F1),
        compose(F1,Symbol,[X1]),
        alphaConvertDrs(X1,Subs1-_,Y2),
        compose(F2,Symbol,[Y2]).

alphaConvertCondition(F1,Subs1,F2):-
	basicFormula(F1),
        compose(F1,Symbol,[X1,X2]),
        alphaConvertDrs(X1,Subs1-_,Y1),
	alphaConvertDrs(X2,Subs1-_,Y2),
        compose(F2,Symbol,[Y1,Y2]).


/*========================================================================
   Compute super- and subordinated DRSs
========================================================================*/

superSubDrs(drs(D,[Sub > _|C]),Drs-Super,Sub):-
   mergeDrs(merge(Drs,drs(D,C)),Super).

superSubDrs(drs(D,[B > Sub|C]),Drs-Super,Sub):-
   mergeDrs(merge(merge(drs(D,C),B),Drs),Super).

superSubDrs(drs(D,[B1 > B2|C]),Drs-Super,Sub):-
   superSubDrs(B2,merge(Drs,merge(merge(drs(D,C),B1),B2))-Super,Sub).

superSubDrs(drs(D,[question(Q,Sub,_)|C]),Drs-Super,Sub):-
   mergeDrs(merge(Drs,drs([Q|D],C)),Super).

superSubDrs(drs(D,[question(Q,B,Sub)|C]),Drs-Super,Sub):-
   mergeDrs(merge(merge(drs([Q|D],C),B),Drs),Super).

superSubDrs(drs(D,[question(Q,B1,B2)|C]),Drs-Super,Sub):-
   superSubDrs(B2,merge(Drs,merge(merge(drs([Q|D],C),B1),B2))-Super,Sub).

superSubDrs(drs(D,[Sub v _|C]),Drs-Super,Sub):-
   mergeDrs(merge(Drs,drs(D,C)),Super).

superSubDrs(drs(D,[_ v Sub|C]),Drs-Super,Sub):-
   mergeDrs(merge(Drs,drs(D,C)),Super).

superSubDrs(drs(D,[B v _|C]),Drs-Super,Sub):-
   superSubDrs(B,merge(Drs,merge(drs(D,C),B))-Super,Sub).

superSubDrs(drs(D,[_ v B|C]),Drs-Super,Sub):-
   superSubDrs(B,merge(Drs,merge(drs(D,C),B))-Super,Sub).

superSubDrs(drs(D,[~ Sub|C]),Drs-Super,Sub):-
   mergeDrs(merge(Drs,drs(D,C)),Super).

superSubDrs(drs(D,[~ B|C]),Drs-Super,Sub):-
   superSubDrs(B,merge(Drs,merge(drs(D,C),B))-Super,Sub).

superSubDrs(drs(D,[Cond|C]),Drs-Super,Sub):-
   superSubDrs(drs([],C),Drs-B,Sub),
   mergeDrs(merge(drs(D,[Cond]),B),Super).


