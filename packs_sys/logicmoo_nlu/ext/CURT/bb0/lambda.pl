/*************************************************************************

         name: lambda.pl (Volume 1, Chapter 2)
      version: April 27, 2001
  description: Semantic Construction with Beta Conversion
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(lambda,[lambda/0,lambda/2,lambdaTestSuite/0]).

:- use_module(readLine,[readLine/1]).

:- use_module(comsemPredicates,[printRepresentations/1,
				compose/3]).

:- ensure_loaded(comsemOperators).

:- use_module(betaConversion,[betaConvert/2]).

:- ensure_loaded(englishGrammar).

:- use_module(sentenceTestSuite,[sentence/1]).


/*========================================================================
   Driver Predicates
========================================================================*/

lambda:-
	readLine(Sentence),
	parse(Sentence,Formula),
	printRepresentations([Formula]).

lambda(Sentence,Formulas):-
	setof(Formula,parse(Sentence,Formula),Formulas).


/*========================================================================
   Test Suite Predicates
========================================================================*/

lambdaTestSuite:-
	nl, write('>>>>> LAMBDA ON SENTENCE TEST SUITE <<<<< '), nl,
        sentence(Sentence),
        nl, write('Sentence: '), write(Sentence),
	lambda(Sentence,Formulas),
	printRepresentations(Formulas),
        fail.

lambdaTestSuite.


/*========================================================================
   Parsing 
========================================================================*/

parse(Sentence,Formula):-
	s2(Formula,Sentence,[]).


/*========================================================================
   Semantic Rules
========================================================================*/

combine(s2:A,[s1:A]).

combine(s2:(A>B),[s1:A,cond,s1:B]).

combine(s1:Converted,[np2:A,vp2:B]):-
	betaConvert(A@B,Converted).

combine(np2:A,[np1:A]).
combine(np2:((B@A)@C),[np1:A,coord:B,np1:C]).

combine(np1:(A@B),[det:A,n2:B]).
combine(np1:A,[pn:A]).
combine(np1:A,[np:A]).

combine(n2:A,[n1:A]).
combine(n2:((B@A)@C),[n1:A,coord:B,n1:C]).

combine(n1:(A@B),[adj:A,n1:B]).
combine(n1:A,[noun:A]).
combine(n1:(B@A),[noun:A,pp:B]).
combine(n1:(B@A),[noun:A,rc:B]).

combine(vp2:A,[vp1:A]).
combine(vp2:((B@A)@C),[vp1:A,coord:B,vp1:C]).

combine(vp1:A,[v2:A]).
combine(vp1:(A@B),[mod:A,v2:B]).

combine(v2:(A@B),[cop:A,np2:B]).
combine(v2:(C@(A@B)),[cop:A,neg:C,np2:B]).
combine(v2:A,[v1:A]).
combine(v2:((B@A)@C),[v1:A,coord:B,v1:C]).

combine(v1:A,[iv:A]).
combine(v1:(A@B),[tv:A,np2:B]).

combine(pp:(A@B),[prep:A,np2:B]).
combine(rc:(A@B),[relpro:A,vp2:B]).


/*========================================================================
   Semantic Macros
========================================================================*/

detSem(uni,lambda(P,lambda(Q,forall(X,(P@X)>(Q@X))))).

detSem(indef,lambda(P,lambda(Q,exists(X,(P@X)&(Q@X))))).

detSem(wh,lambda(P,lambda(Q,lambda(X,(P@X)&(Q@X))))).

nounSem(Sym,lambda(X,Formula)):- 
   compose(Formula,Sym,[X]).

pnSem(Sym,_Gender,lambda(P,P@Sym)).

proSem(_Gender,_Type,lambda(P,P@_)).

npSem(wh,Sym,lambda(Q,lambda(X,Formula&(Q@X)))):-
   compose(Formula,Sym,[X]).

ivSem(Sym,lambda(X,Formula)):- 
   compose(Formula,Sym,[X]).

tvSem(Sym,lambda(K,lambda(Y,K @ lambda(X,Formula)))):- 
   compose(Formula,Sym,[Y,X]).

relproSem(lambda(P,lambda(Q,lambda(X,(P@X)&(Q@X))))).

prepSem(Sym,lambda(K,lambda(P,lambda(Y,(K@lambda(X,F)) & (P@Y))))):-
   compose(F,Sym,[Y,X]).

adjSem(Sym,lambda(P,lambda(X,(F&(P@X))))):-
   compose(F,Sym,[X]).

modSem(neg,lambda(P,lambda(X,~(P@X)))).

coordSem(conj,lambda(X,lambda(Y,lambda(P,(X@P) & (Y@P))))).
coordSem(disj,lambda(X,lambda(Y,lambda(P,(X@P) v (Y@P))))).


