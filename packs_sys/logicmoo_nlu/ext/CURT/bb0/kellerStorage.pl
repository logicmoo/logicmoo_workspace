/*************************************************************************

         name: kellerStorage.pl (Volume 1, Chapter 3)
      version: April 27, 2001
  description: Keller Storage Implementation
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(kellerStorage,[kellerStorage/0,
			 kellerStorage/2,
			 kellerStorageTestSuite/0]).

:- ensure_loaded(comsemOperators).

:- use_module(readLine,[readLine/1]).

:- use_module(comsemPredicates,[memberList/2,
				appendLists/3,
				selectFromList/3,
				compose/3,
				printRepresentations/1]).

:- use_module(alphaConversion,[alphabeticVariants/2]).

:- use_module(betaConversion,[betaConvert/2]).

:- [englishGrammar].

:- use_module(sentenceTestSuite,[sentence/1]).


/*========================================================================
   Driver Predicates
========================================================================*/

kellerStorage:-
	readLine(Sentence),
	setof(Sem,parse(Sentence,Sem),Sems1),
	filterAlphabeticVariants(Sems1,Sems2),
	printRepresentations(Sems2).


kellerStorage(Sentence,Sems2):-
	setof(Sem,parse(Sentence,Sem),Sems1),
	filterAlphabeticVariants(Sems1,Sems2).


/*========================================================================
   Test Suite Predicates
========================================================================*/

kellerStorageTestSuite:-
	nl, write('>>>>> KELLER STORAGE ON SENTENCE TEST SUITE <<<<< '), nl,
        sentence(Sentence),
        nl, write('Sentence: '), write(Sentence),
	kellerStorage(Sentence,Sems),
	printRepresentations(Sems),
        fail.

kellerStorageTestSuite.


/*========================================================================
   Filter Alphabetic Variants
========================================================================*/

filterAlphabeticVariants(L1,L2):-
	selectFromList(X,L1,L3),
	memberList(Y,L3),
	alphabeticVariants(X,Y), !,
	filterAlphabeticVariants(L3,L2).

filterAlphabeticVariants(L,L).


/*========================================================================
   Main Predicate
========================================================================*/

parse(Sentence,Formula):-
	s2(Formula,Sentence,[]).


/*========================================================================
   Quantifier Retrieval
========================================================================*/

sRetrieval([S],S).

sRetrieval([Sem|Store],S):-
	selectFromList(bo([Q|NestedStore],X),Store,TempStore),
	appendLists(NestedStore,TempStore,NewStore),
	sRetrieval([Q@lambda(X,Sem)|NewStore],S).


/*========================================================================
   Semantic Rules
========================================================================*/

combine(s2:A,[s1:A]).

combine(s2:(B>C),[s1:B,cond,s1:C]).
	
combine(s1:Converted,[np2:[A|S1],vp2:[B|S2]]):-
	appendLists(S1,S2,S3),
	sRetrieval([A@B|S3],Retrieved),
	betaConvert(Retrieved,Converted).

combine(np2:A,[np1:A]).
combine(np2:[(B@A)@C|S3],[np1:[A|S1],coord:B,np1:[C|S2]]):-
	appendLists(S1,S2,S3).

combine(np1:[lambda(P,P@X),bo([A@B|S],X)],[det:A,n2:[B|S]]).
combine(np1:[A@B|S],[det:A,n2:[B|S]]).
combine(np1:[A],[pn:A]).
combine(np1:[A],[np:A]).

combine(n2:A,[n1:A]).
combine(n2:[(B@A)@C|S3],[n1:[A|S1],coord:B,n1:[C|S2]]):-
	appendLists(S1,S2,S3).

combine(n1:[A@B],[adj:A,n1:[B]]).
combine(n1:[A],[noun:A]).
combine(n1:[B@A|S],[noun:A,pp:[B|S]]).
combine(n1:[B@A|S],[noun:A,rc:[B|S]]).

combine(vp2:A,[vp1:A]).
combine(vp2:[(B@A)@C|S3],[vp1:[A|S1],coord:B,vp1:[C|S2]]):-
	appendLists(S1,S2,S3).

combine(vp1:A,[v2:A]).
combine(vp1:[A@B|S],[mod:A,v2:[B|S]]).

combine(v2:[A@B|S],[cop:A,np2:[B|S]]).
combine(v2:[C@(A@B)|S],[cop:A,neg:C,np2:[B|S]]).
combine(v2:A,[v1:A]).
combine(v2:[(B@A)@C|S3],[v1:[A|S1],coord:B,v1:[C|S2]]):-
	appendLists(S1,S2,S3).

combine(v1:[A],[iv:A]).
combine(v1:[A@B|S],[tv:A,np2:[B|S]]).

combine(pp:[A@B|S],[prep:A,np2:[B|S]]).
combine(rc:[A@B|S],[relpro:A,vp2:[B|S]]).


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
