/*************************************************************************

         name: cooperStorage.pl (Volume 1, Chapter 3)
      version: April 27, 2001
  description: Cooper Storage Implementation
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(cooperStorage,[cooperStorage/0,
			 cooperStorage/2,
			 cooperStorageTestSuite/0]).

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

cooperStorage:-
	readLine(Sentence),
	setof(Sem,parse(Sentence,Sem),Sems1),
	filterAlphabeticVariants(Sems1,Sems2),
	printRepresentations(Sems2).

cooperStorage(Sentence,Sems2):-
	setof(Sem,parse(Sentence,Sem),Sems1),
	filterAlphabeticVariants(Sems1,Sems2).


/*========================================================================
   Test Suite Predicates
========================================================================*/

cooperStorageTestSuite:-
	nl, write('>>>>> COOPER STORAGE ON SENTENCE TEST SUITE <<<<< '), nl,
        sentence(Sentence),
        nl, write('Sentence: '), write(Sentence),
	cooperStorage(Sentence,Sems),
	printRepresentations(Sems),
        fail.

cooperStorageTestSuite.


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
   Parsing
========================================================================*/

parse(Sentence,Formula):-
	s2(Formula,Sentence,[]).

/*========================================================================
   Semantic Rules
========================================================================*/

combine(s2:A,[s1:A]).
combine(s2:(A>B),[s1:A,cond,s1:B]).

combine(s1:S,[np2:[A|S1],vp2:[B|S2]]):-
	appendLists(S1,S2,S3),
	sRetrieval([A@B|S3],Retrieved),
	betaConvert(Retrieved,S).

combine(np2:A,[np1:A]).
combine(np2:[(B@A)@C|S3],[np1:[A|S1],coord:B,np1:[C|S2]]):-
	appendLists(S1,S2,S3).

combine(np1:[A],[pn:A]).
combine(np1:[lambda(P,P@X),bo(A@B,X)|S],[det:A,n2:[B|S]]).
combine(np1:[A@B|S],[det:A,n2:[B|S]]).
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
   Quantifier Retrieval
========================================================================*/

sRetrieval([S],S).

sRetrieval([Sem|Store],S):-
	selectFromList(bo(Q,X),Store,NewStore),
	sRetrieval([Q@lambda(X,Sem)|NewStore],S).


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

