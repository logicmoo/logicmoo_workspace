/*************************************************************************

         name: montague.pl (Volume 1, Chapter 3)
      version: April 27, 2001
  description: Montague's Rule of Quantification
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(montague,[montague/0,montague/2,montagueTestSuite/0]).

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

montague:-
	readLine(Sentence),
	findall(Sem,parse(Sentence,Sem),Sems),
	printRepresentations(Sems).

montague(Sentence,Sems):-
	findall(Sem,parse(Sentence,Sem),Sems).


/*========================================================================
   Testsuite Predicates
========================================================================*/

montagueTestSuite:-
	nl, write('>>>>> MONTAGUE ON SENTENCE TESTSUITE <<<<< '), nl,
        sentence(Sentence),
        nl, write('Sentence: '), write(Sentence),
	montague(Sentence,Sems),
	printRepresentations(Sems),
        fail.

montagueTestSuite.


/*========================================================================
   Parsing
========================================================================*/

parse(Sentence,Formula):-
	s2(Formula,Sentence,[]).


/*========================================================================
   Semantic Rules
========================================================================*/

combine(s2:B,[s1:([],A)]):-
        betaConvert(A,B).

combine(s2:B,[s1:([bo(NP,I)],A)]):-
        betaConvert(NP@lambda(I,A),B).

combine(s2:C,[s1:([],A),cond,s1:([],B)]):-
        betaConvert((A>B),C).

combine(s2:C,[s1:([bo(NP,I)],A),cond,s1:([],B)]):-
        betaConvert(((NP@lambda(I,A))>B),C).

combine(s2:C,[s1:([],A),cond,s1:([bo(NP,I)],B)]):-
        betaConvert((A>(NP@lambda(I,B))),C).

combine(s2:C,[s1:([bo(NP1,I1)],A),cond,s1:([bo(NP2,I2)],B)]):-
        betaConvert((NP1@lambda(I1,A))>(NP2@lambda(I2,B)),C).

combine(s1:(Q,A@B),[np2:([],A),vp2:(Q,B)]).

combine(np2:(Q,A),[np1:(Q,A)]).
combine(np2:(Q,((B@A)@C)),[np1:(Q,A),coord:B,np1:([],C)]).

combine(np1:([],(A@B)),[det:A,n2:B]).
combine(np1:([bo(A@B,I)],lambda(P,P@I)),[det:A,n2:B]).
combine(np1:([],A),[pn:A]).
combine(np1:([],A),[np:A]).

combine(n2:A,[n1:A]).
combine(n2:((B@A)@C),[n1:A,coord:B,n1:C]).

combine(n1:(A@B),[adj:A,n1:B]).
combine(n1:A,[noun:A]).
combine(n1:(B@A),[noun:A,pp:B]).
combine(n1:(B@A),[noun:A,rc:B]).

combine(vp2:A,[vp1:A]).
combine(vp2:([],(B@A)@C),[vp1:([],A),coord:B,vp1:([],C)]).

combine(vp1:(Q,A),[v2:(Q,A)]).
combine(vp1:(Q,(A@B)),[mod:A,v2:(Q,B)]).

combine(v2:(Q,(A@B)),[cop:A,np2:(Q,B)]).
combine(v2:(Q,(C@(A@B))),[cop:A,neg:C,np2:(Q,B)]).
combine(v2:(Q,A),[v1:(Q,A)]).
combine(v2:(Q,((B@A)@C)),[v1:(Q,A),coord:B,v1:([],C)]).

combine(v1:([],A),[iv:A]).
combine(v1:(Q,(A@B)),[tv:A,np2:(Q,B)]).

combine(pp:(A@B),[prep:A,np2:([],B)]).
combine(rc:(A@B),[relpro:A,vp2:([],B)]).


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

whnpSem(lambda(P,lambda(X,P@X))).

whdetSem(lambda(N,lambda(P,lambda(X,(N@X) & (P@X))))).
