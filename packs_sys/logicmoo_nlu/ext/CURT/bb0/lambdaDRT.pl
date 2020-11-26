/*************************************************************************

         name: lambdaDRT.pl (Volume 2, Chapter 2)
      version: April 27, 2001
  description: Semantic Construction with Beta Conversion for DRT
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(lambdaDRT,[lambdaDRT/0,lambdaDRT/2,lambdaDRTTestSuite/0]).

:- use_module(readLine,[readLine/1]).

:- use_module(comsemPredicates,[compose/3,
				printRepresentations/1]).

:- ensure_loaded(comsemOperators).

:- use_module(drsPredicates,[mergeDrs/2,betaConvertDrs/2]).

:- [englishGrammar].

:- [discourseGrammar].

:- use_module(discourseTestSuite,[discourse/1]).


/*========================================================================
   Driver Predicate
========================================================================*/

lambdaDRT:-
	readLine(Discourse),
	d2(DRS,Discourse,[]),
	printRepresentations([DRS]).

lambdaDRT(Discourse,DRSs):-
        setof(DRS,d2(DRS,Discourse,[]),DRSs).


/*========================================================================
   Testsuite Predicates
========================================================================*/

lambdaDRTTestSuite:-
        format('~n>>>>> LAMBDA DRT ON DISCOURE TEST SUITE <<<<<~n',[]),
        discourse(Discourse),
        format('~nDiscourse: ~p',[Discourse]),
        lambdaDRT(Discourse,DRSs),
        printRepresentations(DRSs),
        fail.

lambdaDRTTestSuite.


/*========================================================================
   Semantic Rules
========================================================================*/

combine(d2:C,[d1:A]):-
	betaConvertDrs(A,B),
	mergeDrs(B,C).

combine(d1:A,[s2:A]). 
combine(d1:merge(A,B),[s2:A,conj,d1:B]).
combine(d1:drs([],[A v B]),[s2:A,disj,d1:B]).
	
combine(s2:A,[s1:A]).
combine(s2:drs([],[A>B]),[s1:A,cond,s1:B]).

combine(s1:(A@B),[np2:A,vp2:B]).

combine(np2:A,[np1:A]).
combine(np2:((B@A)@C),[np1:A,coord:B,np1:C]).

combine(np1:(A@B),[det:A,n2:B]).
combine(np1:A,[pn:A]).
combine(np1:A,[pro:A]).
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

detSem(uni,lambda(P,lambda(Q,drs([],[merge(drs([X],[]),P@X)>(Q@X)])))).

detSem(indef,lambda(P,lambda(Q,merge(merge(drs([X],[]),P@X),Q@X)))).

detSem(wh,lambda(P,lambda(Q,lambda(X,merge(P@X,Q@X))))).

nounSem(Sym,lambda(X,drs([],[Cond]))):- 
   compose(Cond,Sym,[X]).

pnSem(Sym,Gender,lambda(P,merge(drs([X],[Cond,X=Sym]),P@X))):-
   compose(Cond,Gender,[X]).

proSem(Gender,_,lambda(Q,merge(drs([X],[Cond]),Q@X))):-
   compose(Cond,Gender,[X]).

npSem(wh,Sym,lambda(Q,lambda(X,merge(drs([],[Cond]),Q@X)))):-
   compose(Cond,Sym,[X]).

ivSem(Sym,lambda(X,drs([],[Cond]))):- 
   compose(Cond,Sym,[X]).

tvSem(Sym,lambda(K,lambda(Y,K @ lambda(X,drs([],[Cond]))))):- 
   compose(Cond,Sym,[Y,X]).

relproSem(lambda(P1,lambda(P2,lambda(X,merge(P1@X,P2@X))))).

prepSem(Sym,lambda(K,lambda(P,lambda(Y,Drs)))):-   
   Drs=merge(K@lambda(X,drs([],[Cond])),P@Y), 
   compose(Cond,Sym,[Y,X]).

adjSem(Sym,lambda(P,lambda(X,merge(drs([],[Cond]),(P@X))))):-
   compose(Cond,Sym,[X]).

modSem(neg,lambda(P,lambda(X,drs([],[~(P @ X)])))).

coordSem(conj,lambda(X,lambda(Y,lambda(P,merge(X@P,Y@P))))).
coordSem(disj,lambda(X,lambda(Y,lambda(P,drs([],[(X@P) v (Y@P)]))))).


