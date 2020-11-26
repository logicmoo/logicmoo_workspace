/**********************************************************************

         name: presupScoreDRT.pl
      version: July 30, 2001
  description: Presupposition Projection with Score Calculation
      authors: Patrick Blackburn & Johan Bos

**********************************************************************/


:- module(presupScoreDRT,[presupScoreDRT/0,
			  presupScoreDRT/2,
			  presupScoreDRT/3,
			  presupScoreDRTTestSuite/0]).

:- use_module(readLine,[readLine/1]).

:- use_module(comsemPredicates,[compose/3,
				printRepresentations/1]).

:- ensure_loaded(comsemOperators).

:- use_module(drsPredicates,[mergeDrs/2,betaConvertDrs/2]).

:- use_module(resolvePresupScore,[projectDrs/2,accommodate/2]).

:- [englishGrammar].

:- [discourseGrammar].

:- use_module(discourseTestSuite,[discourse/1]).


/*========================================================================
   Driver Predicate
========================================================================*/

presupScoreDRT:-
	readLine(Discourse),
	setof(DRS,d2(DRS,Discourse,[]),DRSs),
	printRepresentations(DRSs).

presupScoreDRT(Discourse,DRSs):-
        setof(DRS,d2(DRS,Discourse,[]),DRSs).

presupScoreDRT(Discourse,OldDrs,NewDrs):-
	d1(CurrentDrs,Discourse,[]),
	combine(d2:NewDrs,[d1:merge(OldDrs,CurrentDrs)]).


/*========================================================================
   Testsuite Predicates
========================================================================*/

presupScoreDRTTestSuite:-
        format('~n>>>>> PRESUP DRT ON DISCOURE TEST SUITE <<<<<~n',[]),
        discourse(Discourse),
        format('~nDiscourse: ~p',[Discourse]),
        presupScoreDRT(Discourse,DRSs),
        printRepresentations(DRSs),
        fail.

presupScoreDRTTestSuite.


/*========================================================================
   Semantic Rules
========================================================================*/

combine(d2:(Score:D),[d1:A]):-
	betaConvertDrs(A,B),
	projectDrs([B,pre([])]-[C,pre(P)],1-Score),
	accommodate(P,C-D).

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

detSem(def,lambda(P,lambda(Q,alfa(X,def,merge(drs([X],[]),P@X),Q@X)))).

detSem(wh,lambda(P,lambda(Q,drs([],[question(X,P@X,Q@X)])))).

detSem(poss(Gender),lambda(P,lambda(Q,
       alfa(Y,def,alfa(X,nonrefl,drs([X],[Basic]),
		       merge(drs([Y],[of(Y,X)]),P@Y)),Q@Y)))):-
   compose(Basic,Gender,[X]).

nounSem(Sym,lambda(X,drs([],[Cond]))):- 
   compose(Cond,Sym,[X]).

pnSem(Sym,Gender,lambda(P,alfa(X,name,drs([X],[X=Sym,Cond]),P@X))):-
   compose(Cond,Gender,[X]).

proSem(Gender,Type,lambda(P,alfa(X,Type,drs([X],[Cond]),P@X))):-
   compose(Cond,Gender,[X]).

npSem(wh,Sym,lambda(Q,drs([],[question(X,drs([],[Cond]),Q@X)]))):-
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


