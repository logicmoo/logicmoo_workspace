/*************************************************************************

         name: experiment3.pl (Volume1, Chapter 2)
      version: July 12, 2001
  description: This is the code of the third experiment (lambda calculus)
      authors: Patrick Blackburn & Johan Bos
  modified by: Christof Rumpf, November 18, 2002 (coordination added)
 
*************************************************************************/

:- ensure_loaded(comsemOperators).

:- use_module(betaConversion3,[betaConvert/2]).


/*========================================================================
   Syntax-semantics rules 
========================================================================*/

s(NP@VP)--> np(NP), vp(VP).

np(Det@Noun)--> det(Det), noun(Noun). 

np(PN)--> pn(PN).

np((C@PN1)@PN2)--> pn(PN1), coord(C), pn(PN2).

vp(IV)--> iv(IV).

vp(TV@NP)--> tv(TV), np(NP). 

vp(NP@TV)--> tv2(TV), np(NP). 

/*========================================================================
   Proper Names
========================================================================*/

pn(lambda(P,P@vincent))--> [vincent].

pn(lambda(P,P@mia))--> [mia].

/*========================================================================
   Transitive Verbs
========================================================================*/

tv(lambda(X,lambda(Y,X@lambda(Z,love(Y,Z)))))--> [loves].

tv2(lambda(Y,lambda(X,love(X,Y))))--> [loves].

/*========================================================================
   Intransitive Verbs
========================================================================*/

iv(lambda(Y,snort(Y)))--> [snorts].

iv(lambda(Y,dance(Y)))--> [dance].

/*========================================================================
   Determiners
========================================================================*/

det(lambda(P,lambda(Q,forall(X,(P@X)>(Q@X)))))--> [every].

det(lambda(P,lambda(Q,exists(X,(P@X)&(Q@X)))))--> [a].

/*========================================================================
   Nouns
========================================================================*/

noun(lambda(X,woman(X)))--> [woman].

noun(lambda(X,footmassage(X)))--> [foot,massage].

/*========================================================================
   Coordination
========================================================================*/

coord(lambda(X,lambda(Y,lambda(P,(X@P)&(Y@P)))))--> [and].
