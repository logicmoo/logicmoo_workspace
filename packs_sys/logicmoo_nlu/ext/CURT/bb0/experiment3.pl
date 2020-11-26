/*************************************************************************

         name: experiment3.pl (Volume1, Chapter 2)
      version: July 12, 2001
  description: This is the code of the third experiment (lambda calculus)
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- ensure_loaded(comsemOperators).

:- use_module(betaConversion,[betaConvert/2]).


/*========================================================================
   Syntax-semantics rules 
========================================================================*/

s(NP@VP)--> np(NP), vp(VP).

np(Det@Noun)--> det(Det), noun(Noun). 

np(PN)--> pn(PN).

vp(IV)--> iv(IV).

vp(TV@NP)--> tv(TV), np(NP). 

/*========================================================================
   Proper Names
========================================================================*/

pn(lambda(P,P@vincent))--> [vincent].

pn(lambda(P,P@mia))--> [mia].

/*========================================================================
   Transitive Verbs
========================================================================*/

tv(lambda(X,lambda(Y,X@lambda(Z,love(Y,Z)))))--> [loves].

/*========================================================================
   Intransitive Verbs
========================================================================*/

iv(lambda(Y,snort(Y)))--> [snorts].

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
