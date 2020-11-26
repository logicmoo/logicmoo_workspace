/*************************************************************************

         name: holeSemantics.pl (Volume 1, Chapter 3)
      version: Jan 31, 1998
  description: Hole Semantics for First-Order Logic
       author: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(holeSemantics,[holeSemantics/0,
			 holeSemantics/2,
			 holeSemanticsTestSuite/0]).

:- use_module(readLine,[readLine/1]).

:- use_module(comsemPredicates,[compose/3,
				printRepresentations/1]).

:- use_module(sentenceTestSuite,[sentence/1]).

:- ensure_loaded(comsemOperators).

:- use_module(pluggingAlgorithm,[plugHole/4]).

:- use_module(usrPredicates,[mergeUSR/2,betaConvertUSR/2]).

:- [englishGrammar].


/*========================================================================
   Driver Predicates
========================================================================*/

holeSemantics:-
	readLine(Sentence),
	setof(Sem,s2(Sem,Sentence,[]),Sems),
	printRepresentations(Sems).


holeSemantics(Sentence,Sems):-
	setof(Sem,s2(Sem,Sentence,[]),Sems).


/*========================================================================
   Testsuite Predicates
========================================================================*/

holeSemanticsTestSuite:-
	nl, write('>>>>> HOLE SEMANTICS ON SENTENCE TEST SUITE <<<<< '), nl,
        sentence(Sentence),
        nl, write('Sentence: '), write(Sentence),
	holeSemantics(Sentence,Sems),
	printRepresentations(Sems),
        fail.

holeSemanticsTestSuite.


/*========================================================================
   Semantic Rules
========================================================================*/

combine(s2:A,[s1:A]).
combine(s2:(A>B),[s1:A,cond,s1:B]).

combine(s1:Top,[np2:A,vp2:B]):-
	betaConvertUSR(merge(usr([Top,Main],[],[]),A@B@Top@Main),Converted),
	mergeUSR(Converted,usr(_,L,C)),
	plugHole(Top,L-[],C,[]).

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

detSem(
       uni,

       lambda(N,lambda(V,lambda(H,lambda(L,
              merge(
                    merge(
                          usr([X,H1,L1,L2],
                              [L2:forall(X,(L1>H1))],
                              [leq(L,H1),leq(L2,H)]),
                          N@X@H@L1
		         ),
                    V@X@H@L)))))
      ).


detSem(
       indef,

       lambda(N,lambda(V,lambda(H,lambda(L,
              merge(
                    merge(
                          usr([X,H1,L1,L2],
                              [L2:exists(X,(L1&H1))],
                              [leq(L,H1),leq(L2,H)]),
                          N@X@H@L1
		         ),
                    V@X@H@L)))))
      ).

detSem(
       wh,

       lambda(N,lambda(V,lambda(H,lambda(L,
              merge(
                    merge(
                          usr([X,H1,L1,L2],
                              [L2:lambda(X,(L1&H1))],
                              [leq(L,H1),leq(L2,H)]),
                          N@X@H@L1
		         ),
                    V@X@H@L)))))
      ).


nounSem(
	Symbol,

        lambda(X,lambda(H,lambda(L,
               usr([],[L:Formula],[leq(L,H)]))))

       ):- compose(Formula,Symbol,[X]).


pnSem(
      Symbol,
      _Gender,

      lambda(V,lambda(H,lambda(L,V@Symbol@H@L)))
     ).


proSem(
       _Gender,
       _Type,

       lambda(V,lambda(H,lambda(L,V@_Var@H@L)))
      ).


npSem(
      wh,
      Symbol,

      lambda(V,lambda(H,lambda(L,
             merge(
                   usr([X,H1,L2],
                       [L2:lambda(X,(Formula&H1))],
                       [leq(L,H1),leq(L2,H)]),
                   V@X@H@L))))

     ):- compose(Formula,Symbol,[X]).


ivSem(
      Symbol,

      lambda(X,lambda(H,lambda(L,usr([],[L:Formula],[leq(L,H)]))))

     ):- compose(Formula,Symbol,[X]).


tvSem(
      Symbol,

      lambda(NP,lambda(X,NP@lambda(Y,lambda(H,lambda(L,
             usr([],[L:Formula],[leq(L,H)]))))))

     ):- compose(Formula,Symbol,[X,Y]).


relproSem(
          lambda(V,lambda(N,lambda(X,lambda(H,lambda(L,
                 merge(
		       usr([L1,L2,H1],
		           [L:(L1&H1)],
                           [leq(L,H),leq(L2,H1)]),
		       merge(
			     V@X@H@L2,
			     N@X@H@L1
			    )
		      ))))))
	 ).


prepSem(
	Sym,
        lambda(NP,lambda(N,lambda(X,lambda(H,lambda(L,
               merge(
		     usr([L2,L3,H2],[L:(L2&H2)],[leq(L,H),leq(L3,H2)]),
                     merge(
			   NP@lambda(Y,lambda(H1,lambda(L1,usr([],[L1:Formula],[leq(L1,H1)]))))@H@L3,
                           N@X@H@L2
			  )))))))

       ):- compose(Formula,Sym,[X,Y]).


modSem(
       neg,
       lambda(V,lambda(X,lambda(H,lambda(L,
              merge(
		    usr([N,S],[N:(~S)],[leq(N,H),leq(L,S)]),
		    V@X@H@L
		   )))))
      ).


adjSem(
       Sym,
       lambda(P,lambda(X,lambda(H,lambda(L,
              merge(
                    usr([L1],[L:(Formula&L1)],[leq(L,H)]),
                    P@X@H@L1
		   )))))

      ):- compose(Formula,Sym,[X]).


coordSem(conj,
         lambda(C1,lambda(C2,lambda(X,lambda(H,lambda(L,
                merge(
		      usr([L1,L2],
			  [L:(L1&L2)],
			  [leq(L,H)]),
                      merge(
			    C1@X@H@L1,
			    C2@X@H@L2)))))))
	).


coordSem(disj,
         lambda(C1,lambda(C2,lambda(X,lambda(H,lambda(L,
                merge(
		      usr([L1,L2],
			  [L:(L1 v L2)],
			  [leq(L,H)]),
                      merge(
			    C1@X@H@L1,
			    C2@X@H@L2)))))))
	).


