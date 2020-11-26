/*************************************************************************

         name: holeSemanticsDRT.pl (Volume 2, Chapter 2)
      version: July 30, 2001
  description: Hole Semantics for DRSs
       author: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(holeSemanticsDRT,[holeSemanticsDRT/0,
			    holeSemanticsDRT/2,
			    holeSemanticsDRTTestSuite/0]).

:- use_module(readLine,[readLine/1]).

:- use_module(comsemPredicates,[compose/3,
				printRepresentations/1]).

:- use_module(discourseTestSuite,[discourse/1]).

:- ensure_loaded(comsemOperators).

:- use_module(pluggingAlgorithm,[plugHole/4]).

:- use_module(usrPredicates,[mergeUSR/2,betaConvertUSR/2]).

:- use_module(drsPredicates,[mergeDrs/2,betaConvertDrs/2]).

:- [englishGrammar].

:- [discourseGrammar].


/*========================================================================
   Driver Predicates
========================================================================*/

holeSemanticsDRT:-
	readLine(Discourse),
	setof(Sem,d2(Sem,Discourse,[]),DRSs),
	printRepresentations(DRSs).


holeSemanticsDRT(Discourse,DRSs):-
	setof(DRS,d2(DRS,Discourse,[]),DRSs).


/*========================================================================
   Testsuite Predicates
========================================================================*/

holeSemanticsDRTTestSuite:-
   format('~n~n>>>>> DRT HOLE SEMANTICS ON DISCOURSE TEST SUITE <<<<<~n',[]),
   discourse(Discourse),
   format('~nDiscourse: ~p',[Discourse]),
   holeSemanticsDRT(Discourse,DRSs),
   printRepresentations(DRSs),
   fail.

holeSemanticsDRTTestSuite.


/*========================================================================
   Semantic Rules
========================================================================*/

combine(d2:DRS,[d1:USR]):- 
	betaConvertUSR(merge(usr([Top,Main],[],[]),USR@Top@Main),Converted),
	mergeUSR(Converted,usr(D,L,C)),
        printRepresentations([usr(D,L,C)]),
	plugHole(Top,L-[],C,[]),
	betaConvertDrs(Top,Merge),
	mergeDrs(Merge,DRS).

combine(d1:A,[s2:A]). 
combine(d1:lambda(H,lambda(L,merge(usr([L1,L2,H1,H2],[L:merge(H1,H2)],[leq(L,H)]),merge(U1@H1@L1,U2@H2@L2)))),[s2:U1,conj,d1:U2]).
combine(d1:lambda(H,lambda(L,merge(usr([L1,L2,H1,H2],[L:drs([],[H1 v H2])],[leq(L,H)]),merge(U1@H1@L1,U2@H2@L2)))),[s2:U1,disj,d1:U2]).
	
combine(s2:A,[s1:A]).
combine(s2:lambda(H,lambda(L,merge(usr([L1,L2,H1,H2],[L:drs([],[H1 > H2])],[leq(L,H)]),merge(U1@H1@L1,U2@H2@L2)))),[s1:U1,cond,s1:U2]).
	
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

detSem(
       uni,

       lambda(N,lambda(V,lambda(H,lambda(L,
              merge(
                    merge(
                          usr([X,H1,L1,L2],
                              [L2:drs([],[merge(drs([X],[]),L1)>H1])],
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
                              [L2:merge(drs([X],[]),merge(L1,H1))],
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
                              [L2:lambda(X,merge(L1,H1))],
                              [leq(L,H1),leq(L2,H)]),
                          N@X@H@L1
		         ),
                    V@X@H@L)))))
      ).


nounSem(
	Symbol,

        lambda(X,lambda(H,lambda(L,
               usr([],[L:drs([],[Condition])],[leq(L,H)]))))

       ):- compose(Condition,Symbol,[X]).

pnSem(
      Symbol,
      Gender,

       lambda(V,lambda(H,lambda(L,merge(
                       usr([X,L2],
                           [
                            L:merge(drs([X],[Cond,X=Symbol]),L2)
                           ],
                           [leq(L,H)]),
                       V@X@H@L2
                     ))))

      ):- compose(Cond,Gender,[X]).

proSem(
       Gender,
       _Type,

       lambda(V,lambda(H,lambda(L,merge(
                       usr([X,L2],
                           [
                            L:merge(drs([X],[Cond]),L2)
                           ],
                           [leq(L,H)]),
                       V@X@H@L2
                     ))))

      ):- compose(Cond,Gender,[X]).



npSem(
      wh,
      Symbol,

      lambda(V,lambda(H,lambda(L,
             merge(
                   usr([X,H1,L2],
                       [L2:lambda(X,merge(drs([],[Condition]),H1))],
                       [leq(L,H1),leq(L2,H)]),
                   V@X@H@L))))

     ):- compose(Condition,Symbol,[X]).


ivSem(
      Symbol,

      lambda(X,lambda(H,lambda(L,usr([],[L:drs([],[Condition])],[leq(L,H)]))))

     ):- compose(Condition,Symbol,[X]).


tvSem(
      Symbol,

      lambda(NP,lambda(X,NP@lambda(Y,lambda(H,lambda(L,
             usr([],[L:drs([],[Condition])],[leq(L,H)]))))))

     ):- compose(Condition,Symbol,[X,Y]).


relproSem(
          lambda(V,lambda(N,lambda(X,lambda(H,lambda(L,
                 merge(
		       usr([L1,L2,H1],
		           [L:merge(L1,H1)],
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
			   NP@lambda(Y,lambda(H1,lambda(L1,usr([],[L1:drs([],[Condition])],[leq(L1,H1)]))))@H@L3,
                           N@X@H@L2
			  )))))))

       ):- compose(Condition,Sym,[X,Y]).


modSem(
       neg,
       lambda(V,lambda(X,lambda(H,lambda(L,
              merge(
		    usr([N,S],[N:drs([],[~S])],[leq(N,H),leq(L,S)]),
		    V@X@H@L
		   )))))
      ).


adjSem(
       Sym,
       lambda(P,lambda(X,lambda(H,lambda(L,
              merge(
                    usr([L1],[L:merge(drs([],[Condition]),L1)],[leq(L,H)]),
                    P@X@H@L1
		   )))))

      ):- compose(Condition,Sym,[X]).


coordSem(conj,
         lambda(C1,lambda(C2,lambda(X,lambda(H,lambda(L,
                merge(
		      usr([L1,L2],
			  [L:merge(L1,L2)],
			  [leq(L,H)]),
                      merge(
			    C1@X@H@L1,
			    C2@X@H@L2)))))))
	).


coordSem(disj,
         lambda(C1,lambda(C2,lambda(X,lambda(H,lambda(L,
                merge(
		      usr([L1,L2],
			  [L:drs([],[L1 v L2])],
			  [leq(L,H)]),
                      merge(
			    C1@X@H@L1,
			    C2@X@H@L2)))))))
	).




