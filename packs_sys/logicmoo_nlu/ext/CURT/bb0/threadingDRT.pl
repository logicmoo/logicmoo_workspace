/*************************************************************************

         name: threadingDRT.pl (Volume 2, Chapter 2)
      version: July 29, 2001
  description: DRS-threading (Johnson & Klein 1986)
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(threadingDRT,[threadingDRT/0,
			threadingDRT/2,
			threadingDRTTestSuite/0]).

:- ensure_loaded(comsemOperators).

:- use_module(readLine,[readLine/1]).

:- use_module(comsemPredicates,[compose/3,
				printRepresentations/1]).

:- use_module(englishLexicon,[lexicon/4]).

:- [englishGrammar].

:- [discourseGrammar].

:- use_module(discourseTestSuite,[discourse/1]).


/*========================================================================
   Driver Predicate
========================================================================*/

threadingDRT:-
	readLine(Discourse),
	d2(DRS,Discourse,[]),
	printRepresentations([DRS]).

threadingDRT(Discourse,DRSs):-
        setof(DRS,d2(DRS,Discourse,[]),DRSs).


/*========================================================================
   Testsuite Predicates
========================================================================*/

threadingDRTTestSuite:-
        format('~n>>>>> THREADING DRT ON DISCOURE TEST SUITE <<<<<~n',[]),
        discourse(Discourse),
        format('~nDiscourse: ~p',[Discourse]),
        threadingDRT(Discourse,DRSs),
        printRepresentations(DRSs),
        fail.

threadingDRTTestSuite.


/*========================================================================
   Semantic Rules
========================================================================*/

combine(d2:Drs,[d1:[sem:[in:[drs([],[])],out:[Drs]]]]).

combine(d1:[sem:Sem],[s2:[sem:Sem]]). 
combine(d1:[sem:[in:A,out:C]],[s2:[sem:[in:A,out:B]],conj,d1:[sem:[in:B,out:C]]]).
	
combine(s2:[sem:[in:In,out:Out]],[s1:[sem:[in:In,out:Out|_]]]).

combine(s2:[sem:[in:[drs(D,C)|S],
		 out:[drs(D,[A > B|C])|S]]],
	[s1:[sem:[in:[drs([],[]),drs(D,C)|S],
		  out:[A|_]|_]],
	 cond,
	 s1:[sem:[in:[drs([],[]),drs(D,C),A|S],
		  out:[B|_]|_]]
	]).

combine(s1:[sem:Sem],
	[np2:[syn:[index:I],
	      sem:Sem],
	 vp2:[syn:Syn,
	      sem:VP]
	]
       ):-
	Sem=[in:_,out:_,restr:_,scope:VP],
	Syn=[subj:I,obj:_].

combine(np2:A,[np1:A]).

combine(np1:[syn:Syn,sem:Det],[det:[sem:Det],n2:[syn:Syn,sem:N]]):-
   Det=[in:_,out:_,restr:N,scope:_].

combine(np1:A,[pn:A]).
combine(np1:A,[pro:A]).
combine(np1:A,[np:A]).

combine(n2:A,[n1:A]).
combine(n2:[syn:Syn,sem:C],[n1:[syn:Syn,sem:A],coord:[sem:[arg1:A,arg2:B,coord:C]],n1:[syn:Syn,sem:B]]).

combine(n1:[syn:Syn,sem:[in:A,out:C]],[adj:[syn:Syn,sem:[in:A,out:B]],n1:[syn:Syn,sem:[in:B,out:C]]]).
combine(n1:A,[noun:A]).
combine(n1:[syn:Syn,sem:[in:A,out:C]],[noun:[syn:Syn,sem:[in:A,out:B]],pp:[syn:Syn,sem:[in:B,out:C]]]).
combine(n1:[syn:Syn,sem:[in:A,out:C]],[noun:[syn:Syn,sem:[in:A,out:B]],rc:[syn:Syn,sem:[in:B,out:C]]]).

combine(vp2:A,[vp1:A]).

combine(vp1:A,[v2:A]).
combine(vp1:[syn:Syn,sem:[in:A,out:B]],[mod:[sem:[in:A,out:B,scope:VP]],v2:[syn:Syn,sem:VP]]).

combine(v2:[syn:Syn,sem:[in:In,out:Out]],[cop:[syn:Syn,sem:Cop],np2:[syn:[index:I],sem:Sem]]):-
   Sem=[in:In,out:Out,restr:_,scope:Cop],
   Syn=[subj:_,obj:I].   

combine(v2:[syn:Syn,sem:[in:In,out:Out]],[cop:[syn:Syn,sem:Cop],neg:[sem:Neg],np2:[syn:[index:I],sem:Sem]]):-
   Neg=[in:In,out:Out,scope:[in:A,out:B]],
   Sem=[in:A,out:B,restr:_,scope:Cop],
   Syn=[subj:_,obj:I].   

combine(v2:A,[v1:A]).

combine(v1:A,[iv:A]).
combine(v1:[syn:Syn,sem:[in:In,out:Out]],[tv:[syn:Syn,sem:TV],np2:[syn:[index:I],sem:Sem]]):-
   Sem=[in:In,out:Out,restr:_,scope:TV],
   Syn=[subj:_,obj:I].   

combine(pp:[syn:[index:X],sem:[in:In,out:Out]],[prep:[syn:[ext:X,int:Y],sem:P],np2:[syn:[index:Y],sem:Sem]]):-
   Sem=[in:In,out:Out,restr:_,scope:P].

combine(rc:[syn:[index:X],sem:Sem],[relpro:_,vp2:[syn:[subj:X,_],sem:Sem]]).


/*========================================================================
   Semantic Macros
========================================================================*/

detSem(indef,[sem:[in:X,out:Z,restr:[in:X,out:Y],scope:[in:Y,out:Z]]]).

detSem(uni,
       [
        sem:[in:[drs(D,C)|S],
	     out:[drs(D,[Y>Z|C])|S],
	     restr:[in:[drs([],[]),drs(D,C)|S],out:[Y|T]],
	     scope:[in:[drs([],[]),Y|T],out:[Z|_]]]
       ]
      ).

nounSem(Sym,
	[
	 syn:[index:X],
	 sem:[in:[drs(D,C)|S],out:[drs([X|D],[Cond|C])|S]]
	]
       ):- compose(Cond,Sym,[X]).

pnSem(Sym,Gender,
      [
       syn:[index:X],
       sem:[in:[drs(D,C)|S],
	    out:Out,
	    restr:_,
	    scope:[in:[drs([X|D],[Cond,X=Sym|C])|S],out:Out]]
      ]
     ):- compose(Cond,Gender,[X]).

npSem(wh,Sym,
      [
       syn:[index:X],
       sem:[in:[drs(D,C)|S],
	    out:lambda(X,Out),
	    restr:_,
	    scope:[in:[drs(D,[Cond|C])|S],out:[Out]]]
      ]
     ):- compose(Cond,Sym,[X]).

proSem(Gender,_Type,
       [syn:[index:X],
	sem:[in:[drs(D,C)|S],
	     out:Out,
	     restr:_,
	     scope:[in:[drs(D,[Cond|C])|S],out:Out]]
       ]
      ):- compose(Cond,Gender,[X]),
	  accessible(X,[drs(D,C)|S]).

ivSem(Sym,
      [
       syn:[subj:X,obj:_],
       sem:[in:[drs(D,C)|S],
	    out:[drs(D,[Cond|C])|S]]
      ]
     ):- compose(Cond,Sym,[X]).

tvSem(Sym,
      [
       syn:[subj:X,obj:Y],
       sem:[in:[drs(D,C)|S],
	    out:[drs(D,[Cond|C])|S]]
      ]
     ):- compose(Cond,Sym,[X,Y]).

relproSem([syn:[],sem:[]]).

prepSem(Sym,
	[
	 syn:[ext:X,int:Y],
	 sem:[in:[drs(D,C)|S],
	      out:[drs(D,[Cond|C])|S]]
	]
       ):- compose(Cond,Sym,[X,Y]).

adjSem(Sym,
       [
	syn:[index:X],
	sem:[in:[drs(D,C)|S],
	     out:[drs(D,[Cond|C])|S]]
       ]
      ):-compose(Cond,Sym,[X]).

modSem(neg,
       [
	sem:[in:[drs(D,C)|S],
	     out:[drs(D,[~B|C])|S],scope:[in:[drs([],[]),drs(D,C)|S],out:[B|_]]]
       ]).

coordSem(conj,
	 [
	  sem:[arg1:[in:A,out:B],
	       arg2:[in:B,out:C],
	       coord:[in:A,out:C]]
	 ]).

coordSem(disj,
	 [
	  sem:[arg1:[in:[drs([],[]),drs(D,C)|S],out:[A|_]],
	       arg2:[in:[drs([],[]),drs(D,C)|S],out:[B|_]],
	       coord:[in:[drs(D,C)|S],out:[drs(D,[A v B|C])|S]]]
	 ]).



/*========================================================================
   Accessibilty checking with freeze
========================================================================*/

accessible(X,Space):-
	memberList(drs(D,_),Space),
	memberList(X,D).
	   
memberList(X,A):-
	freeze(A,memberList2(X,A)).

memberList2(X,[X|_]).
memberList2(X,[_|L]):-
	memberList(X,L).
