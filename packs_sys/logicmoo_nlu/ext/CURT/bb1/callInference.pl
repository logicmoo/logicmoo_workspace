/*************************************************************************

    File: callInference.pl
    Copyright (C) 2004 Patrick Blackburn & Johan Bos

    This file is part of BB1, version 1.2 (August 2005).

    BB1 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB1 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB1; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(callInference,[infix/0,
                         prefix/0,
                         callTP/3,
                         callMB/4,
                         callTPandMB/6,
                         mbTestSuite/0,
                         tpTestSuite/0,
                         tpmbTestSuite/0]).

:- use_module(fol2otter,[fol2otter/2,
                         fol2mace/2]).

:- use_module(fol2bliksem,[fol2bliksem/2]).

:- use_module(fol2tptp,[fol2tptp/2]).

:- use_module(comsemPredicates,[infix/0,
                                prefix/0,
                                appendLists/3,
     			        memberList/2,
                                selectFromList/3,
                                executeCommand/1,
				concatStrings/2]).

:- use_module(folTestSuite,[formula/2]).

:- [inferenceEngines].


/*========================================================================
   Initialise Theorem Provers
========================================================================*/

initTheoremProvers([],_).

initTheoremProvers([otter|L],Formula):- !,
   open('otter.in',write,Stream),
   fol2otter(not(Formula),Stream),
   close(Stream),
   initTheoremProvers(L,Formula).

initTheoremProvers([bliksem|L],Formula):- !,
   open('bliksem.in',write,Stream),
   fol2bliksem(not(Formula),Stream),
   close(Stream),
%   executeCommand('dos2unix bliksem.in'), %%% use for MS-DOS/Windows
   initTheoremProvers(L,Formula).

initTheoremProvers([_|L],Formula):-
   initTheoremProvers(L,Formula).


/*========================================================================
   Initialise Model Builders
========================================================================*/

initModelBuilders([],_).

initModelBuilders([mace|L],Formula):- !,
   open('mace.in',write,Stream),
   fol2mace(Formula,Stream),
   close(Stream),
   initModelBuilders(L,Formula).

initModelBuilders([paradox|L],Formula):- !,
   open('paradox.in',write,Stream),
   fol2tptp(not(Formula),Stream),
   close(Stream),
   initModelBuilders(L,Formula).

initModelBuilders([_|L],Formula):-
   initModelBuilders(L,Formula).


/*========================================================================
   Calls to Theorem Provers 
========================================================================*/

callTP(Problem,Proof,Engine):-
   executeCommand('chmod 700 interfaceTP.perl'),
   inferenceEngines(Engines),
   initTheoremProvers(Engines,Problem),
   concatStrings(['perl ./interfaceTP.perl '|Engines],Shell),        
   executeCommand(Shell),
   open('tp.out',read,Out),
   read(Out,Result),
   (
      Result=proof, !, 
      read(Out,engine(Engine)),
      Proof=proof
   ;
      Proof=unknown,
      Engine=unknown
   ),       
   close(Out).


/*========================================================================
   Calls to Model Builders
========================================================================*/

callMB(Problem,DomainSize,Model,Engine):-
   executeCommand('chmod 700 interfaceMB.perl'),
   number(DomainSize),
   inferenceEngines(Engines),
   initModelBuilders(Engines,Problem),
   concatStrings(['perl ./interfaceMB.perl ',DomainSize,' '|Engines],Shell),        
   executeCommand(Shell),
   open('mb.out',read,Out),
   read(Out,Result),
   (
      Result=interpretation(_,_),
      read(Out,engine(Engine)),
      mace2blackburnbos(Result,Model), !
   ;
      Result=paradox(_),
      read(Out,engine(Engine)),
      paradox2blackburnbos(Result,Model), !
   ;
      Model=unknown,
      Engine=unknown
   ),       
   close(Out).


/*========================================================================
   Calls to Theorem Provers and Model Builders
========================================================================*/

callTPandMB(TPProblem,MBProblem,DomainSize,Proof,Model,Engine):-
   executeCommand('chmod 700 interfaceTPandMB.perl'),
   number(DomainSize),
   inferenceEngines(Engines),
   initTheoremProvers(Engines,TPProblem),
   initModelBuilders(Engines,MBProblem),
   concatStrings(['perl ./interfaceTPandMB.perl ',DomainSize,' '|Engines],Shell),        
   executeCommand(Shell),
   open('tpmb.out',read,Out),
   read(Out,Result),
   (
      Result=proof, !, 
      read(Out,engine(Engine)),
      Proof=proof,
      Model=unknown
   ;
      Result=interpretation(_,_),
      read(Out,engine(Engine)),
      mace2blackburnbos(Result,Model), !,
      Proof=unknown
   ;
      Result=paradox(_),
      read(Out,engine(Engine)),
      paradox2blackburnbos(Result,Model), !,
      Proof=unknown
   ;
      Model=unknown,
      Proof=unknown,
      Engine=unknown
   ),       
   close(Out).



/*========================================================================
   Prove all formulas from the test suite (using Theorem Provers)
========================================================================*/

tpTestSuite:-
   format('~n~n>>>>> INFERENCE TEST SUITE <<<<<',[]),
   formula(Formula,Status),
   format('~nInput formula: ~p~nStatus: ~p',[Formula,Status]),
   callTP(Formula,Proof,Engine),
   ( 
      Proof=proof, 
      Result=theorem
   ;
      \+ Proof=proof, 
      Result=unknown
   ),
   format('~nInference engine ~p says: ~p~n',[Engine,Result]),
   fail.

tpTestSuite.


/*========================================================================
   Build models for formulas from the test suite (using Model Builders)
========================================================================*/

mbTestSuite:-
   format('~n~n>>>>> INFERENCE TEST SUITE <<<<<',[]),
   formula(Formula,Status),
   format('~nInput formula: ~p~nStatus: ~p',[Formula,Status]),
   callMB(Formula,30,Model,Engine),
   format('~nInference engine ~p says: ~p~n',[Engine,Model]),
   fail.

mbTestSuite.


/*========================================================================
   Prove or build models for all formulas from the test suite 
   (using Theorem Provers and model builders)
========================================================================*/

tpmbTestSuite:-
   format('~n~n>>>>> INFERENCE TEST SUITE <<<<<',[]),
   formula(Formula,Status),
   format('~nInput formula: ~p~nStatus: ~p',[Formula,Status]),
   callTPandMB(Formula,Formula,30,Proof,Model,Engine),
   ( 
      Proof=proof, 
      Result=theorem
   ;
      Proof=unknown,
      Model=model(_,_), 
      Result=Model
   ;
      Proof=unknown, 
      Model=unknown,
      Result=unknown
   ),
   format('~nInference engine ~p says: ~p~n',[Engine,Result]),
   fail.

tpmbTestSuite.


/*========================================================================
   Translate Paradox-type Model into Blackburn & Bos Models
========================================================================*/

paradox2blackburnbos(Paradox,model(D,F)):-
   Paradox = paradox(Terms), \+ Terms=[],
   paradox2d(Terms,[d1]-D),
   paradox2f(Terms,[]-F).

paradox2blackburnbos(Paradox,model([],[])):-
   Paradox = paradox([]).

paradox2blackburnbos(Paradox,unknown):-
   \+ Paradox = paradox(_).


/*========================================================================
   Translate Paradox Terms to Domain
========================================================================*/

paradox2d([],D-D).

paradox2d([_Constant=Entity|L],D1-D2):-
   \+ memberList(Entity,D1), !,
   paradox2d(L,[Entity|D1]-D2).

paradox2d([Symbol:1|L],D1-D2):-
   functor(Symbol,_Functor,1),
   arg(1,Symbol,Entity),
   \+ memberList(Entity,D1), !,
   paradox2d(L,[Entity|D1]-D2).

paradox2d([Symbol:1|L],D1-D2):-
   functor(Symbol,_Functor,2),
   arg(1,Symbol,Entity1),
   arg(2,Symbol,Entity2),
   (
      \+ memberList(Entity1,D1), !,
      (
         \+ memberList(Entity2,D2), !,
         paradox2d(L,[Entity1,Entity2|D1]-D2)
      ;
         paradox2d(L,[Entity1|D1]-D2) 
      )
   ;
      \+ memberList(Entity2,D2), 
      paradox2d(L,[Entity2|D1]-D2) 
   ), !.

paradox2d([_|L],D1-D2):-
   paradox2d(L,D1-D2).


/*========================================================================
   Translate Paradox Terms to Interpretation Function
========================================================================*/

paradox2f([],F-F).

paradox2f([Constant=Entity|L],D1-D2):-
   Term = f(0,Constant,Entity),
   \+ memberList(Term,D1), !,
   paradox2f(L,[Term|D1]-D2).

paradox2f([Symbol:1|L],D1-D2):-
   functor(Symbol,Functor,1), !,
   arg(1,Symbol,Arg),
   (
      selectFromList(f(1,Functor,E),D1,D3), !,
      paradox2f(L,[f(1,Functor,[Arg|E])|D3]-D2)
   ;
      paradox2f(L,[f(1,Functor,[Arg])|D1]-D2)
   ).

paradox2f([Symbol:0|L],D1-D2):-
   functor(Symbol,Functor,1), !,
   (
      memberList(f(1,Functor,_),D1), !,
      paradox2f(L,D1-D2)
   ;
      paradox2f(L,[f(1,Functor,[])|D1]-D2)
   ).

paradox2f([Symbol:1|L],D1-D2):-
   functor(Symbol,Functor,2), !,
   arg(1,Symbol,Arg1),
   arg(2,Symbol,Arg2),
   (
      selectFromList(f(2,Functor,E),D1,D3), !,
      paradox2f(L,[f(2,Functor,[(Arg1,Arg2)|E])|D3]-D2)
   ;
      paradox2f(L,[f(2,Functor,[(Arg1,Arg2)])|D1]-D2)
   ).

paradox2f([Symbol:0|L],D1-D2):-
   functor(Symbol,Functor,2), !,
   (
      memberList(f(2,Functor,_),D1), !,
      paradox2f(L,D1-D2)
   ;
      paradox2f(L,[f(2,Functor,[])|D1]-D2)
   ).

paradox2f([_|L],D1-D2):-
   paradox2f(L,D1-D2).


/*========================================================================
   Translate Mace-type Model into Blackburn & Bos Models
========================================================================*/

mace2blackburnbos(Mace,model(D,F)):-
   Mace = interpretation(Size,Terms),
   mace2d(1,Size,D),
   mace2f(Terms,D,F).

mace2blackburnbos(Mace,unknown):-
   \+ Mace = interpretation(_Size,_Terms).


/*========================================================================
   Translate Mace Model to Domain
========================================================================*/

mace2d(N,N,[V]):-
	name(N,Codes),
	name(V,[100|Codes]).

mace2d(I,N,[V|D]):-
	I < N,
	name(I,Codes),
	name(V,[100|Codes]),
	J is I + 1,
	mace2d(J,N,D).


/*========================================================================
   Translate Mace Model to Interpretation Function
========================================================================*/

mace2f([],_,[]):- !.

mace2f([function(Skolem,_)|Terms],D,F):-
	\+ atom(Skolem), !,
	mace2f(Terms,D,F).

mace2f([function(Constant,[V])|Terms],D,[f(0,Constant,X)|F]):-
	atom(Constant), !,
	Index is V + 1,
	name(Index,Codes),
	name(X,[100|Codes]),
	mace2f(Terms,D,F).

mace2f([predicate(Relation,V)|Terms],D,[f(1,Functor,X)|F]):-
	Relation =.. [Functor,_], !,
	positiveValues(V,1,X),
	mace2f(Terms,D,F).

mace2f([predicate(Relation,V)|Terms],D,[f(2,Functor,X)|F]):-
	Relation =.. [Functor,_,_], !,
	length(D,Size),
	positivePairValues(V,Size,1,1,X),
	mace2f(Terms,D,F).

mace2f([_|Terms],D,F):-
	mace2f(Terms,D,F).


/*========================================================================
   Take positive values of one-place predicates
========================================================================*/

positiveValues([],_,[]).

positiveValues([1|Values],I1,[X|Rest]):-
	name(I1,Codes),
	name(X,[100|Codes]),
	I2 is I1 + 1,
	positiveValues(Values,I2,Rest).
		
positiveValues([0|Values],I1,Rest):-
	I2 is I1 + 1,
	positiveValues(Values,I2,Rest).
		

/*========================================================================
   Take positive values of two-place predicates
========================================================================*/

positivePairValues([],_,_,_,[]).

positivePairValues([1|Values],Size,I1,J1,[(X2,X1)|Rest]):-
	name(I1,Codes1),
	name(X1,[100|Codes1]),
	name(J1,Codes2),
	name(X2,[100|Codes2]),
	(
	    I1 < Size,
	    I2 is I1 + 1,
	    J2 is J1
	;   
	    I1 = Size,
	    I2 = 1,
	    J2 is J1 + 1
	),
	positivePairValues(Values,Size,I2,J2,Rest).

positivePairValues([0|Values],Size,I1,J1,Rest):-
	(
	    I1 < Size, 
	    I2 is I1 + 1,
	    J2 is J1
	;
	    I1 = Size,
	    I2 = 1,
	    J2 is J1 + 1
	),
	positivePairValues(Values,Size,I2,J2,Rest).


/*========================================================================
   Info
========================================================================*/

info:-
   inferenceEngines(Engines),
   format('~n> ---------------------------------------------------------- <',[]),
   format('~n> callInference.pl, by Patrick Blackburn and Johan Bos       <',[]),
   format('~n>                                                            <',[]),
   format('~n> ?- callTP(Problem,Proof,Engine).                           <',[]),
   format('~n> ?- callMB(Problem,DomainSize,Model,Engine)                 <',[]),
   format('~n> ?- callTPandMB(TPProblem,MBProblem,Size,Proof,Model,Eng).  <',[]),
   format('~n> ?- mbTestSuite.                                            <',[]),
   format('~n> ?- tpTestSuite.                                            <',[]),
   format('~n> ?- tpmbTestSuite.                                          <',[]),
   format('~n> ?- infix.                                                  <',[]),
   format('~n> ?- prefix.                                                 <',[]),
   format('~n>                                                            <',[]),
   format('~n> Selected Inference Engines (inferenceEngines/1)            <',[]),
   format('~n> ~p',[Engines]),
   format('~n> ---------------------------------------------------------- <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info.

