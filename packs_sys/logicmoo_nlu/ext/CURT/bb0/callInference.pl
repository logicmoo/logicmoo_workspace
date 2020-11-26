/*************************************************************************

         name: callInference.pl (Chapter 5)
      version: June 18, 1998
  description: Prolog Interface to various off-the-shelf Inference Engines
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(callInference,[callTheoremProver/2,
			 callTheoremProver/3,
			 callModelBuilder/3,
			 callModelBuilder/4,
			 tpTestSuite/1,
			 mbTestSuite/1
			]).

:- ensure_loaded(comsemOperators).

:- use_module(library(system),[shell/2]).

:- use_module(fol2otter,[fol2otter/2]).

:- use_module(fol2bliksem,[fol2bliksem/2]).

:- use_module(comsemPredicates,[appendLists/3,
				concatStrings/2]).

:- use_module(folTestSuite,[formula/2]).


/*========================================================================
   Calls to Theorem Provers 
========================================================================*/

callTheoremProver(Formula,Proof):-
	write('Calling Theorem Prover'), nl,
%	callTheoremProver(bliksem,Formula,Proof).
	callTheoremProver(otter,Formula,Proof).


/*========================================================================
   Calls to Model Builders
========================================================================*/

callModelBuilder(Formula,DomainSize,Model):-
	write('Calling Model Builder'), nl,
	callModelBuilder(mace,Formula,DomainSize,Model).


/*========================================================================
   Prove all formulas from the test suite 
========================================================================*/

tpTestSuite(Prover):-
        format('~n~n>>>>> PROVER ~p ON TEST SUITE <<<<<',[Prover]),
        formula(Formula,Status),
        format('~n~nInput formula: ~p~nStatus: ~p',[Formula,Status]),
        format('~nProver says: ',[]),
	callTheoremProver(Prover,Formula,Proof),
	write(Proof),
        fail.

tpTestSuite(_).


/*========================================================================
   Prove all formulas from the test suite 
========================================================================*/

mbTestSuite(MB):-
        format('~n~n>>>>> MODEL BUILDER ~p ON TEST SUITE <<<<<',[MB]),
        formula(Formula,Status),
        format('~nInput formula: ~p~nStatus: ~p',[Formula,Status]),
        format('~nModel Builder says: ',[]),
	callModelBuilder(MB,Formula,20,Model),
	write(Model), nl,
        fail.

mbTestSuite(_).



/*========================================================================
   Calls to Theorem Prover Otter
========================================================================*/

callTheoremProver(otter,Formula,Proof):-
        InFile='otter-in',
        OutFile='otter-out',
        open(InFile,write,Stream),
        fol2otter(~ Formula,Stream),
        close(Stream),
        concatStrings(['./callTheoremProver.pl otter < ',InFile,' > ',OutFile],Shell),
        shell(Shell,_),
        open(OutFile,read,Result),
        read(Result,Proof),
        close(Result).


/*========================================================================
   Calls to Theorem Prover Bliksem
========================================================================*/

callTheoremProver(bliksem,Formula,Proof):-
        InFile='../tmp/bliksem-in',
        OutFile='../tmp/bliksem-out',
        open(InFile,write,Stream),
        fol2bliksem(~ Formula,Stream),
        close(Stream),
        concatStrings(['./callTheoremProver.pl bliksem ',InFile,' ',OutFile],Shell),
        shell(Shell,_),
        open(OutFile,read,Result),
        read(Result,Proof),
        close(Result).


/*========================================================================
   Calls to Model Generator (Mace)
========================================================================*/

callModelBuilder(mace,Formula,DomainSize,Model):-
        InFile='mace-in',
        OutFile='mace-out',
        number(DomainSize),
        open(InFile,write,Stream),
        fol2otter(Formula,Stream),
        close(Stream),
        concatStrings(['./callModelBuilder.pl mace ',InFile,' ',OutFile,' ',DomainSize],Shell),
        shell(Shell,_),
        open(OutFile,read,Result),
        read(Result,Mace),
        close(Result),
        mace2blackburnbos(Mace,Model).


/*========================================================================
   Translate Mace Model into Blackburn & Bos Models
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
