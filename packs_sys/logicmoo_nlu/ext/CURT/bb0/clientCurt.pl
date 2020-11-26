/*************************************************************************

         name: clientCurt.pl (Volume 1, Chapter 6)
      version: April 17, 2001
  description: Curt (server-client version - requires Sicstus prolog)
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- ensure_loaded(comsemOperators).

:- use_module(readLine,[readLine/1]).

:- use_module(comsemPredicates,[memberList/2,
				selectFromList/3,
				appendLists/3,
				basicFormula/1,
				printRepresentations/1]).

:- use_module(kellerStorage,[kellerStorage/2]).

:- use_module(semOntology,[generateOntology/2]).

:- use_module(modelChecker2,[satisfy/4]).

:- use_module(englishLexicon,[lexicon/4]).

:- use_module(library('linda/client')).

:- use_module(library(system),[pid/1,sleep/1]).

:- dynamic(problemCounter/1).



/*========================================================================
   Start Curt
========================================================================*/

curt:- 
        init_client,
        pid(Pid),
        out(pid(Pid)),
	curtTalk([],[],run),
        close_client,
        halt.


init_client:-

        open('/tmp/curtHostPort',read,Stream),
        read(Stream,host_port(Host,Port)),
        close(Stream),
        linda_client(Host:Port).



/*========================================================================
   Control
========================================================================*/

curtTalk(_,_,quit).

curtTalk(OldReadings,OldModels,run):-
   curtInput(Input),
   curtUpdate(Input,OldReadings-NewReadings,OldModels-NewModels,Moves,State), !,
   curtOutput(Moves),
   curtTalk(NewReadings,NewModels,State).

	
/*========================================================================
   Curt's Input
========================================================================*/

curtInput(X):- readLine(X).


/*========================================================================
   Curt Help
========================================================================*/

curtHelp:-
	nl, write('bye: no more talking'),
	nl, write('readings: prints current readings'),
	nl, write('models: prints current models'),
	nl, write('new: starts a new discourse'),
	nl.


/*========================================================================
   Curt's output
========================================================================*/

curtOutput([]).
curtOutput([Move|Moves]):-
	realizeMove(Move,Output),
	format('~n~nCurt: ~p~n',[Output]),
	curtOutput(Moves).


/*========================================================================
   Curt's Moves
========================================================================*/

realizeMove(clarify,'Want to tell me something?').

realizeMove(bye,'Bye bye!').

realizeMove(accept,'OK.').

realizeMove(contradiction,'No! I do not believe that!').

realizeMove(obvious,'Well, that is obvious!').

realizeMove(noparse,'What?').

realizeMove(Answer,String):-
	(
	    calculateAnswers(Answer,String), !
	;
	    String='I have no idea'
	).



/*========================================================================
   Calculate an answer
========================================================================*/

calculateAnswers(answer([Value],Formula,model(D,F),G),Answers):-
	setof([Member],memberList(Member,D),Possible),
	checkPossibleAnswers(Possible,answer([Value],Formula,model(D,F),G),Answers).

calculateAnswers(answer([Value1,Value2],Formula,model(D,F),G),Answers):-
	setof([Member1,Member2],(memberList(Member1,D),memberList(Member2,D)),Possible),
	checkPossibleAnswers(Possible,answer([Value1,Value2],Formula,model(D,F),G),Answers).


/*========================================================================
   Check possible answers
========================================================================*/

checkPossibleAnswers([],_,[]).

checkPossibleAnswers([Value|L],answer(V,Formula,Model,G),[Answer|Answers]):-
	    \+ \+ (
		      V=Value,
		      satisfy(Formula,Model,G,pos)
		  ),
	    !,
	    realizeAnswer(Value,Model,Answer),
	    checkPossibleAnswers(L,answer(V,Formula,Model,G),Answers).

checkPossibleAnswers([_|L],Q,A):-
	    checkPossibleAnswers(L,Q,A).


/*========================================================================
   Update Curt's Information State
========================================================================*/

curtUpdate([],R-R,M-M,[clarify],run):- !.

curtUpdate([bye],R-R,M-M,[bye],quit):- !.

curtUpdate([new],_-[],_-[],[],run):- !.

curtUpdate([help],R-R,M-M,[],run):- !,
	curtHelp.

curtUpdate([readings],Readings-Readings,M-M,[],run):- !,
	printRepresentations(Readings).

curtUpdate([models],R-R,Models-Models,[],run):- !,
	printRepresentations(Models).

curtUpdate(Input,OldReadings-NewReadings,OldModels-NewModels,Moves,run):-
	kellerStorage(Input,Readings), !,
	\+ Readings = [],
	findall(
		Formula,
		(
		    memberList(New,Readings),
 		    (
		     memberList(Old,OldReadings),
		     Formula=(New&Old)
		    ;
		     OldReadings=[],
		     Formula=New
		    )
		),
	        UpdatedReadings
	       ),
	eliminateEquivalentReadings(UpdatedReadings,UniqueReadings),
	(
	    questions(UniqueReadings,OldModels,Moves), !,
	    NewReadings=OldReadings,
	    NewModels=OldModels
	;
	    consistentReadings(UniqueReadings-NewReadings,NewModels,TempMoves),
	    informativeReadings(NewReadings,TempMoves,Moves)
	).


curtUpdate(_,R-R,M-M,[noparse],run).


/*========================================================================
   Select Consistent Readings
========================================================================*/

consistentReadings(UpdatedReadings-ConsistentReadings,Models,[Move]):-
        format('~nEliminating inconsistent readings. ',[]),
		findall((Reading,Model),
		(
		    memberList(Reading,UpdatedReadings),
		    consistent(Reading,Model)
		),
		Readings),
	(
	    Readings=[],
	    Move=contradiction
	;
	    \+ Readings=[],
	    Move=accept
	),
	findall(R,memberList((R,_),Readings),ConsistentReadings),
	findall(M,memberList((_,M),Readings),Models).


/*========================================================================
   Consistency Checking calling Theorem Prover and Model Builder
========================================================================*/

consistent(Formula,Model):-
	DomainSize=15,
	generateOntology(Formula,Ontology),
	newProblemCounter(ID),
	out(callTheoremProver(ID,~(Ontology & Formula))),
	out(callModelBuilder(ID,Ontology & Formula,DomainSize)),
	readInferenceResults(ID,Result,Model,10),
	Result=satisfiable.



/*========================================================================
   Select Informative Readings
========================================================================*/

informativeReadings(Readings,OldMoves,NewMoves):-
	\+ memberList((_&_),Readings), !,
	NewMoves=OldMoves.
	
informativeReadings(Readings,OldMoves,NewMoves):-
        format('~nEliminating uninformative readings. ',[]),
	findall((New&Old),
		(
		    memberList((New&Old),Readings),
		    informative(Old,New)
		),
		InformativeReadings),
	(
	    InformativeReadings=[],
	    NewMoves=[obvious]
	;
	    \+ InformativeReadings=[],
	    NewMoves=OldMoves
	).


/*========================================================================
   Informativity Checking calling Theorem Prover and Model Builder
========================================================================*/

informative(Old,New):-
	DomainSize=20,
	generateOntology((Old & New),Ontology),
	newProblemCounter(ID),
	out(callTheoremProver(ID,~(Ontology & Old & ~New))),
	out(callModelBuilder(ID,(Ontology & Old & ~New),DomainSize)),
	readInferenceResults(ID,Result,_Model,15),
	Result=satisfiable.


/*========================================================================
   Eliminate Equivalent Readings
========================================================================*/

eliminateEquivalentReadings(Readings,UniqueReadings):-
	numberReadings(Readings,1,NumberedReadings),
	format('~nEliminating equivalent readings. ',[]),
	eliminateEquivalentReadings(NumberedReadings,[],UniqueReadings).

numberReadings([],_,[]).
numberReadings([X|L1],N,[n(N,X)|L2]):-
	M is N + 1,
	numberReadings(L1,M,L2).

eliminateEquivalentReadings(NumberedReadings,Different,UniqueReadings):-
	DomainSize=20,
	selectFromList(n(N1,R1),NumberedReadings,Readings),
	memberList(n(N2,R2),Readings),
	\+ memberList(different(N1,N2),Different), !,
	newProblemCounter(ID),
	out(callTheoremProver(ID,((R1 > R2) & (R2 > R1)))),
	out(callModelBuilder(ID,~((R1 > R2) & (R2 > R1)),DomainSize)),
	readInferenceResults(ID,Result,_,20),
	(   
	    Result=unsatisfiable, !,
	    eliminateEquivalentReadings(Readings,Different,UniqueReadings)
	;
	    eliminateEquivalentReadings([n(N1,R1)|Readings],[different(N1,N2),different(N2,N1)|Different],UniqueReadings)
	).

eliminateEquivalentReadings(NumberedReadings,_,UniqueReadings):-
	findall(Reading,memberList(n(_,Reading),NumberedReadings),UniqueReadings).



       
/*========================================================================
    Prepare Answer to Question
========================================================================*/

questions(Q,M,A):-
	questions(Q,M,[],A).

questions(Q,[],A1,A2):-
	question(Q,[],A1,A2).

questions(Q,[Model],A1,A2):-
	question(Q,Model,A1,A2).

questions(Q,[Model1,Model2|M],A1,A3):-
	question(Q,Model1,A1,A2),
	questions(Q,[Model2|M],A2,A3).

question([],_,A,A).


question([Q1|Q],Model,A1,[answer([Answer],Q2,Model,[g(X,Answer)])|A2]):-
	convertLambda(Q1,Q2,[X]),
	question(Q,Model,A1,A2).

question([Q1|Q],Model,A1,[answer([Answer1,Answer2],Q2,Model,[g(X,Answer1),g(Y,Answer2)])|A2]):-
	convertLambda(Q1,Q2,[X,Y]),
	question(Q,Model,A1,A2).


/*========================================================================
    Convert lambda-formulas to formulas with free variables
========================================================================*/

convertLambda(lambda(X,Formula1),Formula2,[X|L]):- 
	convertLambda(Formula1,Formula2,L).

convertLambda(Formula1 & Formula2,F & Formula2,L):- 
	convertLambda(Formula1,F,L).

convertLambda(Formula1 & Formula2,Formula1 & F,L):- 
	convertLambda(Formula2,F,L).

convertLambda(Formula1 > Formula2,F > Formula2,L):-
	convertLambda(Formula1,F,L).

convertLambda(Formula1 > Formula2,Formula1 > F,L):- 
	convertLambda(Formula2,F,L).

convertLambda(Formula1 v Formula2,F v Formula2,L):-
	convertLambda(Formula1,F,L).

convertLambda(Formula1 v Formula2,Formula1 v F,L):- 
	convertLambda(Formula2,F,L).

convertLambda(forall(Y,Formula),forall(Y,F),L):- 
	convertLambda(Formula,F,L).

convertLambda(exists(Y,Formula),exists(Y,F),L):- 
	convertLambda(Formula,F,L).

convertLambda(~ Formula,~ F,L):- 
	convertLambda(Formula,F,L).

convertLambda(Formula,Formula,[]):-
	basicFormula(Formula).


/*========================================================================
    Realize all answers
========================================================================*/

realizeAnswer([Value],Model,String):-
	realizeString(Value,Model,String).

realizeAnswer([Value|Values],Model,String3):-
	realizeString(Value,Model,String1),
	realizeAnswer(Values,Model,String2),
	name(String1,Codes1),
	name(String2,Codes2),
	appendLists(Codes1,[32|Codes2],Codes3),
	name(String3,Codes3).


/*========================================================================
    Realize a single answer
========================================================================*/

realizeString(Value,model(_,F),String):-
	memberList(f(0,Symbol,Value),F),
	lexicon(pn,Symbol,Answer,_), !,
	list2string(Answer,String).

realizeString(Value,model(_,F),String):-
	memberList(f(1,Symbol,Values),F),
	memberList(Value,Values),
	lexicon(noun,Symbol,[Noun|Answer],_), !,
	list2string([a,Noun|Answer],String).

realizeString(Value,_,Value).


/*========================================================================
    Covert a list of words to a string
========================================================================*/

list2string([Word],Word).

list2string([Word|L],String2):-
	list2string(L,String1),
	name(Word,Codes1),
	name(String1,Codes2),
	appendLists(Codes1,[32|Codes2],Codes3),
	name(String2,Codes3).



/*========================================================================
   Counter
========================================================================*/

problemCounter(1).

newProblemCounter(N):-
   problemCounter(N), M is N+1,
   retract(problemCounter(N)),
   asserta(problemCounter(M)).


/*========================================================================
   Wait for Model Builder to be ready
========================================================================*/

waitForModelBuilder(ID):-
	rd_noblock(callModelBuilder(ID,_,_)), !,
	format('~nWaiting for model builder to be ready',[]),
	sleep(3),
	waitForModelBuilder(ID).

waitForModelBuilder(_).


/*========================================================================
   Wait for Theorem Prover to be ready
========================================================================*/

waitForTheoremProver(ID):-
	rd_noblock(callTheoremProver(ID,_)), !,
	format('~nWaiting for theorem prover to be ready',[]),
	sleep(3),
	waitForTheoremProver(ID).

waitForTheoremProver(_).


/*========================================================================
   Reading Results from Server
========================================================================*/

readInferenceResults(ID,unknown,model([],[]),0):-
	format('~nNo result found for ~p!',[ID]),
	waitForModelBuilder(ID),
	waitForTheoremProver(ID).
	       
readInferenceResults(ID,Result,Model,TimeOut):-
	TimeOut > 0,
	sleep(1),
	(
	 in_noblock( proof(ID,TheoremProver,proof) ), !,
         format('~nTheorem Prover ~p found proof for ~p!',[TheoremProver,ID]),
	 Result=unsatisfiable,
	 Model=model([],[]),
	 waitForModelBuilder(ID)
	;
	 in_noblock( model(ID,ModelBuilder,model(D,F)) ), !,
         format('~nModel Builder ~p found model for ~p!',[ModelBuilder,ID]),
	 Result=satisfiable,
	 Model=model(D,F),
	 waitForTheoremProver(ID)
	;
   	 NewTimeOut is TimeOut - 1,
	 readInferenceResults(ID,Result,Model,NewTimeOut)
	).
	 
	   
	

/*========================================================================
   Self Starting
========================================================================*/

:- curt.
