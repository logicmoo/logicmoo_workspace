/*************************************************************************

         name: curtDRT.pl (Volume 2, Chapter 6)
      version: July 30, 2001
  description: User-System discourse interaction (DRT-based)
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(curt,[curt/0]).

:- use_module(readLine,[readLine/1]).

:- ensure_loaded(comsemOperators).

:- use_module(acceptabilityConstraints,[consistent/2,
					noFreeVars/1,
					localConsistent/1,
					localInformative/1]).

:- use_module(presupScoreDRT,[presupScoreDRT/3]).

:- use_module(callInference,[callTheoremProver/2]).

:- use_module(comsemPredicates,[memberList/2,
				selectFromList/3,
				appendLists/3,
				basicFormula/1,
				printRepresentations/1]).

:- use_module(modelCheckerDRT2,[satisfyDrs/4]).

:- use_module(englishLexicon,[lexicon/4]).

:- use_module(drs2fol,[drs2fol/2]).

:- use_module(printDrs,[printDrs/1]).


/*========================================================================
   Start Curt
========================================================================*/

curt:- 
   curtTalk([drs([],[])],[],run).


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
   Update Curt's Information State
========================================================================*/

curtUpdate([],R-R,M-M,[clarify],run):- !.

curtUpdate([bye],R-R,M-M,[bye],quit):- !.

curtUpdate([new],_-[drs([],[])],_-[],[],run):- !.

curtUpdate([help],R-R,M-M,[],run):- !,
	curtHelp.

curtUpdate([readings],Readings-Readings,M-M,[],run):- !,
	printDRSs(Readings).

curtUpdate([models],R-R,Models-Models,[],run):- !,
	printRepresentations(Models).

curtUpdate(Input,OldReadings-NewReadings,OldModels-NewModels,Moves,run):-

        setof(New,
	      Old^(memberList(Old,OldReadings),presupScoreDRT(Input,Old,New)),
	      ScoredReadings),

	eliminateEquivalentReadings(ScoredReadings,UniqueScoredReadings),
	consistentScoredReadings(UniqueScoredReadings,ConsReadingsModels),
	localInformativeScoredReadings(ConsReadingsModels,LIConsReadingsModels),
	localConsistentScoredReadings(LIConsReadingsModels,LCConsReadingsModels),
	selectReadings(LCConsReadingsModels,Readings,Models),
	
	(
	    Readings=[],
	    NewReadings=[],
	    NewModels=[],
	    Moves=[contradiction]
	;
	    \+ Readings=[], 
	    question(Readings,Models,[]-Answers),
	    (
		Answers=[],
		NewReadings=Readings,
		NewModels=Models
	    ;
		\+ Answers=[],
		NewReadings=OldReadings,
		NewModels=OldModels
	    ),
	    Moves=[accept|Answers]
	).
				 

curtUpdate(_,R-R,M-M,[noparse],run).


/*========================================================================
   Select Readings with highest Scores
========================================================================*/

selectReadings(Readings,Selected,Models):-
	findall(R,(memberList((Score,R,_),Readings),
		   \+ (memberList((Higher,_,_),Readings),Score < Higher)),
		Selected),
	findall(M,(memberList((Score,_,M),Readings),
		   \+ (memberList((Higher,_,_),Readings),Score < Higher)),
		Models).


/*========================================================================
   Select Consistent Readings
========================================================================*/

consistentScoredReadings(ScoredReadings,Readings):-
	findall((Score,Reading,Model),
		(
		    memberList((Score:Reading),ScoredReadings),
		    noFreeVars(Reading),
		    consistent(Reading,Model)
		),
		Readings).


/*========================================================================
   Select Local Consistent Scored Readings 
========================================================================*/

localConsistentScoredReadings([],[]).

localConsistentScoredReadings([(Score,Drs,Model)|Potential],[(NewScore,Drs,Model)|Selected]):-
	(
	    localConsistent(Drs), !,
	    NewScore = Score
	;
	    NewScore is Score * 0.7
	),
	localConsistentScoredReadings(Potential,Selected).


/*========================================================================
   Select Local Informative Scored Readings 
========================================================================*/

localInformativeScoredReadings([],[]).

localInformativeScoredReadings([(Score,Drs,Model)|Potential],[(NewScore,Drs,Model)|Selected]):-
	(
	    localInformative(Drs), !,
	    NewScore = Score
	;
	    NewScore is Score * 0.7
	),
	localInformativeScoredReadings(Potential,Selected).



/*========================================================================
    Extract Question from DRSs
========================================================================*/

extractQuestionDrs(drs(D,C1)-drs(D,C2),L1-L2):-
	extractQuestionConds(C1-C2,L1-L2).

extractQuestionConds([question(Q,B1,B2)|C1]-[Condition|C2],L1-[Q|L2]):-
	B1 = drs(D,C),
	Condition = (~drs([],[~drs(D,[~drs([],[~B2])|C])])),
	extractQuestionConds(C1-C2,L1-L2).

extractQuestionConds([B1 > B2|C1]-[B3 > B4|C2],L1-L4):-
	extractQuestionDrs(B1-B3,L1-L2),
	extractQuestionDrs(B2-B4,L2-L3),
	extractQuestionConds(C1-C2,L3-L4).

extractQuestionConds([B1 v B2|C1]-[B3 v B4|C2],L1-L4):-
	extractQuestionDrs(B1-B3,L1-L2),
	extractQuestionDrs(B2-B4,L2-L3),
	extractQuestionConds(C1-C2,L3-L4).

extractQuestionConds([~ B1|C1]-[~ B2|C2],L1-L3):-
	extractQuestionDrs(B1-B2,L1-L2),
	extractQuestionConds(C1-C2,L2-L3).

extractQuestionConds([Basic|C1]-[Basic|C2],L1-L2):-
	basicFormula(Basic),
	extractQuestionConds(C1-C2,L1-L2).

extractQuestionConds([]-[],[]-[]).


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
    Prepare Answer to Question
========================================================================*/

question([],[],A-A).

question([Q1|Questions],[M|Models],A1-A3):-
	extractQuestionDrs(Q1-Q2,[]-Free),
	(
	    Free=[],
	    A3=A2
	;
	    Free=[X],
	    A3=[answer([Answer],Q2,M,[g(X,Answer)])|A2]
	;
	    Free=[X,Y],
	    A3=[answer([Answer1,Answer2],Q2,M,[g(X,Answer1),g(Y,Answer2)])|A2]
	),
	question(Questions,Models,A1-A2).



/*========================================================================
   Calculate an answer
========================================================================*/

calculateAnswers(answer([Value],Drs,model(D,F),G),Answers):-
	setof([Member],memberList(Member,D),Possible),
	checkPossibleAnswers(Possible,answer([Value],Drs,model(D,F),G),Answers).

calculateAnswers(answer([Value1,Value2],Drs,model(D,F),G),Answers):-
	setof([Member1,Member2],(memberList(Member1,D),memberList(Member2,D)),Possible),
	checkPossibleAnswers(Possible,answer([Value1,Value2],Drs,model(D,F),G),Answers).


/*========================================================================
   Check possible answers
========================================================================*/

checkPossibleAnswers([],_,[]).

checkPossibleAnswers([Value|L],answer(V,Drs,Model,G),[Answer|Answers]):-
	    \+ \+ (
		      V=Value,
		      satisfyDrs(Drs,Model,G-_,pos)
		  ),
	    !,
	    realizeAnswer(Value,Model,Answer),
	    checkPossibleAnswers(L,answer(V,Drs,Model,G),Answers).

checkPossibleAnswers([_|L],Q,A):-
	    checkPossibleAnswers(L,Q,A).



/*========================================================================
   Eliminate Equivalent Readings (with same score)
========================================================================*/

eliminateEquivalentReadings(ScoredReadings,UniqueScoredReadings):-
	numberReadings(ScoredReadings,1,NumberedScoredReadings),
	eliminateEquivalentReadings(NumberedScoredReadings,[],UniqueScoredReadings).

numberReadings([],_,[]).
numberReadings([S:X|L1],N,[n(N,S,X)|L2]):-
	M is N + 1,
	numberReadings(L1,M,L2).

eliminateEquivalentReadings(NumberedReadings,Different,UniqueReadings):-
	selectFromList(n(N1,Score,B1),NumberedReadings,Readings),
	memberList(n(N2,Score,B2),Readings),
	\+ memberList(different(N1,N2),Different), !,
	drs2fol(B1,F1),
	drs2fol(B2,F2),
	callTheoremProver(((F1 > F2) & (F2 > F1)),Proof),
	(
	    Proof=proof, !,
	    eliminateEquivalentReadings(Readings,Different,UniqueReadings)
	;
	    eliminateEquivalentReadings([n(N1,Score,B1)|Readings],[different(N1,N2),different(N2,N1)|Different],UniqueReadings)
	).

eliminateEquivalentReadings(NumberedReadings,_,UniqueReadings):-
	findall(Score:Reading,memberList(n(_,Score,Reading),NumberedReadings),UniqueReadings).


/*========================================================================
   Printing a set of DRSs
========================================================================*/

printDRSs(Readings):-
   printDRSs(Readings,0).

printDRSs([],_):- nl.
printDRSs([Reading|OtherReadings],M):-
   N is M + 1,
   format('~nDRS ~p:',[N]), 
   printDrs(Reading), nl,
   printDRSs(OtherReadings,N).

