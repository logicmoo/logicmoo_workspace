:- dynamic naturalLanguageQueryHistory/2,
naturalLanguageAnswerHistory/3, naturalLanguageCommandHistory/3,
currentAgent/1, currentSpeaker/1, currentWSMContext/1.

setByRetractingAllAndAsserting(Term) :-
	Term =.. [Predicate|Args],
	length(Args,Length),
	length(TmpArgs,Length),
	TmpTerm =.. [Predicate|TmpArgs],
	retractall(TmpTerm),
	assert(Term).

alexaSkillFLPQuery(Query,Response) :-
	alexaSkillFLPQueryHelper(Query,Response,0,1).

alexaSkillFLPQueryHelper(Query,Response,Simulate,Log) :-
	currentWSMContext(TmpContext),
	atom_concat('Org::Cyc::WSMContext::',TmpContext,Context),
	(   Log = 1 ->
	    (	getCurrentDateTime(DateTime),
		fassert_argt('Agent1','Yaswi1',[term(naturalLanguageQueryHistory(Query,DateTime)),context(Context)],Result1)) ;
	    true ),
	naturalLanguageQuery(Query,Result2),
	(   Result2  = assert(Assertion) ->
	    (	view([asserting,[assertion,Assertion,context,Context]]),
		(   Simulate = 1 ->
		    true ;
		    (	
			%% FIXME: before actually asserting we will
			%% want to verify that it doesn't contradict
			%% the KB, as with FreeKBS2

			%% FIXME: distinguish which WSM Context to
			%% assert to.

			fassert_argt('Agent1','Yaswi1',[term(Assertion),context(Context)],Result3),
			Result4 = [command(assert(Assertion)),returnValue(Result3),answer(-1)],
			view([assertion,Result4])
		    ) ) ) ; 
	    (	Result2 = query(ActualQuery) ->
		(   view([querying,[query,ActualQuery,context,Context]]),
		    (	Simulate = 1 ->
			true ;
			(
			 term_variables(ActualQuery,Vars),
			 view([query,ActualQuery,vars,Vars]),
			 
			 %% FIXME: distinguish which WSM Context to
			 %% query from.

			 call(ActualQuery),
			 view([query,ActualQuery]),
			 Result4 = [command(query(ActualQuery)),returnValue(ReturnValue),answer(Vars)],
			 view([query,Result4])
			) ) ) ;
		Result4 = [command(Result2),returnValue(-1),answer(-1)]
	    ) ),
	view([result4,Result4]),
	generateResponseFromTemplate(Result4,Answer),

	%% (   Log = 1 ->
	%%     (
	%%      fassert_argt('Agent1','Yaswi1',[term(naturalLanguageAnswerHistory(Query,Answer,DateTime)),context(Context)],Result1),
	%%      fassert_argt('Agent1','Yaswi1',[term(naturalLanguageCommandHistory(Query,Result4,DateTime)),context(Context)],Result1)
	%%     ) ;
	%%     true ),
	atomic_list_concat(['<alexa-skill-flp-result>',Answer,'</alexa-skill-flp-result>'],'',Response),
	!.
alexaSkillFLPQueryHelper(Query,Response,Simulate,Log) :-
	atom_concat('I heard and cannot understand this: ',Query,Atom),
	atomic_list_concat(['<alexa-skill-flp-result>',Atom,'</alexa-skill-flp-result>'],'',Response),
	!.

alexa(Query,Response) :-
	alexaSkillFLPQueryHelper(Query,Response,0,1).

testAlexa(Query,Response) :-
	alexaSkillFLPQueryHelper(Query,Response,0,0).

testAlexaSim(Query,Response) :-
	alexaSkillFLPQueryHelper(Query,Response,1,0).

hasEnglishGloss(Symbol,Gloss) :-
	hasEnglishGlosses(Symbol,Glosses),
	member(Gloss,Glosses).

hasEnglishGloss2(Symbol,Gloss) :-
	setof(Symbol,Glosses^(hasEnglishGlosses(Symbol,Glosses),member(Gloss,Glosses)),Results),
	nth(1,Results,Symbol).

hasEnglishGlosses(X,Y) :-
	not(hasEnglishGlossesData(X,Y)),
	Y = [X],!.
hasEnglishGlosses(X,Y) :-
	hasEnglishGlossesData(X,Y).

resolveProperName(NameString,Term) :-
	findall(Agent,hasFirstName(Agent,firstNameFn(NameString)),Agents),
	((length(Agents,0)) ->
	 (findall(Agent,hasLastName(Agent,lastNameFn(NameString)),Agents),
	  ((length(Agents,0)) ->
	   (Term = unknownAgentFn(Agent)) ;
	   ((length(Agents,1)) ->
	    ([Term] = Agents) ;
	    (Term = oneOf(Agents))))) ;
	 ((length(Agents,1)) ->
	  ([Term] = Agents) ;
	  (Term = oneOf(Agents)))).

%% develop common substitutions, such as (8|eight|ate), and use this
%% to correct the lack of language modeling in our (borrowed)
%% circumvention of the deprecation of LITERAL slots
phoneticallySimilar(['8','eight','ate']).

%% naturalLanguageQuery(Query,Response) :-
%% 	atomic_list_concat(['Logging: ',Query],'',Response).

%% currentlyHolds(atTime(DateTime,Assertion)) :-
%% 	findall().

%% have the option of distinguishing between things that change over
%% time and events that are recorded at specific times, i.e. that you
%% saw the keyboard in the kitchen at two different times is different
%% than Mom eating grapes two different times.  Figure out this
%% difference.

listQueryDateTimes(QueryDateTimes) :-
	findall(naturalLanguageQueryHistory(Query,DateTime),naturalLanguageQueryHistory(Query,DateTime),QueryDateTimes).

listQueries(Queries) :-
	findall(Query,naturalLanguageQueryHistory(Query,DateTime),Queries).
