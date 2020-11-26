:- ensure_loaded('util').

:- prolog_use_module(library(julian)).

:- prolog_ensure_loaded('args').

:- consult('parser').
:- consult('pddl_wrapper').
:- consult('failure_modes').
:- consult('wopr_data').
:- consult('iem').

%% flag(lower_case).

naturalNumbers([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99]).
varNames(['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z']).

generatePageFor(iem,User,Result) :-
	%% choose the selected plan, for now, make it flp
	currentPlanningCapsule(Capsule),
	doPlan(User,Capsule,Result).

doPlan(User,Capsule,Result) :-
	normalForm([capsule(Capsule)]),
	getPlan('worlds/',Capsule,Transformed),
	%% getPlan('templates/',Capsule,Transformed),
	%% updateFLPInternalSchedule(plans,[transformed(Transformed)]),
	Result = Transformed.

getPlan(Dir,Capsule,Transformed) :-
	loadIEM(Dir,Capsule,Transformed).

loadIEM(Dir,Capsule,Transformed) :-
	%% parseCapsule([templateDir(Dir),capsule(Capsule),parsed(Parsed),extension('verb')]),
	parseCapsule([templateDir(Dir),capsule(Capsule),parsed(Parsed),extension('pddl')]),
	view([capsule,parsed]),
	transform_capsule(Parsed,Transformed),
	view([transformed,Transformed]),
	processPlanIEM(Parsed,Results).

transform_capsule(Parsed,[Parsed,TransformedGoals,TransformedSteps]) :-
	viewIf([parsed,Parsed]),
	argt(Parsed,[domainFile(DomainFile),domain(Domain),problemFile(ProblemFile),problem(Problem),solutionFile(SolutionFile),solution(Solution),verbFile(VerbFile),verb(Verb)]),
	viewIf([items,[domainFile(DomainFile),domain(Domain),problemFile(ProblemFile),problem(Problem),solutionFile(SolutionFile),solution(Solution),verbFile(VerbFile),verb(Verb)]]),
	Solution = solution(Steps),
	pddlGoals(Problem,Goals),
	findall(TransformedGoal,
		(
		 member(Goal,Goals),
		 transformGoal(Goal,TransformedGoal)
		),
	       TransformedGoals),
	actionNLGTemplates(List),
	findall(TransformedStep,
		(   
		    member([StartTime,Predicate,Args,Duration],Steps),
		    viewIf([step,[StartTime,Predicate,Args,Duration]]),
		    getWritten(List,[StartTime,Predicate,Args,Duration],Parsed,[Written,Abbrev],[PreconditionDescriptions,EffectDescriptions,PlanStepStartDateTime,PlanStepStartDateTimeGloss,PlanStepEndDateTime,PlanStepEndDateTimeGloss]),
		    viewIf([written,Written]),
		    Action = [Predicate|Args],
		    TransformedStep = [StartTime,Action,Duration,[Written,Abbrev],[PreconditionDescriptions,EffectDescriptions,PlanStepStartDateTime,PlanStepStartDateTimeGloss,PlanStepEndDateTime,PlanStepEndDateTimeGloss]],
		    viewIf([transformedStep,TransformedStep]),
		    true
		),
		TmpTransformedSteps),
	naturalNumbers(NaturalNumbers),
	viewIf([naturalNumbers,NaturalNumbers,tmpTransformedSteps,TmpTransformedSteps]),
	pairLists(NaturalNumbers,TmpTransformedSteps,TransformedSteps,_),
	viewIf([transformedSteps,TransformedSteps]),
	true.

actionNLGTemplates([
		    %% ['arm','<B> <SHOULD> <A> the <C> in the <D>'],
		    %% ['charge-fully','<D> <SHOULD> <A> the <B> using the <C> in the <E>'],
		    %% ['clean-location','<B> <SHOULD> <A> the <C>'],
		    %% ['dry-laundry-load','<B> <SHOULD> <A> the loads <F>, <G> and <H> in the <D> in the <E>'],
		    %% ['eat','<B> <SHOULD> <A> a(n) <C> in the <D>'],
		    %% ['load','<B> <SHOULD> <A> the <C> into the <D> in the <E>'],
		    %% ['lock-container','<B> <SHOULD> <A> the <C> in the <D>'],
		    %% ['lock-door','<B> <SHOULD> <A> the door <C> from inside the <D> to the <E>'],
		    %% ['move','<B> <SHOULD> <A> from the <C> to the <D>'],
		    %% ['pick-up','<B> <SHOULD> <A> the <C> in the <D>'],
		    %% ['plug-in','<B> <SHOULD> <A> the <C> into the <D> in the <E>'],
		    %% ['set-down','<B> <SHOULD> <A> the <C> at the <D>'],
		    %% ['shave','<C> <SHOULD> <A> with the <B> in the <D>'],
		    %% ['shower','<B> <SHOULD> <A> using the <C> in the <D> in the <E>'],
		    %% ['sleep','<B> <SHOULD> <A> on the <C> in the <D>'],
		    %% ['unload','<B> <SHOULD> <A> the <C> from the <D> in the <E>'],
		    %% ['unlock-container','<B> <SHOULD> <A> the <C> in the <D>'],
		    %% ['unlock-door','<B> <SHOULD> <A> the door <C> from inside the <D> to the <E>'],
		    %% ['unplug','<B> <SHOULD> <A> the <C> from the <D> in the <E>'],
		    %% ['use-object','<B> <SHOULD> <A> the <C> in the <D>'],
		    %% ['use-the-restroom','<B> <SHOULD> <A> called <C>'],
		    %% ['wait','<B> <SHOULD> <A> in the <C>'],
		    %% ['complete','<C> <SHOULD> <A> the task: <B>'],

		    %% ['work','<B> <SHOULD> <A> at <C>'],
		    %% ['wash-laundry','<B> <SHOULD> <A> at <C> on <D>'],
		    %% ['dry-laundry','<B> <SHOULD> <A> at <C> on <D>'],
		    %% ['lock','<B> <SHOULD> <A> the <C> at <D>'],
		    %% ['unlock','<B> <SHOULD> <A> the <C> at <D>'],
		    %% ['charge','<B> <SHOULD> <A> the <C> at <D>'],
		    %% ['use-tool','<B> <SHOULD> <A> the <C> at <D>'],

		    ['travel','<B> should <A> from <C> to <D>'],
		    ['pick-up','<B> <SHOULD> <A> the <C> in the <D>'],
		    ['set-down','<B> <SHOULD> <A> the <C> at the <D>'],
		    ['carry','<B> should <A> the <C> from <D> to <E>'],
		    ['place-into','<B> should <A> the <C> into the <D> at <E>'],
		    ['drive','<B> should <A> the <C> from <D> to <E>'],

		    ['purchase','<B> <SHOULD> <A> <C>'],
		    ['consume','<B> <SHOULD> <A> <C>'],
		    ['replete','<B> <SHOULD> be <A>']

		   ]).

pairLists([EltA|RestA],[EltB],[RestPairedList],[RestA,[]]) :-
	length(RestA,A),A > 0,
	RestPairedList = [EltA,EltB].
pairLists([EltA],[EltB|RestB],[RestPairedList],[[],RestB]) :-
	length(RestB,B),B > 0,
	RestPairedList = [EltA,EltB].
pairLists([EltA|RestListA],[EltB|RestListB],Result,Res) :-
	(   length(RestListA,A), A > 0 ; length(RestListB,B), B > 0) ->
	(   pairLists(RestListA,RestListB,RestPairedList,Res),
	    Result = [[EltA,EltB]|RestPairedList]) ;
	(   Result = [[EltA,EltB]],
	    Res = []).

getWritten(List,[StartTime,Predicate,Args,Duration],Parsed,[Written,Abbrev],[PreconditionDescriptions,EffectDescriptions,PlanStepStartDateTime,PlanStepStartDateTimeGloss,PlanStepEndDateTime,PlanStepEndDateTimeGloss]) :-
	viewIf([predicate2,Predicate]),
	downcase_atom(Predicate,DCPredicate),
	member([DCPredicate,Template],List),
	viewIf([template,Template]),
	Entries = [Predicate|Args],
	length(Entries,EntriesLength),
	varNames(Vars),
	viewIf([vars,Vars,entries,Entries]),
	pairLists(Vars,Entries,TmpPaired,_),
	viewIf([tmpPaired,TmpPaired]),
	append(TmpPaired,[['SHOULD','SHOULD']],Paired),
	viewIf([paired,Paired]),
	getWrittenHelper(Paired,Template,Written),
	view([1]),
	view([written,Written]),
	(   regex('^.*should (.+)$',[],Written,[String]) -> (atom_string(Abbrev,String),viewIf([abbrev,Abbrev])) ; (Abbrev is Written)),
	getPreconditionsAndEffects([StartTime,DCPredicate,Args,Duration],Parsed,[PreconditionDescriptions,EffectDescriptions,PlanStepStartDateTime,PlanStepStartDateTimeGloss,PlanStepEndDateTime,PlanStepEndDateTimeGloss]),
	!.

properNounPhrases(['andy','cathedral-of-learning','doherty-4201','flagstaff-hill','doherty-hall','ibm-r30','cs-lounge','baker-locker','baker-hall','forbes-and-chesterfield']).

stopwords([a,at,the,of,in,on,at]).

stopword(Word) :-
	stopwords(Stopwords),
	downcase_atom(Word,DCWord),
	member(DCWord,Stopwords).

generateGlossForHelper(Entry,Gloss) :-
	hasEnglishGlossesData(Entry,Glosses),
	nth1(1,Glosses,Gloss),
	!.

generateGlossForHelper(Entry,Gloss) :-
	not(hasEnglishGlossesData(Entry,_)),
	properNounPhrases(ProperNounPhraseList),
	member(Entry,ProperNounPhraseList),
	split_string(Entry,'-','',Words),
	findall(Word,
		(
		 member(Tmp,Words),
		 downcase_atom(Tmp,Tmp2),
		 (   stopword(Tmp2) -> (Word = Tmp2) ; fixCapitalize(Tmp2,Word))
		),
		AllWords),
	viewIf([allWords,AllWords]),
	atomic_list_concat(AllWords,' ',Gloss),
	!.

generateGlossForHelper(Entry,Gloss) :-
	not(hasEnglishGlossesData(Entry,_)),
	split_string(Entry,'-','',Words),
	findall(Word,(member(Tmp,Words),downcase_atom(Tmp,Word)),AllWords),
	atomic_list_concat(AllWords,' ',Gloss),
	!.

getWrittenHelper([[Var,Entry]|Rest],Template,Result) :-
	viewIf([var,Var,entry,Entry]),
	generateGlossForHelper(Entry,Gloss),
	atomic_list_concat(['(<',Var,'>)'],'',Regex),
	viewIf([regex,Regex]),
	regex_replace(Template,Regex,Gloss,[],NextTemplate),
	view([a]),
	getWrittenHelper(Rest,NextTemplate,Result),
	view([b]).
getWrittenHelper([],Template,Template) :-
	true,view([c]),!.

fixCapitalize(X,Y) :-
	capitalize(X,Y),!.

%% member(Arg,Args),
%% atomic_list_concat(['(<',A,'>)'],'',Regex),
%% regex_replace(Template,Regex,, Flags, Result)

getPreconditionsAndEffects([StartTime,Predicate,Args,TmpDuration1],Parsed,[PreconditionDescriptions,EffectDescriptions,PlanStepStartDateTime,PlanStepStartDateTimeGloss,PlanStepEndDateTime,PlanStepEndDateTimeGloss]) :-
	viewIf([predicate1,Predicate]),	
	argt(Parsed,[domainFile(DomainFile),domain(Domain),problemFile(ProblemFile),problem(Problem),solutionFile(SolutionFile),solution(Solution),verbFile(VerbFile),verb(Verb)]),
	view([verb,[verbFile(VerbFile),verb(Verb)]]),
	%% view([problem,Problem]),
	Problem = problem(ProblemName,DomainName,_,_,_,_),

	view([specs,[Predicate,TypeSpec,TmpDuration1,Preconditions,Effects]]),
	view(1),
	view([domain,Domain]),
	actions(Domain,durativeAction(Predicate,TypeSpec,TmpDuration2,Preconditions,Effects)),
	%% view(1),
	%% FloatDuration1 is float(TmpDuration1),
	%% view(1),
	%% view([tmpDuration2,TmpDuration2]),
	%% FloatDuration2 is float(TmpDuration2),
	%% view(1),
	%% FloatDuration1 = FloatDuration2,
	%% view(1),
	%% Duration = FloatDuration1,
	view([2]),
	Duration = 0.1,
	viewIf([durativeAction,durativeAction(Predicate,TypeSpec,Duration,Preconditions,Effects)]),	

	findall([Var,Type],(member(are(Vars,Type),TypeSpec),member(Var,Vars)),VarSpecs),
	pairLists(VarSpecs,Args,Pairs,_),
	viewIf([pairs,Pairs]),	

	calculatePlanTime(DomainName,ProblemName,Verb,StartTime,Duration,[PlanStepStartDateTime,PlanStepStartDateTimeGloss,PlanStepEndDateTime,PlanStepEndDateTimeGloss]),

	viewIf([planTime,[StartTime,Duration,PlanStepStartDateTime,PlanStepEndDateTime]]),
	findall([PreconditionDescription,Checked],
		(
		 member(Precondition,Preconditions),
		 view([precondition,Precondition]),
		 convert_pregd(Precondition,Pairs,PreconditionDescription),
		 %% Checked = 'checked'
		 instantiatePrecondition(Precondition,Pairs,InstantiatedPrecondition),
		 (   verifyPrecondition(InstantiatedPrecondition,PlanStepStartDateTime,PlanStepEndDateTime) -> (Checked = 'checked') ; (Checked = '')),
		 view([checked,Checked])
		),
		PreconditionDescriptions),
	findall(EffectDescription,(member(Effect,Effects),view([effect,Effect]),convert_pregd(Effect,Pairs,EffectDescription)),EffectDescriptions),
	view([preconditions,Preconditions,effects,Effects,preconditionDescriptions,PreconditionDescriptions,effectDescriptions,EffectDescriptions]).

instantiatePrecondition(Precondition,Pairs,InstantiatedPrecondition) :-
	length(Pairs,Length),
	view([pairs,Pairs]),
	(   Precondition =.. [Condition,not(PreGD)] -> (Concat = '*not* ') ;
	    (	Precondition =.. [Condition,PreGD] -> (Concat = '') ; true )),
	view([preGd,PreGD]),
	PreGD =.. [P|A],
	view([p,P,a,A]),
	findall(ObjectName,(member(Pair,Pairs),member('$VAR'(VarName),A),Pair = [['$VAR'(VarName),_],ObjectName]),ObjectNames),
	Fluent =.. [P|ObjectNames],
	InstantiatedPrecondition =.. [Condition,Fluent].

verifyPrecondition(Precondition,StartDateTime,EndDateTime) :-
	view([verifyPrecondition1,[Precondition,StartDateTime,EndDateTime]]),
	Precondition =.. [Condition,PreGD],
	%% FIXME: I don't think atTimeQuery is correct here, it does
	%% unification.  what we are looking for is the truth value.  Also, it doesn't handle negation, etc
	(   Condition = 'at start' ->
	    (	view(atTimeQuery(StartDateTime,PreGD)),
		atTimeQuery(StartDateTime,PreGD));
	    (	Condition = 'at end' ->
		(   view(atTimeQuery(EndDateTime,PreGD)),
		    atTimeQuery(EndDateTime,PreGD));
		(   Condition = 'over all' ->
		    (	
			%% FIXME, we have to definitvely search it, this will work for now
			view([
			      atTimeQuery(StartDateTime,PreGD),
			      atTimeQuery(EndDateTime,PreGD)
			     ]),
			atTimeQuery(StartDateTime,PreGD),
			atTimeQuery(EndDateTime,PreGD)
		    ) ; fail) ; fail) ; fail).

verifyPrecondition(Precondition,StartDateTime,EndDateTime) :-
	view([verifyPrecondition2,[Precondition,StartDateTime,EndDateTime]]),
	Precondition =.. [Condition,Tmp],
	Tmp =.. [not,PreGD],
	%% FIXME: I don't think atTimeQuery is correct here, it does
	%% unification.  what we are looking for is the truth value.  Also, it doesn't handle negation, etc
	(   Condition = 'at start' ->
	    (	not(atTimeQuery(StartDateTime,PreGD)));
	    (	Condition = 'at end' ->
		(   not(atTimeQuery(EndDateTIme,PreGD)));
		(   Condition = 'over all' ->
		    (	
			%% FIXME, we have to definitvely search it, this will work for now
			not(atTimeQuery(StartDateTime,PreGD)),
			not(atTimeQuery(EndDateTime,PreGD))
		    ) ; fail) ; fail) ; fail).

transformGoal(Goal,[Goal,TransformedGoal]) :-
	viewIf([goal1,Goal]),
	Goal =.. [P|Args],

	P \= 'not',

	(   Args = [A] ->
	    getThisSentence([A,P],[],Sentence) ;
	    (	Args = [A,B] ->
		getThisSentence([A,B,P],[],Sentence) ;
		(   Args = [A,B,C] ->
		    getThisSentence([A,B,C,P],[],Sentence) ; true) ; true) ; true),
	view([sentence,Sentence]),
	(   forall(member(Item,Sentence),atom(Item)) -> 
	    atomic_list_concat(Sentence,' ',TransformedGoal) ; 
	    print_to_atom(Goal,TransformedGoal)),
	view([transformGoal,TransformedGoal]),
	!.
transformGoal(Goal,[Goal,TransformedGoal]) :-
	view([goal2,Goal]),
	Goal =.. [TmpP|[SubGoal]],
	view(1),
	TmpP = 'not',
	view(2),
	SubGoal =.. [P|Args],
	view(3),
	NegList = ['<em>*not*</em>'],
	(   Args = [A] ->
	    (	view(4),
		getThisSentence([A,P],NegList,Sentence)) ;   
	    (	Args = [A,B] ->
		(   view(5),
		    getThisSentence([A,B,P],NegList,Sentence)) ;
		(   Args = [A,B,C] ->
		    (	view(6),
			getThisSentence([A,B,C,P],NegList,Sentence)) ; true)) ; true);
	view([sentence,Sentence]),
	(   forall(member(Item,Sentence),atom(Item)) -> 
	    atomic_list_concat(Sentence,' ',TransformedGoal) ; 
	    print_to_atom(Goal,TransformedGoal)),
	view([transformGoal,TransformedGoal]),
	!.

getThisSentence([A,P],Neg,Sentence) :-
	describeParticularItem(A,DA),
	describeParticularPredicate(P,DP),
	append([DA,[is],Neg,DP],Sentence).
getThisSentence([A,B,P],Neg,Sentence) :-
	describeParticularItem(A,DA),
	decamelcase(B,DB),
	describeParticularPredicate(P,DP),
	append([DA,[is],Neg,DP,[DB]],Sentence).
getThisSentence([A,B,C,P],Neg,Sentence) :-
	describeParticularItem(A,DA),
	decamelcase(B,DB),
	decamelcase(C,DC),
	describeParticularPredicate(P,DP),
	append([DA,[is],Neg,DP,[DB,DC]],Sentence).

describeParticularItem(A,X) :-
	view([a,A]),
	decamelcase(A,DA),
	(   isa(A,person) ->
	    (	
		X = [DA]) ;
	    (	X = [the,DA])).

describeParticularPredicate(P,DP) :-
	atomic_list_concat(DP,'-',P).

print_to_atom(Item,Atom) :-
	with_output_to(atom(Atom),write_term(Item,[quoted(true)])).

processPlanIEM(Parsed,Results) :-
	argt(Parsed,[domainFile(DomainFile),domain(Domain),problemFile(ProblemFile),problem(Problem),solutionFile(SolutionFile),solution(Solution),verbFile(VerbFile),verb(Verb)]),
	nl,nl,view(parsed),nl,nl,
	orderEventsIEM(Parsed,Sorted),
	nl,nl,view(eventsOrdered),nl,nl,
	updateWorldsIEM(Parsed,Sorted,Worlds).

orderEventsIEM(Parsed,Sorted) :-
	argt(Parsed,[domainFile(DomainFile),domain(Domain),problemFile(ProblemFile),problem(Problem),solutionFile(SolutionFile),solution(Solution),verbFile(VerbFile),verb(Verb)]),
	Domain = domain(DomainName,_,_,_,_,_),
	view([domain,Domain]),
	Problem = problem(ProblemName,DomainName,_,_,_,_),
	view([problem,Problem]),
	Solution = solution(Steps),
	findall(at(Start,action(start,Term)),(member([Start,Action,Arguments,_],Steps),Term =.. [Action|Arguments]),ActionStarts),
	view([actionStarts,ActionStarts]),
	findall(at(End,action(end,Term)),(member([Start,Action1,Arguments1,Duration],Steps),Term =.. [Action1|Arguments1],End is Start + Duration),ActionEnds),
	view([actionEnds,ActionEnds]),
	findall(at(Time,proposition(Prop)),(init(Problem,at(Time,Prop)),float(Time),not(atomic(Prop))),Props),
	view([props,Props]),
	append(ActionStarts,ActionEnds,Tmp1),
	append(Tmp1,Props,Updates),
        predsort(mcompare,Updates,Sorted),!.


updateWorldsIEM(Parsed,Sorted,Worlds) :-
	argt(Parsed,[domainFile(DomainFile),domain(Domain),problemFile(ProblemFile),problem(Problem),solutionFile(SolutionFile),solution(Solution),verbFile(VerbFile),verb(Verb)]),
	%% get all inits that aren't at etc, and create the initial state t0
	getInitIEM(Problem,WorldN),
	view([sorted,Sorted]),
	Problem = problem(DomainName,ProblemName,_,_,_,_),
	view([init,WorldN]),

	%% do we assert it, no, we don't.  we need to generate the
	%% plan from it.  For now, assert it though, but fix later.

	%% we should load the facts, once, then disable.  actually we
	%% will have to reload, cause there are going to be new
	%% assertions.  we should back up the Context, and reload
	%% whenever rerunning it, cause we are going to be asserting
	%% new atTime(DateTime,F) assertions.  We'll have to reset
	%% that.  We'll want to develop a script to do that, and then
	%% launch this, cause there's going to be a lot of debugging
	%% I'm guessing.

	%% assertWorld(DomainName,ProblemName,Verb,WorldN),

	iterateAndUpdateIEM(Parsed,Sorted,Sorted,WorldN).

iterateAndUpdateIEM(Parsed,[],Sorted,World0).
iterateAndUpdateIEM(Parsed,Steps,Sorted,World0) :-
	argt(Parsed,[domainFile(DomainFile),domain(Domain),problemFile(ProblemFile),problem(Problem),solutionFile(SolutionFile),solution(Solution),verbFile(VerbFile),verb(Verb)]),
	Steps = [Step|Rest],
	view([step,Step]),
	viewIf([world,World0]),
	%% updateWorldIEM(Parsed,Steps,Sorted,World0,Step,World1),
	iterateAndUpdateIEM(Parsed,Rest,Sorted,World1).

updateWorldIEM(Parsed,Steps,Sorted,World0,Step,World1) :-
	argt(Parsed,[domainFile(DomainFile),domain(Domain),problemFile(ProblemFile),problem(Problem),solutionFile(SolutionFile),solution(Solution),verbFile(VerbFile),verb(Verb)]),
	(   (	Step = at(Time,action(Point,Action)),(number(Time) ; float(Time))) -> 
	    (	(   Point = start ->
		    (	Step = at(StartTime,action(start,Action)),
			member(at(EndTime,action(end,Action)),Sorted) ) ;
		    (	Step = at(EndTime,action(end,Action)),
			member(at(StartTime,action(start,Action)),Sorted) )
		),
		findall(at(Time2,Item2),(member(at(Time2,Item2),Sorted),Time2 >= StartTime, Time2 =< EndTime),Items),
		view([items,Items]),
		Action =.. [Predicate|Args],
		view([predicate,Predicate]),
		actions(Domain,durativeAction(Predicate,TypeSpec,Duration,Preconditions,Effects)),
		checkPreconditionsIEM([Step,Time,Point,Action,Items],durativeAction(Predicate,TypeSpec,Duration,Preconditions,Effects),Sorted,World0,World1))
	;   
	    true),
	World1 = World0.

checkPreconditionsIEM([Step,Time,Point,Action,Items],DurativeAction,Sorted,World0,World1) :-
	view([checkPreconditions,[[Step,Time,Point,Action,Items],DurativeAction,Sorted,World0,World1]]),
	view([durativeAction,DurativeAction]),
	DurativeAction = durativeAction(Predicate,TypeSpec,Duration,Preconditions,Effects),
	findall([Var,Type],(member(are(Vars,Type),TypeSpec),member(Var,Vars)),VarSpecs),
	Action =.. [Predicate|Args],
	length(Args,Arity),
	length(VarSpecs,Arity),
	checkTypesIEM(Args,VarSpecs),
	member(Precondition,Preconditions),
	(   Precondition = 'at start'(Condition) -> (true) ;
	    (	Precondition = 'at end'(Condition) -> (true) ;		
		(   Precondition = 'over all'(Condition) -> ( true) ; (true) ) ) ),

	%% holds(Precondition,World).

	%% [at(36.148,action(start,load(andy,'duffel-bag',bookbag,'cathedral-of-learning'))),at(36.215,action(end,load(andy,bookbag,'laptop-backpack','cathedral-of-learning'))),at(36.248000000000005,action(end,load(andy,'duffel-bag',bookbag,'cathedral-of-learning')))]
	true.

	%% ['over all'(mobile('$VAR'('Ob'))),'over all'(forall([are(['$VAR'('Lc')],'lockable-container')],not(['$VAR'('Lc')='$VAR'('Lo'),locked('$VAR'('Lc'))]))),'at start'(at('$VAR'('P'),'$VAR'('L'))),'at start'(at('$VAR'('Ob'),'$VAR'('L'))),'at start'(at('$VAR'('Lo'),'$VAR'('L')))]

checkTypesIEM(Args,VarSpecs) :-
	Args = [Arg|ArgsRest],
	VarSpecs = [[Var,Type]|VarSpecsRest],
	view([isa,isa(Arg,Type)]),
	checkTypesIEM(ArgsRest,VarSpecsRest).
checkTypesIEM([],[]).

propagateEffectsIEM([Step,Time,Point,Action,Items],DurativeAction,Sorted,World0,World1) :-
	%% take the world, and update items as needed.  use deduction
	%% rather than unification, i.e. followsFrom(Fact,World),
	%% rather than member(Fact,World).  Also try to first see if
	%% it contradicts the world.
	

	true.


%% checkPreconditions(Step,DurativeAction,World) :-
%%      DurativeAction = durativeAction(Predicate,TypeSpec,Duration,Preconditions,Effects),
%%      %% checkTypes(TypeSpec,World),
%% 	((forall((member(Precondition,Preconditions),Precondition =.. [Constraint|Fluent]),
%% 		 ((Constaint = 'over all') ->
%% 		  () ;
%% 		  ((Constraint = 'at start') ->
%% 		   () ;
%% 		   ((Constraint = 'at end') ->
%% 		    () ;
%% 		    ()))))) ->
%% 	 () ;
%% 	 ()).

%% 'over all' -> get all time points between (inclusive) two times,
%% then iterate over those and verify holds(P,Z).

getInitIEM(problem(A,B,C,D,E,F),Init) :-
	findall(Assertion,init(problem(A,B,C,D,E,F),Assertion),Assertions),
	findall(Assertion,
		(
		 member(Item,Assertions),
		 not(((Item = at(NumberOrFloat,Fluent), NumberOrFloat \= 0, (float(NumberOrFloat); number(NumberOrFloat))) ;
		       (Item = set(Function,Value)))),
		 (Item = at(0,Tmp) ->  (Assertion = Tmp) ; (Assertion = Item))
		),
		Init).

assertWorld(DomainName,ProblemName,Verb,WorldN) :-
	viewIf([1,[verb,Verb]]),
	member(verber(domain(DomainName),problem(ProblemName),includes(Includes),startDate(StartDateTime),units(TimeUnits)),Verb),
	viewIf(2),
	member(InitAssertion,WorldN),
	viewIf(3),
	fassert('Agent1','Yaswi1',atTime(StartDateTime,InitAssertion),Result),
	viewIf(4),
	fail.
assertWorld(DomainName,ProblemName,WorldN) :-
	viewIf(5),
	true.

 	%% setof([Y,M,D,H,Mi,S],
	%%       (
	%%        julian:form_time([DOW,[Y-M-D]]),
	%%        julian:compare_time(>,[Y-M-D,H:Mi:S],Start),
	%%        julian:compare_time(<,[Y-M-D,H:Mi:S],Finish),
	%%        label([Y,M,D])
	%%       ),Timestamps).


calculatePlanTime(DomainName,ProblemName,Verb,PlanStepStartTime,PlanStepDuration,[StartDateTime,StartDateTimeGloss,EndDateTime,EndDateTimeGloss]) :-
	view([calculatePlanTime,calculatePlanTime(DomainName,ProblemName,Verb,PlanStepStartTime,PlanStepDuration,PlanStepStartDateTime,PlanStepEndDateTime)]),
	member(verber(domain(DomainName),problem(ProblemName),includes(Includes),startDate(PlanStartDateTime),units(DurationUnit)),Verb),
	view(2),
	getSecondsForScalarAmountOfTimeUnits(PlanStepStartTime,DurationUnit,PlanStepStartTimeSeconds),
	view([planStepStartTimeSeconds,PlanStepStartTimeSeconds]),
	julian:delta_time(PlanStartDateTime,s(PlanStepStartTimeSeconds),[Y1-M1-D1,H1:Mi1:S1]),
	view(1),
	view([tmp1,[Y1,M1,D1,H1,Mi1,S1]]),
	StartDateTime = [Y1-M1-D1,H1:Mi1:S1],
	generateGlossFor(StartDateTime,StartDateTimeGloss),

	getSecondsForScalarAmountOfTimeUnits(PlanStepDuration,DurationUnit,PlanStepDurationSeconds),
	view([planStepDurationSeconds,PlanStepDurationSeconds]),
	julian:delta_time(StartDateTime,s(PlanStepDurationSeconds),[Y2-M2-D2,H2:Mi2:S2]),
	view([tmp1,[Y2,M2,D2,H2,Mi2,S2]]),
	EndDateTime = [Y2-M2-D2,H2:Mi2:S2],
	generateGlossFor(EndDateTime,EndDateTimeGloss),

	view([startDateTime,StartDateTime,startDateTimeGloss,StartDateTimeGloss,endDateTime,EndDateTime,endDateTimeGloss,EndDateTimeGloss]).

tuneSystem1 :-
	nl,nl,
	(   atTimeQuery([2017-3-30,12:0:0],autonomous(andy)) ->
	    print(hi) ;
	    print(ho)),
	nl,nl,
	(   atTimeQuery([2017-3-30,12:0:0],autonomous('andrew-dougherty')) ->
	    print(hi) ;
	    print(ho)),
	nl,nl.

tuneSystem2 :-
	findall([DateTime,Assertion],atTime(DateTime,Assertion),Items),print_list(Items).


%% 
%% atTimeQuery([2017-3-30,12:0:0],autonomous('andrew-dougherty')).
%% verifyPrecondition('at start'(autonomous('andrew-dougherty')),[2017-3-30,12:0:0],[2017-3-30,13:0:0]).


%% setof([StartDateTime],
%%       (
%%        getSecondsForScalarAmountOfTimeUnits(PlanStepStartTime,DurationUnit,PlanStepStartTimeSeconds),
%%        view([planStartDateTime,PlanStartDateTime]),
%%        julian:delta_time(PlanStartDateTime,s(PlanStepStartTimeSeconds),[Y1-M1-D1,H1:Mi1:S1]),
%%        view([tmp1,[Y1,M1,D1,H1,Mi1,S1]]),
%%        StartDateTime = [Y1,M1,D1,H1,Mi1,S1],
%%        label(StartDateTime)
%%        ),StartDateTimes),

%% setof([EndDateTime],
%%       (
%%        getSecondsForScalarAmountOfTimeUnits(PlanStepDuration,DurationUnit,PlanStepDurationSeconds),
%%        julian:delta_time(PlanStepStartDateTime,s(PlanStepDurationSeconds),[Y2,M2,D2,H2,Mi2,S2]),
%%        EndDateTime = [Y2,M2,D2,H2,Mi2,S2],
%%        label(EndDateTime)
%%       ),EndDateTimes),

getSecondsForDuration(DurationUnit,DurationUnitSeconds) :-
	%% julian:form_time(DurationUnit,DurationUnitSeconds),
	(   DurationUnit = [0-0-0,1:0:0] ->
	    DurationUnitSeconds = 3600 ;
	    fail ).

getSecondsForScalarAmountOfTimeUnits(PlanStepDuration,DurationUnit,PlanStepDurationSeconds) :-
	getSecondsForDuration(DurationUnit,DurationUnitSeconds),
	PlanStepDurationSeconds is round(PlanStepDuration * DurationUnitSeconds).

%% getSecondsForScalarAmountOfTimeUnits(0.35,[0-0-0,1:0:0],PlanStepDurationSeconds).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FLP IEM STUFF

flpIEMCurrentTask() :-
	true.

flpIEMVerifyPreconditions() :-
	true.

flpIEMPropagateEffects() :-
	true.

downcaseTerm(Pre,Post) :-
	(   (	is_list(Pre)) ->
	    (	view([pre0,Pre]),
		findall(Item,(member(SubPre,Pre),correctCases(SubPre,Item)),Post)
	    )

	;   
	    
	    (	(   Pre =.. [PredicateName|Args]) ->	
		(   
		    view([pre2,Pre,predicateName,PredicateName,args,Args]),
		    (	downcase_atom(PredicateName,LCPredicateName) -> true ; LCPredicateName = PredicateName),
		    view([predicateName,PredicateName,lcPredicateName,LCPredicateName]),
		    findall(Item,(member(SubArgs,Args),correctCases(SubArgs,Item)),Items),
		    Post =.. [LCPredicateName|Items]
		) ;
		(   
		    view([pre1,Pre]),
		    downcase_atom(Pre,Post)
		))).

correctCases(TmpSolution,Solution) :-
	%% view([tmpSolution,tmpSolution]),
	Pre = TmpSolution,
	Post = Solution,
	%% view([pre3153,Pre]),
	(   (	is_list(Pre)) ->
	    (
	     %% view([pre0,Pre]),
	     findall(Item,(member(SubPre,Pre),correctCases(SubPre,Item)),Post)
	    ) ;   
	    (	(   Pre =.. [PredicateName|Args]) ->	
		(   
		    %% view([pre2,Pre,predicateName,PredicateName,args,Args]),
		    (	fixCase(PredicateName,LCPredicateName) -> true ; LCPredicateName = PredicateName),
		    %% view([predicateName,PredicateName,lcPredicateName,LCPredicateName]),
		    findall(Item,(member(SubArgs,Args),correctCases(SubArgs,Item)),Items),
		    Post =.. [LCPredicateName|Items]
		) ;
		(   
		    %% view([pre1,Pre]),
		    fixCase(Pre,Post)
		))).

fixCase('MEREDITHMCGHANSCAR','meredithMcGhansCar').
fixCase('DRIVE','drive').
fixCase('TRAVEL','travel').
fixCase('PICK-UP','pick-up').
fixCase('FLINTMICHIGAN',flintMichigan).
fixCase('MEREDITHMCGHAN',meredithMcGhan).
fixCase('ANDREWDOUGHERTY',andrewDougherty).
fixCase('BLUETOOTHKEYBOARD',bluetoothKeyboard).
fixCase('TOWNHOMEOFELEANORANDANDREWANDMEREDITH',townhomeOfEleanorAndAndrewAndMeredith).
fixCase('AURORAILLINOIS',auroraIllinois).
fixCase('ANDREWDOUGHERTYSHYPOTHETICALCAR',andrewDoughertysHypotheticalCar).

fixCase('BEEFRAVIOLI_CHEFBOYARDEE1',beefRavioli_ChefBoyardee1).
fixCase('MACANDCHEESE_CHEFBOYARDEE1',macAndCheese_ChefBoyardee1).
fixCase('SPAGHETTIRINGSPASTAWITHMEATBALLS_GREATVALUE1',spaghettiRingsPastaWithMeatballs_GreatValue1).
fixCase('CHICKENNOODESOUP_GREATVALUE1',chickenNoodeSoup_GreatValue1).
fixCase('TRADITIONALSPLITPEAWITHHAM_PROGRESSO1',traditionalSplitPeaWithHam_Progresso1).

addNewObjective(NewObjectiveText,Result) :-
	parseNewObjective(NewObjectiveText,NewObjective),
	Result = true.
