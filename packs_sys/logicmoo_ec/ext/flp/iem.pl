:- ensure_loaded('pddl_wrapper').

%% loadInit([Domain,Problem,Solution]) :-
%% 	init(Problem,Asserition),
%% 	assertz(holds(0,Assertion)),
%% 	fail.
%% loadInit([Domain,Problem,Solution]).

orderEvents([Domain,Problem,Solution],Sorted) :-
	ProblemName = hygiene,
	Domain = domain(DomainName,_,_,_,_,_),
	view([domain,Domain]),
	Problem = problem(ProblemName,DomainName,_,_,_,_),
	view([problem,Problem]),
	view([solution,Solution]),
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

%% mfloat(T) :-
%% 	atomic_list_concat([A,B],'.',T),
%% 	number(A),
%% 	number(B).

mcompare(Delta, at(A,_), at(B,_)) :-
	A == B;
	compare(Delta, A, B).

updateWorlds([Domain,Problem,Solution],Sorted,Worlds) :-
	%% get all inits that aren't at etc, and create the initial state t0
	getInit(Problem,WorldN),
	view([sorted,Sorted]),
	view([init,WorldN]),
	iterateAndUpdate([Domain,Problem,Solution],Sorted,Sorted,WorldN).

iterateAndUpdate([Domain,Problem,Solution],[],Sorted,World0).
iterateAndUpdate([Domain,Problem,Solution],Steps,Sorted,World0) :-
	Steps = [Step|Rest],
	view([step,Step]),
	view([world,World0]),
	updateWorld([Domain,Problem,Solution],Steps,Sorted,World0,Step,World1),
	iterateAndUpdate([Domain,Problem,Solution],Rest,Sorted,World1).

updateWorld([Domain,Problem,Solution],Steps,Sorted,World0,Step,World1) :-
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
		checkPreconditions([Step,Time,Point,Action,Items],durativeAction(Predicate,TypeSpec,Duration,Preconditions,Effects),Sorted,World0,World1))
	;   
	    true),
	World1 = World0.

%% updateWorld([Domain,Problem,Solution],World0,Step,World1) :-
%% 	Step 
%% 	%% ((Step = at(Time,proposition(Literal))) ->
%% 	%%  %% so if it is positive, and it was already true, no change
%% 	%%  %% to the world state.  If it is positive and it was
%% 	%%  %% previously negative, remove negative if exists, add
%% 	%%  %% positive.  If it is negative and it was previously
%% 	%%  %% negative, no change.  If it is negative and it was
%% 	%%  %% previously positive, remove positive from KB and assert
%% 	%%  %% negative.  We need to figure out how to do this properly
%% 	%%  %% given closed world.  I think basically Fluent /
%% 	%%  %% neg(Fluent).
%% 	%%  true ;
%% 	%%  ((Step = at(Time,action(Position,Literal))) ->
%% 	%%   (Literal =.. [ActionName|Args],
%% 	%%    true
%% 	%%    ) ;
%% 	%%   ())).
%% 	true.


checkPreconditions([Step,Time,Point,Action,Items],DurativeAction,Sorted,World0,World1) :-
	view([durativeAction,DurativeAction]),
	DurativeAction = durativeAction(Predicate,TypeSpec,Duration,Preconditions,Effects),
	findall([Var,Type],(member(are(Vars,Type),TypeSpec),member(Var,Vars)),VarSpecs),
	Action =.. [Predicate|Args],
	length(Args,Arity),
	length(VarSpecs,Arity),
	checkTypes(Args,VarSpecs),
	member(Precondition,Preconditions),
	(   Precondition = 'at start'(Condition) -> (true) ;
	    (	Precondition = 'at end'(Condition) -> (true) ;		
		(   Precondition = 'over all'(Condition) -> ( true) ; (true) ) ) ),

	%% holds(Precondition,World).

	%% [at(36.148,action(start,load(andy,'duffel-bag',bookbag,'cathedral-of-learning'))),at(36.215,action(end,load(andy,bookbag,'laptop-backpack','cathedral-of-learning'))),at(36.248000000000005,action(end,load(andy,'duffel-bag',bookbag,'cathedral-of-learning')))]
	true.

	%% ['over all'(mobile('$VAR'('Ob'))),'over all'(forall([are(['$VAR'('Lc')],'lockable-container')],not(['$VAR'('Lc')='$VAR'('Lo'),locked('$VAR'('Lc'))]))),'at start'(at('$VAR'('P'),'$VAR'('L'))),'at start'(at('$VAR'('Ob'),'$VAR'('L'))),'at start'(at('$VAR'('Lo'),'$VAR'('L')))]

checkTypes(Args,VarSpecs) :-
	Args = [Arg|ArgsRest],
	VarSpecs = [[Var,Type]|VarSpecsRest],
	view([isa,isa(Arg,Type)]),
	checkTypes(ArgsRest,VarSpecsRest).
checkTypes([],[]).

propagateEffects([Step,Time,Point,Action,Items],DurativeAction,Sorted,World0,World1) :-
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

getInit(problem(A,B,C,D,E,F),Init) :-
	findall(Assertion,init(problem(A,B,C,D,E,F),Assertion),Assertions),
	findall(Assertion,
		(   
		    member(Item,Assertions),
		    not(
			(   
			    (	
				Item = at(NumberOrFloat,Fluent),
				NumberOrFloat \= 0,
				(   
				    float(NumberOrFloat) ;
				    number(NumberOrFloat)
				)
			    ) ;
			    (	
				Item = set(Function,Value)
			    )
			)
		       ),
		    (	Item = at(0,Tmp) ->  (Assertion = Tmp) ; (Assertion = Item))
		),
		Init).
