- use_module(library(oset)).

%% we can actually use cyc and semantics here.  For instance, if a
%% precondition that's violated is that the item isn't autonomous, we
%% can use the formal semantics of that to infer what it might do, and
%% hence how it might fail.

listModesForAction(Parsed,Action,Modes) :-

	listSuccessModeForAction(Parsed,Action,SuccessModes),
	listInferrableFailureModesForAction(Parsed,Action,FailureModes1),
	listExplicitFailureModesForAction(Parsed,Action,FailureModes2),
	listWOPRFailureModesForAction(Parsed,Action,FailureModes3),

	append(FailureModes1,FailureModes2,FailureModes1And2),
	append(FailureModes1And2,FailureModes3,FailureModes123),
	append(SuccessModes,FailureModes123,Modes).
        %% Modes = FailureModes1.

requiresFormalization(Atom) :-
	hasSubstring(Atom,' ').

listSuccessModeForAction(_,_,[success]).

listInferrableFailureModesForAction(Parsed,Action,FailureModes) :-
	Action =.. [P|B],
	argt(Parsed,[domainFile(DomainFile),domain(Domain),problemFile(ProblemFile),problem(Problem),solutionFile(SolutionFile),solution(Solution),verbFile(VerbFile),verb(Verb)]),
	actions(Domain,DurativeAction),
	DurativeAction = durativeAction(P,Parameters,Duration,Preconditions,Postconditions),
	view([durativeAction(P,parameters(Parameters),duration(Duration),preconditions(Preconditions),postconditions(Postconditions))]),
	oset_power(Preconditions,PowerSetOfPreconditions),
	oset_power(Postconditions,PowerSetOfPostconditions),
	findall(FailureMode,
		(   
		    member(PreconditionsUnsatisfied,PowerSetOfPreconditions),
		    member(PostconditionsUnsatisfied,PowerSetOfPostconditions),
		    (	
			FailureMode = failureEffects(attemptedAction(Action,preconditionsViolated(PreconditionsUnsatisfied)),Effects) ;
			FailureMode = failureEffects(neg(attemptedAction(Action,preconditionsViolated(PreconditionsUnsatisfied))),Effects) ;
			FailureMode = failureEffects(attemptedAction(Action,postconditionsViolated(PostconditionsUnsatisfied)),Effects) ;
			FailureMode = failureEffects(neg(attemptedAction(Action,postconditionsViolated(PostconditionsUnsatisfied))),Effects) ;
			FailureMode = failureEffects(attemptedAction(Action,incorrectlyDesignedPreconditions(PreconditionsUnsatisfied)),Effects) ;
			FailureMode = failureEffects(neg(attemptedAction(Action,incorrectlyDesignedPreconditions(PreconditionsUnsatisfied))),Effects) ;
			FailureMode = failureEffects(attemptedAction(Action,incorrectlyDesignedPostconditions(PostconditionsUnsatisfied)),Effects) ;
			FailureMode = failureEffects(neg(attemptedAction(Action,incorrectlyDesignedPostconditions(PostconditionsUnsatisfied))),Effects)
		    )
		),
	       FailureModes).

listExplicitFailureModesForAction(_,_,FailureModes) :-
	FailureModes = [].

listWOPRFailureModesForAction(_,_,FailureModes) :-
	FailureModes = [].
	%% findall(occurs(Contingency),planForContingency(Contingency),FailureModes).


generateContingencyPlansForPlan(WorldState0,PlanSteps,ContingencyPlans) :-
	[at(StartTime,Action,Duration)|RemainingPlanSteps] = PlanSteps,
	listModesForAction(Action,Modes),
	findall(ContingencyPlan,
		(member(Mode,Modes),
		 updateWorldStateWithMode(WorldState0,Mode,WorldState1),
		 generateContingencyPlansForPlan(WorldState1,RemainingPlanSteps,ContingencyPlans),
		 member(ContingencyPlan,ContingencyPlans)),
		ContingencyPlans).

updateWorldStateWithMode(WorldState0,Mode,WorldState1) :-
	WorldState1 = WorldState0.

	%% checkPreconditionsAndApplyPostconditions(WorldState0,at(StartTime,PlanStep,Duration),WorldState1),
	%% generateContingencyPlansForPlan(WorldState0,Plan,ContingencyPlans)
	%% generatePlanForContingency().

%% abduceArgNIsa(Predicate/Arity,N,Type) :-
%% 	findall(ArgN,(Term =.. [Predicate|Args],nth0(Args,N,ArgN),nonvar(ArgN)),AllArgNs),
%% 	findall(AllArgNIsas,(member(ArgN,AllArgNs),hasType(ArgN,Type),allIsa(ArgN,AllArgNIsas))).

generateContingencyPlans(Domain,ContingencyPlans) :-
	planSteps(Domain,PlanSteps),
	seer(PlanSteps),
	generateContingencyPlansForPlan([],PlanSteps,ContingencyPlans).

%% execute :-
%% 	domain(Domain),
%% 	generateContingencyPlans(Domain,ContingencyPlans),
%% 	write_list(ContingencyPlans).

%% failureEffects(FailureEffects,Results) :-
%% 	attemptedAction(Action,preconditionsViolated(PreconditionsUnsatisfied)).

displayModes(Parsed,Action) :-
	listModesForAction(Parsed,Action,Modes),
	view(Modes).
