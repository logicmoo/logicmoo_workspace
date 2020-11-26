:- use_module(library(lists)).

pddlExport(Parsed) :-
	%% pddlExportSample([domain,Domain,problem,Problem,solution,Solution]),
	view([parsed1,Parsed]),
	generateProblemFromDomain(Parsed),
	true.

generateProblemFromDomain(Arguments) :-
	argt(Arguments,[domainFile(DomainFile),domain(Domain),problemFile(ProblemFile),problem(Problem),solutionFile(SolutionFile),solution(Solution),verbFile(VerbFile),verb(Verb)]),
	viewIf([arguments3,[domainFile(DomainFile),domain(Domain),problemFile(ProblemFile),problem(Problem),solutionFile(SolutionFile),solution(Solution),verbFile(VerbFile),verb(Verb)]]),
	view([1]),
	view([verb,Verb]),
	Verb = [verber(A,B,C,D,E)],
	view(2),
	VerbArguments = [A,B,C,D,E],
	view(3),
	argt(VerbArguments,[problem(ProblemName)]),
	view(4),

	%% if we have the domain, procede to query and export the
	%% different objects, and inits, etc.  Also, potential goals,
	%% such as are covered by derived.
	domain(Domain,ProblemDomainName),
	view(5),

	getCurrentDateTime(DateTime),
	ExtraArgs = [dateTime(DateTime)],

	exportFromWSM([domain(Domain),objects(Objects),types(Types),objectList(ObjectList),extraArgs(ExtraArgs)]),
	viewIf([objects,Objects]),
	exportFromWSM([domain(Domain),init(Init),objects(Objects),types(Types),objectList(ObjectList),extraArgs(ExtraArgs)]),
	view([abba]),
	viewIf([init,Init]),
	exportFromWSM([domain(Domain),goals(Goals),extraArgs(ExtraArgs)]),

	Problem = problem(ProblemName,ProblemDomainName,Objects,Init,Goals),
	Arguments = [domainFile(DomainFile),domain(Domain),problemFile(ProblemFile),problem(Problem),solutionFile(SolutionFile),solution(Solution),verbFile(VerbFile),verb(Verb)],!.

wsmHoldsNow(Expression,_) :-
	getCurrentDateTime(DateTime),
	atTimeQuery(DateTime,Expression).

wsmHolds(Expression,Context) :- %% for now
	Expression.

wsmHolds(DateTime,Expression,Context) :- %% for now
	%% Expression ->
	%% true ;
	atTimeQuery(DateTime,Expression).

wsmHoldsWrapper(Expression,Context) :-
	(   wsmHolds(Expression,Context) ;
	    wsmHoldsNow(Expression,Context)).

exportFromWSM(Arguments) :-
	argt(Arguments,[domain(Domain),objects(Objects),types(Types),objectList(ObjectList),extraArgs(ExtraArgs)]),
	argt(ExtraArgs,[dateTime(DateTime)]),
	findall(TmpTypes,types(Domain,TmpTypes),AllTypes),
	viewIf([allTypes,AllTypes]),

	%% setof(Type,AllTypes^TypesList^(member(genls(TypesList,_),AllTypes),member(Type,TypesList),Type \= '-'),Types),
	findall(Type,
		(
		 member(genls(TypesList,_),AllTypes),
		 member(Type,TypesList),
		 Type \= '-'
		),
		TmpTypes1),
	findall(Type,
		member(genls(_,Type),AllTypes),
		TmpTypes2),
	append([TmpTypes1,TmpTypes2],TmpTypes),
	setof(Type,member(Type,TmpTypes),Types),
	view([types,Types]),

	viewIf([types,Types]),
	findall(Declaration,
		(   
		    member(Type,Types),
		    view([tryingType,Type]),
		    setof(ObjectOfType,Type^(wsmHolds(isa(ObjectOfType,Type),z) ; wsmHoldsNow(isa(ObjectOfType,Type),z)),ObjectsOfType),
		    append([ObjectsOfType,['-'],[Type]],Declaration)
		),
		Declarations),
	viewIf([declarations,Declarations]),
	append(Declarations,Objects),

	findall(TmpObjectList,
		(   
		    member(Type,Types),
		    view([tryingType,Type]),
		    setof(ObjectOfType,Type^(wsmHolds(isa(ObjectOfType,Type),z) ; wsmHoldsNow(isa(ObjectOfType,Type),z)),TmpObjectList)
		),
		TmpObjectLists),
	view([tmpObjectLists,TmpObjectLists]),
	findall(Object,(member(TmpObjectList,TmpObjectLists),member(Object,TmpObjectList)),TmpObjectList),
	setof(Object,member(Object,TmpObjectList),ObjectList),

	Arguments = [domain(Domain),objects(Objects),types(Types),objectList(ObjectList),extraArgs(ExtraArgs)],!.

exportFromWSM(Arguments) :-
	%% FIXME: get the arity thing to be correct,maybe even wrong predicate
	%% name or order, i don't remember
	argt(Arguments,[domain(Domain),init(Init),objects(Objects),types(Types),objectList(ObjectList),extraArgs(ExtraArgs)]),
	argt(ExtraArgs,[dateTime(DateTime)]),
	%% we're going to have to get the timing information properly integrated, for now, just use now
	findall(Predicate,predicates(Domain,Predicate),Predicates),
	findall(QueryPredicate,
		(   
		    member(Predicate,Predicates),
		    Predicate =.. [PredicateName|TypeSpec],
		    findall([Var,Type],(member(are(Vars,Type),TypeSpec),member(Var,Vars)),VarSpecs),
		    length(VarSpecs,ArgsLength),
		    length(QueryPredicateArgs,ArgsLength),
		    QueryPredicate =.. [PredicateName|QueryPredicateArgs],
		    wsmHolds(DateTime,QueryPredicate,z),
		    validateAllArgumentsAreObjects(QueryPredicate,ObjectList),
		    validateQueryPredicateArgsHaveCorrectTypes(QueryPredicate,QueryPredicateArgs)
		),
		QueryPredicates),
	viewIf([queryPredicates,QueryPredicates]),

	%% findall(Function,functions(Domain,Function),Functions),
	%% viewIf([functions,Functions]),
	%% findall(QueryFunction,
	%% 	(   
	%% 	    member(Function,Functions),
	%% 	    Function =.. [FunctionName|Args],
	%% 	    length(Args,ArgsLength),
	%% 	    length(QueryFunctionArgs,ArgsLength),
	%% 	    QueryFunction =.. [FunctionName|QueryFunctionArgs],
	%% 	    wsmHolds(DateTime,QueryFunction,z),
	%% 	    validateAllArgumentsAreObjects(QueryFunction,ObjectList),
	%% 	    validateQueryFunctionArgsHaveCorrectTypes(QueryFunction,QueryFunctionArgs)
	%% 	),
	%% 	QueryFunctions),
	%% viewIf([queryFunctions,QueryFunctions]),

	findall(Function,functions(Domain,Function),Functions),
	findall('='(QueryAssignment,Value),
		(   
		    member(Assignment,Functions),
		    Assignment = f(AssignmentName,TypeSpec),
		    findall([Var,Type],(member(are(Vars,Type),TypeSpec),member(Var,Vars)),VarSpecs),
		    length(VarSpecs,ArgsLength),
		    length(QueryAssignmentArgs,ArgsLength),
		    QueryAssignment =.. [AssignmentName|QueryAssignmentArgs],
		    view([wsmHolds(DateTime,set(QueryAssignment,Value),z)]),
		    wsmHolds(DateTime,set(QueryAssignment,Value),z),
		    validateAllArgumentsAreObjects(QueryAssignment,ObjectList),
		    validateQueryFunctionArgsHaveCorrectTypes(QueryAssignment,QueryAssignmentArgs)
		),
		QueryAssignments),
	viewIf([queryAssignments,QueryAssignments]),

	%% FIXME: figure out how to handle derived predicates
	append([QueryPredicates,QueryFunctions,QueryAssignments],Init),
	Arguments = [domain(Domain),init(Init),objects(Objects),types(Types),objectList(ObjectList),extraArgs(ExtraArgs)],!.

validateAllArgumentsAreObjects(Term,ObjectList) :-
	%% view([term,Term,objectList,ObjectList]),
	(   is_list(Term) ->
	    foreach(member(SubTerm,Term),validateAllArgumentsAreObjects(SubTerm,ObjectList)) ;
	    (	(   Term =.. ['$VAR',VariableName]) ->
		true ;
		(   (	Term =.. [PredicateName|Args], length(Args,N),N > 0) ->	
		    foreach(member(SubArg,Args),validateAllArgumentsAreObjects(SubArg,ObjectList)) ;
		    (
		     (	 
			 member(Term,ObjectList) -> view([true]) ; view([false])
		     ),
		     member(Term,ObjectList)
		    )))).

exportFromWSM(Arguments) :-
	argt(Arguments,[domain(Domain),goals(Goals),extraArgs(ExtraArgs)]),
	currentPlanningGoals(Goals),

	Arguments = [domain(Domain),goals(Goals),extraArgs(ExtraArgs)].

	%% viewIf([goals,Goals]),
	%% findall(QueryGoal,
	%% 	(   
	%% 	    member(Goal,Goals),
	%% 	    Goal =.. [GoalName|Args],
	%% 	    length(Args,ArgsLength),
	%% 	    length(QueryGoalArgs,ArgsLength),
	%% 	    QueryGoal =.. [GoalName|QueryGoalArgs],
	%% 	    wsmHolds(DateTime,QueryGoal,z),
	%% 	    validateQueryGoalArgsHaveCorrectTypes(QueryGoal,QueryGoalArgs)
	%% 	),
	%% 	QueryGoals),
	%% viewIf([queryGoals,QueryGoals]),	
	%% append([QueryGoals],Init),
	%% Arguments = [domain(Domain),init(Init),extraArgs(ExtraArgs)],!.

validateQueryPredicateArgsHaveCorrectTypes(A,B) :-
	true.

validateQueryFunctionArgsHaveCorrectTypes(A,B) :-
	true.

validateQueryGoalArgsHaveCorrectTypes(A,B) :-
	true.
