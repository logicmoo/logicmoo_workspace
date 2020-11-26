%% for now have the plan manager or whatever allow you to select whichxp
%% capsule, get the logic all worked out there

%% have it enable replanning.  have the replanning button prominent.

%% whenever a new verbal command is issued, look at the assertion and
%% see if it impacts any of the domains of the exported plans.  For
%% now just replan every verbal command.

normalForm([capsule(Capsule)]) :-
	%% clean up the existing solution file.
	timestamp(TimeStamp),
	exportPDDLDomainAndSave([
				 templateDir('templates/'),
				 worldDir('worlds/'),
				 capsule(Capsule)
				]),
	currentPlanner(Planner),
	Extension = 'pddl',
	atomic_list_concat(['worlds/',Capsule,'.d.',Extension],'',DomainFile),
	atomic_list_concat(['worlds/',Capsule,'.p.',Extension],'',ProblemFile),
	atomic_list_concat(['worlds/',Capsule,'.p.',Extension,'.',Planner,'.sol'],'',SolutionFile),
	(   Planner = 'LPG' ->
	    (	atomic_list_concat(['lpg-td-1.0 -o ',DomainFile,' -f ', ProblemFile, ' -out ', SolutionFile,' -speed ; mv ',SolutionFile,'.SOL ',SolutionFile],'',Command) ) ; 
	    (	Planner = 'OPTIC_CLP' ->
		(   atomic_list_concat(['optic-clp -N ', DomainFile, ' ', ProblemFile, ' > ', SolutionFile],'',Command)) ;
		true)),
	view([command,Command]),
	shell(Command).
