flpFlag(neg(debug)).
%% flpFlag(debug).

viewIf(Item) :-
 	(   flpFlag(debug) -> 
	    view(Item) ;
	    true).

:- ensure_loaded('util').

:- dynamic verber/5.

:- ensure_loaded('args').

fixvars(P,_,[],P):-!.
fixvars(P,N,[V|VARS],PO):-
	atom_string(Name,V),clip_qm(Name,NB),Var = '$VAR'(NB),
	subst(P,'$VAR'(N),Var,PM0),
	subst(PM0,'$VAR'(Name),Var,PM),
				%  (get_varname_list(Vs)->true;Vs=[]),
				%   append(Vs,[Name=Var],NVs),
				%   nput_variable_names( NVs),
	N2 is N + 1, fixvars(PM,N2,VARS,PO).

:- ensure_loaded('parseDomainPDDL2.2').
:- ensure_loaded('parseProblemPDDL2.2').
:- ensure_loaded('parseDomainVerb').
:- ensure_loaded('parseProblemVerb').
:- ensure_loaded('parseSolutionLPGTD').

%% parse(File,Result) :-
%% 	(   parseDomain(File,Result) ; parseProblem(File,Result) ; parseSolution(File,Result)).

%% parsePair(DomainFile,ProblemFile,[domain(Domain),problem(Problem)]) :-
%% 	parse(DomainFile,Domain),
%% 	parse(ProblemFile,Problem).

parseCapsule(Arguments) :-
	argt(Arguments,[templateDir(Dir),capsule(Capsule),parsed(Parsed),extension(Extension)]),
	atomic_list_concat([Dir,Capsule],'',Prefix),
	atomic_list_concat([Prefix,'.d.',Extension],'',DomainFile),	
	atomic_list_concat([Prefix,'.p.',Extension],'',ProblemFile),	
	currentPlanner(Planner),
	atomic_list_concat(['worlds/',Capsule,'.p.',Extension,'.',Planner,'.sol'],'',SolutionFile),
	%% atomic_list_concat(['worlds/',Capsule,'.p.','pddl','.OPTIC_CLP.sol'],'',SolutionFile),
	%% atomic_list_concat([Prefix,'.p.',Extension,'.LPG.sol'],'',SolutionFile),
	%% SolutionFile = 'worlds/flp/flp.p.pddl.LPG.sol',
	atomic_list_concat([Prefix,'.metadata.pl'],'',VerbFile),

	(   (	Extension = 'pddl') ->
	    (	parseDomainPDDL(DomainFile,Domain),
		viewIf([domain,Domain]),

		parseProblemPDDL(ProblemFile,Problem),
		viewIf([problem,Problem])
	    ) ;
	    (	(   Extension = 'verb') ->
		(   parseDomainVerb(DomainFile,Domain),
		    viewIf([domain,Domain]),

		    parseProblemVerb(ProblemFile,Problem),
		    viewIf([problem,Problem])
		) ;
		true)),
	view([hi]),
	(   exists_file(SolutionFile) ->
	    (
	     view([ho]),
	     parseSolution(SolutionFile,TmpSolution),
	     view([ho1]),
	     correctCases(TmpSolution,Solution),
	     view([ho2]),
	     viewIf([solution,Solution]),
	     view([ho3])
	    ) ; TmpSolution = []),
	(   exists_file(VerbFile) ->
	    (
	     view([he]),
	     retractall(verber(_,_,_,_,_)),
	     consult(VerbFile),
	     findall(verber(A,B,C,D,E),verber(A,B,C,D,E),Verb)) ;
	    Verb = []),
	view([ha]),
	viewIf([verb,Verb]),
	!,
	Parsed = [domainFile(DomainFile),domain(Domain),problemFile(ProblemFile),problem(Problem),solutionFile(SolutionFile),solution(Solution),verbFile(VerbFile),verb(Verb)],
	Arguments = [templateDir(Dir),capsule(Capsule),parsed(Parsed),extension(Extension)].

