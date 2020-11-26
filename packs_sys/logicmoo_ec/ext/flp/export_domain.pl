exportVerbDomain :-
	verbExport(Arguments),
	argl(Arguments,domain,Domain),
	argl(Arguments,problem,Problem),
	argl(Arguments,solution,Solution),
	flp_convert_pl_to_pddl([domain(Domain),problem(Problem),solution(Solution),results(Results)]),
	view([results333,Results]),
	saveFiles(Results).

exportPDDLDomainAndSave([templateDir(DomainDir),worldDir(WorldDir),capsule(Capsule)]) :-
	exportPDDLDomain([templateDir(DomainDir),worldDir(WorldDir),capsule(Capsule),parsed(Parsed2),results(Results)]),
	viewIf([results,Results]),
	view([results334,Results]),
	saveFiles([worldDir(WorldDir),capsule(Capsule),parsed(Parsed2),results(Results)]).

exportPDDLDomain([templateDir(DomainDir),worldDir(WorldDir),capsule(Capsule),parsed(Parsed2),results(Results)]) :-
	loadDomain([templateDir(DomainDir),worldDir(WorldDir),capsule(Capsule),parsed(Parsed),extension('verb')]),
	argt(Parsed,[domainFile(DomainFile),domain(Domain),problemFile(Tmp1ProblemFile),problem(Tmp1Problem),solutionFile(SolutionFile),solution(Solution),verbFile(VerbFile),verb(Verb)]),
	Parsed2 = [domainFile(DomainFile),domain(Domain),problemFile(Tmp2ProblemFile),problem(Tmp2Problem),solutionFile(SolutionFile),solution(Solution),verbFile(VerbFile),verb(Verb)],
	pddlExport(Parsed2),
	view([parsed2,Parsed2]),
	argt(Parsed2,[problem(Problem)]),
	flp_convert_pl_to_pddl([domain(Domain),problem(Problem),solution(Solution),results(Results)]).

loadDomain(Arguments) :-
	argt(Arguments,[templateDir(DomainDir),worldDir(WorldDir),capsule(Capsule),parsed(Parsed),extension(Extension)]),
	%% figure out if its template or world dir, and if its verb or pddl
	parseCapsule([templateDir(DomainDir),capsule(Capsule),parsed(Parsed),extension(Extension)]),
	Arguments = [templateDir(DomainDir),worldDir(WorldDir),capsule(Capsule),parsed(Parsed),extension(Extension)],
	%% transform_capsule(Parsed,Transformed),
	true.

invokePlanner(verber,Domain,Problem) :-
	true.

invokePlanner(lpg,Domain,Problem) :-
	true.

saveFiles([worldDir(WorldDir),capsule(Capsule),parsed(Parsed),results(Results)]) :-
	Parsed = [domainFile(DomainFile),domain(Domain),problemFile(ProblemFile),problem(Problem),solutionFile(SolutionFile),solution(Solution),verbFile(VerbFile),verb(Verb)],
	Results = [domainResults(DomainResults),problemResults(ProblemResults),solution(Solution,SolutionResults)],
	atomic_list_concat([WorldDir,Capsule],'',Prefix),
	atomic_list_concat([Prefix,'.d.pddl'],'',WorldDomainFile),	
	atomic_list_concat([Prefix,'.p.pddl'],'',WorldProblemFile),	
	%% atomic_list_concat([Prefix,'.p.pddl.LPG.sol'],'',WorldSolutionFile),	
	%% atomic_list_concat([Prefix,'.metadata.pl'],'',WorldVerbFile),
	view([worldDomainFile1,WorldDomainFile,worldProblemFile1,WorldProblemFile]),
	write_data_to_file(DomainResults,WorldDomainFile),
	write_data_to_file(ProblemResults,WorldProblemFile),
	true.

%% Name => $self->Name,
%% WorldDir => $args{WorldDir},
%% TemplateDir => $args{TemplateDir},
%% PrototypeDir => $args{PrototypeDir},
%% Extension => $args{Extension},


%% CONTEXT

%% $self->Extension($args{Extension} || "pddl");
%% $self->TemplateExtension($args{Extension} || "verb");
%% $self->Worlds($args{Worlds} || {});
%% $self->ContextDir($args{ContextDir} || $UNIVERSAL::systemdir."/data/worldmodel");
%% $self->WorldDir
%% ($args{WorldDir} ||
%% ConcatDir($self->ContextDir,"worlds"));
%% $self->TemplateDir
%% ($args{TemplateDir} ||
%% ConcatDir($self->ContextDir,"templates"));
%% $self->PrototypeDir
%% ($args{PrototypeDir} ||
%% ConcatDir($self->ContextDir,"prototypes"));
%% $self->MyBeautifier
%% (Verber::Util::Beautifier->new);
%% print "Fed Manager started.\n" if $UNIVERSAL::verber->Debug;
%% $self->MyFedManager
%% (Verber::FedManager->new);
%% print "Fed Manager completed.\n" if $UNIVERSAL::verber->Debug >= 2;


%% CAPSULE

%% $self->ContextDir($args{ContextDir});
%% $self->WorldDir($args{WorldDir});
%% $self->TemplateDir($args{TemplateDir});
%% $self->PrototypeDir($args{PrototypeDir});
%% $self->Name($args{Name});
%% my $nametr = lc($self->Name);
%% $nametr =~ tr/_/\//;
%% $self->DomainFile($nametr.".d.".$args{Extension});
%% $self->ProblemFile($nametr.".p.".$args{Extension});
%% $self->DomainFileFull($self->Dir."/".$self->DomainFile);
%% $self->ProblemFileFull($self->Dir."/".$self->ProblemFile);
