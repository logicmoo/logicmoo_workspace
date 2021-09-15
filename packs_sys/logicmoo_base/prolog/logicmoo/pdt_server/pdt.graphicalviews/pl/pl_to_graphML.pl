/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module(pl_to_graphML, [	%write_project_graph_to_file/2,
								write_focus_to_graphML/4,
								write_global_to_graphML/3,
								write_dependencies_to_graphML/3,
								write_logtalk_project_files_to_graphML/4,
								write_logtalk_library_to_graphML/3,
								write_logtalk_recursive_library_to_graphML/3,
								call_term_position/7]).

:- use_module(graphML_api).
:- use_module(util_for_graphML).
:- use_module(pdt_common_pl('callgraph/pdt_call_graph')).
:- use_module(pdt_common_pl(pdt_search)).
:- use_module(pdt_prolog_library(utils4modules_visibility)).
:- use_module(library(lists), [list_to_set/2, append/3, member/2]).
:- use_module( prolog_connector_pl(split_file_path), [
	split_file_path/5 % (File,Folder,FileName,BaseName,Extension)
]).
:- use_module(pdt_prolog_library(compatibility), [
	pdt_source_file/2
]).

:- op(600, xfy, ::).   % Logtalk message sending operator

%write_project_graph_to_file(Project, OutputFile):-
%	parse_util:generate_facts(Project),
%	writeln('generating graphml-file'),
%    time(write_facts_to_graphML(Project,OutputFile)).

%/*
% * write_facts_to_graphML(+Project,+File)
% *   Arg1 has to be a fully qualified file name. The file must not exist.
% *   The predicated collects the relevant informations about the following 
% *   facts and converts them into abba-sources for Bashaars Tool. This 
% *   sources are written into the file specified by arg1-
% *   The facts that are considered are:
% *   ###### to be completed #########
% **/
%write_facts_to_graphML(Project, File):-
%    prepare_for_writing(File,OutStream),
%    member(FirstProject,Project),
%    write_all_files(FirstProject,OutStream), flush_output(OutStream),
%  	write_load_edges(OutStream), flush_output(OutStream),
%  	write_call_edges(OutStream), flush_output(OutStream),
% 	finish_writing(OutStream).


write_focus_to_graphML(FocusFile, GraphFile, DependentFiles, Settings):-
    write_to_graphML(GraphFile, write_focus_facts_to_graphML(FocusFile, DependentFiles, Settings)).  

write_global_to_graphML(ProjectFilePaths, GraphFile, Settings):-
    filter_consulted(ProjectFilePaths, ConsultedFilePaths),
    write_to_graphML(GraphFile, write_global_facts_to_graphML(ConsultedFilePaths, Settings)).

write_dependencies_to_graphML(ProjectFilePaths, ProjectPath, GraphFile):-
    filter_consulted(ProjectFilePaths, ConsultedFilePaths),
    write_to_graphML(GraphFile, write_dependencies_facts_to_graphML(ProjectPath, ConsultedFilePaths)).

write_logtalk_project_files_to_graphML(DiagramType, ProjectFilePaths, ProjectPath, GraphFile):-
    (	current_predicate(logtalk_load/1)
    ->	filter_consulted(ProjectFilePaths, ConsultedFilePaths),
    	filter_logtalk(ConsultedFilePaths, _, ConsultedLogtalkFilePaths),
    	split_file_path(GraphFile, Directory, FileName, _, _),
    	graphml_writer::set_file_name(FileName),
    	DiagramObject =.. [DiagramType, graphml],
    	DiagramObject::files(ProjectPath, ConsultedLogtalkFilePaths, [output_directory(Directory)])
    ;	true
    ).

write_logtalk_library_to_graphML(DiagramType, Library, GraphFile):-
    (	current_predicate(logtalk_load/1)
    ->	split_file_path(GraphFile, Directory, FileName, _, _),
    	graphml_writer::set_file_name(FileName),
    	DiagramObject =.. [DiagramType, graphml],
    	DiagramObject::library(Library, [output_directory(Directory)])
    ;	true
    ).

write_logtalk_recursive_library_to_graphML(DiagramType, Library, GraphFile):-
    (	current_predicate(logtalk_load/1)
    ->	split_file_path(GraphFile, Directory, FileName, _, _),
    	graphml_writer::set_file_name(FileName),
    	DiagramObject =.. [DiagramType, graphml],
    	DiagramObject::rlibrary(Library, [output_directory(Directory)])
    ;	true
    ).

relative_paths(_, [], []).
relative_paths(BasePath, [H|T], [FormattedH|FormattedT]) :-
    relative_path(BasePath, H, FormattedH),
    relative_paths(BasePath, T, FormattedT).
    
relative_path(BasePath, Path, RelativePath) :-
	atom_chars(BasePath, BasePathChars),
    atom_chars(Path, PathChars),
    relative_path_(BasePathChars, PathChars, RelativePathChars),
    atom_chars(RelativePath, RelativePathChars).
        

relative_path_([], Head, Head).
relative_path_([H|T], [H|T2], Result) :-
    relative_path_(T, T2, Result).

filter_logtalk([], [], []) :- !.
filter_logtalk([F|Fs], Ps, [F|Ls]) :-
	(	atom_concat(_, lgt, F)
	;	atom_concat(_, logtalk, F)
	),
	!,
	filter_logtalk(Fs, Ps, Ls).
filter_logtalk([F|Fs], [F|Ps], Ls) :-
	filter_logtalk(Fs, Ps, Ls).

:- meta_predicate(write_to_graphML(+, 1)).
write_to_graphML(GraphFile, CALL) :-
    with_mutex(prolog_factbase,
    	with_mutex(meta_pred_finder,
    		setup_call_cleanup(
    			prepare_for_writing(GraphFile,OutStream),
				call(CALL, OutStream),
  	  			finish_writing(OutStream)
  	  		)
  	 	)
  	 ).
  	 
filter_consulted(ProjectFilePaths, ConsultedFilePaths) :-
    findall(Path, (
    		member(Path, ProjectFilePaths), source_file(Path)
    	), ConsultedFilePaths).

pdt_location(Location) :-
	predicate_property(pl_to_graphML:pdt_location(_), file(F)),
	atom_concat(Location, 'pdt.contextview/pl/pl_to_graphml.pl', F).

hide_swi_predicates(M:F/A) :- functor(H, F, A), not(predicate_property(M:H, built_in)).
hide_swi_metapredicates(M:F/A) :- functor(H, F, A), not((predicate_property(M:H, built_in), predicate_property(M:H, (meta_predicate _)))).

hide_pdt_predicates(M:F/A) :- functor(H, F, A), not((predicate_property(M:H, file(File)), pdt_location(Location), atom_concat(Location, _, File))).
hide_pdt_metapredicates(M:F/A) :- functor(H, F, A), not((predicate_property(M:H, file(File)), pdt_location(Location), atom_concat(Location, _, File), predicate_property(M:H, (meta_predicate _)))).

filter(Setting,  pl_to_graphML:Setting).

init_filters(Settings, Filters) :- 
	findall(F, (member(S, Settings), filter(S, F)), Filters).

:- dynamic focus_facts_filter/1.

write_focus_facts_to_graphML(FocusFile, DependentFiles, Settings, OutStream):-
    source_file(FocusFile),
    !,
	
	ensure_call_graph_generated,
	collect_ids_for_focus_file(FocusFile, DependentFiles, ReferencedPredicates, Calls),
	
	retractall(focus_facts_filter(_)),
	assert((focus_facts_filter(X) :- member(X, ReferencedPredicates))),
	
	init_filters(Settings, Filters),
	
   	write_files(FocusFile, DependentFiles, [pl_to_graphML:focus_facts_filter|Filters], OutStream),
    forall(
    	member((SourceModule:SourceName/SourceArity, TargetModule:TargetName/TargetArity), Calls),
    	write_call_edge(OutStream, SourceModule, SourceName, SourceArity, TargetModule, TargetName, TargetArity, DependentFiles)
    ).
    
write_global_facts_to_graphML(ProjectFiles, Settings, OutStream) :-
    
    ensure_call_graph_generated,
    
    init_filters(Settings, Filters),
    
    forall(member(File, ProjectFiles),
		(	
			main_module_of_file(File, Module),
			write_file(OutStream, File, Filters, File, Module),
    		flush_output(OutStream)
    	)
    ),
    
    findall(Module:Name/Arity,
    	(
			member(File, ProjectFiles),
    		predicate_in_file(File, Module, Name, Arity)
    	),
    	Predicates
    ),
	forall((
		member(SourceModule:SourceName/SourceArity, Predicates),
		calls(TargetModule, TargetName, TargetArity, SourceModule, SourceName, SourceArity, _NumberOfCalls),
		once(member(TargetModule:TargetName/TargetArity, Predicates))
	),(
		write_call_edge(OutStream, SourceModule, SourceName, SourceArity, TargetModule, TargetName, TargetArity, ProjectFiles)
	)).
    
write_dependencies_facts_to_graphML(ProjectPath, ProjectFilePaths, OutStream) :-
    
	findall((SourceFile, TargetFile),
		(
			member(SourceFile, ProjectFilePaths),
			member(TargetFile, ProjectFilePaths),
    		loaded_by(TargetFile, SourceFile, _, _)
    	),
    	FoundDependencies
    ),
    
    forall(
    	(
    		nth1(Id, ProjectFilePaths, FilePath),
    		file_node_name(FilePath, ProjectPath, FileNodeName),
    		file_exports(FilePath, FileType, ExportedStaticPredicates, ExportedDynamicPredicates)
    	),	
		write_file_as_element(OutStream, Id, FilePath, FileNodeName, FileType, ExportedStaticPredicates, ExportedDynamicPredicates)
    ),

    forall(
    	(
    		member((S, T), FoundDependencies),
    		nth1(SId, ProjectFilePaths, S),
    		nth1(TId, ProjectFilePaths, T),
    		file_imports(S, T, Imports),
    		length(Imports, NImports)
    	),
		(	NImports > 0
		->	write_load_edge(OutStream, SId, TId, Imports, NImports)
		;	write_load_edge(OutStream, SId, TId, Imports)
		)
    ).
   
file_exports(FilePath, module, ExportedStaticPredicates, ExportedDynamicPredicates) :-
	module_property(Module, file(FilePath)),
	module_property(Module, exports(Exports)),
	exports_classification(Exports, ExportedStaticPredicates, ExportedDynamicPredicates, Module),
	!.
file_exports(FilePath, non_module_file, StaticPredicates, DynamicPredicates) :-
	findall(
		N/A,
		(	pdt_source_file(user:H, FilePath),
			functor(H, N, A),
			\+ atom_concat('$', _, N)
		;	loaded_by(LoadedFile, FilePath, _, _),
			file_imports(FilePath, LoadedFile, Imports),
			member(N/A, Imports)
		),
		Predicates
	),
	exports_classification(Predicates, StaticPredicates, DynamicPredicates, user),
	!.

exports_classification([Name/Arity|Tail], S, [Name/Arity|DTail], Module) :-
	functor(H, Name, Arity),
	predicate_property(Module:H, dynamic),
	!,
	exports_classification(Tail, S, DTail, Module).
exports_classification([E|Tail], [E|STail], D, Module) :-
    exports_classification(Tail, STail, D, Module).
exports_classification([], [], [], _Module) :- !.

file_imports(File1, File2, PredNames) :-
    once(module_of_file(File1,M1)),
    once(module_of_file(File2,M2)), 
    module_imports_from(M1,M2,Preds),
    findall(Name/Arity,
    	(
    		member(P, Preds),
    		functor(P, Name, Arity)
    	),
    	PredNames).
    
module_imports_from(M1,M2,Preds) :-   
    setof( Head,
           predicate_property(M1:Head, imported_from(M2)),
           Preds),
    !.
module_imports_from(_M1,_M2,[]).

% Module consults all Preds from File:
module_consults_from(Module,File,Preds) :-
    setof( Head,
           module_consults_from__(Module,File,Head),
           Preds).

% Module consults Head from File:
module_consults_from__(Module,File,Head) :-
    module_of_file(ModuleFile,Module),
    declared_in_module(Module, Head),
    predicate_property(Module:Head, file(File)),
    File \== ModuleFile.


file_node_name(FilePath, _, ModuleName) :-
	module_property(ModuleName, file(FilePath)), !.
		
file_node_name(FilePath, _, Name)	:-
	directory_file_path(_, Name, FilePath), !.
	%relative_path(ProjectPath, FilePath, RelativePath), !.
	
file_node_name(FilePath, _, FilePath).
    
    
file_node_type(FilePath, Dependencies, 'top') :-
    not(member((_, FilePath), Dependencies)), !.
        
    
file_node_type(FilePath, Dependencies, 'bottom') :-
    not(member((FilePath, _), Dependencies)), !.
    
file_node_type(_, _, 'intermediate') :- !.
    
    
file_node_type(FilePath, Dependencies, 'top') :-
    not(member((_, FilePath), Dependencies)), !.
    
file_node_type(FilePath, Dependencies, 'bottom') :-
    not(member((FilePath, _), Dependencies)), !.
    
file_node_type(_, _, 'intermediate') :- !.

write_logtalk_entity_facts_to_graphML(ProjectPath, ConsultedLogtalkFilePaths, OutStream) :-
	lgt_to_graphML::write_logtalk_entity_facts_to_graphML(ProjectPath, ConsultedLogtalkFilePaths, OutStream).
	
collect_ids_for_focus_file(FocusFile, Files, CalledPredicates, Calls):-
    findall(
    	Module:Name/Arity,
    	predicate_in_file(FocusFile, Module, Name, Arity),
    	OwnPredicates
    ),
    collect_calls_to_predicates(OwnPredicates, [], IncomingCalls),
    collect_calling_predicates_and_files(IncomingCalls, OwnPredicates, CallingPreds, [FocusFile], CallingFiles),
    list_to_set(CallingFiles, CallingFiles0),
    collect_calls_from_predicates(OwnPredicates,[], OutgoingCalls, FocusFile),
    collect_called_predicates_and_files(OutgoingCalls, CallingPreds, AllPreds, CallingFiles0, AllFiles),
    list_to_set(AllPreds, CalledPredicates),
    list_to_set(AllFiles, Files),
    append(IncomingCalls, OutgoingCalls, CallsList),
    list_to_set(CallsList, Calls).
    
collect_calls_to_predicates([],KnownCalls,KnownCalls).
collect_calls_to_predicates([TargetModule:TargetName/TargetArity|OtherPredicates], KnownCalls, AllCalls):-
    findall(
    	(SourceModule:SourceName/SourceArity, TargetModule:TargetName/TargetArity),
    	calls(TargetModule, TargetName, TargetArity, SourceModule, SourceName, SourceArity, _NumberOfCalls),
    	FoundCalls
    ),
    (	FoundCalls \= []
   	->	(	append(FoundCalls,KnownCalls,CallList), 
   			list_to_set(CallList,CallSet)
   		)
   	;	CallSet = KnownCalls
   	),
    collect_calls_to_predicates(OtherPredicates,CallSet,AllCalls).
    
collect_calls_from_predicates([],KnownCalls,KnownCalls, _FocusFile).
collect_calls_from_predicates([SourceModule:SourceName/SourceArity|OtherPredicates],KnownCalls,AllCalls, FocusFile):-
    findall(
    	(SourceModule:SourceName/SourceArity, TargetModule:TargetName/TargetArity),
		(	functor(SourceHead, SourceName, SourceArity),
			(	predicate_property(SourceModule:SourceHead, multifile)
			->	calls_multifile(TargetModule, TargetName, TargetArity, SourceModule, SourceName, SourceArity, FocusFile, _)
	    	;	calls(TargetModule, TargetName, TargetArity, SourceModule, SourceName, SourceArity, _)
	    	)
    	),
    	FoundCalls
    ),
	(	FoundCalls \= []
   	->	(	append(FoundCalls,KnownCalls,CallList), 
   			list_to_set(CallList,CallSet)
   		)
   	;	CallSet = KnownCalls
   	),
    collect_calls_from_predicates(OtherPredicates, CallSet, AllCalls, FocusFile).   
    
collect_calling_predicates_and_files([],Preds,Preds,Files,Files).    
collect_calling_predicates_and_files([(SourceModule:SourceName/SourceArity,TargetModule:TargetName/TargetArity)|OtherCalls],KnownCalledPreds, CalledPreds,KnownCalledFiles,CalledFiles):-
    findall(
    	File,
		(	functor(SourceHead, SourceName, SourceArity),
			(	predicate_property(SourceModule:SourceHead, multifile)
			->	calls_multifile(TargetModule, TargetName, TargetArity, SourceModule, SourceName, SourceArity, File, _)
	    	;	predicate_property(SourceModule:SourceHead, file(File))
	    	)
    	),
    	Files
    ),
    append(Files, KnownCalledFiles, NewKnownCalledFiles),
    collect_calling_predicates_and_files(OtherCalls,[SourceModule:SourceName/SourceArity|KnownCalledPreds],CalledPreds,NewKnownCalledFiles,CalledFiles). 
 
    
collect_called_predicates_and_files([],Preds,Preds,Files,Files).    
collect_called_predicates_and_files([(_Caller,TargetModule:TargetName/TargetArity)|OtherCalls],KnownCalledPreds, CalledPreds,KnownCalledFiles,CalledFiles):-
    findall(File, file_of_predicate(TargetModule, TargetName, TargetArity, File), Files),
    append(Files, KnownCalledFiles, NewKnownCalledFiles),
    collect_called_predicates_and_files(OtherCalls,[TargetModule:TargetName/TargetArity|KnownCalledPreds],CalledPreds,NewKnownCalledFiles,CalledFiles).
    
%/*
% * write_files(+Stream)
% *    writes #### dito ####
% */
%write_all_files(RelativePath,Stream):-
%    forall(	fileT(Id,File,Module),
%    		(	write_file(Stream,RelativePath,all_preds,Id,File,Module),
%    			flush_output(Stream)
%    		)
%    	  ).
		

write_files(RelativePath, Files, Filters, Stream):-
	forall(	
		member(File,Files),
		(	main_module_of_file(File,Module),
			write_file(Stream, RelativePath, Filters, File, Module),
    		flush_output(Stream)
    	)
    ).	

%write_load_edges(Stream):-
%    forall(load_edge(LoadingFileId,FileId,_,_),
%    	(	(	fileT(LoadingFileId,_,_),
%    			fileT(FileId,_,_)
%    		)
%    	->	write_load_edge(Stream,LoadingFileId,FileId)
%    		%format(Stream,'<edge source="~w" target="~w"/>~n', [LoadingFileId, FileId])
%    	;	format('Problem with load-edge: ~w, ~w~n',[LoadingFileId, FileId])
%	    )
%	).


	

%write_call_edges(Stream):-
%    ensure_call_graph_generated,
%    forall(call_edges_for_predicates(SourceId,TargetId,_Counter),
%    	(	write_call_edge(Stream,SourceId,TargetId)
%    	)
%    ).
    

%pl_test_graph:-	
%    pl_test_graph(['Z:/Git-Data/pdt.git/pdt.runtime.builder/prolog-src'],'Z:/Workspaces/WorkspaceFresh/test6.graphml'). 
%pl_test_graph(Project, OutputFile):-
%	write_project_graph_to_file(Project, OutputFile).
    

