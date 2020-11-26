/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Günter Kniesel
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module( pdt_search,
         [ find_reference_to/12                  % (+Functor,+Arity,?DefFile,?DefModule,?RefModule,?RefName,?RefArity,?RefFile,?RefLine,?Nth,?Kind)
         , find_definitions_categorized/12       % (+EnclFile,+SelectionLine, +Term, -Functor, -Arity, -This, -DeclOrDef, -DefiningEntity, -FullPath, -Line, -Properties,-Visibility)
         , find_primary_definition_visible_in/6  % (EnclFile,TermString,ReferencedModule,MainFile,FirstLine,MultifileResult)
         , find_definition_contained_in/8
         , find_pred/8
         ]).

:- use_module( split_file_path,
             [ split_file_path/4                % (File,Folder,FileName,BaseName,Extension)
             ] ).
:- use_module( pdt_xref, 
             [ find_reference_to/12             % ...
             ] ).
:- use_module( properties, 
             [ properties_for_predicate/4
             ] ).
:- use_module( pdt_prolog_library(utils4modules),
             [ module_of_file/2                 % (File,FileModule)
             , defined_in/4             % (SubModule,Name,Arity,DeclModule),
             , defined_in_module/3              % (Module,Name,Arity),
             , declared_in_file/4               % (Module,Name,Arity,Location)
             , defined_in_files/4               % (Module,Name,Arity,Locations)
             ] ).


% TODO: Why this import?
:- user:consult(pdt_runtime_builder_analyzer('meta_pred_toplevel.pl')).


:- op(600, xfy, ::).   % Logtalk message sending operator

        /***********************************************************************
         * Find Definitions and Declarations and categorize them by visibility *
         * --------------------------------------------------------------------*
         * for "Find All Declarations" (Ctrl+G) action                         *
         ***********************************************************************/ 


% find_definitions_categorized(+ReferencingFile,+-ReferencingLine,+ReferencingTerm,-Name,-Arity, 
%                               ???ReferencingModule, -DefiningModule, -DeclOrDef, -Visibility, -File,-Line)
%                                                      ^^^^^^^^^^^^^^ TODO: moved to this place (two arguments forward)
% Logtalk
find_definitions_categorized(EnclFile,SelectionLine, Term, Functor, Arity, This, DeclOrDef, DefiningEntity, FullPath, Line, Properties, Visibility):-
    Term \= _:_,
    split_file_path(EnclFile, _Directory,_FileName,_,lgt),
    !,
    logtalk_adapter::find_definitions_categorized(EnclFile,SelectionLine, Term, Functor, Arity, This, DeclOrDef, DefiningEntity, FullPath, Line, Properties, Visibility).
    
find_definitions_categorized(EnclFile,_SelectionLine,Term,Functor,Arity, ReferencingModule, DeclOrDef, DefiningModule, File,Line, PropertyList, Visibility):-
    search_term_to_predicate_indicator(Term, Functor/Arity),
    module_of_file(EnclFile,FileModule),
    (  atom(ReferencingModule)
    -> true                            % Explicit entity reference ReferencedModule:Term (or ReferencedModule::Term
    ;  ReferencingModule = FileModule   % Implicit module reference
    ),    
    find_definition(ReferencingModule,Functor,Arity,Sources),
    member(DeclOrDef-Visibility-DefiningModule-Location,Sources),
    member(File-Lines,Location),
    member(Line,Lines),
    properties_for_predicate(ReferencingModule,Functor,Arity,PropertyList).

search_term_to_predicate_indicator(_:Term, Functor/Arity) :- !, functor(Term, Functor, Arity).
search_term_to_predicate_indicator(  Term, Functor/Arity) :- functor(Term, Functor, Arity).


%% find_definition(+ReferencingModule,+Name,?Arity,-Visibility,-Sources)

find_definition(ReferencingModule,Name,Arity,Sources) :-
    var(Name),
    throw( input_argument_free(find_definition(ReferencingModule,Name,Arity,Sources)) ).

find_definition(Name,Arity,ReferencingModule,Module,Visibility,File,LineNr,N,Case) :-	
%	(  pdt_option(search, restrict_arity(true))
%	->
%	), 
%	(  pdt_option( search, restrict_module(true) 
%	-> 
%	),
	defined_in(Module,Name,Arity),
    visibility(Module,Name,Arity,ReferencingModule,Visibility),
    location(Module,Name,Arity,File,LineNr,N,Case).
%    locations_by_file(Module,Name,Arity,File,Lines).
%
%locations_by_file(Module,Name,Arity,File,Line) :-    
%    setof( LineNr-N-Case, 
%           Module^Name^Arity^ location(Module,Name,Arity,File,LineNr,N,Case),
%           Lines
%    ),
%    member(Line,Lines)
%    .
    
%% location(+Module,+Name,+Arity, ?File, ?Line, ?N, ?Case)
%
%  @param File The full path of the file containing the N-th clause
%  @param Case = clause|declaration|foreign|dynamic
%
%  * Case==clause: 
%      a clause of the predicate Module:Name/Arity starts at 
%      line Line in File.
%  * Case==declaration: 
%      a declaration for the predicate Module:Name/Arity is
%      somewhere in file. Lacking information about positions
%      of declarations, we guess the Line to be 1.
%  * Case==foreign
%      There is no source location for Module:Name/Arity 
%      since it is defined by foreign language code. 
%      In this case File==none and Line==0.
%  * Case==dynamic
%      There is no source location for Module:Name/Arity 
%      since it is defined only by dynamic code. 
%      In this case File==none and Line==0.
%	
location(Module,Name,Arity, File,Line,N,clause) :-      % Clause
    clause_location(Module,Name,Arity,File,Line,N).
    
location(Module,Name,Arity,File,Line,N,declaration) :-  % Declaration
    \+ clause_location(Module,Name,Arity,_,_,_),
    module_property(Module, file(File)),  
    !,
    Line=1,
    N=0.
	
location(Module,Name,Arity,File,Line,N,Prop) :-         % No source code 
    \+ clause_location(Module,Name,Arity,_,_,_),
    functor(Head,Name,Arity),
    ( (Prop = foreign, predicate_property(Module:Head,Prop))
    ; (Prop = (dynamic), predicate_property(Module:Head,Prop))
    ),
    !,
    File=none,
    Line=0,
    N=0.


%% clause_location(+Module,+Name,+Arity,?N,?File,?Line) is nondet
%
%  The N-th clause of the predicate Module:Name/Arity starts at
%  line Line in File.
%   
%  @param File The full path of the file containing the N-th clause

clause_location(Module,Name,Arity,N,File,Line) :-
	functor(Head,Name,Arity),
	nth_clause(Module:Head,N,Ref), 
	clause_property(Ref, file(File)),
	clause_property(Ref, line_count(Line)).



imports_pred_from(Sub,Head,Super) :-
    predicate_property(Sub:Head, imported_from(Super)).

%% visibility(+ContextModule,+Name,+Arity,?DeclModule)
      
    
    
visibility(ContextModule,Name,Arity,DeclModule, supermodule) :-
    defined_in(ContextModule,Name,Arity,DeclModule),
    ContextModule \== DeclModule. 

visibility(ContextModule,Name,Arity,DeclModule, local) :-
    defined_in(ContextModule,Name,Arity,DeclModule),
    ContextModule == DeclModule.

visibility(ContextModule,Name,Arity,DeclModule, submodule) :-
    defined_in(DeclModule,Name,Arity,DeclModule),
    % DeclModule is a submodule of ContextModule
    defined_in(DeclModule,_,_,ContextModule), % submodule
    ContextModule \== DeclModule. 
visibility(ContextModule,Name,Arity,DeclModule, invisible) :-    
    % There is some DeclaringModule 
    defined_in(DeclModule,Name,Arity,DeclModule),
    DeclModule \== ContextModule,
    % ... but the ContextModule neither is imported to it
    % nor imports from it:
    functor(Head,Name,Arity),
    \+ imports_pred_from(DeclModule,Head,ContextModule),
    \+ imports_pred_from(ContextModule,Head,DeclModule).


   
   
        /***********************************************************************
         * Find Primary Definition                                             *
         * --------------------------------------------------------------------*
         * for "Open Primary Declaration" (F3) action                          *
         ***********************************************************************/ 

%% find_primary_definition_visible_in(+EnclFile,+Name,+Arity,?ReferencedModule,?MainFile,?FirstLine,?MultifileResult)
%
% Find first line of first clause in the *primary* file defining the predicate Name/Arity 
% visible in ReferencedModule. In case of multifile predicates, the primary file is either 
% the file whose module is the DefiningModule or otherwise (this case only occurs
% for "magic" system modules, (e.g. 'system')) the file containing most clauses.
%
% Used for the open declaration action (F3) in 
% pdt/src/org/cs3/pdt/internal/actions/FindPredicateActionDelegate.java

        
find_primary_definition_visible_in(EnclFile,TermString,ReferencedModule,MainFile,FirstLine,MultifileResult) :-
    split_file_path(EnclFile, _Directory,_FileName,_,lgt),
    !,
    logtalk_adapter::find_primary_definition_visible_in(EnclFile,TermString,ReferencedModule,MainFile,FirstLine,MultifileResult).


% The second argument is just an atom contianing the string representation of the term:     
find_primary_definition_visible_in(EnclFile,TermString,ReferencedModule,MainFile,FirstLine,MultifileResult) :-
	retrieve_term_from_atom(EnclFile, TermString, Term),
    extract_name_arity(Term,Head,Name,Arity),
    find_primary_definition_visible_in__(EnclFile,Head,Name,Arity,ReferencedModule,MainFile,FirstLine,MultifileResult).

retrieve_term_from_atom(EnclFile, TermString, Term) :-
	(	module_property(Module, file(EnclFile))
	->	atom_concat(TermString, '.', TermStringWithDot),
		open_chars_stream(TermStringWithDot, Stream),
		read_term(Stream, Term, [module(Module)])
	;	atom_to_term(TermString, Term, _)
	).

extract_name_arity(Term,Head,Name,Arity) :-
    (  var(Term) 
    -> throw( 'Cannot display the definition of a variable. Please select a predicate name.' )
     ; true
    ),
    % Special treatment of Name/Arity terms:
    (  Term = Name/Arity
    -> true
     ; (  Term = _Module:Term2
       -> functor(Term2, Name, Arity)
       ;  functor(Term,Name,Arity)
       )
    ),
    % Create most general head
    functor(Head,Name,Arity).

% Now the second argument is a real term that is 
%  a) a file loading directive:     
find_primary_definition_visible_in__(_,Term,_,_,_,File,Line,no):-
    find_file(Term,File,Line).

%  b) a literal (call or clause head):    
find_primary_definition_visible_in__(EnclFile,Term,Name,Arity,ReferencedModule,MainFile,FirstLine,MultifileResult) :-
	find_definition_visible_in(EnclFile,Term,Name,Arity,ReferencedModule,DefiningModule,Locations),
	(	Locations = [_,_|_]
	->	MultifileResult = yes
	;	MultifileResult = no
	),
	primary_location(Locations,DefiningModule,MainFile,FirstLine).


% If Term is a loading directive, find the related file,
% eventually interpreting a FileSPec that contains an alias
find_file(Term,File,Line) :-
    extract_file_spec(Term,FileSpec),
    catch( absolute_file_name(FileSpec,[solutions(all),extensions(['.pl', '.lgt', '.ct', '.ctc'])], File),
           _,
           fail
    ),
    access_file(File, read),
    !,
    Line=1.
    
% Work regardelessly whether the user selected the entire consult/use_module
% statement or just the file spec. Does NOT work if he only selected a file
% name within an alias but not the complete alias.
extract_file_spec(consult(FileSpec),FileSpec) :- !.
extract_file_spec(use_module(FileSpec),FileSpec) :- !.
extract_file_spec(ensure_loaded(FileSpec),FileSpec) :- !.
extract_file_spec(Term,Term).
    
find_definition_visible_in(EnclFile,_Term,Name,Arity,ReferencedModule,DefiningModule,Locations) :-
    module_of_file(EnclFile,FileModule),
    (  atom(ReferencedModule)
    -> true                            % Explicit module reference
    ;  ReferencedModule = FileModule   % Implicit module reference
    ),
    (  defined_in_module(ReferencedModule,Name,Arity,DefiningModule)
    -> defined_in_files(DefiningModule,Name,Arity,Locations)
    ;  ( defined_in(ReferencedModule,Name,Arity,DeclaringModule),
    defined_in_files(DeclaringModule,Name,Arity,Locations)
       )
    ).

primary_location(Locations,DefiningModule,File,FirstLine) :-
    member(File-Lines,Locations),
    module_of_file(File,DefiningModule),
    !,
    Lines = [FirstLine|_].
primary_location(Locations,_,File,FirstLine) :-
    findall( NrOfClauses-File-FirstLine,
             ( member(File-Lines,Locations),
               length(Lines,NrOfClauses),
               Lines=[FirstLine|_]
             ),
             All
    ),
    sort(All, Sorted),
    Sorted = [ NrOfClauses-File-FirstLine |_ ].
    

        /***********************************************************************
         * Find Definitions in File                                            *
         * --------------------------------------------------------------------*
         * for Outline                                                         *
         ***********************************************************************/ 
         
% TODO: This is meanwhile subsumed by other predicates. Integrate!
   
%% find_definition_contained_in(+File, -Name,-Arity,-Line,-PropertyList) is nondet.
%
% Looks up the starting line of each clause of each  
% predicate Name/Arity defined in File. Core properties
% of the predicate are contained in the PropertyList.
%
% Called from PDTOutlineQuery.java

find_definition_contained_in(File, Entity, EntityKind, Functor, Arity, SearchCategory, Line, PropertyList) :-
    split_file_path(File, _Directory,_FileName,_,lgt),
    !,
    logtalk_adapter::find_definition_contained_in(File, Entity, EntityKind, Functor, Arity, SearchCategory, Line, PropertyList).

find_definition_contained_in(File, Module, module, Functor, Arity, SearchCategory, Line, PropertyList) :-
    % Backtrack over all predicates defined in File:
    source_file(ModuleCandidate:Head, File), 
	% strip_module(ModuleHead,ModuleCandidate,Head),
	(	module_property(ModuleCandidate, file(File))
	->	Module = ModuleCandidate
	;	Module = user
	),
    functor(Head, Functor, Arity),
    properties_for_predicate(ModuleCandidate,Functor, Arity, PropertyList0),
    % In the case of a multifile predicate, we want to find all clauses for this 
    % predicate, even when they occur in other files
    (	member(multifile, PropertyList0)
    -> (	defined_in_file(ModuleCandidate, Functor, Arity, _, DeclFile, Line),
    		(	DeclFile \= File
    		-> 	(	module_property(MultiModule, file(DeclFile)),
    				append([for(MultiModule), defining_file(DeclFile)], PropertyList0, PropertyList),
    				SearchCategory = multifile
    			)
    		;	(	PropertyList = PropertyList0,
    				SearchCategory = definition
    			)
    		)
    	)
    ;	(	PropertyList = PropertyList0,
    		SearchCategory = definition,
    % The following backtracks over each clause of each predicate.
    % Do this at the end, after the things that are deterministic: 
    		(	defined_in_file(ModuleCandidate, Functor, Arity, _, File, _)
    		->	Module2 = ModuleCandidate
    		;	Module2 = Module
    		),
    		defined_in_file(Module2, Functor, Arity, _, File, Line)
    	)
    ),
    \+find_blacklist(Functor,Arity,Module).
    
        
% The following clause searches for clauses inside the given file, which contribute to multifile 
% predicates, defined in foreign modules.
find_definition_contained_in(File, Module, module, Functor, Arity, multifile, Line, PropertyList):-
    module_property(FileModule, file(File)),
    declared_in_module(Module,Head),
    Module \= FileModule,
    predicate_property(Module:Head, multifile),
    nth_clause(Module:Head,_,Ref),
    clause_property(Ref,file(File)),     
    clause_property(Ref,line_count(Line)),
    functor(Head, Functor, Arity),
    properties_for_predicate(Module, Functor, Arity, PropertyList0),
    append([from(Module)], PropertyList0, PropertyList),
    \+find_blacklist(Functor,Arity,Module).
   

%% find_blacklist(?Functor, ?Arity, ?Module) is nondet.
%
% Used to remove (internal) predicates from the results of find_definition_contained_in/8.
%
%
find_blacklist('$load_context_module',2,_).
find_blacklist('$mode',2,_).
find_blacklist('$pldoc',4,_).

    
    


               /***********************************************
                * FIND VISIBLE PREDICATE (FOR AUTOCOMPLETION) *
                ***********************************************/

%% find_pred(+EnclFile,+Prefix,-EnclModule,-Name,-Arity,-Exported,-Builtin,-Help) is nondet.
%
% Looks up all predicates with prefix Prefix defined or imported in file EnclFile.
%
% Used by the PLEditor content assist.
%
% The meaning of Arity is overloaded: -2: atom, -1 : module, >= 0 : predicate
% 
% For performance reasons an empty prefix with an unspecified module
% will only bind predicates if EnclFile is specified.
%
% <EnclFile> specifies the file in which this query is triggered
% <Prefix> specifies the prefix of the predicate
% <Module> specifies the module associated to the file.

find_pred(EnclFile,Prefix,Module,Name,Arity,Exported,Builtin,Help) :-
    split_file_path(EnclFile, _Directory,_FileName,_,lgt),
    !,
    logtalk_adapter::find_pred(EnclFile,Prefix,Module,Name,Arity,Exported,Builtin,Help).


find_pred(EnclFile,Prefix,Module,Name,Arity,Exported,Builtin,Help) :-
    \+ atom(EnclFile),
    throw( first_argument_free_in_call_to(find_pred(EnclFile,Prefix,Module,Name,Arity,Exported,Builtin,Help))).

find_pred(EnclFile,Prefix,Module,Name,Arity,Exported,Builtin,Help) :-
	setof(
	   (Name,Arity),
	   Prefix^Module^
	   ( my_module_of_file(EnclFile,Module),
	     find_pred_(Prefix,Module,Name,Arity,true)
	   ),
	   All
	),
	member((Name,Arity),All),
	
	% no enclosing module specified in the code via modulename:..
	get_declaring_module(EnclFile,Module,Name,Arity),
	functor(Term,Name,Arity),
	( predicate_property(Module:Term,exported)->
	  Exported=true
	; Exported=false
	),
	( predicate_property(Module:Term,built_in)->
	  Builtin=true
	; Builtin=false
	),
	predicate_manual_entry(Module,Name,Arity,Help).

find_pred_(Prefix,Module,Name,Arity,true) :-
    ( var(Module)->
    	Prefix \== ''
    ; true
    ), % performance tweak:
    current_predicate(Module:Name/Arity),
    atom_concat(Prefix,_,Name),
    % rule out used built-ins, like =../2, in case the enclosing module is given (in this case the prefix might be empty):   
    ( nonvar(Module) ->
      ( functor(Term,Name,Arity),
    	(Prefix \== ''; \+ predicate_property(Term, built_in)) )
      ; true
    ).


get_declaring_module(EnclFile,Module,Name,Arity) :-
	var(Module),
     my_module_of_file(EnclFile,ContainingModule),
     current_predicate(ContainingModule:Name/Arity),
     functor(Head,Name,Arity),
     ( predicate_property(ContainingModule:Head,imported_from(Module))
     ; Module = ContainingModule
     ),
     !.

get_declaring_module(_EnclFile,Module,_Name,_Arity) :-
	nonvar(Module),
	!.

%% find_pred(+EnclFile,+Prefix,-EnclModule,-Name,-Arity,-Exported,-Builtin,-Help, -Kind) is nondet.
%

find_pred_for_editor_completion(EnclFile,Prefix,Module,Name,Arity,Exported,Builtin,Help,Kind) :-
    \+ atom(EnclFile),
    throw( first_argument_free_in_call_to(find_pred_for_editor_completion(EnclFile,Prefix,Module,Name,Arity,Exported,Builtin,Help,Kind))).

find_pred_for_editor_completion(EnclFile,Prefix,Module,Name,Arity,Exported,Builtin,Help,predicate) :-
	find_pred(EnclFile,Prefix,Module,Name,Arity,Exported,Builtin,Help).

find_pred_for_editor_completion(_EnclFile,Prefix,EnclModule,Name,-1,true,false,'nodoc', module) :-
    var(EnclModule),
	current_module(Name),
    atom_concat(Prefix,_,Name).

% TODO: Improvement Idea: use "string" Prefix instead 
%  of atom to avoid Prefix to be added to the set of atoms
find_pred_for_editor_completion(_EnclFile,Prefix,'',Atom,-1,fail,true,'nodoc', atom) :-
	'$atom_completions'(Prefix, Atoms),
	member(Atom,Atoms), 
	Atom \= Prefix,
	garbage_collect_atoms,
	\+ current_predicate(Atom/_Arity).

my_module_of_file(_File, Module) :-
	atom(Module),
	current_module(Module),
	!.

my_module_of_file(File,Module):-
    module_property(Module2,file(File)),
	(	Module = Module2
	;	Module = user
	).
                                       
my_module_of_file(File,Module):-
    atom(File),                           
    \+ module_property(Module,file(File)),
    ( Module=user                         
    ; Module=system                       
    ).
