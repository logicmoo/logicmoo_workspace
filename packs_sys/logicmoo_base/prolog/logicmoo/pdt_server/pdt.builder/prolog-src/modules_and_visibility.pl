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

:- module(modules_and_visibility, [	compute_visibility_graph/0,
									visible_in_module/2,
									get_predicate_referenced_as/4,
									exporting/3]).

:- use_module(library(lists)).
:- ensure_loaded('pdt_factbase').

:- dynamic exporting/3.	%exporting(Module,PredId,FileId)
:- dynamic currently_missing_export/3. %currently_missing_export(Functor,Arity,FileId)

compute_visibility_graph:-
    compute_exports.
    
    
compute_exports:-
    retractall(exporting(_,_,_)),
    export_dir(Exports,Directive),		
    directiveT(Directive,FileId,_),			
    	flatten(Exports,ExportsFlatt),
    	build_export_edge_from_list(ExportsFlatt, FileId),
    fail.
compute_exports:-
    fileT(FileId,_,user),
    parse_util:predicateT(Id,FileId,_,_,_),    
    	assert(exporting(user,Id,FileId)),
    fail.
compute_exports.
    
build_export_edge_from_list([], _).    
build_export_edge_from_list([A|B], FileId):-
    build_export_edge(A, FileId),
    build_export_edge_from_list(B, FileId).
    
    
    
build_export_edge(reexport(Directive,all), FileId):-
	import_dir(RefFileId,Directive),
	fileT(RefFileId,_,Module),
	forall(	
		parse_util:predicateT_ri(_,_,Module,Id),
		assert(exporting(Module,Id,FileId))
	),!.
build_export_edge(reexport(_,[]),_):-
	!.
build_export_edge(reexport(Directive,[A|B]),FileId):-
    build_export_edge(reexport(Directive,A),FileId),
    build_export_edge(reexport(Directive,B),FileId),
    !.
build_export_edge(reexport(Directive,[Functor/Arity]),FileId):-
    import_dir(RefFileId,Directive),
    fileT(RefFileId,Directive),
    fileT(RefFileId,_,Module),
    parse_util:predicateT_ri(Functor,Arity,Module,Id),
    assert(exporting(Module,Id,FileId)).
build_export_edge(Functor/Arity,FileId):-
    fileT(FileId,_,Module),
    parse_util:predicateT_ri(Functor,Arity,Module,Id),    
    assert(exporting(Module,Id,FileId)),
    !.
build_export_edge(Functor/Arity,FileId):-	% this has to be a predicate imported from somewhere else
    fileT(FileId,_,Module),
    (	visible_in_module_as(Module, Functor, Arity, Id)	% maybe we already know that it is visible in the current context
    ->	assert(exporting(Module,Id,FileId))					% than it's the one reexported
    ;	assert(currently_missing_export(Functor,Arity,FileId)) % otherwise we have to look for it, when most information was created
    ). 

%build_export_edge(Functor/Arity,FileId):-
%    format('Warning for ~w/~w -> ~w: ',[Functor,Arity,FileId]),
%    fileT(FileId,_,Module),
%    format('~w fails to create export-edge ',[Module]),!,
%    (	parse_util:predicateT_ri(Functor,Arity,AModule,Id)
%    -> format('to Module: ~w, Id: ~w~n',[AModule, Id]) 
%    ; format('~n',[])
%    ). 
%build_export_edge(_,_):-!.



    
%% 
% get_predicate_referenced_as(+Module, +Functor, +Arity, ?PId)
%
get_predicate_referenced_as(Module, Functor, Arity, PId):-
    predicateT_ri(Functor, Arity, _AModule,PId),
    visible_in_module(PId, Module), 
    !.
get_predicate_referenced_as(Module, Functor, Arity, PId):-
    visible_in_module_as(PId, Module, Functor,[Module]),    
	predicateT(PId,_,_,Arity,_),
	!.
get_predicate_referenced_as(Module, Functor, Arity, Predefined):-
    functor(Term, Functor, Arity),
    predicate_property(Module:Term, built_in),
    defined_in(DefModule, Functor, Arity),
    Predefined = predefined(DefModule, Functor, Arity).
    


visible_in_module(Predicate,Module):-
    visible_in_module_as(Predicate,Module,_,[Module]).
    
    
    
visible_in_module_as(Predicate,Module,Functor,_):-
    predicateT(Predicate,_,Functor,_,Module).%,
    %!.
visible_in_module_as(Predicate,Module,Functor,PreviousModules):-
    fileT(ModuleFile,_,Module),
    load_edge(ModuleFile,DefiningFile,Imports,_),				   %TODO: import_dir verarbeiten irgendwo!!!!
    fileT(DefiningFile,_,DefiningModule),
    \+ member(DefiningModule, PreviousModules),
    compute_importing_functor(Imports,DefiningFunctor,Functor),	   %Eva: !!!!! TEST this!!!!!   
    visible_in_module_as(Predicate,DefiningModule,DefiningFunctor, [DefiningModule|PreviousModules]),
    exporting(DefiningModule,Predicate,_).   
   																	
compute_importing_functor(all,Functor,Functor):-
    !.
compute_importing_functor([A|B],Functor,NewFunctor):-
	compute_importing_functor_for_list([A|B],Functor,NewFunctor).
compute_importing_functor(except(List),Functor,NewFunctor):-   
    compute_importing_functor_with_exceptions(List,Functor,NewFunctor).
   
   
compute_importing_functor_for_list([A|B],Functor,NewFunctor):-
    (	(is_searched_functor(A,Functor,NewFunctor), !)
    ;	compute_importing_functor_for_list(B,Functor,NewFunctor)
    ).
    
compute_importing_functor_with_exceptions([],Functor,Functor).
compute_importing_functor_with_exceptions([A|B],Functor,NewFunctor):-
    (	is_searched_functor(A,Functor,AFunctor)
    ->	(	Functor == AFunctor
    	->	fail
    	;	NewFunctor = AFunctor
    	)
    ;	compute_importing_functor_for_list(B,Functor,NewFunctor)
    ).
    
    
    
is_searched_functor(Functor/_Arity,Functor,Functor):-
    !.
is_searched_functor(Functor/_A 'as' NewFunctor,Functor,NewFunctor):- 
    !.   
     



    


