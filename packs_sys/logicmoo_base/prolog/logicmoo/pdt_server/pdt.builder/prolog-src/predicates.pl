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

:-module(predicates,[	derive_all_predicates/0,
						derive_predicates_of_files/1,
						%derive_predicate_for_clause/6,
						derive_directive_collections/0,
						derive_directive_collection_of_files/1,
						compute_all_predicate_properties/0,
						compute_predicate_properties_for_files/1]).
:- ensure_loaded(pdt_factbase).
:- use_module(library(lists)).

derive_all_predicates:-
    forall(
    	fileT(FileId,_,_),
    	derive_predicates_of_file(FileId)
    ).
    
    
    
derive_predicates_of_files(Files):-
	forall(
		member(File,Files),
	    (	fileT_ri(File,FileId)
	    	->	derive_predicates_of_file(FileId)
			;	true   %if a file does not exist as a PEF, the rest should be evaluated, anyway
	    )
	).

	
	
derive_predicates_of_file(FileId):-
    forall( 
    	clauseT(CId,FileId,Module,Functor,Arity),
    	derive_predicate_for_clause(CId,Functor,Arity,Module,FileId,_PId)
    ),!.
derive_predicates_of_file(File):-
	format('Warning: An error occured while coputing predicates for file-id: ~w.~n',[File]).
    
    	  
derive_predicate_for_clause(CId,Functor,Arity,Module,_File,PId):-
	predicateT_ri(Functor,Arity,Module,PId),
	!,
	compute_new_length(PId,CId),
    assert(pred_edge(CId,PId)).
derive_predicate_for_clause(CId,Functor,Arity,Module,File,PId):-    
    assert_predicate(CId,Functor,Arity,Module,File,PId),
    assert(pred_edge(CId,PId)).
    
    
derive_directive_collections:-
    forall(
    	fileT(File,_,_),
    	derive_directive_collection_of_file(File)
    ).
    
    
derive_directive_collection_of_files(Files):-
    forall(
    	member(FileName,Files),
    	(	fileT_ri(FileName,File)
    	->	derive_directive_collection_of_file(File)
    	;	true	%if a file does not exist as a PEF, the rest should be evaluated, anyway
    	)
    ).
    
    
derive_directive_collection_of_file(File):-
   % fileT(File,FileName,_),
    forall( 
    	directiveT(Id,File,Module),
		(	(	(	onloadT(PId,File,Module)	
    			->	(	assert(onload_edge(Id,PId)),
    					compute_new_length(PId,Id)
    				)	  					
    			;	(	new_node_id_pdt(PId),	
    					assert(node_id(PId)),
    					assert(onloadT(PId,File,Module)),  
    						
    					filePosT(Id,Begin,Length),
    					assert(filePosT(PId,Begin,Length)),  
   
    					assert(onload_edge(Id,PId))
    				)	
    		  	) 			
    		)
    	; 	termT(Id,Term),
    		writeln(Term)
    	)
	),!.
derive_directive_collection_of_file(File):-
	format('Warning: An error occured while collecting directives for file-id: ~w.~n',[File]).
	

compute_new_length(PId,Id) :-
	filePosT(PId,PBegin,PLength),
    PEnd is PBegin + PLength,
    filePosT(Id,Begin,Length),
   	End is Begin + Length,
    NewBegin is min(PBegin,Begin),
    NewEnd is max(PEnd,End),
    NewLength is NewEnd - NewBegin,
    retract(filePosT(PId,PBegin,PLength)),
    assert(filePosT(PId,NewBegin,NewLength)).
    
    

compute_all_predicate_properties:-
    forall(	
    	property_dir(DirectiveId, Functor, Args),
    	(	directiveT(DirectiveId, File, Module),
    		compute_predicate_property(Functor, Args, DirectiveId, File, Module)
    	)
    ).


compute_predicate_properties_for_files(Files):-
	forall(
		member(File,Files),
	    (	fileT_ri(File,FileId)
		->	compute_predicate_properties_for_file(FileId)
		;	true   	%if a file does not exist as a PEF, the rest should be evaluated, anyway
	    )
	).

compute_predicate_properties_for_file(File):-
    forall(
    	directiveT(DirectiveId, File, Module),
    	(	property_dir(DirectiveId,Functor,Args)
    	->	compute_predicate_property(Functor, Args, DirectiveId, File, Module)
    	;	true	%it may be another kind of property
    	)
    ),!.
compute_predicate_properties_for_file(File):-
	format('Warning: An error occured while computing predicate properties for file-id: ~w.~n',[File]).

/*
 * analyse_directive(+Directive,+ParentId,+Module)
 *   looks into the term of Arg1 and if it is a known 
 *   kind of directive term stores accordingly information
 *   that can be used in the former parsing or x-referencing
 *   process (like modules, operators, dynamics, transparencies,
 *   metafile,...)
 **/
compute_predicate_property(Prop, Preds, DirectiveId, File, Module):-     % dynamic
	conjunction_to_list(Preds,Predicates),
	forall(	
		member(Functor/Arity, Predicates),
		(	(	predicateT_ri(Functor,Arity,Module,PId)
			->	true
			;	%(Prop = dynamic)	% because order is not deterministic it is easier to not do this check
									% this also represents the read code, more. But this should lead to a later
									% check for missing dynamic declarations - or for a smell about this...

    			%directiveT(DirectiveId, File, _),
    			assert_predicate(DirectiveId,Functor,Arity,Module,File,PId)
			),
			assert_prop(Prop, PId, DirectiveId)
		)
	),!.	
compute_predicate_property(Prop, Preds, _DirectiveId, _File, Module):-
	format('Warning: Error occured while computing predicate property ~w for ~w of Module ~w~n.',[Prop,Preds,Module]). 

 	

conjunction_to_list([],[]).
conjunction_to_list([A|B],[A|B]).
conjunction_to_list((A,B),[A,B]) :-
   atom(B),!. 
conjunction_to_list((A,B),[A|BasList]) :-
   conjunction_to_list(B,BasList).
   
   
assert_prop(dynamic, PredId, DirectiveId):-
    assert(dynamicT(PredId, DirectiveId)).
assert_prop(module_transparent, PredId, DirectiveId):-
    assert(transparentT(PredId, DirectiveId)).
assert_prop(multifile, PredId, DirectiveId):-
    assert(multifileT(PredId, DirectiveId)).
assert_prop(meta_predicate, PredId, DirectiveId):-
    assert(meta_predT(PredId, DirectiveId)). 
assert_prop(_,_,_).



assert_predicate(CId,Functor,Arity,Module,File,PId):-
    new_node_id_pdt(PId),	
    assert(node_id(PId)),
    assert(predicateT(PId,File,Functor,Arity,Module)),
    assert(predicateT_ri(Functor,Arity,Module,PId)),
    filePosT(CId,Begin,Length),
    assert(filePosT(PId,Begin,Length)).


