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

:- module(parse_util, [	generate_facts/1, 
						update_facts/2,
						assert_new_node/4, 
						cleanup_nodes/0, 
						cleanup_nodes/1, 
						cleanup_computed_facts/0]).

:- reexport('pdt_factbase.pl').
:- ensure_loaded('util/walking_prolog_files.pl').
:- use_module(preparser).
:- use_module(predicates).
:- use_module(load_graph).
:- use_module(modules_and_visibility).
:- use_module(literal_parser).
:- use_module(cross_reference_builder).

generate_facts(Project):-
    cleanup_nodes,
    walking_file_list(Project,parse,1),
	build_load_graph,
    derive_all_predicates,
	derive_directive_collections,
	compute_all_predicate_properties,
	compute_visibility_graph,
	parse_bodies,
	derive_edges.    

%generate_facts(Project):-
%    writeln('cleaning up'),
%    cleanup_nodes,
%    writeln('start parsing clauses'),
%	time(walking_file_list(Project,parse,1)),
%	writeln('generating loadgraph'),
%	time(build_load_graph),
%    writeln('generating predicates'),
%	time(derive_all_predicates),
%	writeln('genereating directive collections'),
%	time(derive_directive_collections),
%	writeln('compute_predicate_properties'),
%	time(compute_all_predicate_properties),
%	writeln('compute_visibilities'),
%	time(compute_visibility_graph),
%	writeln('parse literals'),
%	time(parse_bodies),
%	writeln('generate edges'),
%	time(derive_edges).    
	
update_facts(File, Project):-				
	cleanup_nodes(File),
	cleanup_computed_facts,
   	walking_file_list([File|Project],parse,1),	
	build_load_graph,
	derive_predicates_of_files([File|Project]),
	derive_directive_collection_of_files([File|Project]),
	compute_predicate_properties_for_files([File|Project]),
	compute_visibility_graph,
	parse_bodies,
	derive_edges.
	
%update_facts(File, Project):-				
%	format('cleaning up facts for ~w~n',File),
%	cleanup_nodes(File),
%	cleanup_computed_facts,
%    writeln('start parsing clauses'),			
%	time(walking_file_list(Project,parse,1)),	
%	writeln('generating loadgraph'),
%	time(build_load_graph),
%    writeln('generating predicates'),
%	time(derive_all_predicates),
%	writeln('genereating directive collections'),
%	time(derive_onloads),
%	writeln('compute_predicate_properties'),
%	time(compute_all_predicate_properties),
%	writeln('compute_visibilities'),
%	time(compute_visibility_graph),
%	writeln('parse literals'),
%	time(parse_bodies),
%	writeln('generate edges'),
%	time(derive_edges).
	

		
/*
 * cleanup_nodes/0 isdet
 * retracts everything a former run of parse_util:generate_facts/1 could have asserted.
 **/  
cleanup_nodes:-
	retractall(fileT(_,_,_)),
	retractall(literalT(_,_,_,_,_,_)),
	retractall(metaT(_,_,_,_,_,_)),
	retractall(headT(_,_,_,_,_)),
	retractall(clauseT(_,_,_,_,_)),
	retractall(directiveT(_,_,_)),
	retractall(operatorT(_,_,_,_,_,_,_,_)),
	retractall(predicateT(_,_,_,_,_)),
	retractall(onloadT(_,_,_)),
	retractall(dynamicT(_,_)),
	retractall(load_dir(_,_,_)),
	retractall(import_dir(_,_)),
	retractall(export_dir(_,_)),
	retractall(library_dir(_,_,_)),
	retractall(property_dir(_,_,_)),
	retractall(transparentT(_,_)),						
	retractall(multifileT(_,_)),	
	retractall(meta_predT(_,_)),
	retractall(termT(_,_)),
	retractall(filePosT(_,_,_)),
	retractall(literalT_ri(_,_,_,_)),     
	retractall(fileT_ri(_,_)),
	retractall(predicateT_ri(_,_,_,_)),
	retractall(pred_edge(_,_)),
	retractall(onload_edge(_,_)),
	retractall(pos_and_vars(_,_,_)),	
	retractall(error(_,_,_)),
	retractall(warning(_,_,_)),
	cleanup_computed_facts,
	ctc_id_init_pdt.
	
cleanup_computed_facts:-
    retractall(call_edge(_,_)),	
    
	retractall(load_edge(_,_,_,_)),
	retractall(call_built_in(_,_,_,_)),

	retractall(meta_predT(_,found)).
    

cleanup_nodes(File):-
    fileT_ri(File,Id), !,
    clean_file_entries(Id),
    retractall(fileT_ri(File,Id)),
    retractall(fileT(Id,_,_)),
    retractall(error(_,_,Id)).		%TODO: gegebenenfalls zu clean_general_references_to/1 schieben
cleanup_nodes(_).
   
   
    
clean_file_entries(FileId):-
	directiveT(DirId,FileId,_),
		termT(DirId,_),
		clean_directives(DirId),
%		retractall(directiveT(DirId,_,_)),
%		retractall(import_dir(_,DirId)),
%		retractall(export_dir(_,DirId)),
%		retractall(load_dir(DirId,_,_)),
		clean_general_references_to(DirId),
		retractall(directiveT(DirId,_,_)),
	fail.
clean_file_entries(FileId):-
	clauseT(ClauseId,FileId,_,_,_),
		clean_clause_references(ClauseId),
		clean_general_references_to(ClauseId),
		retractall(clauseT(ClauseId,_,_,_,_)),
	fail.	
clean_file_entries(FileId):-
    predicateT(PredId,FileId,_,_,_),
    	retractall(predicateT(PredId,_,_,_,_)),
    	retractall(predicateT_ri(_,_,_,PredId)),
    fail.
clean_file_entries(FileId):-
    onloadT(Id,FileId,_),
    	retractall(onloadT(Id,_,_)),
    fail.
clean_file_entries(_).
	
	
	
clean_clause_references(ClauseId):-
    headT(HeadId,ClauseId,_,_,_),
    	clean_clause_references(HeadId),
    	retractall(headT(HeadId,_,_,_,_,_)),
    fail.
clean_clause_references(ClauseId):-   
	literalT(LitId,_,ClauseId,_,_,_),
		clean_clause_references(LitId),
    	retractall(literalT(LitId,_,ClauseId,M,F,A)),
    	retractall(literalT_ri(F,A,M,LitId)),
    	retractall(metaT(LitId,_,ClauseId,_,_,_)),
    	retractall(pred_edge(ClauseId,_)),
    fail.
clean_clause_references(_).



clean_directives(DirectiveId):-
    retractall(import_dir(_,DirectiveId)),
    retractall(export_dir(_,DirectiveId)),
    retractall(load_dir(DirectiveId,_,_)),
    retractall(property_dir(DirectiveId,_,_)),
    retractall(library_dir(_,_,DirectiveId)),
    retractall(meta_pred(_,DirectiveId)),
    retractall(onload_edge(DirectiveId,_)).



clean_general_references_to(Id):-
	retractall(termT(Id,_)),
	retractall(filePosT(Id,_,_)),
	retractall(warning(Id,_,_)),
	retractall(pos_and_vars(Id,_,_)).


/*
 * assert_new_node(+Term,+From,+To,-Id)
 * 	creates new identity Arg4 and asserts termT and filePosT with the information given
 *  by Arg1-Arg3 to this identity. 
 *  the Arg6. 
 */   
assert_new_node(Term,From,To,Id):- 
    new_node_id_pdt(Id),	
	assert(termT(Id,Term)),
    Length is To - From,
    assert(filePosT(Id,From,Length)).	


