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

%:- ensure_loaded(source_files).
:- ensure_loaded('parse_util').
:- use_module('analyzer/metapred_finder').
:- use_module(library(lists)).


ensure_generated_factbase_for_source_file(File) :-
	source_file(File),
	exists_file(File),
	generate_factbase(File).
%	(	fileT_ri(File, _)
%	->	true
%	;	generate_factbase(File)
%	).

generate_factbase:-
    with_mutex(prolog_factbase,
    	(	find_all_loaded_files(ProjectFiles),
    		parse_util:generate_facts(ProjectFiles)
    	)
    ).       


generate_factbase(File):-
    with_mutex(prolog_factbase,
    	(	find_all_loaded_files(Project),
    		filter_already_known_files(Project,MissingFiles),
    		flatten(MissingFiles,FlatMissingFiles),
    		parse_util:update_facts(File,FlatMissingFiles)
    	)
    ).
    	       

    
find_all_loaded_files(Project):-
    current_prolog_flag(home, PrologHome),
    findall(
    	File,					%the following removes the files from prolog itself - maybe this should be changed back
    	(	source_file(File),
    		\+(string_concat(PrologHome, _, File))
    	), 
    	Project
    ).	
 
filter_already_known_files([],[]).
filter_already_known_files([File|Tail],[MissingTail]):-
    fileT_ri(File,_),
  	%format('!!!!Already there: ~w~n',[File]),
    !,
    filter_already_known_files(Tail,MissingTail).
filter_already_known_files([File|Tail],[File|MissingTail]):-
    filter_already_known_files(Tail,MissingTail).    
    
	 
 
pl_test(Project):-
	parse_util:generate_facts(Project).
	
	
pl_test_fix:-	
    pl_test(['C:/Data/Git-Data/pdt.git/pdt.runtime.builder/prolog-src']). 

