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

:- module(loading_roots_finder,[find_loading_roots/0]).

:- ensure_loaded('../parse_util').

find_loading_roots:-
    fileT(FileId,FileName,_),
    	not(load_edge(_,FileId,_,_)),
    	format('Loading root: ~w~n',[FileName]).
    
find_standalone_files:-
    fileT(FileId,FileName,_),
    	not(load_edge(FileId,_,_,_)),
    	not(load_edge(_,FileId,_,_)),
    	format('Standalone File: ~w~n',[FileName]).
    
find_loading_leafs:-
    fileT(FileId,FileName,_),
    	not(load_edge(FileId,_,_,_)),
    	format('Loading root: ~w~n',[FileName]).
    
:- dynamic load_level/2. %load_level(FileId,Level).
:- dynamic load_level_ri/2. %load_level_ri(Level,FileId).    
:- dynamic loading_cycle/1.
    	
compute_loading_levels:-
    remove_old_loading_levels,
    assign_level0_to_leaves,
    compute_further_levels,
    assign_consistent_level_to_cycles.
   
remove_old_loading_levels:-
    retractall(load_level(_,_)),
    retractall(load_level_ri(_,_)),
    retractall(loading_cycle(_)).
    
    
:- dynamic next_level/1. 

assign_level0_to_leaves:-
    fileT(FileId,_,_),
    	not(load_edge(FileId,_,_,_)),
    	assert(load_level(FileId,0)),
    	assert(load_level_ri(0,FileId)),
    	fail.
assign_level0_to_leaves:-
	retractall(next_level(_)),
	assert(next_level(1)).	 
	
	  
compute_further_levels:-
    repeat,
    	next_level(CurrentLevel),
    	compute_next_loading_level,
    not(load_level_ri(CurrentLevel,_)).



compute_next_loading_level:-
    next_level(CurrentLevel),
    format('currentLevel: ~w~n',[CurrentLevel]),
    LastLevel is CurrentLevel - 1,
    load_level_ri(LastLevel,LoadedFileId),
    	load_edge(FileId,LoadedFileId,_,_),
    		(	load_level(FileId,_)
    		->	(	exists_loading_cycle(LoadedFileId,FileId,[],CircleList)
    			->	(	save_loading_cycle(CircleList),
    					fail
    				)
    			;	true	
    			)
    		;	true
    		),
    		set_level_to_elem(CurrentLevel,FileId),	
    fail.
compute_next_loading_level:-
    next_level(CurrentLevel),
    retractall(next_level(_)),
    NextLevel is CurrentLevel + 1,
    assert(next_level(NextLevel)).
    
    
exists_loading_cycle(Source,Target,[],[Source,Target]):-
    load_edge(Source,Target,_,_),!.
exists_loading_cycle(Source,Target,List,CompleteCycleList):-  
	load_edge(Source,Help,_,_),  
    exists_loading_cycle(Help,Target,[Source|List],CompleteCycleList).
    
    
:- dynamic loading_cycle/1.
save_loading_cycle(Cycle):-
    not(existing_cycle(Cycle)),
    assert(loading_cycle(Cycle)).
    
existing_cycle([First,Second|_]):-		
    loading_cycle(ExistingCycle),
    	member(First,ExistingCycle),
    	member(Second,ExistingCycle).
    	
assign_consistent_level_to_cycles:-
    loading_cycle(Cycle),
    	get_cycle_levels(Cycle,0,Max),
    	set_members_to(Cycle,Max),
    fail.
assign_consistent_level_to_cycles. 

get_cycle_levels([],Max,Max).
get_cycle_levels([Head|Tail],OldMax,Max):-
	load_level(Head,Current),
	CurrentMax is max(Current,OldMax),
	get_cycle_levels(Tail,CurrentMax,Max).
	
set_members_to(Cycle,Max):-
    forall(member(Elem,Cycle),
    	set_level_to_elem(Max,Elem)).
	    
set_level_to_elem(Level,Elem):-
    ground(Elem),
    retractall(load_level(Elem,_)),	
    retractall(load_level(_,Elem)),
    assert(load_level(Elem,Level)),
    assert(load_level_ri(Level,Elem)).
    
    


