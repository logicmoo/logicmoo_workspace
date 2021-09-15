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

:- module(cross_reference_builder, [derive_edges/0,
									cross_references_for_predicate/1]).

:- ensure_loaded(modules_and_visibility).
:- ensure_loaded(parse_util).
:- ensure_loaded(pdt_prolog_library(utils4modules)).

derive_edges:-
    forall( 
    	literalT(LId,_,_,Module,Functor,Arity),
		find_reference_for(LId,Module,Functor,Arity)
    ).


cross_references_for_predicate(PredId):-
    predicateT(PredId,_,Functor,Arity,_),
    visible_in_module(PredId, Module),
    forall(
    	literalT_ri(Functor,Arity,Module,LitId),
    	assert(call_edge(PredId,LitId))
    	).
cross_references_for_predicate(PredId):-
    forall(
    	(	pred_edge(ClauseId,PredId),
    		literalT(LId,_,ClauseId,Module,Functor,Arity)
    	),
    	find_reference_for(LId,Module,Functor,Arity)
    ).
    
find_reference_for(LId,Module,Functor,Arity):-    	
    (	(	predicateT_ri(Functor,Arity,Module,Id)
    	->	true
   		;	(	predicateT_ri(Functor ,Arity ,_DecModule, Id),
   				visible_in_module(Id, Module)
   			)
   					%get_predicate_referenced_as(Module, Functor, Arity, Id)
   		)
   	->	assert(call_edge(Id,LId))	
	;	catch(	(	(	functor(Term, Functor, Arity),
    					defined_in(Module, Term, DefModule),
    					/*(	var(Module)
    					->	
    					;	*/predicate_property(DefModule:Term, built_in)
    			  		%)
    		  		)
    			->	assert(call_built_in(Functor, Arity, DefModule, LId))
    			;	true			%TODO: here is a possible place to create a warning as soon as it's reduced to "real" problems...
    			),
    			_Error,
    			%(	format('Problem with crossref -> Module: ~w, Functor: ~w, Arity: ~w, LId: ~w, Error: ~n',[Module,Functor,Arity,LId, Error]),
    				true 
    			%)
   			)
	).


