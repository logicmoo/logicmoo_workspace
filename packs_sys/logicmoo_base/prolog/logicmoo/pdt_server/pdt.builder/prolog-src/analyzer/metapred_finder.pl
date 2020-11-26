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

:-module(metapred_finder, [	get_all_userdefined_meta_predicates/1,
							find_all_meta_predicates/0]).

:- use_module(library(lists)).
:- use_module(metafile_referencer).
:- use_module(pdt_prolog_library(utils4modules)).
:- use_module(term_based_metapred_finder).
:- use_module('../modules_and_visibility.pl').


:- dynamic new_meta_pred/2.	%new_meta_pred(MetaSpec, Module)

get_all_userdefined_meta_predicates(MetaPreds):-
    findall(
    	Module:NewMetaSpec,
    	metafile_referencer:user_defined_meta_pred(_Functor, _Arity, Module, NewMetaSpec),
    	MetaPreds
    ).
    

find_all_meta_predicates:-
    initialize_meta_pred_search,
    repeat,
    	collect_candidates(Candidates),
    	forall(
    		(	member(Module:Candidate, Candidates),
    			(	Module = user
    			->	(	functor(Candidate, Functor, Arity),
    					visible_in_module(AModule,Functor,Arity),
    					infer_meta_arguments_for(AModule, Candidate, MetaSpec)
    				)	
    			;	infer_meta_arguments_for(Module,Candidate,MetaSpec)
    			)
 			),
 			assert(new_meta_pred(MetaSpec, Module))
		),
		(	new_meta_pred(_,_)
		->	(	prepare_next_step,
				fail
			)
		;	true
		),
	!.
	    
    
    
initialize_meta_pred_search:-
    retractall(metafile_referencer:user_defined_meta_pred(_,_,_,_)),
    retractall(new_meta_pred(_,_)),
    forall(	
    	(   find_predefined_metas(Spec, Module)
    	),
    	assert(new_meta_pred(Spec, Module))
    		%format('Initial: ~w:~w~n', [Module, Spec])
    ).
    
  
find_predefined_metas(Spec, Module):-
    defined_in(Module,Functor, Arity, Module),
    functor(Head,Functor,Arity),
    predicate_property(Module:Head, built_in),
   	predicate_property(Module:Head, meta_predicate(Spec)),
   	is_metaterm(Module, Head, MetaArgs),
    (MetaArgs \= []).
        
        
        
collect_candidates(Candidates):-
	findall(
		CandModule:Candidate,
    	(	new_meta_pred(MetaSpec, Module),
    		retract(new_meta_pred(MetaSpec, Module)),
    		functor(MetaSpec, Functor, Arity),
    		%visible_in_module(AModule, Functor, Arity),		%TODO: hier müsste man eigentlich die Module suchen, die das Modul sehen
    														%		für die ..T-Fakten möglich, aber nicht für die vordefinierten...
    														%		andererseits: der genaue Test ist ja eh später, hier nur Kandidaten.
    		(	parse_util:predicateT_ri(Functor,Arity,Module,PredId)
    		->	parse_util:call_edge(PredId,LiteralId)
			;	parse_util:call_built_in(Functor, Arity, Module, LiteralId)
			),
			parse_util:literalT(LiteralId,_ParentId,ClauseId,_AModule,_Functor,_Arity),
			parse_util:clauseT(ClauseId,_,CandModule,CandFunctor,CandArity),
			functor(Candidate, CandFunctor, CandArity),
			\+ (predicate_property(CandModule:Candidate, built-in))%,
			%format('Candidate: ~w:~w~n', [CandModule, Candidate])
        ),
        CandidateList
	), 
	list_to_set(CandidateList, Candidates).	
	
	
    
prepare_next_step:-
    forall(	
    	new_meta_pred(MetaSpec, Module),
    	(	functor(MetaSpec, Functor, Arity),
    		(	metafile_referencer:user_defined_meta_pred(Functor, Arity, Module, OldMetaSpec)
    		->	(	(MetaSpec \= OldMetaSpec)
    			->	(	combine_two_arg_lists(OldMetaSpec, MetaSpec, NewMetaSpec),
    					retractall(metafile_referencer:user_defined_meta_pred(Functor,Arity,Module,_)),
    					assert(metafile_referencer:user_defined_meta_pred(Functor, Arity, Module, NewMetaSpec))
    				)
    			;	retract(new_meta_pred(MetaSpec, Module))	% was already there, no need to handle it again 		
    			)
    		;	assert(metafile_referencer:user_defined_meta_pred(Functor, Arity, Module, MetaSpec)),
    			update_factbase(Functor, Arity, Module)
    		)
    	)
    ).
    
update_factbase(Functor, Arity, Module):-
    parse_util:predicateT_ri(Functor,Arity,Module,PId),
 	assert(parse_util:meta_predT(PId, found)).
    
    



