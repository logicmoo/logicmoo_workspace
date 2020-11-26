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

:- consult('../load_builder').

:- use_module(library(lists)).

:- multifile(pdt_reload:pdt_reload_listener/1).

pdt_reload:pdt_reload_listener(Files) :-
    with_mutex(meta_pred_finder,
		(	flag(pdt_generate_factbase, true, true)
		->	generate_factbase_with_metapred_analysis(Files)
		;	true
		)
	).

generate_factbase_with_metapred_analysis([]) :-
	(	flag(pdt_meta_pred_analysis, true, true)
			->	find_all_meta_predicates
			;	true
	), !.
    
generate_factbase_with_metapred_analysis([File|Rest]) :-
    generate_factbase(File),
    generate_factbase_with_metapred_analysis(Rest).
    
	
%	generate_factbase_with_metapred_analysis(File):-
%	%format('###File: ~w~n', [File]), 
%	with_mutex(meta_pred_finder,
%		(	flag(pdt_generate_factbase, true, true)
%		->	generate_factbase(File),
%			(	flag(pdt_meta_pred_analysis, true, true)
%			->	find_all_meta_predicates
%			;	true
%			)
%		;	true
%		)
%	).
	
	
%    get_all_userdefined_meta_predicates(_MetaPreds).
   % format('### Userdefined meta pred: ~w~n', [MetaPreds]).

find_undeclared_meta_predicates_position(File, Offset, MetaSpec):-
    %generate_factbase_with_metapred_analysis(File),
    with_mutex(meta_pred_finder,
    	get_all_userdefined_meta_predicates(MetaPreds)
    ),
    !,
    member(Module:MetaSpec,MetaPreds),
    filter_undeclared_predicate(Module, MetaSpec,File,Offset).
   

filter_undeclared_predicate(Module, MetaSpec, FileName, Offset):-
    \+(predicate_property(Module:MetaSpec,meta_predicate(_))),
    lookup_filename_and_offset(Module:MetaSpec, FileName, Offset).
    

  
%% find_missdeclared_meta_predicates_position(?File, ?Offset, ?DeclaredMetaSpec, ?CorrectMetaSpec)
find_missdeclared_meta_predicates_position(File, Offset, DeclaredMetaSpec, CorrectMetaSpec):-
	get_all_userdefined_meta_predicates(MetaPreds),  
    !,
    member(CorrectMetaSpec,MetaPreds),
    CorrectMetaSpec = Module:MetaSpec,
    predicate_property(Module:MetaSpec, meta_predicate(DeclaredMetaSpec)),
    \+(same_meta_specs(MetaSpec, DeclaredMetaSpec)),
    lookup_filename_and_offset(Module:MetaSpec, File, Offset).
    
    
same_meta_specs(MetaSpec, DeclaredMetaSpec):-
    MetaSpec =.. [Functor|Args],
    DeclaredMetaSpec =.. [Functor|DeclaredArgs],
    same_meta_args(Args,DeclaredArgs).
    	
    	
same_meta_args([],[]).
same_meta_args([A1|Rest1],[A2|Rest2]):-
    number(A1), 
    !,
    number(A2),
    same_meta_args(Rest1, Rest2).
same_meta_args([A1|Rest1],[A2|Rest2]):-
    member(A1, [?,+,-,:]),				%":" also counts as non-meta-arg	 
    !,
    member(A2, [?,+,-,:]),				%":" also counts as non-meta-arg
	same_meta_args(Rest1, Rest2).     
    
    
lookup_filename_and_offset(Module:Term, FileName, Offset):-
    functor(Term, Functor ,Arity),
    parse_util:predicateT_ri(Functor, Arity, Module, PId),
    parse_util:predicateT(PId,FileId,_,_,_),
    parse_util:fileT(FileId,FileName,_),
    parse_util:filePosT(PId,Offset,_).  


