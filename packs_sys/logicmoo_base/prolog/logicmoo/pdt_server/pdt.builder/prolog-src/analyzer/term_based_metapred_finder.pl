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

:-module(term_based_metapred_finder, [	infer_meta_arguments_for/3,
										find_meta_pred_args_in_clause/3]).
:- use_module(pdt_prolog_library(utils4modules)).
:- use_module(library(lists)).
:- use_module(metafile_referencer).

/*
 * infer_meta_arguments_for(?Module,?AHead,?MetaSpec) is det
 *
 * Arg3 is the infered meta_predicate specification of the
 * predicat Arg2 in module Arg1. 
 * 
 * Fails, if Arg2 is not a meta-predicate.
 * 
 * Currently only infers ? and 0.
 * For built-in predicates the original specification is used. 
 **/
infer_meta_arguments_for(Module,AHead,MetaSpec):-
    (	var(AHead)
    ->	defined_in_module(Module,Functor,Arity)
    ;	functor(AHead, Functor, Arity)	
    ),
    functor(Head, Functor, Arity), 	%get most general head to find all clauses of the predicate
    findall(	MetaArgs,
				find_meta_pred_args_in_clause(Module, Head, MetaArgs),
				AllMetaArgs
			),
	(	AllMetaArgs = []
	->	fail
	;	(	combine_meta_args(AllMetaArgs,CombinedArgs),
			MetaSpec =.. [Functor|CombinedArgs]
		)
	). 


find_meta_pred_args_in_clause(Module, Head, MetaArgs):-
    \+(var(Head)),
    predicate_property(Module:Head, built_in), !,
    predicate_property(Module:Head, meta_predicate(Spec)),
    Spec =.. [_|MetaArgs].
find_meta_pred_args_in_clause(Module, Head, MetaArgs):-
	clause(Module:Head, Body), !,
	find_meta_vars_in_body(Body, Module, [],  MetaVars),
	find_meta_vars_in_head(Head, MetaVars, MetaArgs).
find_meta_pred_args_in_clause(AModule, Head, MetaArgs):-
    defined_in(AModule, Head, Module),
	clause(Module:Head, Body), !,
	find_meta_vars_in_body(Body, Module, [],  MetaVars),
	find_meta_vars_in_head(Head, MetaVars, MetaArgs).
	 
	 


	 
/*
 * find_meta_vars_in_body(+Term, +Context, +MetaVars, -MetaVars) is det
 * 
 * Analyses the code of Arg1 for calls to known meta_predicates (in the
 * module context of Arg2).
 * If such a meta-call is found, all terms that appear 
 *  - as arguments of those meta-calls,  
 *  - are unified / aliased to them,
 *  - are part of those terms, 
 *  - or are connected to them via term-manupilation
 * previously in the code of Arg1, are stored in Arg4. 
 * Arg3 helps as an accumulator of previously found arguments / terms.
 */  	 
find_meta_vars_in_body(A, _, MetaVars, MetaVars):-
    (	atomic(A)
    ;	var(A)
    ),
    !.
  
find_meta_vars_in_body(Module:Term, _, KnownMetaVars, MetaVars):-
    !, 
    find_meta_vars_in_body(Term, Module, KnownMetaVars, MetaVars).
    
find_meta_vars_in_body((Cond -> Then ; Else), Context, KnownMetaVars, MetaVars):-
    !,			%TODO: check ob das in jedem Fall stimmt - vor allem die Bedingung
    find_meta_vars_in_body(Then, Context, KnownMetaVars, MetaVarsA),	
    (	(KnownMetaVars \= MetaVarsA)										% for meta vars in the true case
    ->	find_meta_vars_in_body(Cond, Context, MetaVarsA, MetaVars)		% the condition may be relevant
    ;	find_meta_vars_in_body(Else, Context, KnownMetaVars, MetaVars)	% the else case does not 
    ).																	% have bindings of the condition or true case
   	      
find_meta_vars_in_body((TermA, TermB), Context, KnownMetaVars, MetaVars):-
	!, 														
   	find_meta_vars_in_body(TermB, Context, KnownMetaVars, MetaVarsB),		
   	find_meta_vars_in_body(TermA, Context, MetaVarsB, MetaVars).		%erst B dann A -> nach vorne propagieren
   															% alternativ evtl einfach aliasse / unifizierungen merken
find_meta_vars_in_body((TermA; TermB), Context, KnownMetaVars, MetaVars):-
    !, 
   	find_meta_vars_in_body(TermB, Context, KnownMetaVars, MetaVarsB),
   	find_meta_vars_in_body(TermA, Context, MetaVarsB, MetaVars).
   	
find_meta_vars_in_body((TermA = TermB), _Context, KnownMetaVars, MetaVars):-
    !,
   	(	occurs_in(TermA, KnownMetaVars)
   	->	add_var_to_set(TermB, KnownMetaVars, OwnMetaVars2)
   	;	OwnMetaVars2 = KnownMetaVars
   	),
   	(	occurs_in(TermB, OwnMetaVars2)
   	->	add_var_to_set(TermA, OwnMetaVars2, MetaVars3)
   	;	MetaVars3 = OwnMetaVars2
   	),
   	check_inner_vars(TermA, TermB, MetaVars3, MetaVars).
   	
find_meta_vars_in_body(functor(Term,Functor,_), _Context, KnownMetaVars, MetaVars):-  
    !,
    (  occurs_in(Term,KnownMetaVars)
    -> add_var_to_set(Functor, KnownMetaVars, MetaVars)
    ;	(	occurs_in(Functor,KnownMetaVars)
    	-> 	add_var_to_set(Term, KnownMetaVars, MetaVars)
    	;  	MetaVars = KnownMetaVars
    	)
    ).
find_meta_vars_in_body(atom_concat(A,B,C), _Context, KnownMetaVars, AllMeta):-  
    !,
    free_vars_of([A,B,C],Candidates),
    add_meta_vars(Candidates,KnownMetaVars,AllMeta).
          
find_meta_vars_in_body(( Term =.. List ), _Context, KnownMetaVars, MetaVars):-
    !,
    (	occurs_in(Term,KnownMetaVars)
    ->  (	add_var_to_set(List, KnownMetaVars, MetaVars1),
    		(	(	\+(var(List)),
					List = [Functor|_]											
				)
%    	->	combine_sets_nonbinding(List, [List|KnownMetaVars], MetaVars) TODO: etwas in der Art um Zahl raus zu kriegen 
			->	add_var_to_set(Functor, MetaVars1, MetaVars)				%	oder versteckte meta-pred-Suche	
    		;	MetaVars = MetaVars1
    		)	
    	)
    ;  (	(	occurs_in(List,KnownMetaVars)
    		-> 	add_var_to_set(Term, KnownMetaVars, MetaVars)
    		;  (	(	\+(var(List)),
    					List = [Functor|_],
    					occurs_in(Functor, KnownMetaVars)
    				)
    			->	add_var_to_set(Term, KnownMetaVars, MetaVars)	
    			;	MetaVars = KnownMetaVars
    			)
    		)
    	)
    ).
find_meta_vars_in_body(arg(_,Term,Arg), _Context, KnownMetaVars, MetaVars):-
    !,							%TODO: das doch nur in bestimtmen Fällen (mit functor/3 das erste Argument z.B.)
    (  occurs_in(Term,KnownMetaVars)
    -> add_var_to_set(Arg, KnownMetaVars, MetaVars)
    ;  (	occurs_in(Arg,KnownMetaVars)
    	-> 	add_var_to_set(Term, KnownMetaVars, MetaVars)
    	;  	MetaVars = KnownMetaVars
    	)
    ).

     
find_meta_vars_in_body(Term, Context, KnownMetaVars, MetaVars):-
    is_metaterm(Context, Term, MetaCombos), !, 
    extract_vars(MetaCombos, MetaArgs),
    handel_meta_args(MetaArgs, Context, KnownMetaVars, MetaVars).

find_meta_vars_in_body(_Term, _Context, MetaVars, MetaVars). 
		% everything else is a direct call [aliasing]
      
      

/*
 * find_meta_vars_in_head(+Head, +MetaVars, ?MetaArgs) is det
 *
 * Succeeds if Arg1 is the head of a meta-predicate-clause and Arg2 all
 * possible bindings for meta-arguments used in the body in the clause.
 * In this case, Arg3 is bound to a list that represents the 
 * meta-argument-binding of the arguments of the Clause.
 * 
 * (Currently only working with ? and 0, but should work for each 
 *  number and +, - in the futuroe.)
 */
find_meta_vars_in_head(Head, MetaVars, MetaArgs):-		%TODO: hier noch sharing realisieren
    Head =.. [_Functor|Args],
    find_args_in_list(Args, MetaVars, MetaArgs, IsMeta),
    (	IsMeta = true
    ->	true
    ;	fail
    ).
    
    
find_args_in_list([],_,[], false).
find_args_in_list([Arg|Rest], MetaVars, MetaArgs, IsMeta):-
    find_args_in_list(Rest,MetaVars,RestMetaArgs, MetaFound),
    (	occurs_in(Arg,MetaVars)
    ->	(	MetaArgs=[0|RestMetaArgs],
    		IsMeta = true
    	)
    ;	(	MetaArgs=[?|RestMetaArgs],
    		IsMeta = MetaFound
    	)
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
      
extract_vars([],[]).
extract_vars([(_,Var)|RestCombo], [Var|RestVars]):-
    extract_vars(RestCombo, RestVars).
    
    
    
    
  
handel_meta_args([], _, Known, Known).
handel_meta_args([A|Rest], Context, Known, MetaVars):-
    var(A), !,  
    add_var_to_set(A, Known, OwnMetaVars),
    handel_meta_args(Rest, Context, OwnMetaVars, MetaVars).
handel_meta_args([A|Rest], Context, Known, MetaVars):-
    handel_meta_args(Rest, Context, Known, AllOthers),
    find_meta_vars_in_body(A, Context, AllOthers, MetaVars).
   
   

	 
	
check_inner_vars(TermA,TermB,OldMetaVars,NewMetaVars):-
	unifiable(TermA, TermB, Unifiers),	!,		%TODO: diese Lösung funktioniert nur für Variablen nicht für
	check_unifier_list(Unifiers,OldMetaVars,NewMetaVars).%      Terme in OldMetaVars			
check_inner_vars(_, _, MetaVars, MetaVars).


check_unifier_list([], Metas, Metas).
check_unifier_list([A=B|Rest], OldMetas, Metas):-	%	TODO: p(A):- term(A,B)= term(C,C), call(B)
	(	occurs_in(A, OldMetas)						% 	funktioniert so nicht! 
	->	add_var_to_set(B, OldMetas, Metas1)			
	;	Metas1 = OldMetas
	),
	(	occurs_in(B, OldMetas)
	->	add_var_to_set(A, Metas1, Metas2)
	;	Metas2 = Metas1
	),
	check_unifier_list(Rest, Metas2, Metas). 


%% free_vars_of(+List,-Free)
%
% Free contains the element of List that are free variables.
% If there are none, Free is an empty list.  
free_vars_of(List,Free) :-
    ( setof( X, (member(X,List), var(X)), Free),  
      !
    ; Free = []
    ).
    
    
%% add_meta_vars(+Candidates,+KnownMeta,?AllMeta)
%
% Candidates and KnownMeta are lists of free variables.
% If some variable from Candidates is in AllMeta all the
% other candidates that do not occur in KnownMeta are 
% prepended to KnownMeta yielding AllMeta.
add_meta_vars(Candidates,KnownMeta,AllMeta) :- 
    select(Var,Candidates,OtherCandidates),
    occurs_in(Var,KnownMeta), 
    combine_sets_nonbinding(OtherCandidates, KnownMeta, AllMeta),
    !.

combine_sets_nonbinding([],Set,Set).
combine_sets_nonbinding([E|Rest],OldSet,NewSet):-
    add_var_to_set(E,OldSet,Set),
    combine_sets_nonbinding(Rest,Set,NewSet).
    
    
/* add_var_to_set(?Var, +Set, ?NewSet) is det.
 * 
 * Arg3 is the same as Arg2 but if Arg1 is not already an element
 * of Arg2 it is addes as a first element to Arg3.
 * 
 * Attention: the comparision is based on == instead of =, so
 * 			  different variables are treated differently.
 */
add_var_to_set(Var, Set, NewSet):-
    (	occurs_in(Var, Set)
    ->	NewSet = Set
    ;	NewSet = [Var|Set]
    ).
    
    
/* occurs_in(?Var, +Set) is det.
 * 
 * Succseds, if Arg1 is equal to a member of Arg2.
 * The comparision is done with == instead of =!
 */ 
occurs_in(Var, Set):-
	nth1(_, Set, OldVar),
    OldVar == Var,
    !. 

combine_meta_args([],[]):- !.    
combine_meta_args([List],List):- !.
combine_meta_args([MetaArgs|RestMetaArgs],CombinedArgs):-
    combine_meta_args(RestMetaArgs,RestCombinedArgs),
    combine_two_arg_lists(MetaArgs, RestCombinedArgs, CombinedArgs).
    
combine_two_arg_lists([], [], []):- !.
combine_two_arg_lists([ArgA|ArgsA], [ArgB|ArgsB], [CombinedArg|CombinedRest]):-
	 combine_two_arg_lists(ArgsA,ArgsB,CombinedRest),
	(	number(ArgA)
	->	(	number(ArgB)
		->	max_list([ArgA,ArgB],CombinedArg)
		;	CombinedArg = ArgA
		)
	;	(	number(ArgB)
		->	CombinedArg = ArgB
		;	CombinedArg = ?
		)   
	).


