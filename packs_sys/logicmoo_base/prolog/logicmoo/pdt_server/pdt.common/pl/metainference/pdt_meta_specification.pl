/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module(pdt_meta_specification, [extended_meta_predicate/2]).

:- meta_predicate(extended_meta_predicate(:, ?)).
extended_meta_predicate(Module:Head, MetaSpec) :-
	extended_meta_spec(Head, Module, MetaSpec).

:- dynamic(extended_meta_spec/3).

:- meta_predicate(extended_meta_predicate(:)).
extended_meta_predicate(Module:MetaSpec) :-
	(	valid_meta_spec(MetaSpec)
	->	store_meta_spec(Module, MetaSpec)
	;	throw(invalid_extended_meta_predicate_specification(Module:MetaSpec))
	).

store_meta_spec(Module, MetaSpec) :-
	functor(MetaSpec, Name, Arity),
	functor(Head, Name, Arity),
	retractall(extended_meta_spec(Head, Module, _)),
	assertz(extended_meta_spec(Head, Module, MetaSpec)).

valid_meta_spec(Head) :-
	valid_meta_args(Head, 1).
valid_meta_args(Head, N) :-
	arg(N, Head, Arg),
	!,
	valid_meta_arg(Arg),
	N2 is N + 1,
	valid_meta_args(Head, N2).
valid_meta_args(_, _).

valid_meta_arg(I) :- integer(I), !.
valid_meta_arg(^).                 
valid_meta_arg(//).                
valid_meta_arg(:).                
valid_meta_arg(?).                
valid_meta_arg(+).                
valid_meta_arg(-).                
valid_meta_arg(*).                
valid_meta_arg(database).
valid_meta_arg(functor(M)) :- valid_meta_arg(M).       
valid_meta_arg(add_prefix(P, M)) :- atom(P), valid_meta_arg(M).
valid_meta_arg(add_suffix(S, M)) :- atom(S), valid_meta_arg(M).
valid_meta_arg(is_prefix(M)) :- valid_meta_arg(M).     
valid_meta_arg(is_suffix(M)) :- valid_meta_arg(M).     
valid_meta_arg(has_arity(I,M)) :- integer(I), valid_meta_arg(M).    
valid_meta_arg(arity(M)) :- valid_meta_arg(M).          
valid_meta_arg(univ_list(M)) :- valid_meta_arg(M).  
