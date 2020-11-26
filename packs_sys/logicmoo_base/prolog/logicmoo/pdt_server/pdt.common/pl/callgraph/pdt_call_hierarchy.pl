/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2014, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module(pdt_call_hierarchy, [
	find_predicate_declaration_and_visibility/5,
	find_caller/9,
	find_callee/9,
	find_call_location/9
]).

:- use_module(pdt_prolog_library(utils4modules_visibility), [
	declared_in_module/4,
	visible_in_module/3,
	module_of_file/2
]).

:- use_module(pdt_call_graph, [
	ensure_call_graph_generated/0,
	calls/7,
	calls_multifile/8,
	pdt_walk_code/1
]).

:- use_module(library(lists), [
	sum_list/2
]).

%% find_predicate_declaration_and_visibility(ModuleOrFile, Name, Arity, DeclaringModule, Visibility)
find_predicate_declaration_and_visibility(ModuleOrFile, Name, Arity, DeclaringModule, Visibility) :-
	(	ModuleOrFile = module(Module)
	->	true
	;	ModuleOrFile = file(File),
		source_file(File),
		once(module_of_file(File, Module))
	),
	(	declared_in_module(Module, Name, Arity, DeclaringModule)
	->	true
	;	DeclaringModule = Module
	),
	predicate_visibility(DeclaringModule, Name, Arity, Visibility).

%% predicate_visibility(Module, Name, Arity, Visibility)
predicate_visibility(Module, Name, Arity, Visibility) :-
	(	visible_in_module(Module, Name, Arity)
	->	(	(	Module == user
			;	functor(Head, Name, Arity),
				predicate_property(Module:Head, exported)
			)
		->	Visibility = exported
		;	Visibility = non_exported
		)
	;	Visibility = undefined
	).

%% find_caller(Module, Name, Arity, Root, CallerModule, CallerName, CallerArity, Count, Visibility)
find_caller(Module, Name, Arity, Root, CallerModule, CallerName, CallerArity, Count, Visibility) :-
	ensure_call_graph_generated,
	(	var(Root)
	->	calls(Module, Name, Arity, CallerModule, CallerName, CallerArity, Count)
	;	(	functor(Head, Name, Arity),	
			predicate_property(Module:Head, multifile)
		->	bagof(
				NumberOfCalls,
				Module^Name^Arity^File^(	
					calls_multifile(Module, Name, Arity, CallerModule, CallerName, CallerArity, File, NumberOfCalls),
					atom_concat(Root, _, File)
				),
				Counts
			),
			sum_list(Counts, Count)
		;	calls(Module, Name, Arity, CallerModule, CallerName, CallerArity, Count),
			functor(CallerHead, CallerName, CallerArity),
			predicate_property(CallerModule:CallerHead, file(File)),
			atom_concat(Root, _, File)
		)
	),
	predicate_visibility(CallerModule, CallerName, CallerArity, Visibility).
	
%% find_callee(Module, Name, Arity, Root, CalleeModule, CalleeName, CalleeArity, Count, Visibility)
find_callee(Module, Name, Arity, Root, CalleeModule, CalleeName, CalleeArity, Count, Visibility) :-
	ensure_call_graph_generated,
	(	var(Root)
	->	calls(CalleeModule, CalleeName, CalleeArity, Module, Name, Arity, Count)
	;	functor(Head, Name, Arity),
		(	predicate_property(Module:Head, multifile)
		->	bagof(
				NumberOfCalls,
				Module^Name^Arity^File^(	
					calls_multifile(CalleeModule, CalleeName, CalleeArity, Module, Name, Arity, File, NumberOfCalls),
					atom_concat(Root, _, File)
				),
				Counts
			),
			sum_list(Counts, Count)
		;	predicate_property(Module:Head, file(File)),
			atom_concat(Root, _, File),
			calls(CalleeModule, CalleeName, CalleeArity, Module, Name, Arity, Count)
		)
	),
	predicate_visibility(CalleeModule, CalleeName, CalleeArity, Visibility).
 
%% find_call_location(CallerModule, CallerName, CallerArity, CalleeModule, CalleeName, CalleeArity, Root, File, Location)
find_call_location(CallerModule, CallerName, CallerArity, CalleeModule, CalleeName, CalleeArity, Root, File, Location) :-
	retractall(result(_, _, _, _)),
	functor(CalleeHead, CalleeName, CalleeArity),
	pdt_walk_code([trace_reference(CalleeModule:CalleeHead), predicates([CallerModule:CallerName/CallerArity]), on_trace(pdt_call_hierarchy:assert_result)]),
	!,
	retract(result(_Goal, Ref, TermPosition, _Kind)),
	(	(	TermPosition = term_position(Start, End, _, _, _)
		;	TermPosition = Start-End
		)
	->	format(atom(Location), '~w-~w', [Start, End])
	;	clause_property(Ref, line_count(Location))
	),
	clause_property(Ref, file(File)),
	(	nonvar(Root)
	->	sub_atom(File, 0, _, _, Root)
	;	true
	).

:- dynamic(result/4).

assert_result(Goal, _, clause(Ref), Kind) :-
    (	result(Goal, Ref, [], Kind)
    ->	true
    ;	assertz(result(Goal, Ref, [], Kind))
    ),
    !.
assert_result(Goal, _, clause_term_position(Ref, TermPosition), Kind) :-
    (	result(Goal, Ref, TermPosition, Kind)
    ->	true
    ;	assertz(result(Goal, Ref, TermPosition, Kind))
    ),
    !.
assert_result(_,_,_,_).

