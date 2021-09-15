/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker, Ilshat Aliev
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module(util_for_graphML, [
	predicate_in_file/4,
	file_of_predicate/4,
	main_module_of_file/2,
	first_line_of_predicate_in_file/5,
	exported_predicate/3,
	exported_predicate/2,
	locally_dead_predicate/3,
	call_term_position/7
]).

:- use_module(library(lists)).
:- use_module(pdt_prolog_library(utils4modules_visibility)).
:- use_module(pdt_common_pl('callgraph/pdt_call_graph')).
:- use_module(pdt_prolog_library(compatibility), [
	pdt_source_file/2
]).

predicate_in_file(File, Module, Name, Arity) :-
	pdt_source_file(Module:Head, File),
	functor(Head, Name, Arity),
	\+ find_blacklist(Name, Arity, Module).

find_blacklist('$load_context_module',2,_).
find_blacklist('$load_context_module',3,_).
find_blacklist('$mode',2,_).
find_blacklist('$pldoc',4,_).


file_of_predicate(Module, Name, Arity, File) :-
	functor(Head, Name, Arity),
	(	predicate_property(Module:Head, multifile)
	->	defined_in_files(Module, Name, Arity, Locations),
		member(File-_, Locations)
	;	(	predicate_property(Module:Head, file(File))
		->	true
		;	module_property(Module, file(File))
		)
	).

main_module_of_file(File, Module) :-
	module_property(Module, File),
	\+ atom_concat(plunit_, _, Module),
	!.

main_module_of_file(_File, user).

first_line_of_predicate_in_file(Module, Name, Arity, File, Line) :-
	functor(Head, Name, Arity),
	(	predicate_property(Module:Head, multifile)
	->	defined_in_files(Module, Name, Arity, Locations),
		member(File-LineAndRefs, Locations),
		findall(L, member(location(L, _), LineAndRefs), Ls),
		min_list(Ls, Line)
	;	(	predicate_property(Module:Head, line_count(Line))
		->	true
		;	Line = 1
		)
	).
		

exported_predicate(Module, Name, Arity) :-
	functor(Head, Name, Arity),
	exported_predicate(Module, Head).

exported_predicate(Module, Head) :-
	(	Module == user
	;	predicate_property(Module:Head, exported)
	),
	!.

locally_dead_predicate(Module, Name, Arity) :-
	locally_dead_predicate(Module, Name, Arity, [Module:Name/Arity]).

locally_dead_predicate(Module, Name, Arity, _Visited):-
	uncalled_local_predicate(Module, Name, Arity).
locally_dead_predicate(Module, Name, Arity, Visited):-
	\+ exported_predicate(Module, Name, Arity),
	forall((
		calls(Module, Name, Arity, CallerModule, CallerName, CallerArity, _NumberOfCalls),
		\+ member(CallerModule:CallerName/CallerArity, Visited)
	),(
		locally_dead_predicate(CallerModule, CallerName, CallerArity, [CallerModule:CallerName/CallerArity | Visited])
	)).
			
uncalled_local_predicate(Module, Name, Arity):-
	uncalled_predicate(Module, Name, Arity),
	\+ exported_predicate(Module, Name, Arity).

uncalled_predicate(Module, Name, Arity):-
	declared_in_module(Module, Name, Arity, _),
	\+ calls(Module, Name, Arity, _, _, _, _).

assert_location(_, _, clause_term_position(_Ref, TermPosition), _) :-
	asserta(location(TermPosition)).

:- dynamic(location/1).

call_term_position(SourceModule, SourceFunctor, SourceArity, TargetModule, TargetFunctor, TargetArity, Position) :-
	retractall(location(_)),
	functor(Target, TargetFunctor, TargetArity),
	pdt_walk_code([trace_reference(TargetModule:Target), predicates([SourceModule:SourceFunctor/SourceArity]), on_trace(util_for_graphML:assert_location)]),
	location(TermPosition),
	!,
	(	TermPosition = term_position(Start, End, _, _, _)
	;	TermPosition = Start-End
	),
	format(atom(Position), '~w-~w', [Start, End]).
