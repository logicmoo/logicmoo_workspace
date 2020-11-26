:- encoding(iso_latin_1).
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

:- module(pdt_call_analysis, [find_undefined_call/11, find_dead_predicate/9, find_undeclared_meta_predicate/10]).

:- use_module(pdt_prolog_codewalk).
:- use_module(pdt_call_graph).
:- use_module(pdt_prolog_library(utils4modules_visibility)).
:- use_module(pdt_common_pl('metainference/pdt_prolog_metainference')).
:- use_module(pdt_common_pl('metainference/pdt_meta_specification')).
:- use_module(pdt_common_pl(pdt_entry_points)).
:- use_module(pdt_common_pl(properties)).
%:- use_module(library(prolog_clause),except([hidden_module/1])).
:- use_module(pdt_prolog_library(compatibility), [
	pdt_source_file/2
]).

:- dynamic(result/4).
:- dynamic(result_transparent/5).

assert_result(M:Goal, Caller, clause_term_position(Ref, TermPosition), Kind) :-
	(	predicate_property(Caller, transparent),
		\+ predicate_property(Caller, meta_predicate(_)),
		Kind = undefined(NestedKind),
		(	NestedKind = metacall(_, _)
		;	NestedKind = database(_, _)
		)
	->	(	retract(result_transparent(Goal, Ref, TermPosition, Kind, Modules))
		->	(	member(M, Modules)
			->	NewModules = Modules
			;	NewModules = [M|Modules]
			)
		;	NewModules = [M]
		),
		assertz(result_transparent(Goal, Ref, TermPosition, Kind, NewModules))
	;	(	result(M:Goal, Ref, TermPosition, Kind)
		->	true
		;	assertz(result(M:Goal, Ref, TermPosition, Kind))
		)
	),
	!.
assert_result(_,_,_,_).

%% find_undefined_call(Root, IncludeSomeExecutionContext, Module, Name, Arity, File, Start, End, UndefName, UndefArity, PropertyList) 
find_undefined_call(Root, IncludeSomeExecutionContext, Module, Name, Arity, File, Start, End, UndefName, UndefArity, [line(Line)|PropertyList]) :-
	retractall(result(_, _, _, _)),
	retractall(result_transparent(_, _, _, _, _)),
	pdt_walk_code([undefined(trace), on_trace(pdt_call_analysis:assert_result)]),
	!,
	(	retract(result(M:Goal, Ref, TermPosition, _Kind))
	;	retract(result_transparent(Goal, Ref, TermPosition, _Kind, Ms))
	),
	(	TermPosition = term_position(Start, End, _, _, _)
	->	true
	;	TermPosition = Start-End
	),
	clause_property(Ref, file(File)),
	(	nonvar(Root)
	->	sub_atom(File, 0, _, _, Root)
	;	true
	),
	clause_property(Ref, predicate(Module:Name/Arity)),
	clause_property(Ref, line_count(Line)),
	properties_for_predicate(Module,Name,Arity,PropertyList0),
	(	nonvar(M)
	->	(	M \== Module
		->	format(atom(Prefix), '~w:', [M]),
			PropertyList = [prefix(Prefix)|PropertyList0]
		;	PropertyList = PropertyList0
		)
	;	nonvar(Ms),
		sort(Ms, SortedMs),
		(	IncludeSomeExecutionContext \== true
		->	functor(Head, Name, Arity),
			setof(TM,
				Module^Head^(
					(	TM = Module
					;	predicate_property(TM:Head, imported_from(Module))
					)
				),
				TargetModules
			),
			TargetModules == SortedMs
		;	true
		),
		atomic_list_concat(SortedMs, ', ', ModuleList),
		format(atom(TransparentTargetsAtom), ' in execution context ~w (context dependend)', [ModuleList]),
		PropertyList = [suffix(TransparentTargetsAtom)|PropertyList0]
	),
	functor(Goal, UndefName, UndefArity).
%	format(atom(GoalAsAtom), '~w', [Goal]).

%% find_dead_predicate(Root, Module, Functor, Arity, File, HeadLocation, ClauseStart, ClauseEnd, PropertyList) 
%
find_dead_predicate(Root, Module, Functor, Arity, File, HeadLocation, ClauseStart, ClauseEnd, PropertyList) :-
	find_dead_predicates,
	!,
	is_dead(Module, Functor, Arity),
	\+ find_blacklist(Functor, Arity, Module),
%	once(accept_dead_predicate(Module:Functor/Arity)),
	defined_in_files(Module, Functor, Arity, Locations),
	member(File-LineAndClauseRefs, Locations),
	(	nonvar(Root)
	->	sub_atom(File, 0, _, _, Root)
	;	true
	),
    member(location(Line, Ref), LineAndClauseRefs),
    properties_for_predicate(Module, Functor, Arity, PropertyList0),
    (	positions_of_clause(Ref, Position, ClauseStart, ClauseEnd)
    ->	HeadLocation = Position,
    	PropertyList = [line(Line)|PropertyList0]
    ;	HeadLocation = Line,
    	PropertyList = PropertyList0
    ).

positions_of_clause(Ref, Position, ClauseStart, ClauseEnd) :-
	catch(clause_info(Ref, _, TermPosition, _),_,fail),
	(	clause_property(Ref, fact)
	->	% fact
		TermPosition = HeadPosition,
		Start = ClauseStart,
		End = ClauseEnd
	;	% clause with body
		TermPosition = term_position(ClauseStart, ClauseEnd, _, _, [HeadPosition|_])
	),
	(	HeadPosition = Start-End
	->	% no arguments
		true
	;	% at least one argument
		HeadPosition = term_position(Start, End, _, _, _)
	),
	format(atom(Position), '~w-~w', [Start, End]).


:- multifile(entry_point/1).

:- multifile(accept_dead_predicate/1).

:- dynamic(is_called/3).
:- dynamic(is_dead/3).

find_dead_predicates :-
	ensure_call_graph_generated,
	retractall(is_called(_, _, _)),
	retractall(is_dead(_, _, _)),
	forall((
		entry_point_predicate(M, F, A)
	),(
		(	is_called(M, F, A)
		->	true
		;	assertz(is_called(M, F, A)),
			follow_call_edge(M, F, A)
		)
	)),
	(	is_called(_, _, _)
	->	forall((
			declared_in_module(M, F, A, M),
			\+ is_called(M, F, A)
		),(
			(	is_dead(M, F, A)
			->	true
			;	assertz(is_dead(M, F, A))
			)
		))
	;	true
	).

entry_point_predicate(M, F, A) :-
	entry_point(M), % module
	atomic(M),
	(	M == user
	->	declared_in_module(user, F, A, user)
	;	module_property(M, exports(ExportList)),
		member(F/A, ExportList)
	).
entry_point_predicate(M, F, A) :-
	entry_point(M:F/A). % predicate
entry_point_predicate(M, F, A) :-
	pdt_entry_point(File),
	(	module_property(M, file(File))
	*->	module_property(M, exports(ExportList)),
		member(F/A, ExportList)
	;	pdt_source_file(M:Head, File),
		functor(Head, F, A)
	).

follow_call_edge(M, F, A) :-
	calls(M2, F2, A2, M, F, A, _),
	\+ is_called(M2, F2, A2),
	assertz(is_called(M2, F2, A2)),
	follow_call_edge(M2, F2, A2),
	fail.
follow_call_edge(_, _, _).

find_blacklist('$load_context_module',2,_).
find_blacklist('$load_context_module',3,_).
find_blacklist('$mode',2,_).
find_blacklist('$pldoc',4,_).

%% find_undeclared_meta_predicate(Root, Module, Name, Arity, MetaSpec, MetaSpecAtom, File, Line, PropertyList, Directive)
find_undeclared_meta_predicate(Root, Module, Name, Arity, MetaSpec, MetaSpecAtom, File, Line, [label(MetaSpecAtom)|PropertyList], Directive) :-
	ensure_call_graph_generated,
	!,
	declared_in_module(Module, Name, Arity, Module),
	functor(Head, Name, Arity),
	\+ predicate_property(Module:Head, built_in),
%	\+ predicate_property(Module:Head, multifile),
	inferred_meta(Module:Head, MetaSpec),
	predicate_property(Module:Head, line_count(Line)),
	\+ extended_meta_predicate(Module:Head, _ExtendedMetaSpec),
	(	predicate_property(Module:Head, meta_predicate(DeclaredMetaSpec))
	->	DeclaredMetaSpec \== MetaSpec
	;	true
	),
	properties_for_predicate(Module, Name, Arity, PropertyList),
	member(file(File), PropertyList),
	(	nonvar(Root)
	->	sub_atom(File, 0, _, _, Root)
	;	true
	),
	format(atom(MetaSpecAtom), '~w', [MetaSpec]),
	(	swi_meta_predicate_spec(MetaSpec)
	->	format(atom(Directive), ':- meta_predicate(~w).~n', [MetaSpec])
	;	format(atom(Directive), ':- extended_meta_predicate(~w).~n', [MetaSpec])
	).

swi_meta_predicate_spec(Head) :-
	swi_meta_predicate_spec(Head, 1).

swi_meta_predicate_spec(Head, N) :-
	arg(N, Head, Arg),
	!,
	swi_meta_spec(Arg),
	N2 is N + 1,
	swi_meta_predicate_spec(Head, N2).
swi_meta_predicate_spec(_, _).

swi_meta_spec(I) :- integer(I), !.
swi_meta_spec(^).                 
swi_meta_spec(//).                
swi_meta_spec(:).                
swi_meta_spec(?).                
swi_meta_spec(+).                
swi_meta_spec(-).                
swi_meta_spec(*).                
