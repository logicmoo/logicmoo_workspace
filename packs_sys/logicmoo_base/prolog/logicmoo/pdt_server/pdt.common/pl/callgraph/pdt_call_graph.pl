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

:- module(pdt_call_graph, [ensure_call_graph_generated/0, calls/7, call_type/7, calls_multifile/8, pdt_walk_code/1]).

:- use_module(pdt_prolog_codewalk).
:- use_module(library(lists)).
:- use_module(pdt_prolog_library(compatibility), [
	pdt_source_file/2
]).

pdt_walk_code(Options) :-
	ensure_call_graph_generated,
	pdt_prolog_walk_code(Options).

:- dynamic(first_run/0).
first_run.

reset :-
	with_mutex(pdt_call_graph, (
		(	first_run
		->	true
		;	assertz(first_run),
			retractall(calls_(_, _, _, _, _, _, _, _, _)),
			retractall(calls_multifile_(_, _, _, _, _, _, _, _))
		)
	)).

ensure_call_graph_generated :-
	with_mutex(pdt_call_graph, (
		first_run,
		!,
		generate_call_graph,
		retractall(first_run)
	)),
	!.
ensure_call_graph_generated.

%% calls(CalleeModule, CalleeName, CalleeArity, CallerModule, CallerName, CallerArity, NumberOfCalls)
calls(CalleeModule, CalleeName, CalleeArity, CallerModule, CallerName, CallerArity, NumberOfCalls) :-
	calls_(CalleeModule, CalleeName, CalleeArity, CallerModule, CallerName, CallerArity, NumberOfCalls, _TermPosition, _Info).

call_type(CalleeModule, CalleeName, CalleeArity, CallerModule, CallerName, CallerArity, Info) :-
	calls_(CalleeModule, CalleeName, CalleeArity, CallerModule, CallerName, CallerArity, _NumberOfCalls, _TermPosition, [Info|_]).

%% calls_multifile(CalleeModule, CalleeName, CalleeArity, CallerModule, CallerName, CallerArity, File, NumberOfCalls)
calls_multifile(CalleeModule, CalleeName, CalleeArity, CallerModule, CallerName, CallerArity, File, NumberOfCalls) :-
	calls_multifile_(CalleeModule, CalleeName, CalleeArity, CallerModule, CallerName, CallerArity, File, NumberOfCalls).

:- dynamic(calls_/9).
:- dynamic(calls_multifile_/8).


clear([]).
clear([Module:Name/Arity|Predicates]) :-
	retractall(calls_(_,_,_,Module,Name,Arity,_, _, _)),
	retractall(calls_multifile_(_,_,_,Module,Name,Arity,_,_)),
	clear(Predicates).

:- dynamic(predicates_to_walk/1).

generate_call_graph :-
	with_mutex(pdt_call_graph, generate_call_graph__).

generate_call_graph__ :-
	pdt_prolog_walk_code([ trace_reference(_),
			on_trace(pdt_call_graph:assert_edge),
			new_meta_specs(pdt_call_graph:generate_call_graph_new_meta_specs),
			reiterate(false),
			source(false)
			]),
	(	predicates_to_walk(NewPredicates)
	->	retractall(predicates_to_walk(_)),
		clear(NewPredicates),
		generate_call_graph__(NewPredicates)
	;	true
	).

generate_call_graph(Predicates) :-
	with_mutex(pdt_call_graph, generate_call_graph__(Predicates)).

generate_call_graph__(Predicates) :-
	pdt_prolog_walk_code([ trace_reference(_),
			on_trace(pdt_call_graph:assert_edge),
			new_meta_specs(pdt_call_graph:generate_call_graph_new_meta_specs),
			reiterate(false),
			source(false),
			predicates(Predicates)
			]),
	(	predicates_to_walk(NewPredicates)
	->	retractall(predicates_to_walk(_)),
		clear(NewPredicates),
		generate_call_graph__(NewPredicates)
	;	true
	).

generate_call_graph_new_meta_specs(MetaSpecs) :-
	retractall(predicates_to_walk(_)),
	findall(Module:Name/Arity, (
		member(MetaSpec, MetaSpecs),
		pi_of_head(MetaSpec, M, N, A),
		calls_(M, N, A, Module, Name, Arity, _, _, _)
	), Predicates),
	(	Predicates \== []
	->	sort(Predicates, PredicatesUnique),
		assertz(predicates_to_walk(PredicatesUnique))
	;	true
	).
	
assert_edge(M1:Callee, M2:Caller, clause(Ref), Info) :-
	assert_edge(M1:Callee, M2:Caller, clause_term_position(Ref, undefined), Info).
	
assert_edge(M1:Callee, M2:Caller, clause_term_position(Ref, TermPosition), Info) :-
	functor(Callee,F1,N1),
	(	predicate_property(M1:Callee, imported_from(M0))
	->	M = M0
	;	M = M1
	),
	functor(Caller,F2,N2), 
	assert_edge_(M,F1,N1, M2,F2,N2,TermPosition,Info),
	(	predicate_property(M2:Caller, multifile),
		clause_property(Ref, file(File))
	->	assert_multifile_edge(M,F1,N1, M2,F2,N2, File)
	;	true
	).
assert_edge(_, '<initialization>', _, _) :- !.

assert_edge_(M1,F1,N1, M2,F2,N2,TermPos,Info) :-
	retract( calls_(M1,F1,N1, M2,F2,N2, Counter, TermPosTail, InfoTail) ), 
	!,
	Cnt_plus_1 is Counter + 1,
	assertz(calls_(M1,F1,N1, M2,F2,N2, Cnt_plus_1, [TermPos|TermPosTail], [Info|InfoTail])).
assert_edge_(M1,F1,N1, M2,F2,N2, TermPos, Info) :-
	assertz(calls_(M1,F1,N1, M2,F2,N2, 1, [TermPos], [Info])).

assert_multifile_edge(M1,F1,N1, M2,F2,N2, File) :-
	retract( calls_multifile_(M1,F1,N1, M2,F2,N2, File, Counter) ), 
	!,
	Cnt_plus_1 is Counter + 1,
	assertz(calls_multifile_(M1,F1,N1, M2,F2,N2, File, Cnt_plus_1)).
assert_multifile_edge(M1,F1,N1, M2,F2,N2, File) :-
	assertz(calls_multifile_(M1,F1,N1, M2,F2,N2, File, 1)).

:- multifile(pdt_reload:pdt_reload_listener/1).
pdt_reload:pdt_reload_listener(_Files) :-
	(	first_run
	->	true
	;	setof(Module:Name/Arity, Head^File^FileModule^(
			(	pdt_reload:reloaded_file(File),
				(	pdt_source_file(Module:Head, File)
				;	module_property(FileModule, file(File)),
					predicate_property(FileModule:Head, imported_from(Module)),
					predicate_property(Module:Head, transparent),
					\+ predicate_property(Module:Head, meta_predicate(_))
				),
				functor(Head, Name, Arity)
			;	retract(predicate_to_clear(Module, Name, Arity))
			)
		), Predicates),
		!,
		clear(Predicates),
		generate_call_graph(Predicates)
	).

:- multifile(user:message_hook/3).
:- dynamic(user:message_hook/3).
user:message_hook(load_file(start(_, file(_, File))),_,_) :-
	\+ first_run,
	(	pdt_source_file(Module:Head, File)
	;	module_property(FileModule, file(File)),
		predicate_property(FileModule:Head, imported_from(Module)),
		predicate_property(Module:Head, transparent),
		\+ predicate_property(Module:Head, meta_predicate(_))
	),
	functor(Head, Name, Arity),
	\+ predicate_to_clear(Module, Name, Arity),
	assertz(predicate_to_clear(Module, Name, Arity)),
	fail.

pi_of_head(Module:Head, Module, Name, Arity) :-
	functor(Head, Name, Arity).

:- dynamic(predicate_to_clear/3).

