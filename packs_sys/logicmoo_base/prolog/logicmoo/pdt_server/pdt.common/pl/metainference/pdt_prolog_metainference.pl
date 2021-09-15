:- encoding(iso_latin_1).
/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Eva Stoewe, Günter Kniesel, Jan Wielemaker, Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module(pdt_prolog_metainference,
	  [ infer_meta/2,		% :Head, -MetaSpec
	    infer_meta/3,		% :Head, -MetaSpec, -NewOrUpdated
	    inferred_meta/2		% :Head, ?MetaSpec
	  ]).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(pdt_prolog_library(utils4modules_visibility)).
:- use_module(pdt_prolog_library(lists)).
:- use_module(pdt_meta_specification).

reset :-
	retractall(inferred_meta_pred(_, _, _)).

run :-
	run(1).

run(Run) :-
	statistics(cputime, CPU0),
	format('Starting meta inference iteration ~w~n', [Run]),
	findall(Module:MetaSpec, (
		declared_in_module(Module, Name, Arity, Module),
		functor(Head, Name, Arity),
		\+ predicate_property(Module:Head, built_in),
		\+ predicate_property(Module:Head, foreign),
		\+ predicate_property(Module:Head, number_of_clauses(0)),
		do_infer_meta(Module:Head, MetaSpec),
		(	inferred_meta_pred(Head, Module, ExistingMetaSpec)
		->	(	MetaSpec == ExistingMetaSpec
			->	fail
			;	retract(inferred_meta_pred(Head, Module, ExistingMetaSpec)),
				assertz(inferred_meta_pred(Head, Module, MetaSpec))
			)
		;	assertz(inferred_meta_pred(Head, Module, MetaSpec))
		)
	), NewOrModifiedMetaSpecs),
	forall(member(Module:MetaSpec, NewOrModifiedMetaSpecs), format('% :- meta_predicate ~w:~w.~n', [Module, MetaSpec])),
	statistics(cputime, CPU1),
	CPU is CPU1 - CPU0,
	length(NewOrModifiedMetaSpecs, NewOrModified),
	format('Finished meta inference iteration ~w: ~w meta predicates found or modified in ~f sec.~n', [Run, NewOrModified, CPU]),
	(	NewOrModified > 0
	->	NextRun is Run + 1,
		run(NextRun)
	;	true
	).

:- meta_predicate
	inferred_meta(:, ?),
	infer_meta(:, -),
	infer_meta(:, -, -).

:- dynamic
	inferred_meta_pred/3.			% Head, Module, Meta

/** <module> Infer meta-predicate properties

This module infers meta-predicate properties   by inspecting the clauses
of predicates that call other predicates.   This is extremely useful for
program analysis and refactoring because  many   programs  `in the wild'
have incomplete or incorrect meta-predicate information.

@see	This library is used by prolog_walk_code/1 to improve the
	accuracy of this analysis.
@tbd	Re-introduce some alias-analysis
@tbd	Not all missing meta-declarations are interesting.  Notably,
	meta-predicates that are private and only pass meta-arguments
	on behalve of a public meta-predicates do not need a declaration.
*/


%%	inferred_meta(:Head, ?MetaSpec) is nondet.
%
%	True when MetaSpec is an   inferred meta-predicate specification
%	for Head.

inferred_meta(M:Head, MetaSpec) :-
	inferred_meta_pred(Head, M, MetaSpec).
inferred_meta(M:Head, MetaSpec) :-
	predicate_property(M:Head, imported_from(From)),
	inferred_meta_pred(Head, From, MetaSpec).


%%	infer_meta(:Head, -MetaSpec) is semidet.
%
%	True  when  MetaSpec  is  a  meta-predicate  specifier  for  the
%	predicate Head. Derived meta-predicates are   collected and made
%	available through inferred_meta_predicate/2.

infer_meta(M:Head, MetaSpec) :-
	infer_meta(M:Head, MetaSpec, _).

infer_meta(M:Head, MetaSpec, NewOrUpdated) :-
	predicate_property(M:Head, imported_from(From)), !,
	do_infer_meta(From:Head, MetaSpec),
	update_inferred_metaspec(Head, From, MetaSpec, NewOrUpdated).
infer_meta(M:Head, MetaSpec, NewOrUpdated) :-
	do_infer_meta(M:Head, MetaSpec),
	update_inferred_metaspec(Head, M, MetaSpec, NewOrUpdated).

update_inferred_metaspec(Head, M, NewMetaSpec, NewOrUpdated) :-
	inferred_meta_pred(Head, M, OldMetaSpec),
	!,
	(	OldMetaSpec == NewMetaSpec
	->	NewOrUpdated = false
	;	retract(inferred_meta_pred(Head, M, OldMetaSpec)),
		assertz(inferred_meta_pred(Head, M, NewMetaSpec)),
		NewOrUpdated = true
	).
update_inferred_metaspec(Head, M, NewMetaSpec, true) :-
	assertz(inferred_meta_pred(Head, M, NewMetaSpec)).

:- meta_predicate
	do_infer_meta(:, -).

do_infer_meta(Module:AHead, MetaSpec):-
	functor(AHead, Functor, Arity),
	functor(Head, Functor, Arity),	% Generalise the head
	findall(MetaTerm,
		meta_pred_args_in_clause(Module, Head, MetaTerm),
		MetaTerms),
	MetaTerms \== [],
	combine_meta_args(MetaTerms, MetaTerm),
	meta_terms_to_arg_specs_in_head(MetaTerm, MetaSpec).


%%	meta_pred_args_in_clause(+Module, +Head, -MetaTermHead) is nondet.

meta_pred_args_in_clause(Module, Head, MetaTermHead) :-
	catch(clause(Module:Head, Body), _, fail),
	annotate_meta_vars_in_body(Body, Module),
	meta_term_head(Head, MetaTermHead).

%%	annotate_meta_vars_in_body(+Term, +Module) is det
%
%	Annotate variables in Term if they appear as meta-arguments.
%
%	@tbd	Aliasing.  Previous code detected aliasing for
%		- =/2
%		- functor/3
%		- atom_concat/3
%		- =../2
%		- arg/3
%	@tbd	We can make this nondet, exploring multiple aliasing
%		paths in disjunctions.

:- discontiguous(annotate_meta_vars_in_body/2).
annotate_meta_vars_in_body(A, _) :-
	atomic(A), !.
annotate_meta_vars_in_body(Var, _) :-
	var(Var), !,
	annotate(0, Var).
annotate_meta_vars_in_body(Module:Term, _) :- !,
	(   atom(Module)
	->  annotate_meta_vars_in_body(Term, Module)
	;   var(Module)
	->  annotate(m, Module)
	;   true			% may continue if Term is a system
	).				% predicate?
annotate_meta_vars_in_body((TermA, TermB), Module) :- !,
	annotate_meta_vars_in_body(TermB, Module),
	annotate_meta_vars_in_body(TermA, Module).
annotate_meta_vars_in_body((TermA; TermB), Module) :- !,
	(	annotate_meta_vars_in_body(TermB, Module)
	;	annotate_meta_vars_in_body(TermA, Module)
	).
annotate_meta_vars_in_body((TermA->TermB), Module) :- !,
	annotate_meta_vars_in_body(TermB, Module),
	annotate_meta_vars_in_body(TermA, Module).
annotate_meta_vars_in_body((TermA*->TermB), Module) :- !,
	annotate_meta_vars_in_body(TermB, Module),
	annotate_meta_vars_in_body(TermA, Module).
annotate_meta_vars_in_body(A=B, _) :-
	!,
	unifiable(A, B, Unifiers),
	new_aliases(Unifiers).

%% new_aliases(+Aliases) is det.
%
%  Add the alias information to the
%  variable annotations. If two variables
%  are aliased their annotations are
%  merged.
new_aliases([]).
new_aliases([A = B|Aliases]) :-
	(	var(B)
	->	merge_variable_annotations(A, B),
		new_alias(A, B),
		new_alias(B, A)
	;	new_alias(A, B)
	),
	new_aliases(Aliases).

%% merge_variable_annotations(+V1, +V2) is det.
%
%  Merge the annotations of the variables V1 and
%  V2 (without aliases).
merge_variable_annotations(V1, V2) :-
	(	get_attr(V1, pmi, Meta1)
	->	(	get_attr(V2, pmi, Meta2)
		->	% V1 and V2 are both annotated
			get_aliased(Meta1, Aliased1),	             % remember their aliases
			get_aliased(Meta2, Aliased2),                
			merge_annotations(Meta1, Meta2, MetaMerged), % merge annotations (without aliases) into MetaMerged
			duplicate_term(MetaMerged, MetaMergedCopy),  % make a "real" copy of MetaMerged (necessary because of the use of setarg/3)
			set_aliased(MetaMerged, Aliased1),           % fill the remembered aliases into the merged term
			put_attr(V1, pmi, MetaMerged),
			set_aliased(MetaMergedCopy, Aliased2),
			put_attr(V2, pmi, MetaMergedCopy)
		;	% only V1 is annotated
			duplicate_term(Meta1, MetaCopy),             % V2 gets a copy of the annotatinos 
			set_aliased(MetaCopy, []),                   % of V1 (except for aliases)
			put_attr(V2, pmi, MetaCopy)
		)
	;	(	get_attr(V2, pmi, Meta2)
		->	% only V2 is annotated
			duplicate_term(Meta2, MetaCopy),             % V1 gets a copy of the annotatinos
			set_aliased(MetaCopy, []),                   % of V2(except for aliases)
			put_attr(V1, pmi, MetaCopy)
		;	% V1 and V2 are not annotated -> nothing to merge
			true
		)
	).

%% new_alias(+A, +B) is det.
%
%  Add the alias B to the annotations
%  of the variable A.
%  If B is a variable and not yet
%  contained in the annotated aliases
%  of A, B is also unified to all
%  aliases of A. This is checked
%  in check_for_unifiers/2.
new_alias(A, B) :-
	(	get_attr(A, pmi, Meta)
	->	get_aliased(Meta, Aliases),
		(	eq_member(B, Aliases)
		->	true
		;	annotate(aliased(B), A),
			check_for_unifiers(Aliases, B)
		)
	;	annotate(aliased(B), A)
	).

%% check_for_unifiers(+Aliases, +Var) is det.
%
%  Compute the unifiers of Var and each alias
%  in Aliases. The resulting unifiers are
%  aliases which are added to the variable
%  annotations using new_aliases/1.
check_for_unifiers([], _).
check_for_unifiers([Alias|Aliases], Var) :-
	unifiable(Alias, Var, Unifiers),
	new_aliases(Unifiers),
	check_for_unifiers(Aliases, Var).

annotate_meta_vars_in_body(functor(Term, Functor, Arity), _Module) :-
	!,
	(	var(Term),
		var_is_meta_called(Term, Annotation)       % Term is metacalled
	->	(	var(Functor)
		->	(	integer(Arity)
			->	annotate(has_arity(Arity, Annotation), Functor) % Arity is given
			;	annotate(functor(Annotation), Functor)          % Arity is unknown
			)
		;	true
		),
		(	var(Arity)
		->	annotate(arity(Annotation), Arity)
		;	true
		)
	;	true
	).

%% var_is_meta_called(+Var, -MetaAnnotations) is semidet.
% 
%  True when Var is metacalled, i.e. Var has at least one
%  corresponding annotation (0..9, ^, // or database). 
%  MetaAnnotations is bound to this annotation (in case of 
%  one) or to a list of these annotations.
var_is_meta_called(Var, MetaAnnotations) :-
	get_attr(Var, pmi, Meta),
	setof(Annotation,
		(	get_metacalled(Meta, Metacalled),
			member(Annotation, Metacalled)
		;	get_existential(Meta, ^),
			Annotation = ^
		),
	MetaAnnotations0),
	remove_list_if_possible(MetaAnnotations0, MetaAnnotations),
	!.

annotate_meta_vars_in_body(atom_concat(A, B, C), _Module) :-
	!,
	(	var(C)
	->	(	var_is_functor_or_meta_called(C, Annotation)
		->	annotate_atom_concat(A, B, Annotation)
		;	true
		)
	;	true
	).

%% var_is_functor_or_meta_called(+Var, -MetaAnnotations) is semidet.
% 
%  True when Var is metacalled or the functor of a metacalled term, 
%  i.e. Var has at least one corresponding annotation (0..9, ^, //,
%  database, functor/1 or has_arity/2). 
%  MetaAnnotations is bound to this annotation (in case of 
%  one) or to a list of these annotations.
var_is_functor_or_meta_called(Var, MetaAnnotations) :-
	get_attr(Var, pmi, Meta),
	setof(Annotation,
		(	get_metacalled(Meta, Metacalled),
			member(Annotation, Metacalled)
		;	get_existential(Meta, ^),
			Annotation = ^
		;	get_components(Meta, Components),
			member(Annotation, Components),
			(	Annotation = functor(_)
			;	Annotation = has_arity(_, _)
			)
		),
	MetaAnnotations0),
	remove_list_if_possible(MetaAnnotations0, MetaAnnotations),
	!.

annotate_atom_concat(A, B, Annotation) :-
	var(A), var(B), !,
	annotate(is_prefix(Annotation), A),
	annotate(is_suffix(Annotation), B).
annotate_atom_concat(A, B, Annotation) :-
	atomic(A), var(B), !,
	annotate(add_prefix(A, Annotation), B).
annotate_atom_concat(A, B, Annotation) :-
	var(A), atomic(B), !,
	annotate(add_suffix(B, Annotation), A).
annotate_atom_concat(_, _, _).

annotate_meta_vars_in_body(Term =.. List, _Module) :-
	!,
	(	var(Term),
		var_is_meta_called(Term, Annotation)
	->	(	var(List)
		->	annotate(univ_list(Annotation), List)
		;	(	List = [Functor|Args],
				var(Functor)
			->	(	finite_length(Args, ArgsLength)
				->	annotate(has_arity(ArgsLength, Annotation), Functor)
				;	annotate(functor(Annotation), Functor)
				)
			;	true
			)
		)
	;	true
	).

annotate_meta_vars_in_body(asserta(Clause), _Module) :-
	!,
	annotate_database_argument(Clause).
annotate_meta_vars_in_body(assert(Clause), _Module) :-
	!,
	annotate_database_argument(Clause).
annotate_meta_vars_in_body(assertz(Clause), _Module) :-
	!,
	annotate_database_argument(Clause).
annotate_meta_vars_in_body(retract(Clause), _Module) :-
	!,
	annotate_database_argument(Clause).
annotate_meta_vars_in_body(retractall(Clause), _Module) :-
	!,
	annotate_database_argument(Clause).

annotate_database_argument(Clause) :-
	var(Clause),
	!,
	annotate(database, Clause).
annotate_database_argument(Module:Clause) :-
	!,
	(	var(Module)
	->	annotate(m, Module)
	;	true
	),
	(	var(Clause)
	->	annotate(database, Clause)
	;	true
	).
annotate_database_argument(_Clause).

annotate_meta_vars_in_body(Goal, Module) :-
	extended_meta_predicate(Module:Goal, Head),
	!,
	functor(Goal, _, Arity),
	annotate_meta_args(1, Arity, Goal, Head, Module).
annotate_meta_vars_in_body(Goal, Module) :- % TBD: do we trust this?
	predicate_property(Module:Goal, meta_predicate(Head)),
	!,
	functor(Goal, _, Arity),
	annotate_meta_args(1, Arity, Goal, Head, Module).
annotate_meta_vars_in_body(Goal, Module) :-
	inferred_meta(Module:Goal, Head),
	!,
	functor(Goal, _, Arity),
	annotate_meta_args(1, Arity, Goal, Head, Module).
annotate_meta_vars_in_body(_, _).


%%	annotate_meta_args(+Arg, +Arity, +Goal, +MetaSpec, +Module)

annotate_meta_args(I, Arity, Goal, MetaSpec, Module) :-
	I =< Arity, !,
	arg(I, MetaSpec, MetaArg),
	arg(I, Goal, Arg),
	annotate_meta_arg(MetaArg, Arg, Module),
	I2 is I + 1,
	annotate_meta_args(I2, Arity, Goal, MetaSpec, Module).
annotate_meta_args(_, _, _, _, _).

annotate_meta_arg(Spec, Arg, _) :-
	var(Arg), !,
	annotate(Spec, Arg).
annotate_meta_arg(0, Arg, Module) :- !,
	annotate_meta_vars_in_body(Arg, Module).
annotate_meta_arg(N, Arg, Module) :-
	integer(N),
	callable(Arg), !,
	Arg =.. List,
	length(Extra, N),
	append(List, Extra, ListX),
	ArgX =.. ListX,
	annotate_meta_vars_in_body(ArgX, Module).
annotate_meta_arg(Spec, Arg, _) :-
	(	Spec == :
	;	meta_calling_specifier(Spec)
	),
	compound(Arg),
	Arg = Module:_,
	var(Module), !,
	annotate(m, Module).
annotate_meta_arg(_,_,_).


%% annotate(+Annotation, +Position, +Var) is det.
% 
% Add Annotation(s) 
%   meta( Aliased,          a list of aliases (variables and terms)
%         Metacalled,       a list of metacall specifiers (database or 0..9)
%         Components,       functor and arity building components
%         Existential,      ^
%         ModuleSensitive,  :
%         Mode,             one of +, - or ?
%         Nothing           *
%   )      
% to the attributes of Var for this module

annotate([], _) :- !.
annotate([Annotation|Annotations], Var) :-
	!,
	annotate(Annotation, Var),
	annotate(Annotations, Var).
annotate(aliased(Alias), Var) :-
	!,
	get_or_create_meta(Var, Meta),
	add_aliased(Meta, Alias),
	put_attr(Var, pmi, Meta).
annotate(^, Var) :-
	!,
	get_or_create_meta(Var, Meta),
	add_existential(Meta, ^),
	put_attr(Var, pmi, Meta).
annotate(:, Var) :-
	!,
	get_or_create_meta(Var, Meta),
	add_msensitive(Meta, :),
	put_attr(Var, pmi, Meta).
annotate(m, Var) :-
	!,
	get_or_create_meta(Var, Meta),
	add_module(Meta, m),
	put_attr(Var, pmi, Meta).
annotate(*, Var) :-
	!,
	get_or_create_meta(Var, Meta),
	add_bottom(Meta, *),
	put_attr(Var, pmi, Meta).
annotate(MetaCall, Var) :-
	meta_calling_specifier(MetaCall),
	!,
	get_or_create_meta(Var, Meta),
	add_metacalled(Meta, MetaCall),
	put_attr(Var, pmi, Meta).
annotate(Component, Var) :-
	component_specifier(Component),
	!,
	get_or_create_meta(Var, Meta),
	add_components(Meta, Component),
	put_attr(Var, pmi, Meta).
annotate(Mode, Var) :-
	mode_specifier(Mode),
	!,
	get_or_create_meta(Var, Meta),
	add_mode(Meta, Mode),
	put_attr(Var, pmi, Meta).
annotate(_, _).

% annotations of variables that are metacalled
meta_calling_specifier(I) :- integer(I), !.  % metacalled with I additional arguments
meta_calling_specifier(^).                   % metacalled 0 and can contain existential variables (called via setof/bagof/aggregate)
meta_calling_specifier(//).                  % DCG rule body
meta_calling_specifier(database).            % asserted or retracted

% annotated var represents the ... 
% of a term metacalled according to _M 
component_specifier(functor(_M)).          % ... =               functor        
component_specifier(add_prefix(_P, _M)).   % ... = suffix of the functor with prefix _P 
component_specifier(add_suffix(_S, _M)).   % ... = prefix of the functor with suffix _S
component_specifier(is_prefix(_M)).        % ... = prefix of the functor (with unknown suffix)
component_specifier(is_suffix(_M)).        % ... = suffix of the functor (with unknown prefix)

component_specifier(has_arity(_,_)).       % annotated var is the functor of a term with a fixed number of params

component_specifier(arity(_)).             % annotated var represents the arity of another term     

component_specifier(univ_list(_Meta)).     % The annotated var is a list from which a term is constructed via =.. 
                                       % The constructed term is metacalled according to _Meta

mode_specifier(+).
mode_specifier(-).
mode_specifier(?).


get_or_create_meta(Var, Meta) :-
	(	get_attr(Var, pmi, Meta)
	->	true
	;	new_meta(Meta)
	).
	
new_meta( meta([],[],[],[],[],[],[],[]) ).

get_aliased(Meta,Value)    :- arg(1,Meta,Value).
get_metacalled(Meta,Value) :- arg(2,Meta,Value).
get_components(Meta,Value) :- arg(3,Meta,Value).
get_existential(Meta,Value):- arg(4,Meta,Value).
get_msensitive(Meta,Value) :- arg(5,Meta,Value).
get_module(Meta,Value)     :- arg(6,Meta,Value).
get_mode(Meta,Value)       :- arg(7,Meta,Value).
get_bottom(Meta,Value)     :- arg(8,Meta,Value).

set_aliased(Meta,Value)    :- setarg(1,Meta,Value).
set_metacalled(Meta,Value) :- setarg(2,Meta,Value).
set_components(Meta,Value) :- setarg(3,Meta,Value).
set_existential(Meta,Value):- setarg(4,Meta,Value).
set_msensitive(Meta,Value) :- setarg(5,Meta,Value).
set_module_(Meta,Value)    :- setarg(6,Meta,Value).
set_mode(Meta,Value)       :- setarg(7,Meta,Value).
set_bottom(Meta,Value)     :- setarg(8,Meta,Value).

add_aliased(Meta,Value)    :- get_aliased(Meta, OldValue), merge_aliased([Value], OldValue, NewValue), set_aliased(Meta, NewValue).
add_metacalled(Meta,Value) :- get_metacalled(Meta, OldValue), merge_metacalled([Value], OldValue, NewValue), set_metacalled(Meta, NewValue).
add_components(Meta,Value) :- get_components(Meta, OldValue), merge_components([Value], OldValue, NewValue), set_components(Meta, NewValue).
add_existential(Meta,Value):- set_existential(Meta, Value).
add_msensitive(Meta,Value) :- set_msensitive(Meta, Value).
add_module(Meta,Value)     :- set_module_(Meta, Value).
add_mode(Meta,Value)       :- get_mode(Meta, OldValue), merge_mode(OldValue, Value, NewValue), set_mode(Meta, NewValue).
add_bottom(Meta,Value)     :- set_bottom(Meta, Value).

merge_aliased(Value1, Value2, Value3)     :- eq_union(Value1, Value2, Value3).

merge_metacalled(Value1, Value2, Value3)  :- eq_union(Value1, Value2, Value3).

merge_components(Value1, Value2, Value3)  :- eq_union(Value1, Value2, Value3).

merge_existential(Value, Value, Value) :- !.
merge_existential(_,     _,     ^    ).

merge_msensitive(Value, Value, Value) :- !.
merge_msensitive(_,     _,     :    ).

merge_module(Value, Value, Value) :- !.
merge_module(_,     _,     m    ).

merge_mode(Value, Value, Value) :- !.
merge_mode([],    Value, Value) :- !.
merge_mode(Value, [],    Value) :- !.
merge_mode(_,     _,     ?    ).

merge_bottom(Value, Value, Value) :- !.
merge_bottom(_,     _,     *    ).

%% eq_union(+Set1, +Set2, ?Set3) is det
%
%  Same as lists:union(Set1, Set2, Set3)
%  but uses eq_member/2 instead of 
%  lists:member/2.
eq_union([], L, L) :- !.
eq_union([H|T], L, R) :-
	eq_member(H, L),
	!,
	eq_union(T, L, R).
eq_union([H|T], L, [H|R]) :-
	eq_union(T, L, R).
%% eq_member(+E, +L) is nondet.
%
%  Same as lists:member(E, L) but uses
%  ==/2 as check for membership. 
eq_member(E, L) :-
	member(E2, L),
	E == E2.

%% merge_annotations(+Meta1, +Meta2, -Meta) is det.
%
%  Merge the annotations in the meta/N terms Meta1
%  and Meta2 into the new meta/N term Meta. Aliases
%  are not considered.
merge_annotations(Meta1, Meta2, Meta) :-
	new_meta(Meta),
	get_metacalled( Meta1, Metacalled1) , get_metacalled( Meta2, Metacalled2) , merge_metacalled( Metacalled1,  Metacalled2,  Metacalled) , set_metacalled( Meta, Metacalled),
	get_components( Meta1, Components1) , get_components( Meta2, Components2) , merge_components( Components1,  Components2,  Components) , set_components( Meta, Components),
	get_existential(Meta1, Existential1), get_existential(Meta2, Existential2), merge_existential(Existential1, Existential2, Existential), set_existential(Meta, Existential),
	get_msensitive( Meta1, MSensitive1) , get_msensitive( Meta2, MSensitive2) , merge_msensitive( MSensitive1,  MSensitive2,  MSensitive) , set_msensitive( Meta, MSensitive),
	get_module(     Meta1, Module1)     , get_module(     Meta2, Module2)     , merge_module(     Module1,      Module2,      Module)     , set_module_(    Meta, Module),
	get_mode(       Meta1, Mode1)       , get_mode(       Meta2, Mode2)       , merge_mode(       Mode1,        Mode2,        Mode)       , set_mode(       Meta, Mode),
	get_bottom(     Meta1, Bottom1)     , get_bottom(     Meta2, Bottom2)     , merge_bottom(     Bottom1,      Bottom2,      Bottom)     , set_bottom(     Meta, Bottom).

%% meta_term_head(+Head, -MetaTermHead) is semidet.
%
%  True when MetaTermHead is the same term as Head,
%  but the variables of Head are replaced by their
%  annotated meta/N term and the information carried
%  by the attributed variables in Head contains "real"
%  meta-elements, not just mode information.
meta_term_head(Head, MetaTermHead) :-
	functor(Head, Name, Arity),
	functor(MetaTermHead, Name, Arity),
	meta_args(1, Arity, Head, MetaTermHead, HasMeta),
	HasMeta == true.   % fail if MetaTermHead contains just mode info

meta_args(I, Arity, Head, Meta, HasMeta) :-
	I =< Arity, !,
	arg(I, Head, HeadArg),
	arg(I, Meta, MetaArg),
	meta_term_of_var(HeadArg, MetaArg, HasMeta),
	I2 is I + 1,
	meta_args(I2, Arity, Head, Meta, HasMeta).
meta_args(_, _, _, _, _).

%% meta_term_of_var(+AnnotatedArg, -MetaTerm, -IsMetaArg) is det.
%
%  MetaTerm is bound to the annotated meta/N term of the variable
%  AnnotatedArg. The alias information in the meta/N is removed
%  to avoid unifying of aliases in combine_meta_args/2.
%  If the variable AnnotatedArg is not annotated, MetaTerm is
%  bound to an empty meta/N term.
%  IsMetaArg will be unified to "true" if the MetaTerm is not just
%  mode information (+, -, ?). Otherwise, it is left unbound. 
meta_term_of_var(HeadArg, MetaTerm, IsMetaArg) :-
	get_attr(HeadArg, pmi, MetaTerm),
	!,
	set_aliased(MetaTerm, []),
	(	has_meta_calling_or_component_or_msensitive_annotation(MetaTerm)
	->	IsMetaArg = true
	;	true
	).
meta_term_of_var(HeadArg, MetaTerm, IsMetaArg) :-
	compound(HeadArg),
	HeadArg = M:_,
	get_attr(M, pmi, MMeta),
	get_module(MMeta, m),
	!,
	new_meta(MetaTerm),
	set_msensitive(MetaTerm, :),
	IsMetaArg = true.
meta_term_of_var(_, NewMeta, _) :-
	new_meta(NewMeta).

has_meta_calling_or_component_or_msensitive_annotation(Meta) :-
	(	get_metacalled(Meta, Metacalled),
		member(_, Metacalled)
	;	get_existential(Meta, ^)
	;	get_components(Meta, Components),
		member(_, Components)
	;	get_msensitive(Meta, :)
	),
	!.

remove_list_if_possible([], _) :- !, fail.
remove_list_if_possible([X], X) :- !.
remove_list_if_possible(List, List).

%%	combine_meta_args(+Heads, -Head) is det.
%
%	Combine multiple heads with meta/N terms 
%   from disjunctive branches
combine_meta_args([], []) :- !.
combine_meta_args([List], List) :- !.
combine_meta_args([Spec,Spec|Specs], CombinedArgs) :- !,
	combine_meta_args([Spec|Specs], CombinedArgs).
combine_meta_args([Spec1,Spec2|Specs], CombinedArgs) :-
	Spec1 =.. [Name|Args1],
	Spec2 =.. [Name|Args2],
	maplist(merge_annotations, Args1, Args2, Args),
	Spec =.. [Name|Args],
	combine_meta_args([Spec|Specs], CombinedArgs).

%% meta_terms_to_arg_specs_in_head(+HeadWithMetaTerms, -MetaSpec) is det.
%
%  Translate the head HeadWithMetaTerms with meta/N terms to the 
%  corresponding meta-specification MetaSpec.
meta_terms_to_arg_specs_in_head(HeadWithMetaTerms, MetaSpec) :-
	HeadWithMetaTerms =.. [Name|MetaTerms],
	maplist(meta_term_to_arg_spec, MetaTerms, ArgSpecs),
	MetaSpec =.. [Name|ArgSpecs].

%% meta_term_to_arg_spec(+MetaTerm, -ArgSpec) is det.
%
%  Translate the meta/N term MetaTerm to the corresponding
%  meta argument specifier ArgSpec. ArgSpec is either one
%  specifier or a list of multiple specifiers. The specifier
%  is determined using the following partial order:
%
%  (0..9,database,//)   components
%           |           /
%           ^          /
%            \        /
%             :      /
%              \    /
%               mode
%                 |
%                 *
meta_term_to_arg_spec(MetaTerm, ArgSpec) :-
	get_metacalled_or_existential_or_msensitive(MetaTerm, MetacalledOrExistential),
	get_components(MetaTerm, Components),
	append(MetacalledOrExistential, Components, ArgSpec0),
	sort(ArgSpec0, ArgSpec1),
	remove_list_if_possible(ArgSpec1, ArgSpec),
	!.

meta_term_to_arg_spec(MetaTerm, ArgSpec) :-
	get_mode(MetaTerm, ArgSpec),
	ArgSpec \== [],
	!.

meta_term_to_arg_spec(_, *).

get_metacalled_or_existential_or_msensitive(MetaTerm, Metacalled) :-
	get_metacalled(MetaTerm, Metacalled),
	Metacalled \== [],
	!.
get_metacalled_or_existential_or_msensitive(MetaTerm, [^]) :-
	get_existential(MetaTerm, ^),
	!.
get_metacalled_or_existential_or_msensitive(MetaTerm, [:]) :-
	get_msensitive(MetaTerm, :),
	!.
get_metacalled_or_existential_or_msensitive(_MetaTerm, []).


attr_unify_hook(A0, Other) :-
	writeln(attr_unify_hook(A0, Other)).

