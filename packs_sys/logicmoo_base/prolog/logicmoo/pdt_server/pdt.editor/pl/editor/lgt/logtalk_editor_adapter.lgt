%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file is part of the Prolog Development Tool (PDT) for Eclipse
% http://roots.iai.uni-bonn.de/research/pdt
%
% Authors: Günter Kniesel, Paulo Moura (May 2011)
%          partly based on PDT code by Tobias Rho
%
% All rights reserved. This program is  made available under the terms
% of the Eclipse Public License v1.0 which accompanies this distribution,
% and is available at http://www.eclipse.org/legal/epl-v10.html
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- object(logtalk_editor_adapter).

:- uses(utils4entities, [
	source_file_entity/3, entity/1, entity_property/3
]).

:- uses(list, [
	memberchk/2
]).

:- uses(help, [
	built_in_directive/4, built_in_predicate/4, built_in_method/4, built_in_non_terminal/4
]).

:- use_module(pdt_prolog_library(general), [iso_predicate/4]).

:- public(predicates_with_property/3).

%% predicates_with_property(+Property,+FileName,-Predicates) is det.

%predicates_with_property(Property, FileName, Predicates) :-
predicates_with_property(Property, FileName, Predicates) :-
	setof(
		Name,
		Property^predicate_with_property(Property, FileName, Name),
		Predicates
	).

:- private(predicate_with_property/3).

predicate_with_property(built_in, _FileName, Name) :-
	iso_predicate(Name, _, _, _).

predicate_with_property(built_in, _FileName, Name) :-
	logtalk_built_in(Name, _).

predicate_with_property(Property, _FileName, Name) :-
	(	Property = (dynamic)
	;	Property = meta_predicate(_)
	),
	logtalk_built_in(Name, Arity),
	functor(Head, Name, Arity),
	catch(logtalk<<predicate_property(Head, Property),_,fail).
predicate_with_property(Property, FileName, Name) :-
	(	Property = (dynamic)
	;	Property = meta_predicate(_)
	),
	source_file_entity(FileName, _, Entity),
	possibly_visible_predicate(Entity, _NonPrivate, Name, _Arity, Property, []).

:- private(logtalk_built_in/2).

logtalk_built_in(Name, Arity) :-
	built_in_directive(Name, Arity, _, _).
logtalk_built_in(Name, Arity) :-
	built_in_predicate(Name, Arity, _, _).
logtalk_built_in(Name, Arity) :-
	built_in_method(Name, Arity, _, _).
logtalk_built_in(Name, Arity) :-
	built_in_non_terminal(Name, Arity, _, _).


:- private(possibly_visible_predicate/6).
possibly_visible_predicate(Entity, NonPrivate, Name, Arity, Property, _VisitedAncestors) :-
	entity_property(Entity, _, declares(Name/Arity, Properties)),
	memberchk(Property, Properties),
	(	NonPrivate == true
	->	memberchk(scope(Scope), Properties),
		(	Scope == (public)
		;	Scope == (protected)
		)
	;	true
	).
possibly_visible_predicate(Entity, NonPrivate, Name, Arity, Property, _VisitedAncestors) :-
	NonPrivate \== true,
	entity_property(Entity, _, calls(Object::Name/Arity, _)),
	functor(Head, Name, Arity),
	catch(Object::predicate_property(Head, Property), _, fail).
possibly_visible_predicate(Entity, NonPrivate, Name, Arity, Property, _VisitedAncestors) :-
	NonPrivate \== true,
	entity_property(Entity, _, calls(Module:Name/Arity, _)),
	nonvar(Module),
	functor(Head, Name, Arity),
	catch(user::predicate_property(Module:Head, Property), _, fail).
possibly_visible_predicate(Entity, _NonPrivate, Name, Arity, Property, VisitedAncestors) :-
	parent_entity(Entity, ParentEntity),
	\+ memberchk(ParentEntity, VisitedAncestors),
	possibly_visible_predicate(ParentEntity, true, Name, Arity, Property, [ParentEntity|VisitedAncestors]).

:- private(parent_entity/2).
parent_entity(Entity, ParentEntity) :- extends_object(Entity, ParentEntity).
parent_entity(Entity, ParentEntity) :- extends_category(Entity, ParentEntity).
parent_entity(Entity, ParentEntity) :- atomic(Entity), extends_protocol(Entity, ParentEntity).
parent_entity(Entity, ParentEntity) :- imports_category(Entity, ParentEntity).
parent_entity(Entity, ParentEntity) :- implements_protocol(Entity, ParentEntity).
parent_entity(Entity, ParentEntity) :- specializes_class(Entity, ParentEntity).
parent_entity(Entity, ParentEntity) :- instantiates_class(Entity, ParentEntity).

:- end_object.
