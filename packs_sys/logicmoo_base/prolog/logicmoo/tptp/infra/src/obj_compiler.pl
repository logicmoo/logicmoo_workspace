/*
* Copyright (C) 2002, 2007 Christoph Wernhard
* 
* This program is free software; you can redistribute it and/or modify it
* under the terms of the GNU General Public License as published by the Free
* Software Foundation; either version 2 of the License, or (at your option)
* any later version.
* 
* This program is distributed in the hope that it will be useful, but WITHOUT
* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
* more details.
* 
* You should have received a copy of the GNU General Public License along with
* this program; if not, see <http://www.gnu.org/licenses/>.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Obj Compiler
%%%% 
%%%% 
%%%% Compiler from the Obj format into syntactically unifiable Prolog terms.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(obj_compiler, [obj_to_plain/5,
			 construct_schema/3,
			 construct_access_templates/2,
			 check_schema/1]).


:- use_module('swilib/err').
:- use_module('graphs_util').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Utilities
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_key([X-_|Xs], [X|Xs1]) :-
	map_key(Xs, Xs1).
map_key([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Compiler
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%
%%%% obj_to_plain might fail for inconsistent objects.
%%%%

obj_to_plain(obj(inf_ArithmeticConstraint, PVs), _, OT, OT1, Plain) :-
	!,
	( memberchk(inf_expression=E, PVs) ->
	  atom_to_term(E, E1, VarNames),
	  map_bind_vars(VarNames, OT, OT1),
	  Plain = arithmetic(E1)
	; Plain = true,
	  OT1 = OT
	).
obj_to_plain(obj(inf_OutputConstraint, PVs), _, OT, OT1, Plain) :-
	!,
	( memberchk(inf_expression=E, PVs) ->
	  atom_to_term(E, E1, VarNames),
	  map_bind_vars(VarNames, OT, OT1),
	  Plain = output(E1)
	; Plain = true,
	  OT1 = OT
	).
obj_to_plain(obj(inf_PrologConstraint, PVs), _, OT, OT1, Plain) :-
	!,
	( memberchk(inf_expression=E, PVs) ->
	  atom_to_term(E, E1, VarNames),
	  map_bind_vars(VarNames, OT, OT1),
	  Plain = call(E1)
	; Plain = true,
	  OT1 = OT
	).
obj_to_plain(obj(inf_Literal, PVs), _, OT, OT1, Plain) :-
	!,
	( memberchk(inf_var=Var, PVs) ->
	  ( memberchk(Var=Plain, OT) ->
	    OT1 = OT
	  ; OT1 = [Var=Plain|OT]
	  )
	; OT1 = OT
	),
	( memberchk(inf_value=Value, PVs) ->
	  unify_with_occurs_check(Plain, Value)
	; true
	).
obj_to_plain(obj(Type, PVs), AT, OT, OT1, Plain) :-
	( select(inf_var=Var, PVs, PVs1) ->
	  ( memberchk(Var=Plain, OT) ->
	    OT1 = OT 
          ; obj_to_plain_1(obj(Type, PVs1), AT, [Var=Plain|OT], OT1, Plain)
	  )
	; obj_to_plain_1(obj(Type, PVs), AT, OT, OT1, Plain)
	),
	!.
obj_to_plain(Literal, _, OT, OT, Literal) :-
	atom(Literal),
	!.
obj_to_plain(X, _, _, _, _) :-
	err('Bad obj: ~q.', [X]).

obj_to_plain_1(obj(Type, PVs), AT, OT, OT1, Plain) :-
	!,
	access_template(AT, Type, Accessor, Plain),
	assign_values(PVs, AT, OT, OT1, Accessor, Type).

assign_values([P=V|PVs], AT, OT, OT1, Accessor, Type) :-
	access(P, Accessor, V1, Type, V),
	obj_to_plain(V, AT, OT, OT2, V2),
	unify_with_occurs_check(V1, V2),
	assign_values(PVs, AT, OT2, OT1, Accessor, Type).
assign_values([], _, OT, OT, _, _).

map_bind_vars([Name=Var|NVs], OT, OT1) :-
	memberchk(Name=Val, OT),
	!,
	unify_with_occurs_check(Var, Val),
	map_bind_vars(NVs, OT, OT1).
map_bind_vars([Name=Var|NVs], OT, OT1) :-
	map_bind_vars(NVs, [Name=Var|OT], OT1).
map_bind_vars([], OT, OT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Access Template Table
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%
%%%% Return fresh template (i.e. object of given class) with accessor.
%%%%
%%%% access_template(+AT, +Class, -Accessor, -Template)
%%%% 
access_template(AT, Class, Accessor, Template) :-
	memberchk(at(Class, Accessor1, Template1), AT),
	copy_term(Accessor1-Template1, Accessor-Template).


%%%% 
%%%% Compile-time object access via accessor. 
%%%%
access(P, Accessor, Value, _, _) :-
	memberchk(P=Value, Accessor),
	!.
access(P, Accessor, _, Type, V) :-
	err('Access of property "~q" that is not in schema "~q" of type "~q"; raw value is: "~q".',
	    [P, Accessor, Type, V]).

%%%% 
%%%% construct_access_templates(+Schema, -AT)
%%%% 
%%%% Construct access template table for give schema.
%%%% 
construct_access_templates(Schema, AT) :-
	schema_classgraph(Schema, Classgraph),
	schema_classds(Schema, Classds),
	cat1(Classds, Classgraph, [], AT).

cat1([], _, AT, AT) :-
	!.
cat1(Classds, Classgraph, AT, AT1) :-
	select(Classd, Classds, Classds1),
	classd_id(Classd, Id),
	\+ memberchk(Id-_, Classgraph),
	!,
	cat2(Classd, Classds1, Classds2, Classgraph, [], T-T, AT, AT2),
	cat1(Classds2, Classgraph, AT2, AT1).
cat1(Classds, _, _, _) :-
	err('Bad class graph structure, involving: ~q.', [Classds]).

cat2(Classd, Classds, Classds1,
     Classgraph, SuperAccessor, SuperTemplate, AT, AT1) :-
	classd_id(Classd, CId),
	classd_properties(Classd, Propds),
	direct_propds(Propds, SuperAccessor, DirectPropds),
	map_propd_accessor(DirectPropds, DirectAccessor, PArgs),
	( setof(ScId, member(ScId-CId, Classgraph), SubclassIds) ->
          DArgs = [SubclassTemplate|PArgs],
	  NStart = 2
	; SubclassIds = [],
	  DArgs = PArgs,
	  NStart = 1
	),

	class_functor(CId, DirectFunctor),
	DirectTemplate =.. [DirectFunctor|DArgs],

	copy_term(SuperTemplate-SuperAccessor,
		  (Template-DirectTemplate)-SuperAccessor1),
	append(SuperAccessor1, DirectAccessor, Accessor),
        AT1 = [at(CId, Accessor, Template)|AT2],
	map_cat2(SubclassIds, Classds, Classds1,
		 Classgraph, Accessor, Template-SubclassTemplate,
		 AT, AT2).

map_cat2([], Classds, Classds, _, _, _, AT, AT) :-
	!.
map_cat2([ClassId|ClassIds], Classds, Classds1,
	 Classgraph, SuperAccessor, SuperTemplate,
	 AT, AT1) :-
	select(Classd, Classds, Classds2),
	classd_id(Classd, ClassId),
	!,
	cat2(Classd, Classds2, Classds3,
	     Classgraph, SuperAccessor, SuperTemplate,
	     AT, AT2),
	map_cat2(ClassIds, Classds3, Classds1,
		 Classgraph, SuperAccessor, SuperTemplate,
		 AT2, AT1).
map_cat2(ClassIds, _, _, _, _, _, _, _) :-
	err('Bad class graph structure, involving: ~q.', [ClassIds]).

direct_propds([Propd|Ps], SuperAccessor, Ps1) :-
	propertyd_id(Propd, Id),
	memberchk(Id=_, SuperAccessor),
	!,
	direct_propds(Ps, SuperAccessor, Ps1).
direct_propds([Propd|Ps], SuperAccessor, [Propd|Ps1]) :-
	direct_propds(Ps, SuperAccessor, Ps1).
direct_propds([], _, []).

map_propd_accessor([Propd|Xs], [P=V|Xs1], [V|Vs]) :-
	propertyd_id(Propd, P),	
	map_propd_accessor(Xs, Xs1, Vs).
map_propd_accessor([], [], []).
	
class_functor(Id, CF) :-
	atom_concat('class_', Id, CF).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Construct Schema
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% construct_schema(+KB, +ClassGraph, +ClassesAndProps, -Schema)
%%%% 
%%%% ClassGraph and ClassesAndProps are returned by infer_types.
%%%% 
construct_schema(ClassGraph, ClassesAndProps, Schema) :-
	make_schema(Schema),
	schema_options(Schema, []),
	schema_classds(Schema, Classds),
	schema_classgraph(Schema, ClassGraph),
	map_construct_classd(ClassesAndProps, Classds).

construct_classd(Id-PropIDs, Classd) :-
	make_classd(Classd),
	classd_id(Classd, Id),
	classd_options(Classd, []),
	classd_properties(Classd, Propertyds),
	map_construct_propertyd(PropIDs, Propertyds).

construct_propertyd(Id, Propertyd) :-
	make_propertyd(Propertyd),
	propertyd_id(Propertyd, Id),
	propertyd_options(Propertyd, []).

map_construct_classd([X|Xs], [X1|Xs1]) :-
	construct_classd(X, X1),
	map_construct_classd(Xs, Xs1).
map_construct_classd([], []).

map_construct_propertyd([X|Xs], [X1|Xs1]) :-
	construct_propertyd(X, X1),
	map_construct_propertyd(Xs, Xs1).
map_construct_propertyd([], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Check Schema
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_schema(Schema) :-
	schema_classgraph(Schema, Classgraph),
	( member(C-C1, Classgraph),
	  memberchk(C-C2, Classgraph),
	  C1 \= C1 ->
	  err('Class ~q has multiple direct superclasses: ~q and ~q.',
	      [C, C1, C2])
	; true
	).

%%%% 	
%%%% Schema:
%%%%
%%%% - must be single inheritance
%%%% - a unique top class may or may not exist
%%%% - properties specified for subclasses may or may not include
%%%%   inherited properties
%%%% 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Schema Structure
%%%%
%%%% Only accessed via these predicates, so for now we can choose a
%%%% easy-to-read term structure.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_schema(schema(_, _)).

schema_classds(schema(X, _), X).
schema_classgraph(schema(_, X), X).
schema_options(_, []).

make_classd(_-_).

classd_id(X-_, X).
classd_properties(_-X, X).
classd_options(_, []).

make_propertyd(_).

propertyd_id(X, X).
propertyd_options(_, []).

