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

:- module(obj_inverse,  [ inverse_access_templates/2,
			  plain_to_obj/4,
			  plain_to_obj_fix_vars/3,
			  objs_to_rdf/2 ]).

:- use_module('swilib/term_support').
:- use_module(library(occurs)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Inverse Translation Plain->Obj
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% vars: case1 shared, case2 local (can be removed)

%%%% 
%%%% Fix variables - to be called after plain_to_obj on a list of Objs, that
%%%% should share variables. The Objs themselves should not be vars [?].
%%%% 
%%%% 
%%%% Effect: - only keep vars that appear more than once in Objs-Terms
%%%%         - instantiate those vars to (what?)
%%%% 	       (we do not have object vars here, but might reconstruct them)
%%%%         - scan over sets of Objs to remove properties with unbound values
%%%%           and return this as Objs1.
%%%%         - finally: instantiate remaining vars in Terms
%%%%

plain_to_obj_fix_vars(ObjsSets, Terms, ObjsSets1) :-
	term_variables(ObjsSets-Terms, Vs),
	multiply_occurring_vars(Vs, ObjsSets-Terms, Vs1),
	map_inst_vars(Vs1, 0, N),
	map_map_remove_var_props(ObjsSets, ObjsSets1),
	term_variables(Terms, Vs2),
	map_inst_vars(Vs2, N, _).

map_map_remove_var_props([X|Xs], [X1|Xs1]) :-
	map_remove_var_props(X, X1),
	map_map_remove_var_props(Xs, Xs1).
map_map_remove_var_props([], []).

map_remove_var_props([X|Xs], [X1|Xs1]) :-
	remove_var_props(X, X1),
	map_remove_var_props(Xs, Xs1).
map_remove_var_props([], []).

remove_var_props(obj(T, PVs), obj(T, PVs1)) :-
	!,
	map_rvp_1(PVs, PVs1).
remove_var_props(X, X).

map_rvp_1([_=V|PVs], PVs1) :-
	var(V),
	!,
	map_rvp_1(PVs, PVs1).
map_rvp_1([P=V|PVs], [P=V1|PVs1]) :-
	remove_var_props(V, V1),
	map_rvp_1(PVs, PVs1).
map_rvp_1([], []).

map_inst_vars([V|Vs], N, N1) :-
	varname(N, A),
	make_var_uri(A, V),
	N2 is N + 1,
	map_inst_vars(Vs, N2, N1).
map_inst_vars([], N, N).

%% *** var uris can appear in literal as well as object position?
%%     wrapped for literal position, e.g. <inf:Literal inf:var="Price" />
%%     inf:Literal a subtype of rdfs:Literal
%%     do vars only appear as value of inf_id in non-literal positions?
%%
make_var_uri(Varname, V) :-
	atom_concat('http://www.infraengine.com/planner/var/', Varname, V).
%	V = obj(inf_Literal, [inf_var=A]),

varname(N, A) :-
	C1 is N mod 26 + 65,
	char_code(A1, C1),
	C2 is N // 26,
	( C2 =:= 0 -> A3 = [] ; A3 = [C2] ),
	concat_atom([A1|A3], A).
	
multiply_occurring_vars([V|Vs], Term, [V|Vs1]) :-
	%% This could be implemented more efficiently with a single pass
	%% through Term.
	occurrences_of_var(V, Term, N),
	N > 1,
	!,
	multiply_occurring_vars(Vs, Term, Vs1).
multiply_occurring_vars([_|Vs], Term, Vs1) :-
	multiply_occurring_vars(Vs, Term, Vs1).
multiply_occurring_vars([], _, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plain_to_obj(Plain, _, _, Plain) :-
	var(Plain),
	!.
plain_to_obj(Plain, TA, Bs, Obj) :-
	match_ta(Plain, TA, Type, Accessors),
	!,
	Obj = obj(Type, AVs),
	map_inv_assign_value(Accessors, TA, Bs, AVs).
plain_to_obj(Plain, _, _, Plain).

map_inv_assign_value([A=V|AVs], TA, Bs, [A=V1|AVs1]) :-
	plain_to_obj(V, TA, Bs, V1),
	map_inv_assign_value(AVs, TA, Bs, AVs1).
map_inv_assign_value([], _, _,  []).

match_ta(Plain, TA, Type, Accessors) :-
	member(ta(Template1, Type1, Accessors1), TA),
	subsumeschk(Template1, Plain),
	!,
	copy_term(ta(Template1, Type1, Accessors1),
		  ta(Template, Type, Accessors)),
	unify_with_occurs_check(Template, Plain).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inverse_access_templates(AT, TA) :-
	map_rev_at(AT, TA1),
	%% More special templates should precede more general ones.
	%% ASSUMED: This is achieved through sort and reverse.
	%% Should hold for our representation of subtypes as 1st argument.
	sort(TA1, TA2),
	reverse(TA2, TA).

map_rev_at([X|Xs], [X1|Xs1]) :-
	rev_at(X, X1),
	map_rev_at(Xs, Xs1).
map_rev_at([], []).

%%%% 
%%%% Makes ASSUMPTIONS about implementation of AT in module obj_compiler.
%%%% 
rev_at(at(Type, Accessors, Template), ta(Template, Type, Accessors)).

% *** constrained/unspecified

% plain_to_obj(Plain, AT, Obj) :-
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Inverse Translation Obj->RDF
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

objs_to_rdf(Objs, Triples) :-
	objs_to_rdf_1(Objs, Triples, []).

objs_to_rdf_1([Obj|Objs], Ts, TsRest) :-
	obj_to_rdf(Obj, Ts, Ts1, _),
	objs_to_rdf_1(Objs, Ts1, TsRest).
objs_to_rdf_1([], Ts, Ts).

obj_to_rdf(obj(Type, PVs), [rdf(Ref, rdf_type, Type)|Ts], Ts1, Ref) :-
	%% *** special attribute inf_ref here
	select(inf_ref=Ref, PVs, PVs1), %% *** always an atom?
	!,
	map_pv_to_rdf(PVs1, Ref, Ts, Ts1).
obj_to_rdf(obj(Type, PVs), [rdf(Id, rdf_type, Type)|Ts], Ts1, Id) :-
	!,
	gensym(o, IdSym),
	Id = blank(IdSym),
	map_pv_to_rdf(PVs, Id, Ts, Ts1).
obj_to_rdf(X, Ts, Ts, literal(X)).

map_pv_to_rdf([P=V|PVs], Id, [rdf(Id, P, V1)|Ts], Ts1) :-
	%% *** special handling of inf_id here
	( P = inf_id ->
	  V1 = V,
	  Ts2 = Ts
	; obj_to_rdf(V, Ts, Ts2, V1)
	),
	map_pv_to_rdf(PVs, Id, Ts2, Ts1).
map_pv_to_rdf([], _, Ts, Ts).




