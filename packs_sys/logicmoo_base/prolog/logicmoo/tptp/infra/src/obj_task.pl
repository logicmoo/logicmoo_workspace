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

:- module(obj_task, [extract_task/6,
		     task_goal/2,
		     task_start/2,
		     task_rules/2,
		     task_input_constraints/2,
		     task_action_patterns/2,
		     task_varnames/2]).


:- use_module(obj_rdf).
:- use_module(obj_rdfxml).
:- use_module(obj_compiler).
:- use_module(rdf_convert).
:- use_module(rdf_read).
:- use_module('swilib/err').
:- use_module('swilib/pretty').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%^
%%%% 
%%%% Extract Task
%%%%
%%%%
%%%% extract_task(+KB, +Query, +Rules, -Task, -AT)
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_task(KB, Query, Params, Rules, Task, AT) :-
	msg('Task: Getting task as XML.'),
	query_to_xquery(Query, KB, XQuery),
	map_rule_to_xrule(Rules, KB, XRules),

	%% Type Inference.
	msg('Task: Inferring types.'),	
	map_rdf_xdescs([XQuery|XRules], SynXDescs),
	xml_to_rdf(SynXDescs, SynFacts),
	canonicalize_triples(SynFacts, SynFacts1),

	infer_types(KB, SynFacts1,
		    TypeMap, PropMap, ClassGraph, ClassesAndProps),

	%% Compile to Obj format.
	msg('Task: Translating to Obj intermediate format.'),

	xquery_to_oquery(XQuery, TypeMap, PropMap, OQuery),
	map_xrule_to_orule(XRules, TypeMap, PropMap, ORules),

	%% Compile to syntactically unifiable Prolog terms,
	%% that can be used directly by the planner.
	msg('Task: Translating to Plain intermediate format.'),
	construct_schema(ClassGraph, ClassesAndProps, Schema),

% 	%% Debugging Info:
% 	
% 	fromonto:onto_stream((writeln('DEBUG: Type Map:'),
% 			      pretty:pp(TypeMap),
% 			      nl),
% 			     user_error),
% 
% 	fromonto:onto_stream((writeln('DEBUG: Schema:'),
% 			      pretty:pp(Schema),
% 			      nl),
% 			     user_error),
	
	check_schema(Schema),
	construct_access_templates(Schema, AT),

	convert_params(Params, AT, TypeMap, SynFacts1, Params1),
	
	oquery_to_pquery(OQuery, AT, Params1, PQuery),
	map_orule_to_prule(ORules, AT, PRules),

	%% Convert to input format of the planner.
	ptask_to_task(PQuery, PRules, KB, Task).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Params *** hmmm... rework, put to another place
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% *** CHECK
%%%% Note: If a non-literal object is supplied as a parameter, at least
%%%% one non-blank instance of its class must be declared.
%%%% 

convert_params([A=V|AVs], AT, TypeMap, SynFacts, [A=V1|AVs1]) :-
	memberchk(rdf(S, inf_var, literal(A)), SynFacts),
	memberchk(S-T, TypeMap),
	T \= inf_Literal,
	T \= rdf_Literal,
	!,
	obj_to_plain(obj(T, [inf_id=V]), AT, [], _, V1),
	convert_params(AVs, AT, TypeMap, SynFacts, AVs1).
convert_params([AV|AVs], AT, TypeMap, SynFacts, [AV|AVs1]) :-
	convert_params(AVs, AT, TypeMap, SynFacts, AVs1).
convert_params([], _, _, _, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Retrieving Stored XML
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% The XML is stored as parseType=Literal in the KB. It has already
%%%% passed rdf_read:prepare_xml_rdf/4 (xml:base is resolved,
%%%% meta-attributes (xml:base, xml:lang, xmlns:_ and perhaps others) have
%%%% been removed).
%%%% 

get_xml_once(KB, S, P, O) :-
	fact(KB, S, P, literal(O1)),
	!,
	map_fix_blanks(O1, O).
get_xml_once(_, S, P, _) :-
	err('Failed to get ~q of ~q.', [P, S]).

get_xml_once_optional(KB, S, P, O) :-
	fact(KB, S, P, literal(O1)),
	!,
	map_fix_blanks(O1, O).
get_xml_once_optional(_, _, _, []).	

map_fix_blanks([X|Xs], [X1|Xs1]) :-
	fix_blanks(X, X1),
	map_fix_blanks(Xs, Xs1).
map_fix_blanks([], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% XQuery, XRule
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

query_to_xquery(Query, KB, xquery(Goal, Start, Constrs, Actions)) :-
	get_xml_once(KB, Query, inf_goal, Goal),
	get_xml_once(KB, Query, inf_start, Start),
	get_xml_once_optional(KB, Query, inf_actions, Actions),
	get_xml_once_optional(KB, Query, inf_constrained, Constrs).

rule_to_xrule(Rule, KB, xrule(Action, After, Remaining, Before, Constrs)) :-
	get_xml_once_optional(KB, Rule, inf_action, Action),
	get_xml_once(KB, Rule, inf_after, After),
	get_xml_once_optional(KB, Rule, inf_before, Before),
	get_xml_once_optional(KB, Rule, inf_remaining, Remaining),
	get_xml_once_optional(KB, Rule, inf_constrained, Constrs).

rdf_xdescs( xrule(X1, X2, X3, X4, Cs), XDescs, Rest ) :-
	!,
	map_append([X1, X2, X3, X4, Cs, Rest], XDescs).
rdf_xdescs( xquery(X1, X2, Cs, X3), XDescs, Rest ) :-
	!,
	map_append([X1, X2, X3, Cs, Rest], XDescs).

map_append([L], L) :-
	!.
map_append([L|Ls], L1) :-
	append(L, L2, L1),
	map_append(Ls, L2).
map_append([], []).

map_rdf_xdescs([X|Xs], Es) :-
	rdf_xdescs(X, Es, Es1),
	map_rdf_xdescs(Xs, Es1).
map_rdf_xdescs([], []).

map_rule_to_xrule([X|Xs], Y, [X1|Xs1]) :-
	rule_to_xrule(X, Y, X1),
	map_rule_to_xrule(Xs, Y, Xs1).
map_rule_to_xrule([], _, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% OQuery, ORule
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xquery_to_oquery( xquery(XG, XS, XC, XA), TM, PM, oquery(OG, OS, OC, OA) ) :-
	map_xml_to_obj(XG, TM, PM, OG),
	map_xml_to_obj(XS, TM, PM, OS),
	map_xml_to_obj(XC, TM, PM, OC),
	map_xml_to_obj(XA, TM, PM, OA).

xrule_to_orule( xrule(XA, XP, XR, XB, XC),TM, PM,
		orule(OA, OP, OR, OB, OC) ) :-
	map_xml_to_obj(XA, TM, PM, OA),
	map_xml_to_obj(XP, TM, PM, OP),
	map_xml_to_obj(XR, TM, PM, OR),
	map_xml_to_obj(XB, TM, PM, OB),
	map_xml_to_obj(XC, TM, PM, OC).

map_xml_to_obj([X|Xs], Y1, Y2, [X1|Xs1]) :-
	xml_to_obj(X, Y1, Y2, X1),
	map_xml_to_obj(Xs, Y1, Y2, Xs1).
map_xml_to_obj([], _, _, []).

map_xrule_to_orule([X|Xs], Y1, Y2, [X1|Xs1]) :-
	xrule_to_orule(X, Y1, Y2, X1),
	map_xrule_to_orule(Xs, Y1, Y2, Xs1).
map_xrule_to_orule([], _, _, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% PQuery, PRule
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Note that o_to_p may fail for inconsistent rules and queries.
%%%% Inconsistency may be the result of preprocessing steps and
%%%% not errors.
%%%% 

oquery_to_pquery( oquery(XG, XS, XC, XA), AT, Params,
		  pquery(PG, PS, PC, PA) ) :-
	OT0 = Params,
	map_obj_to_plain(XG, AT, OT0, OT1, PG),
	map_obj_to_plain(XS, AT, OT1, OT2, PS),
	map_obj_to_plain(XC, AT, OT2, _, PC),
	map_obj_to_plain_1(XA, AT, PA).

orule_to_prule( orule(XA, XP, XR, XB, XC), AT, prule(PA, PP, PR, PB, PC) ) :-
	map_obj_to_plain(XA, AT, [], OT1, PA),
	map_obj_to_plain(XP, AT, OT1, OT2, PP),
	map_obj_to_plain(XR, AT, OT2, OT3, PR),
	map_obj_to_plain(XB, AT, OT3, OT4, PB),
	map_obj_to_plain(XC, AT, OT4, _, PC).

map_orule_to_prule([X|Xs], Y1, Xs1) :-
	( orule_to_prule(X, Y1, X1) ->
	  Xs1 = [X1|Xs2]
	; Xs2 = Xs1
	),
	map_orule_to_prule(Xs, Y1, Xs2).
map_orule_to_prule([], _, []).

%%%% 
%%%% Standardize each toplevel obj apart.
%%%% 	
map_obj_to_plain_1([X|Xs], Y1, [X1|Xs1]) :-
	obj_to_plain(X, Y1, [], _, X1),
	map_obj_to_plain_1(Xs, Y1, Xs1).
map_obj_to_plain_1([], _, []).

map_obj_to_plain([X|Xs], Y1, OT, OT1, [X1|Xs1]) :-
	obj_to_plain(X, Y1, OT, OT2, X1),
	map_obj_to_plain(Xs, Y1, OT2, OT1, Xs1).
map_obj_to_plain([], _, OT, OT, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Task
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

task_goal(Task, X) :- arg(1, Task, X).
task_start(Task, X) :- arg(2, Task, X).
task_rules(Task, X) :- arg(3, Task, X).
task_input_constraints(Task, X) :- arg(4, Task, X).
task_action_patterns(Task, X) :- arg(5, Task, X).
task_varnames(Task, X) :- arg(6, Task, X).

make_task(Task) :-
	functor(Task, task, 6).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% PTask to Task
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ptask_to_task(PQuery, PRules, KB, Task) :-
	make_task(Task),
	task_goal(Task, Goal),
	task_start(Task, Start),
	task_rules(Task, Rules),
	task_input_constraints(Task, InputConstraints),
	task_action_patterns(Task, ActionPatterns),
	PQuery = pquery(Goal, Start, InputConstraints1, ActionPatterns),
	pcs_to_cs(InputConstraints1, KB, InputConstraints),
	map_prule_to_rule(PRules, KB, Rules).
	
prule_to_rule(prule(PA, PP, PR, PB, PC), KB, rule(A, P, B, C)) :-
	( PA = [] ->
	  A = '$empty'
	; PA = [A] ->
	  true
	; err('Multiple actions specified for rule: ~q.', [PA])
	),
	append(PP, PR, P),
	append(PB, PR, B),
	pcs_to_cs(PC, KB, C).

map_prule_to_rule([X|Xs], KB, [X1|Xs1]) :-
	prule_to_rule(X, KB, X1),
	map_prule_to_rule(Xs, KB, Xs1).
map_prule_to_rule([], _, []).

pcs_to_cs(PCs, KB, Cs) :-
	pcs_to_cs_1(PCs, KB, [], Cs1),
	( Cs1 = [] ->
	  Cs = []
	; Cs = [cs(Cs1)]
	).


pcs_to_cs_1([fact(S,P,O)|PCs], KB, Bs, Cs) :-
	!,
	Cs = [call(knowledgebase:fact(KB,S,P,O))|Cs1],
	pcs_to_cs_1(PCs, KB, Bs, Cs1).
pcs_to_cs_1([not_fact(S,P,O)|PCs], KB, Bs, Cs) :-
	!,
	Cs = [call(\+ knowledgebase:fact(KB,S,P,O))|Cs1],
	pcs_to_cs_1(PCs, KB, Bs, Cs1).
pcs_to_cs_1([arithmetic(E)|PCs], KB, Bs, Cs) :-
	!,
	term_variables(E, EVs),
	copy_term(E-EVs, E1-EVs1),
	an_vars(EVs, EVs1, Bs, Bs1, ANs),
	append(ANs, [E1|Cs1], Cs),
	pcs_to_cs_1(PCs, KB, Bs1, Cs1).
pcs_to_cs_1([true|PCs], KB, Bs, Cs) :-
	!,
	pcs_to_cs_1(PCs, KB, Bs, Cs).
pcs_to_cs_1([output(X)|PCs], KB, Bs, [output(X)|Cs]) :-
	!,
	pcs_to_cs_1(PCs, KB, Bs, Cs).
pcs_to_cs_1([call(Call)|PCs], KB, Bs, [call(Call)|Cs]) :-
	permitted_constraint_call(Call),
	!,
	pcs_to_cs_1(PCs, KB, Bs, Cs).
pcs_to_cs_1([PC|_], _, _, _) :-
	err('Can not handle constraint: ~q.', [PC]).
pcs_to_cs_1([], _, _, []).

% *** does not work this way for objects
permitted_constraint_call(gensym(_, _)).

an_vars([V|Vs], [V1|Vs1], Bs, Bs1, ANs) :-
	( member(V2=V3, Bs), V2 == V ->
	  unify_with_occurs_check(V3, V1),
	  ANs1 = ANs,
	  Bs2 = Bs
	; ANs = [atom_number(V, V1)|ANs1],
	  Bs2 = [V=V1|Bs]
	),
	an_vars(Vs, Vs1, Bs2, Bs1, ANs1).
an_vars([], [], Bs, Bs, []).
