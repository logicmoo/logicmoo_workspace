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

:- module(pages_planner, [make_item_solution_page/3,
			  make_toc_queries_page/2,
			  make_item_query_page/3,
			  make_command_query_page/3]).


:- use_module('swilib/err').

:- use_module(pages_util).
:- use_module(dotgraph).
:- use_module(solution_dotgraph).
:- use_module(obj_task).
:- use_module(task_plan).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Item Solution Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_item_solution_page(KB, Item, Page) :-
	( fact(KB, Item, inf_solutionPlan, Plan) ->
	  true
	; err('Solution ~q has no plan.', [Item])
	),
	rdf_plan_to_dotgraph(KB, Plan, Dotgraph),
	make_dotgraph_image(KB, Dotgraph, ImageFile, Areas),
	once_uri(KB, ImageFile, ImageUri),
	Images = [ element(map, [name=graph], Areas),
		   element(img, [alt='Graph',
				 src=ImageUri,
				 border=0,
				 class='browserDotgraphImage',
				 usemap='#graph'], []) ],

	AVTable = element(table, [width='100%', border=0,
	                          cellspacing=0, cellpadding=0],
		    [element(tr, [],
		      [element(td, [], [CmdTable])]),
		     element(tr, [],
		      [element(td, [], Images)])]),
	
	( fact(KB, Item, inf_solutionConstraints, ConstrItem),
	  fact(KB, ConstrItem, inf_expression, literal(ConstrExpr)),
	  ConstrExpr \= [] ->
	  Lines = ['Constraints'=xml([ConstrExpr])]
	; Lines = []
	),
	
	make_view_refs(KB, solution, Item, ViewRefs),
	( ViewRefs = [] ->
	  Lines1 = Lines
	; Lines1 = [(0-'Other Views'=xml(ViewRefs)) | Lines ]
	),
	make_atvs(KB, Item, [firsteven], Lines1, CmdTable),
	
	make_item_page_general(KB, Item, solution, 'Solution', Page, Params),
	get_param(header_style_class, Params, 'browserHeaderQuery'),
	get_param(footer_style_class, Params, 'browserFooterQuery'),

	get_param(av_table, Params, AVTable),
	check_params(Params).

make_dotgraph_image(_KB, Dotgraph, ImageFile, Areas) :-
	MimeType = 'image/gif',
	process_dotgraph(Dotgraph, MimeType, ImageFile, Areas),
	webserver_register_file_once(ImageFile).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Toc Queries Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_toc_queries_page(KB, Page) :-
	
	make_meta_page_general(KB, queries, 
	                           'Queries', meta, 'Meta', Page, Params),
	get_param(header_style_class, Params, 'browserHeaderQuery'),
	get_param(footer_style_class, Params, 'browserFooterQuery'),

	findall(Q, fact(KB, Q, rdf_type, inf_Query), Queries),
	
	query_rows(KB, Queries, Rows),
	query_table_atts(TAtts),
	AVTable = element(table, TAtts, Rows),
	
	get_param(av_table, Params, AVTable),   
        check_params(Params).

query_table_atts([width='100%', border='0', cellspacing='0', cellpadding='0']).

query_rows(KB, Queries, Refs) :-
	Options = [brief_comment],
	map_item_uri_ref(Queries, KB, query, Options, SortkeyAndRefs),
	sort(SortkeyAndRefs, SortkeyAndRefs1),
	map_value(SortkeyAndRefs1, Refs1),
	make_rows(Refs1, 1, Refs).

map_item_uri_ref([X|Xs], Y1, Y2, Options, [Pretty-Ref|Xs1]) :-
	item_uri_ref(Y1, Y2, X, [sortkey(Pretty)|Options], Ref),
	map_item_uri_ref(Xs, Y1, Y2, Options, Xs1).
map_item_uri_ref([], _, _, _, []).

make_rows([Ref|Refs], I, [Row|Rows]) :-
	( I = 1 -> TRClass = browserOddRow ; TRClass = browserEvenRow ),
	Row = element(tr, [class=TRClass],
		      [element(td, [class=browserStd], Ref)]),
	I1 is I * -1,
	make_rows(Refs, I1, Rows).
make_rows([], _, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Item Query Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% *** TODO: Parameterized queries
%%%% 

make_item_query_page(KB, Query, Page) :-
	Item = Query,
	make_item_page_general(KB, Item, query, 'Query', Page, Params),
	get_param(header_style_class, Params, 'browserHeaderQuery'),
	get_param(footer_style_class, Params, 'browserFooterQuery'),

	find_page_template('submit.html', [Submit], SubmitParams),
	get_param(submit_value, SubmitParams, 'Evaluate Query'),

	findall(param(ParVar, ParComment, ParDefault),
		( fact(KB, Query, inf_parameter, Par),
		  once(fact(KB, Par, inf_parameterVar, literal(ParVar))),
		  ( fact(KB, Par, rdfs_comment, literal(ParComment)) ->
		    true
		  ; true
		  ),
		  ( fact(KB, Par, inf_parameterDefaultValue, ParDefault) ->
		    true
		  ; true
		  )
		),
		Parameters),

	map_make_param_line(Parameters, ParamLines),
	
	Lines =
	  ['Comments'=values(rdfs_comment):text,
	   'Evaluation Timeout'=
	  xml([element(input, [name=timeout, size='5', value='5'], []),
	       ' seconds']),
	   'Number of Solutions'=
	   xml([element(select,
			[name=mode, size='1'],
			[element(option, [value=single,
					  selected=selected], ['Single']),
			 element(option, [value=multi], ['Many'])])])
	  | ParamLines ],
	   
	make_view_refs(KB, query, Item, ViewRefs),
	( ViewRefs = [] ->
	  Lines1 = Lines
	; Lines1 = [(0-'Other Views'=xml(ViewRefs)) | Lines ]
	),
	make_atvs(KB, Item, [], Lines1, AVTable),

	%% *** hidden: this query and parameters
	kb_id(KB, KBId),
	term_to_atom(KBId, KBId1),

	term_to_atom(Item, Item1),
	
	AVForm = element(form, 
			 [action='/command'],
			 [element(input, 
				  [type=hidden, name=type, value=query],
				  []),
			  element(input, 
				  [type=hidden, name=kb, value=KBId1],
				  []),
			  element(input,
				  [type=hidden, name=query, value=Item1],
				  []),
			  AVTable,
			  Submit]),

	get_param(av_table, Params, AVForm),   
        check_params(Params).

map_make_param_line([X|Xs], [X1|Xs1]) :-
	make_param_line(X, X1),
	map_make_param_line(Xs, Xs1).
map_make_param_line([], []).

make_param_line(param(Var, Comment, Default), Line) :-
	( nonvar(Default),
	  ( Default = literal(Default1), atom(Default1) ->
	    atom_length(Default1, Size1), Size is Size1 + 2,
	    Values = [value=Default1]
	  ; atom(Default) ->
	    Size = 30,
	    values = [value=Default]
	  )
	; Size = 30,
	  Values = []
	),
	( atom(Comment) ->
	  Comments = [' ', Comment]
	; Comments = [] %% *** sopport XML/HTML comments...
	),
	atom_concat('param_', Var, Name),
	atom_concat('Parameter: ', Var, PrettyName),
	Line = ( PrettyName=xml([element(input,
				  [name=Name,
				   size=Size | Values], [])
			 | Comments ]) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Command Query Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


make_command_query_page(KB, Form, Page) :-
	formget(query=Query1, Form),
	term_to_atom(Query, Query1),
	formget_default(mode=Mode, Form, single),
	formget_default(timeout=Timeout1, Form, '10'),
	form_extract_params(Form, Params),
	atom_codes(Timeout1, Timeout2),
	catch(number_codes(Timeout, Timeout2), _,
	      err('Specified timeout is not a number: ~q.', [Timeout1])),
	%% *** GATHER THE RULES...
	%% *** FOR NOW: use just all rules of the KB
	findall(Rule,
		( fact(KB, Rule, rdf_type, inf_Rule),
		  ( fact(KB, Query, inf_useRule, Rule) ->
		    true
		  ; \+ fact(KB, _, inf_useRule, Rule)
		  )
		),
		Rules),
	extract_task(KB, Query, Params, Rules, Task, AT),	
	plan_1(KB, Timeout, Mode, Task, AT, KBOut, Solutions),
	make_solutions_page(KBOut, Solutions, Page).
% 
% 	( Solutions = [Solution|_] ->
% 	  make_item_solution_page(KBOut, Solution, Page)
% 	; err('No solution.')
% 	).


form_extract_params([A=V|AVs], [A1=V|AVs1]) :-
	sub_atom(A, 0, B, _, 'param_'),
	!,
	sub_atom(A, B, _, 0, A1),
	form_extract_params(AVs, AVs1).
form_extract_params([_|AVs], AVs1) :-
	form_extract_params(AVs, AVs1).
form_extract_params([], []).

make_solutions_page(KB, Solutions, Page) :-	
	make_meta_page_general(KB, solutions, 
	                           'Solutions', meta, 'Meta', Page, Params),
	get_param(header_style_class, Params, 'browserHeaderQuery'),
	get_param(footer_style_class, Params, 'browserFooterQuery'),
	solution_rows(KB, Solutions, Rows),
	solution_table_atts(TAtts),
	AVTable = element(table, TAtts, Rows),
	get_param(av_table, Params, AVTable),   
        check_params(Params).

solution_table_atts([width='100%', border='0',
		     cellspacing='0', cellpadding='0']).

solution_rows(KB, Solutions, Rows) :-
	Options = [],
	map_item_uri_ref(Solutions, KB, solution, Options, SortkeyAndRefs),
	map_value(SortkeyAndRefs, Refs1),
	map_solution_size(Solutions, KB, Sizes),
	map_add_key(Sizes, Refs1, SizeAndRefs),
	sort(SizeAndRefs, SizeAndRefs1),
	make_sol_rows(SizeAndRefs1, 1, Rows).

make_sol_rows([Size-Ref|Refs], I, [Row|Rows]) :-
	term_to_atom(Size, Size1),
	( I = 1 -> TRClass = browserOddRow ; TRClass = browserEvenRow ),
	Row = element(tr, [class=TRClass],
		      [element(td, [width='70%', class=browserStd], Ref),
		       element(td, [width='25%', class=browserStd],
			       ['Size = ', Size1])]),
	I1 is I * -1,
	make_sol_rows(Refs, I1, Rows).
make_sol_rows([], _, []).

map_add_key([X|Xs], [Y|Ys], [X-Y|Xs1]) :-
	map_add_key(Xs, Ys, Xs1).
map_add_key([], [], []).

map_solution_size([X|Xs], Y, [X1|Xs1]) :-
	solution_size(Y, X, X1),
	map_solution_size(Xs, Y, Xs1).
map_solution_size([], _, []).

solution_size(KB, Solution, Size) :-
	once(fact(KB, Solution, inf_solutionPlan, GoalNode)),
	rdf_plan_to_gplan(KB, GoalNode, gplan(Nodes, _)),
	!,
	length(Nodes, Size1),
	Size is Size1 - 2.
solution_size(_, _, '-').


