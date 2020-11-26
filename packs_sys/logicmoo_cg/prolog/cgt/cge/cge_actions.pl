/* COPYRIGHT ************************************************************

Conceptual Graph Editor (CGE) - an X-Windows graphical interface to CGT
Copyright (C) 1990 Miguel Alexandre Wermelinger

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

************************************************************************/

/* AUTHOR(S) ************************************************************

Michel Wermelinger
Dept. de Informatica, Univ. Nova de Lisboa, Quinta da Torre
P - 2825 Monte da Caparica, PORTUGAL
Phone: (+351) (1) 295 44 64 ext. 1360  Internet: mw@fct.unl.pt

************************************************************************/

:- use_module(library(cgt/cge/swi_apeal)).
term_expansion(HeadIn,IS,Head,OS):- current_prolog_flag(swi_apeal,true), gui_expansion(HeadIn,Head)->IS=OS.

:- set_prolog_flag(swi_apeal,true).


% 91/01/03 mw   update_linear/1 now updates header too; added update_viewer/1
% 91/02/19 mw   cge_save_gr/1 now updates display

cge :-	shell widget cge(300, 5, 50, _).

cge(DW, MF, LH, E) :- shell widget cge(DW, MF, LH, E).

gen_graphical(type_def, Type, Editor) :-
	concept_type(Type, Label, l/Id, _, _), l(l/Id, CIDs, GID),
	mark, copy_graph(GID, Copy, outer), unmark,
	map(copy_parameter(_, _, GID, Copy), CIDs, Params),
	display_in(Editor, Label, Params, Copy, type_def, Type).
gen_graphical(rel_def, Type, Editor) :-
	relation_type(Type, Label, l/Id, _, _), l(l/Id, CIDs, GID),
	mark, copy_graph(GID, Copy, outer), unmark,
	map(copy_parameter(_, _, GID, Copy), CIDs, Params),
	display_in(Editor, Label, Params, Copy, rel_def, Type).
gen_graphical(schema, LID, Editor) :-
	l(LID, [CID], GID), type(CID, Type), 
	concept_type(Type, Label, _, _, _),
	mark, copy_graph(GID, Copy, outer),  unmark,
	copy_parameter(CID, Param, GID, Copy),
	display_in(Editor, Label, [Param], Copy, schema, LID).
gen_graphical(can_graph, Type, Editor) :-
	( relation_type(Type, Label, _, Can, _)
	; concept_type(Type, Label, _, Can, _)
	), mark, copy_graph(Can, Copy, outer), unmark,
	display_in(Editor, Label, [], Copy, can_graph, Type).
gen_graphical(graph, GID, Editor) :-
	mark, copy_graph(GID, Copy, outer), unmark,
	display_in(Editor, graph, [], Copy, graph, GID).

display_in(Editor, Label, Parameters, GID, Kind, Obj) :-
	( description(Kind, Obj, Header), Marked = [], TmpVar = 0
	; gen_header(Kind, Label, Parameters, Marked, 0, TmpVar, Header, [])
	; Header = '', Marked = [], TmpVar = 0
	),
	recorded(cg_editor, Editor-Title-Graph-_Linear-_, _),
	Graph wproc unmap, 
	display_graph(GID, Graph, Marked, TmpVar, _),
	Graph wproc map,
	update_linear(Editor), replace_text(Title, Header).

display_graph([], _Graph, _, VarIn, VarIn).
display_graph([GID|List], Graph, Marked, VarIn, VarOut) :-
	display_graph(GID, Graph, Marked, VarIn, TmpVar),
	display_graph(List, Graph, Marked, TmpVar, VarOut).
display_graph(GID, Graph, Marked, VarIn, VarOut) :-
	g(GID, CL, RL), dir_reference(CL, RL), 
	display_concept(CL, Graph, Marked, VarIn, VarOut),
	display_relation(RL, Graph),
	recorda(corresponds, GID-Graph, _).

display_concept([], _, _, VarIn, VarIn).
display_concept([CID|List], Graph, Marked, VarIn, VarOut) :-
	display_concept(CID, Graph, Marked, VarIn, TmpVar),
	display_concept(List, Graph, Marked, TmpVar, VarOut).
display_concept(p/Id-_, Graph, _Marked, VarIn, VarOut) :-
	type(p/Id, Type), referent(p/Id, Ref), basic_ref(Ref, '*'),
	Graph widget Context= context([]),
	recorda(corresponds, p/Id-Context, _),
	display_type(Type, Context, VarIn, VarOut).
display_concept(p/Id-_, Graph, Marked, VarIn, VarOut) :-
	type(p/Id, proposition), referent(p/Id, Ref),
	basic_ref(Ref, Basic),		% doesn't handle coreference links
	Graph widget Context= context([]),
	recorda(corresponds, p/Id-Context, _),
	display_ref(Basic, Context, Marked, VarIn, VarOut).
display_concept(CID-_, Graph, Marked, VarIn, VarOut) :-
	type(CID, Type), referent(CID, Ref), 
	basic_ref(Ref, Basic),		% doesn't handle coreference links
	Graph widget Concept= concept([]),
	recorda(corresponds, CID-Concept, _),
	display_type(Type, Concept, VarIn, TmpVar),
	display_ref(Basic, Concept, Marked, TmpVar, VarOut).

display_type(l/Id, Concept, VarIn, VarOut) :-
	l(l/Id, [CID], GIDs),
	succ(VarIn, TmpVar), number2var(VarIn, Var),
	Concept widget typeField(Var, Graph),
	display_graph(GIDs, Graph, [CID+Var], TmpVar, VarOut).
display_type(Type, Concept, VarIn, VarIn) :-
	concept_type(Type, Label, _, _, _),
	Concept widget typeField(Label).

display_ref(Ref, Context, Marked, VarIn, VarOut) :-
	recorded(corresponds, p/_-Context, _),
	Context widget Graph= graphs,
	display_graph(Ref, Graph, Marked, VarIn, VarOut).
display_ref(Ref, Concept, Marked, VarIn, VarIn) :-
	recorded(corresponds, CID-Concept, _),
	member(CID+Var, Marked), reffield(Ref = '*'-Var, L, []),
	Concept widget refField([:|L]).
display_ref('*', _, _, VarIn, VarIn).
display_ref(every, Concept, _Marked, VarIn, VarIn) :-
	Concept widget Ref= refField([:, '"']),
	Ref wset font(symbol).
display_ref(Ref, Concept, _Marked, VarIn, VarIn) :-
	referent(Ref, L, []),
	Concept widget refField([:|L]).
	
display_relation([], _).
display_relation([Rel|List], Graph) :-
	display_relation(Rel, Graph), display_relation(List, Graph).
display_relation(Relation, Graph) :-
	Relation =.. [RelType|Args], relation_type(RelType, Label, _, _, _),
	Graph widget RelWID= relation(Label, []),
	recorda(corresponds, Relation-RelWID, _),
	drawArcs(RelWID, Args).

drawArcs(RelWID, [CID]) :-
	recorded(corresponds, CID-ConWID, _), cge_linkNode(RelWID, ConWID).
drawArcs(RelWID, [CID|T]) :-
	recorded(corresponds, CID-ConWID, _), cge_linkNode(ConWID, RelWID),
	drawArcs(RelWID, T).

cge_linkNode(WID1, WID2) <->
	WID1 wget linkedNodes(L), WID1 wset linkedNodes([WID2|L]).

add_graph(GID, Context) :-
	recorded(cge_context, Context-_/_/Viewer+_Editor, _),
	( Viewer = none
		-> display_ref(GID, Context, [], 0, _)
		 ; display_graph(GID, Viewer, [], 0, _)
	).

change_type(Name, WID) :-
	recorded(cge_concept, WID-Type/_+_, _),
	recorded(cge_type, Type-_/Label/_+_, _),
	Label wset label(Name).

change_ref(Ref, WID) :-
	recorded(cge_concept, WID-_/none+_, _),
	display_ref(Ref, WID, [], 0, _).
change_ref(Ref, WID) :-
	recorded(cge_concept, WID-Type/OldRef+Editor, DbRef),
	OldRef wproc destroy, erase(DbRef), 
	recorda(cge_concept, WID-Type/none+Editor, _),
	display_ref(Ref, WID, [], 0, _). 

cge_move(Node) :-
        Node wproc [drag(X, Y, false, true), move(X, Y)],  % stay inside parent
	Node wset [horizPos(X), vertPos(Y)].

toggle_mode(Mode) :-
	Mode wget label('Auto'),
	recorded(cg_editor, Editor-_-Graph-_-Mode/_/_, _),
	( cge_selected(prim, Editor, viewer, multiple, WIDs)
	; WIDs = Graph
	),
	WIDs wset layoutMode(manual), update_layout_param(Editor).
toggle_mode(Mode) :-
	Mode wget label('Manual'),
	recorded(cg_editor, Editor-_-Graph-_-Mode/_/_, _),
	( cge_selected(prim, Editor, viewer, multiple, WIDs)
	; WIDs = Graph
	),
	WIDs wset layoutMode(automatic), update_layout_param(Editor).

toggle_shadow(Entry, Editor) :-
	recorded(cge_shadow, Editor-Shadow, _),	Entry wget label(L),
	( L = 'Show Miniature'
		-> Shadow wproc map, Entry wset label('Hide Miniature')
		 ; Shadow wproc unmap, Entry wset label('Show Miniature')
	).

cge_style(_Editor, Toggle, _Style) :-
	Toggle wget state(false).
cge_style(Editor, _Toggle, Style) :-
	cge_style(Editor, Style).

cge_style(Editor, Style) :-
	( cge_selected(prim, Editor, viewer, multiple, Selection)
	; recorded(cg_editor, Editor-_-Graph-_-_, _), Selection = Graph
	),
	Selection wset layoutStyle(Style).

cge_layout(Editor, Layout) :-
	( cge_selected(prim, Editor, viewer, multiple, Selection)
	; recorded(cg_editor, Editor-_-Graph-_-_, _), Selection = Graph
	),
	Selection wset graphLayout(Layout).

update_layout_param(Editor) :-
	recorded(cg_editor, Editor-_-Graph-_-Mode/Function/Style, _),
	( cge_selected(prim, Editor, viewer, multiple, Selection)
	; Selection =  [Graph]
	),
	update_layout_mode(Mode, Selection),
	( Mode wget label('Auto') -> Sensitive = true ; Sensitive = false ),
	Function wset sensitive(Sensitive),
	update_layout_function(Function, Selection),
	Style wset sensitive(Sensitive),
	update_layout_style(Style, Selection).

update_layout_mode(Mode, Graphs) :-
	Graphs wgetl layoutMode(L), delete_dup(L, LayoutMode),
	( LayoutMode = [automatic], Mode wset [set(true), label('Auto')]
	; LayoutMode = [manual], Mode wset [set(true), label('Manual')]
	; Graphs = [LastSelected|_], LastSelected wget layoutMode(LastMode),
	LastMode = automatic -> Mode wset [set(false), label('Auto')]
	; Mode wset [set(false), label('Manual')]
	).

update_layout_function([Hier, Spring, Tree], Graphs) :-
	[Hier, Spring, Tree] wset set(false),
	Graphs wgetl graphLayout(F), delete_dup(F, Function),
	( Function = [hierarchical], Hier wset set(true)
	; Function = [spring], Spring wset set(true)
	; Function = [tree], Tree wset set(true)
	; true						% multiple functions
	).

update_layout_style([LR, RL, TD, BU], Graphs) :-
	[LR, RL, TD, BU] wset state(false),
	Graphs wgetl layoutStyle(S), delete_dup(S, Style),
	( Style = [left_right], LR wset state(true)
	; Style = [right_left], RL wset state(true)
	; Style = [top_down], TD wset state(true)
	; Style = [bottom_up], BU wset state(true)
	; true						% multiple styles
	).

replace_text(WID, Text) <->
	WID wproc [get_last_pos(LP), replace(0, LP, Text)],
	WID wset insertPosition(0)
	<= atomic(Text).
replace_text(WID, Text) <->
	WID wproc [get_last_pos(LP), replace(0, LP, ''), stream(OS)],
	current_output(COS), set_output(OS),
	write_linear(0, Text, []), flush_output(OS),
	set_output(COS), close(OS), WID wset insertPosition(0)
	<= is_list(Text).
replace_text(WID, Goal) <->
	WID wproc [get_last_pos(LP), replace(0, LP, ''), stream(OS)],
	current_output(COS), set_output(OS),
	( Goal ; true ), flush_output(OS),
	set_output(COS), close(OS), WID wset insertPosition(0).

/************************************************************************

		S E L E C T I O N   O P E R A T I O N S

************************************************************************/

toggle_sel(Sel, Kind, WID, Graph, Editor) :-
	is_selected(Sel, WID),
	unselect(Sel, Kind, WID, Graph, Editor),
	( Sel = prim -> update_viewer_sel(Graph, Editor) ; true ), !. 
toggle_sel(Sel, Kind, WID, Graph, Editor) :-
	( Sel = prim -> OtherSel = sec ; OtherSel = prim ),
	unselect(OtherSel, Kind, WID, Graph, Editor),
	select(Sel, Kind, WID, Graph, Editor),
	( Sel = prim -> update_viewer_sel(Graph, Editor) ; true ), !.

is_selected(prim, WID) :-
	recorded(cge_selection, _/_-_/WID, _), !.
is_selected(sec, WID) :-
	recorded(cge_sec_sel, _/_-_/WID, _), !.

unselect(prim, Kind, WID, Graph, Editor) :-
	recorded(cge_selection, Graph/Editor-Kind/WID, Ref),
	erase(Ref), cge_turn_sel(prim, off, Kind, WID).
unselect(sec, Kind, WID, Graph, Editor) :-
	recorded(cge_sec_sel, Graph/Editor-Kind/WID, Ref),
	erase(Ref), cge_turn_sel(sec, off, Kind, WID).
unselect(_, _, _, _, _).
 
select(prim, Kind, WID, Graph, Editor) :-
	recorda(cge_selection, Graph/Editor-Kind/WID, _),
	cge_turn_sel(prim, on, Kind, WID), !.
select(sec, Kind, WID, Graph, Editor) :-
	recorda(cge_sec_sel, Graph/Editor-Kind/WID, _),
	cge_turn_sel(sec, on, Kind, WID), !.

cge_turn_sel(prim, on, relation, _).
cge_turn_sel(prim, on, _, WID) :-			% concepts and contexts
	WID wproc unmanage, WID wset borderWidth(2), WID wproc manage.
cge_turn_sel(sec, on, _, WID) :-			% same for all nodes
	WID wproc unmanage, WID wset [borderWidth(2), borderColor(lightGray)],
	WID wproc manage.

cge_turn_sel(prim, off, relation, _).
cge_turn_sel(prim, off, _, WID) :-			% concepts and contexts
	WID wproc unmanage, WID wset borderWidth(1), WID wproc manage.
cge_turn_sel(sec, off, _, WID) :-			% same for all nodes
	WID wproc unmanage, WID wset [borderWidth(1), borderColor(black)],
	WID wproc manage.

update_viewer_sel(Viewer, Editor) :-
	recorded(cge_selection, Viewer/Editor, _),
	recorded(cge_selection, Viewer/Editor-_/_, _).
update_viewer_sel(Viewer, Editor) :-
	recorded(cge_selection, Viewer/Editor, Ref),
	erase(Ref), update_layout_param(Editor).
update_viewer_sel(Viewer, Editor) :-
	recorded(cge_selection, Viewer/Editor-_/_, _),
	recorda(cge_selection, Viewer/Editor, _),
	update_layout_param(Editor).
update_viewer_sel(_, _).

% cge_selected(+Kind, +Editor, +Set, +Cardinality, -Selection)
%		atom    WID	term	atom		list
% Selection is ordered by recentness of selection, i.e., last selected first

cge_selected(Kind, Editor, Set, Card, Selection) :-
	( Kind = prim -> Key = cge_selection ; Key = cge_sec_sel ),
	cge_selected(Key, Editor, Set, Selection), !,
	( Card = multiple -> Selection \= [] ; Selection = [_] ).

cge_selected(Key, Editor, graph, Selection) :-
	findall(GID, 
		( recorded(Key, _/Editor-_/WID, _),
		  cge_which_obj(WID, _, GID, _)
		),
		Tmp),
	delete_dup(Tmp, Selection).
cge_selected(Key, Editor, viewer, Selection) :-
	findall(WID, recorded(Key, WID/Editor, _), Selection).
cge_selected(Key, Editor, all, Selection) :-
	findall(WID, recorded(Key, _/Editor-_/WID, _), Selection).
cge_selected(Key, Editor, only-Kind, _) :-
	recorded(Key, _/Editor-Other/_, _), Other \= Kind, !, fail.
cge_selected(Key, Editor, only-Kind, Selection) :-
	cge_selected(Key, Editor, Kind, Selection).
cge_selected(Key, Editor, Kind, Selection) :-
	findall(WID, recorded(Key, _/Editor-Kind/WID, _), Selection).

unselect_all(Editor) :-
	recorded(cge_selection, Viewer/Editor-Kind/WID, _),
	toggle_sel(prim, Kind, WID, Viewer, Editor), fail.
unselect_all(Editor) :-
	recorded(cge_sec_sel, Viewer/Editor-Kind/WID, _),
	toggle_sel(sec, Kind, WID, Viewer, Editor), fail.
unselect_all(_).

/************************************************************************

			E D I T O R   O P E R A T I O N S

************************************************************************/

cge_open_db(_) :-
	current_editors(Editors), apply(cge_clear_editor(_), Editors),
	( recorded(get_db_modif, true, _)
		-> confirm('Save changes to current database?', Choice),
	   	   ( Choice = yes -> current_db(Canon), save_db(Canon),
				     all_modified(false)
				   ; true
		   )
		 ; true
	),
	ask('Name of database:', Db), name(Canon, Db), !,
	( Canon = '' ; load_db(Canon), all_modified(false) ; true ).

current_editors(Editors) :-
	findall(Ed, recorded(cg_editor, Ed-_-_-_-_, _), Editors).

cge_save_db(_) <->
	current_db(Default),
	ask('Name of database:', Db, Default), name(Canon, Db),
	save_db(Canon),	all_modified(false).

all_modified(Modified) :-
	( Modified -> Action = map ; Action = unmap ),
	recorded(cg_editor, Editor-_-_-_-_, _),
	recorded(cge_modif, Editor-ModWID, _),
	ModWID wproc Action, fail.
all_modified(Modified) :-
	recorded(get_db_modif, Yes, Ref),
	( Yes = Modified ; erase(Ref), recorda(get_db_modif, Modified, _) ).
all_modified(Modified) :-
	recorda(get_db_modif, Modified, _).

cge_clear_editor(Editor) <->
	recorded(cg_editor, Editor-_-Viewer-_-_, _),
	Viewer wproc unmap, clear_graph(GIDs), Viewer wproc map, 
	apply(erasure(_), GIDs),
	update_layout_param(Editor), update_linear(Editor)
	<= cge_selected(prim, Editor, graph, multiple, GIDs), !,
	   recorded(cge_num, Editor-Number, _),
	   confirm(['Do you really want to delete the selected graph(s) ',
		    'in editor #', Number, '?']).
cge_clear_editor(Editor) <->
	recorded(cg_editor, Editor-Header-Viewer-Linear-_, _),
	Viewer wproc unmap, clear_graph(TopGraphs), Viewer wproc map,
	replace_text(Header, ''), replace_text(Linear, ''),
	update_layout_param(Editor), delete_obj(TopGraphs)
	<= top_graphs(Editor, TopGraphs), TopGraphs \= [], !,
	   recorded(cge_num, Editor-Number, _),
	   confirm(['Do you really want to delete the displayed graph(s) ',
		    'in editor #', Number, '?']).
cge_clear_editor(_) <-> true.

cge_help :- acknowledge('Sorry...').

cge_quit(Editor) <->
	cge_clear_editor(Editor),
	( recorded(get_db_modif, Modified, _) ; Modified = false ),
	( Modified
		-> confirm('Save changes to current database?', Choice),
		   ( Choice = yes -> current_db(Canon), save_db(Canon),
				     all_modified(false)
				   ; true
		   )
		 ; true
	),
	Editor wproc destroy.

cge_save_gr(Editor) <->
	recorded(cg_editor, Editor-_-Viewer-Linear-_, _),
	Linear wproc [get_last_pos(LP), get(0, LP, Text)],
	tokenise(Tokens, Text, []), mark, rec_linear(Kind, Obj, Tokens, ['.']),
	unmark, cge_describe(Kind, Obj), 
        top_graphs(Editor, Top), Viewer wproc unmap, 
        clear_graph(Top), delete_obj(Top),
        gen_graphical(Kind, Obj, Editor), Viewer wproc map, all_modified(true).

cge_describe(Kind, Obj) <->
	( retract( description(Kind, Obj, Default) ) ; Default = '' ),
	ask('Description:', Desc, Default), name(Description, Desc),
	( Description = '' ; assert( description(Kind, Obj, Description) ) ).
	
cge_load(Editor) :-
	choice('Load from:', [canonical, definition, description, linear], F),
	( F = linear, !, cge_load_linear(Editor)
	; calc_from(F, Items, Info), 
	  shell widget graphLoader(Loader, List, Items),
	  ( F = description -> C = 1 ; C = 2 ),
	  List wset defaultColumns(C),
	  repeat, next_event(List-Command),
	  load_action(Info, Editor, List, Command, Goal),
	  Loader wproc destroy, !, Goal
	).

calc_from(canonical, Items, Info) :-
	calc_items([rel, con], can, [], Items, [], Info).
calc_from(definition, Items, Info) :-
	calc_items([rel, con], def, [], Items, [], Info).
calc_from(description, Items, Info) :-
	calc_items(gra, _, [], Items, [], Info).

load_action(_, _, _, cancel, fail).
load_action(Info, Editor, List, ok, true) :-
	List wproc show_current(_:I), 
	succ(I, Index),				% because it starts with 0
	nth_member(Kind/Obj, Info, Index), gen_graphical(Kind, Obj, Editor).

calc_items([H|T], K, InItems, OutItems, InInfo, OutInfo) :-
	calc_items(H, K, InItems, TmpItems, InInfo, TmpInfo),
	calc_items(T, K, TmpItems, OutItems, TmpInfo, OutInfo).
calc_items([], _, InItems, InItems, InInfo, InInfo).
calc_items(rel, can, InItems, OutItems, InInfo, OutInfo) :-
	findall(Label/Args+can_graph/Type,
		( relation_type(Type, Label, _, Can, Args), Can \= none ),
		RelCans),
	split_info(RelCans, Items, Info), 
	conc(InItems, Items, OutItems), conc(InInfo, Info, OutInfo).
calc_items(con, can, InItems, OutItems, InInfo, OutInfo) :-
	findall(Label+can_graph/Type,
		( concept_type(Type, Label, _, Can, _), Can \= none ),
		ConCans),
	split_info(ConCans, Items, Info), 
	conc(InItems, Items, OutItems), conc(InInfo, Info, OutInfo).
calc_items(rel, def, InItems, OutItems, InInfo, OutInfo) :-
	findall(Label/Args+rel_def/Type,
		( relation_type(Type, Label, Def, _, Args), Def \= none ),
		RelDefs),
	split_info(RelDefs, Items, Info), 
	conc(InItems, Items, OutItems), conc(InInfo, Info, OutInfo).
calc_items(con, def, InItems, OutItems, InInfo, OutInfo) :-
	findall(Label+type_def/Type,
		( concept_type(Type, Label, Def, _, _), Def \= none ),
		ConDefs),
	split_info(ConDefs, Items, Info), 
	conc(InItems, Items, OutItems), conc(InInfo, Info, OutInfo).
calc_items(con, sch, InItems, OutItems, InInfo, OutInfo) :-
	findall(Desc+schema/Schema,
		( %concept_type(_, _, _, _, SL), member(Schema, SL),
		  description(schema, Schema, Desc) ),
		ConSchemas),
	split_info(ConSchemas, Items, Info), 
	conc(InItems, Items, OutItems), conc(InInfo, Info, OutInfo).
calc_items(gra, _, InItems, OutItems, InInfo, OutInfo) :-
	findall(Desc+graph/Graph,
		description(graph, Graph, Desc),
		Graphs),
	split_info(Graphs, Items, Info), 
	conc(InItems, Items, OutItems), conc(InInfo, Info, OutInfo).

split_info([], [], []).
split_info([A+B|T], [A|T1], [B|T2]) :- split_info(T, T1, T2).

cge_load_linear(Editor) :-
	recorded(cg_editor, Editor-_-_-Linear-_, _),
	Linear wproc [get_last_pos(LP), get(0, LP, Text)],
	tokenise(Tokens, Text, []), mark, rec_linear(Kind, Obj, Tokens, ['.']),
	unmark, gen_graphical(Kind, Obj, Editor).

top_graphs(Editor, Graphs) :-
	recorded(cg_editor, Editor-_-Viewer-_-_, _),
	findall(GID, recorded(corresponds, GID-Viewer, _), Graphs).

update_linear(Editor) :-
	top_graphs(Editor, Graphs),
	recorded(cg_editor, Editor-Header-_-Linear-_, _),
	replace_text(Linear, write_linear(graph, Graphs)),
        replace_text(Header, graph).

cge_which_obj(WID, K/Id, GID, Viewer) :-
	recorded(corresponds, K/Id-WID, _), WID wproc parent(Viewer),
	recorded(corresponds, Graph-Viewer, _), which_graph(K/Id, [Graph], GID).
cge_which_obj(WID, Rel, Graph, Viewer) :-
	recorded(corresponds, Rel-WID, _), WID wproc parent(Viewer),
	recorded(corresponds, Graph-Viewer, _), g(Graph, CL, RL),
	dir_reference(CL, RL), member(Rel, RL). 

/************************************************************************

			N O D E   O P E R A T I O N S

************************************************************************/

cge_restrict_type(WID) :-
	recorded(corresponds, CID-WID, _), type(CID, Type),
	findall(Label, 
		( proper_supertype(Type, T), concept_type(T, Label, _, _, _) ),
		DupNames),
	delete_dup(DupNames, Names), 
	delete_one('ABSURD', Names, SubtypeNames), !,
	( SubtypeNames = [],
	  acknowledge('The chosen concept type has no subtype!'), !, fail
	; true
	),
	choice('Subtypes:', SubtypeNames, Chosen), 
	concept_type(Subtype, Chosen, _, _, _), referent(CID, Ref),
	( conform(Subtype, Ref)
		-> restrict(CID, Subtype, Ref), change_type(Chosen, WID)
		 ; acknowledge('Type and referent don''t conform!')
	).

cge_restrict_ref(WID) :-
	recorded(corresponds, CID-WID, _), referent(CID, '*'), type(CID, Type),
	ask('New referent:', Chars), tokenise(Tokens, Chars, []),
	( referent(NewRef, Tokens, [])
		-> ( conform(Type, NewRef)
			-> restrict(CID, Type, NewRef), 
			   change_ref(NewRef, WID)
			 ; acknowledge('Type and referent don''t conform!')
		   )
		 ; acknowledge('Invalid referent!')
	).
cge_restrict_ref(_) :-
	acknowledge('Referent must be generic!').

cge_max_exp(WID) :-
	cge_which_obj(WID, CID, GID, Viewer),
	clear_graph(GID),
	max_type_expansion(CID, GID, GIDs),
	display_graph(GIDs, Viewer, [], 0, _).

cge_min_exp(WID) :-
	cge_which_obj(WID, CID, GID, Viewer),
	clear_graph(GID),
	min_type_expansion(CID, GID, GIDs),
	display_graph(GIDs, Viewer, [], 0, _).

cge_rel_exp(WID) :-
	cge_which_obj(WID, Rel, GID, Viewer),
	clear_graph(GID),
	rel_expansion(Rel, GID, GIDs),
	display_graph(GIDs, Viewer, [], 0, _).

cge_meas_exp(WID) :-
	cge_which_obj(WID, CID, GID, Viewer),
	referent(CID, Ref), basic_ref(Ref, meas(_)),
	clear_graph(GID), meas_expansion(CID, GID),
	display_graph(GID, Viewer, [], 0, _).
cge_meas_exp(_) :-
	acknowledge('Referent does not denote a measure!').

cge_meas_ctr(WID) :-
	cge_which_obj(WID, meas(X, Y), GID, Viewer),
	clear_graph(GID),
	( meas_contraction(meas(X, Y), GID)
	; acknowledge('Measure contraction failed!')
	),
	display_graph(GID, Viewer, [], 0, _).
cge_meas_ctr(_) :-
	acknowledge('Relation is not MEAS/2!').

cge_name_exp(WID) :-
	cge_which_obj(WID, CID, GID, Viewer),
	referent(CID, Ref), basic_ref(Ref, name(_)),
	clear_graph(GID), name_expansion(CID, GID),
	display_graph(GID, Viewer, [], 0, _).
cge_name_exp(_) :-
	acknowledge('Referent does not denote a name!').

cge_name_ctr(WID) :-
	cge_which_obj(WID, name(X, Y), GID, Viewer),
	clear_graph(GID), 
	( name_contraction(name(X, Y), GID)
	; acknowledge('Name contraction failed!')
	),
	display_graph(GID, Viewer, [], 0, _).
cge_name_ctr(_) :-
	acknowledge('Relation is not NAME/2!').

cge_qty_exp(WID) :-
	cge_which_obj(WID, CID, GID, Viewer),
	referent(CID, Ref), basic_ref(Ref, set(_, _, Card)), nonvar(Card),
	clear_graph(GID), qty_expansion(CID, GID),
	display_graph(GID, Viewer, [], 0, _).
cge_qty_exp(_) :-
	acknowledge('Referent is not a set or has not a number!').

cge_qty_ctr(WID) :-
	cge_which_obj(WID, qty(X, Y), GID, Viewer),
	clear_graph(GID),
	( qty_contraction(qty(X, Y), GID)
	; acknowledge('Quantity contraction failed!')
	),
	display_graph(GID, Viewer, [], 0, _).
cge_qty_ctr(_) :-
	acknowledge('Relation is not QTY/2!').

cge_univ_exp(WID) :-
	cge_which_obj(WID, CID, GID, Viewer),
	referent(CID, Ref), basic_ref(Ref, every),
	clear_graph(GID),
	del_univ_quant(CID, GID, _NewCID, _NewGID, NewGraph),
	display_graph(NewGraph, Viewer, [], 0, _).
cge_univ_exp(_) :-
	acknowledge('Referent is not universally quantified!').

/************************************************************************

			G R A P H   O P E R A T I O N S

************************************************************************/

cge_compare(GID) :-
	which_viewer(GID, Viewer), recorded(cge_graph, Viewer-_+Editor, _),
	check_selection(sec, Editor, graph, single, [GID2]),
	( is_copy(GID, GID2)
		-> Msg = 'a copy of '
		 ; ( is_generalization(GID, GID2)
			-> Msg = 'a generalization of '
			 ; ( is_specialization(GID, GID2)
				-> Msg = 'a specialization of '
				 ; Msg = 'not related to '
			)
		)
	),
	acknowledge(['First graph is ', Msg, 'the second graph.']).

cge_depth(GID) :-
	depth(GID, Depth),
	acknowledge(['Depth of selected graph is ', Depth, '!']).

cge_copy(GID) :-
	which_viewer(GID, Viewer), which_context(GID, Env), 
	iteration(GID, Env, NewGID),
/*	copy_graph(GID, NewGID, Env), 
	( Env = outer ; retractput_graph([NewGID], Env, ) ),
*/	display_graph(NewGID, Viewer, [], 0, _).

cge_join_on([WID1, WID2]) :-
	cge_which_obj(WID1, CID1, GID1, Viewer), 
	cge_which_obj(WID2, CID2, GID2, Viewer),
	( join_concept(CID1, CID2), 
	  clear_graph([GID1, GID2]), join_on(GID1, GID2, [CID1-X], [CID2-X]),
	  %add_graph(GID1, Viewer) 
	  display_graph(GID1, Viewer, [], 0, _)
	; acknowledge('Concepts do not match!')
	).
cge_join_on([_, _]) :-
	acknowledge('Selected concepts must be in the same context!').
cge_join_on(_) :-
	acknowledge('Exactly two concepts must be selected!').

cge_join([GID1, GID2]) :-
	same_context([GID1, GID2], _), which_viewer(GID1, Viewer),
	clear_graph([GID1, GID2]), 
	( join_graph(GID1, GID2, NewGID) ; acknowledge('Join failed!') ),
	display_graph(NewGID, Viewer, [], 0, _).
cge_join([_, _]) :-
	acknowledge('Selected graphs must be in the same context!').
cge_join(_) :-
	acknowledge('Exactly two graphs must be selected!').

cge_max_join([GID1, GID2]) :-
	same_context([GID1, GID2], _), which_viewer(GID1, Viewer),
	clear_graph([GID1, GID2]), 
	( max_join(GID1, GID2, NewGID) ; acknowledge('Maximal join failed!') ),
	display_graph(NewGID, Viewer, [], 0, _).
cge_max_join([_, _]) :-
	acknowledge('Selected graphs must be in the same context!').
cge_max_join(_) :-
	acknowledge('Exactly two graphs must be selected!').

cge_simplify(GID) :-
	which_viewer(GID, Viewer), clear_graph(GID), 
	simplify(GID), display_graph(GID, Viewer, [], 0, _).

cge_erasure(GID) :-
	evenly_enclosed(GID), clear_graph(GID), erasure(GID).
cge_erasure(_) :-
	acknowledge('Selected graph must be evenly enclosed!').

cge_insertion(GID) :-
	which_viewer(GID, Viewer), recorded(cge_graph, Viewer-_+Editor, _),
	check_selection(sec, Editor, context, single, [Context]),
	recorded(corresponds, Env-Context, _),
	( oddly_enclosed(Env), 
	  copy_graph(GID, Copy, outer), insertion(Copy, Env),
	  add_graph(Copy, Context)
	; acknowledge('Context is not oddly enclosed!') 
	).

cge_iteration(GID) :-
	which_viewer(GID, Viewer), recorded(cge_graph, Viewer-_+Editor, _),
	check_selection(sec, Editor, context, single, [Context]),
	recorded(corresponds, Env-Context, _), 
	( check_iteration(GID, Env), iteration(GID, Env, Copy),
	  add_graph(Copy, Context)
	; acknowledge('Context is not dominated by the selected graph!') 
	).

cge_deiteration(GID) :-
	check_deiteration(GID, _, _), which_viewer(GID, Viewer),
        clear_graph(GID), update_viewer(Viewer), deiteration(GID).
cge_deiteration(_) :-
	acknowledge('Selected graph has not a copy in a dominating context!').

%:- style_check(-singleton).

cge_draw_dn(Editor) :-
	cge_selected(prim, Editor, graph, multiple, GIDs),
	same_context(GIDs, Env), which_viewer(GIDs, Viewer), 
        recorded(cg_editor, Editor-_-TopViewer-_-_, _),
        TopViewer wproc unmap, clear_graph(GIDs),	
	double_negation(GIDs, Env, NewGraph),
	display_graph(NewGraph, Viewer, [], 0, _),
        unselect_all(Editor), TopViewer wproc map, update_linear(Editor).
cge_draw_dn(Editor) :-
	cge_selected(prim, Editor, graph, multiple, _),
	acknowledge('Selected graphs must be in the same context!').
cge_draw_dn(Editor) :-
	cge_selected(sec, Editor, only-context, single, [Context]),
	cge_which_obj(Context, Env, _, _Viewer),
	double_negation([], Env, NewGraph), 
	recorded(cg_editor, Editor-_-TopViewer-_-_, _),
        TopViewer wproc unmap, 
        add_graph(NewGraph, Context),
        unselect_all(Editor), TopViewer wproc map, update_linear(Editor).
cge_draw_dn(Editor) :-
	recorded(cg_editor, Editor-_-TopViewer-_-_, _),
	double_negation([], outer, Graph),
	TopViewer wproc unmap, 
	display_graph(Graph, TopViewer, [], 0, _),
        TopViewer wproc map, update_linear(Editor).
	
cge_erase_dn(GID) :-
	is_double_neg(GID), which_context(GID, Env), which_viewer(GID, Viewer),
	clear_graph(GID), double_negation(GID, Env, NewGraphs),
	display_graph(NewGraphs, Viewer, [], 0, _), update_viewer(Viewer).
cge_erase_dn(_) :-
	acknowledge('Selected graph must be a double negation!').

which_viewer([GID|_], Viewer) :-
	which_viewer(GID, Viewer).
which_viewer(GID, Viewer) :-
	recorded(corresponds, GID-Viewer, _).

top_viewer(g/Id, TopViewer) :-
	recorded(corresponds, g/Id-Viewer, _),
	top_viewer(Viewer, TopViewer), !.
top_viewer(Viewer, TopViewer) :-
	recorded(cge_graph, Viewer-_+Editor, _),
	recorded(cg_editor, Editor-_-TopViewer-_-_, _), !.
top_viewer(WID, TopViewer) :-
	WID wproc parent(Viewer),
	top_viewer(Viewer, TopViewer), !.

cge_destroy(Editor) :-
	recorded(cg_editor, Editor-_-_-_-_, Ref1), erase(Ref1),
	recorded(cge_shadow, Editor-_, Ref2), erase(Ref2),
	recorded(cge_modif, Editor-_, Ref3), erase(Ref3)/*,
	( recorded(cg_editor, _, _)
	; recorded(get_db_modif, _, Ref4), erase(Ref4)
	)*/.

clear_graph([]).
clear_graph([GID|List]) :-
	clear_graph(GID), clear_graph(List).
clear_graph(GID) :-
	g(GID, CL, RL), dir_reference(CL, RL), 
	recorded(corresponds, GID-Viewer, Ref), erase(Ref),
	apply(clear_concept(_), CL), apply(clear_relation(_), RL).
        %update_viewer(Viewer).
clear_graph(_).

clear_relation(Rel) :-
	recorded(corresponds, Rel-WID, Ref1), erase(Ref1),
	recorded(cge_relation, WID-_+_, Ref2), erase(Ref2),
	( recorded(cge_selection, _-_/WID, Ref3) -> erase(Ref3) ; true ),
	WID wproc destroy.

clear_concept(c/Id-_) :-
	recorded(corresponds, c/Id-WID, Ref1), erase(Ref1),
	recorded(cge_concept, WID-Type/_+_, Ref2), erase(Ref2),
	( recorded(cge_selection, _-_/WID, Ref3) -> erase(Ref3) ; true ),
	clear_type(Type), WID wproc destroy.
clear_concept(p/Id-_) :-
	recorded(corresponds, p/Id-WID, Ref1), erase(Ref1),
	recorded(cge_context, WID-Type/_/Viewer+_, Ref2), erase(Ref2),
	( recorded(cge_selection, _-_/WID, Ref3) -> erase(Ref3) ; true ),
	clear_type(Type), clear_viewer(Viewer), WID wproc destroy.

clear_type(Type) :- 				% Type is destroyed by parent
	recorded(cge_type, Type-_/_/Viewer+_, Ref), erase(Ref),
	clear_viewer(Viewer).
clear_type(_).

clear_viewer(Viewer) :-				% Viewer is destroyed by parent
	recorded(cge_graph, Viewer-_+_, Ref), erase(Ref),
	findall(GID, recorded(corresponds, GID-Viewer, _), GIDs),
	clear_graph(GIDs).
clear_viewer(_).

update_viewer(Viewer) :-
        recorded(cg_editor, _-_-Viewer-_-_, _), !.
update_viewer(Viewer) :-
        Viewer wproc children([]), clear_viewer(Viewer), Viewer wproc destroy.
update_viewer(_).

cge_action(Apply, Action, Editor, Set, Card) :-
	check_selection(prim, Editor, Set, Card, Selection),
	% cursor wait
	recorded(cg_editor, Editor-_-Viewer-_Linear-_, _), Viewer wproc unmap,
	( Apply = indiv -> Goal =.. [Action, _], (apply(Goal, Selection) ; true)
			 ; Goal =.. [Action, Selection], ( call(Goal) ; true )
	),
	unselect_all(Editor), Viewer wproc map, 
	update_linear(Editor).

check_selection(Kind, Editor, Set, Card, Selection) :-
	cge_selected(Kind, Editor, Set, Card, Selection), !.
check_selection(Kind, _, Set, Card, _) :-
	( Kind = prim -> Sel = 'Primary ' ; Sel = 'Secondary ' ),
	( Set = only-Obj -> Exclusive = 'only ' ; Set = Obj, Exclusive = '' ),
	( Card = single -> Number = 'exactly ' ; Number = 'at least '),
	acknowledge([Sel, 'selection must consist ', Exclusive, 'of ', Number,
		    'one ', Obj, '!']), 
	!, fail.

cge_clear_db :-
	recorded(cge_selection, _, R), erase(R), fail.
cge_clear_db :-
	recorded(cge_sec_sel, _, R), erase(R), fail.
cge_clear_db :-
	recorded(cge_concept, _, R), erase(R), fail.
cge_clear_db :-
	recorded(cge_context, _, R), erase(R), fail.
cge_clear_db :-
	recorded(cge_relation, _, R), erase(R), fail.
cge_clear_db :-
	recorded(corresponds, _, R), erase(R), fail.

% used by sem_int.pl
cge_replace(Editor, Kind, Obj) <->
	recorded(cg_editor, Editor-Header-Viewer-Linear-_, _),
	Viewer wproc unmap, 
	top_graphs(Editor, TopGraphs), clear_graph(TopGraphs), 
	replace_text(Header, ''), replace_text(Linear, ''),
	update_layout_param(Editor), gen_graphical(Kind, Obj, Editor),
	Viewer wproc map.

:- set_prolog_flag(swi_apeal,false).

