:- object(graphml_writer, implements(graph_language_protocol)).
	
	:- public(set_file_name/1).
	set_file_name(FileName) :-
		retractall(file_name_(_)),
		assertz(file_name_(FileName)).
	
	:- private(file_name_/1).
	:- dynamic(file_name_/1).

	:- uses( list, [member/2]).

	:- multifile(graph_language_registry::language_object/2).
	graph_language_registry::language_object(graphml, graphml_writer).

	output_file_name(Name, File) :-
		(	file_name_(File)
		->	true
		;	atom_concat(Name, '.graphml', File)
		).

	file_header(Stream, _Identifier, _Options) :-
	    graphML_api:write_graphML_header(Stream),
	    graphML_api:write_graphML_ast_keys(Stream),
	    graphML_api:start_graph_element(Stream).
%		write(Stream, 'digraph G {\n'),
%		write(Stream, 'rankdir=BT\n'),
%		write(Stream, 'ranksep=1.25\n'),
%		write(Stream, 'compound=true\n'),
%		write(Stream, 'splines=true\n'),
%		write(Stream, 'pack=true\n'),
%		write(Stream, 'clusterrank=local\n'),
%		write(Stream, 'labeljust=l\n'),
%		write(Stream, 'margin=1.0\n'),
%		write(Stream, 'fontname="Courier"\n'),
%		write(Stream, 'fontsize=10\n'),
%		write(Stream, 'fontcolor=snow4\n'),
%		write(Stream, 'pencolor=snow4\n'),
%		write(Stream, 'node [shape=ellipse,style=filled,fillcolor=white,fontname="Courier",fontsize=9]\n'),
%		write(Stream, 'edge [fontname="Courier",fontsize=9]\n'),
%		output_date(Stream, Options),
%		nl(Stream).

	file_footer(Stream, _Identifier, _Options) :-
		graphML_api:close_graph_element(Stream),
    	graphML_api:write_graphML_footer(Stream).

	graph_header(Stream, Identifier, Label, Kind, _Options) :-
		graph_style_margin_color(Kind, Style, Margin, Color),
		graphML_api:open_node(Stream, Identifier),
		graphML_api:write_data(Stream, 'id', Identifier),
		graphML_api:write_data(Stream, 'kind', 'logtalk_group'),
		graphML_api:write_data(Stream, 'node_label', Label),
		format(atom(Styles), 'color=~w;style=~w;margin=~w', [Color, Style, Margin]),
		graphML_api:write_data(Stream, 'styles', Styles),
		graphML_api:start_graph_element(Stream).

	graph_footer(Stream, _Identifier, _Label, _Kind, _Options) :-
		graphML_api:close_graph_element(Stream),
		graphML_api:close_node(Stream).	

	graph_style_margin_color(rlibrary, rounded, 10, snow3).
	graph_style_margin_color(libraries, rounded, 10, snow3).
	graph_style_margin_color(library, rounded, 10, snow2).
	graph_style_margin_color(rdirectory, rounded, 10, snow3).
	graph_style_margin_color(directories, rounded, 10, snow3).
	graph_style_margin_color(directory, rounded, 10, snow2).
	graph_style_margin_color(files, rounded, 10, snow2).
	graph_style_margin_color(file, rounded, 10, snow).
	graph_style_margin_color(external, rounded, 10, white).
	graph_style_margin_color(entity, rounded, 10, snow).

	node(Stream, Identifier, Label, _Caption, Contents, Kind, _Options) :-
		node_shape_style_color(Kind, Shape, Style, Color),
		graphML_api:open_node(Stream, Identifier),
		graphML_api:write_data(Stream, 'id', Identifier),
		graphML_api:write_data(Stream, 'kind', 'logtalk_node'),
		format(atom(Styles), 'shape=~w;style=~w;color=~w', [Shape, Style, Color]),
		graphML_api:write_data(Stream, 'styles', Styles),
		graphML_api:write_data(Stream, 'node_label', Label),
		(	Contents == [] ->
			true
		;	write_lines(Contents, Lines),
			atomic_list_concat(Lines, Content),
			graphML_api:write_data(Stream, 'node_content', Content)
		),
		graphML_api:close_node(Stream).

	node_shape_style_color(Kind, Shape, Style, Color) :-
		node_shape_style_color_(Kind, Shape, Style, Color),
		!.
	
	node_shape_style_color(_, box, filled, white).
	
	% entities belonging to the file or library being documented
	node_shape_style_color_(prototype, box, filled, beige).
	node_shape_style_color_(class, box, filled, yellow).
	node_shape_style_color_(instance, box, filled, yellow).
	node_shape_style_color_(instance_and_class, box, filled, yellow).
	node_shape_style_color_(protocol, note, filled, aquamarine).
	node_shape_style_color_(category, component, filled, cyan).
	node_shape_style_color_(module, tab, filled, gainsboro).
	node_shape_style_color_(file, box, filled, turquoise).

	% external entities to the file or library being documented
	node_shape_style_color_(external_prototype, box, '"filled,dashed"', beige).
	node_shape_style_color_(external_class, box, '"filled,dashed"', yellow).
	node_shape_style_color_(external_instance, box, '"filled,dashed"', yellow).
	node_shape_style_color_(external_instance_and_class, box, '"filled,dashed"', yellow).
	node_shape_style_color_(external_protocol, note, '"filled,dashed"', aquamarine).
	node_shape_style_color_(external_category, component, '"filled,dashed"', cyan).
	node_shape_style_color_(external_module, tab, '"filled,dashed"', gainsboro).
	node_shape_style_color_(external_file, box, '"filled,dashed"', turquoise).

	edge(Stream, Start, End, Labels, Kind, _Options) :-
		edge_arrow(Kind, ArrowHead),
		graphML_api:open_edge(Stream, End, Start),
		graphML_api:write_data(Stream, 'kind', 'logtalk_edge'),
		format(atom(Styles), 'arrowhead=~w', [ArrowHead]),
		graphML_api:write_data(Stream, 'styles', Styles),
		write_lines(Labels, Lines),
		atomic_list_concat(Lines, Label),
		graphML_api:write_data(Stream, 'label', Label),
		graphML_api:close_edge(Stream).

	edge_arrow(Kind, ArrowHead) :-
		edge_arrow_(Kind, ArrowHead),
		!.
	edge_arrow(_, normal).
	
	% entity relations
	edge_arrow_(extends_object, vee).
	edge_arrow_(extends_protocol, vee).
	edge_arrow_(extends_category, vee).
	edge_arrow_(instantiates_class, normal).
	edge_arrow_(specializes_class, onormal).
	edge_arrow_(implements_protocol, dot).
	edge_arrow_(imports_category, box).
	edge_arrow_(complements_object, obox).
	% multifile predicates
	edge_arrow_(provides_clauses, inv).
	% cross-referencing predicate calls
	edge_arrow_(calls_predicate, rdiamond).
	% file relations
	edge_arrow_(depends_on_file, normal).
	edge_arrow_(loads_file, normal).

	write_lines([], []).
	write_lines([Line], ['<![CDATA[', LineAsAtom, ']]>']) :-
		!,
		format(atom(LineAsAtom), '~w', Line).
	write_lines([Line| Lines], ['<![CDATA[', LineAsAtom, ']]>\n'|FLines]) :-
		format(atom(LineAsAtom), '~w', Line),
		write_lines(Lines, FLines).

:- end_object.
