:- object(lgt_to_graphML).
	
	:- uses( list, [member/2]).

	:- private(included_entity_/1).
	:- dynamic(included_entity_/1).

	:- private(referenced_entity_/1).
	:- dynamic(referenced_entity_/1).

	remember_referenced_entity(Entity) :-
		(	referenced_entity_(Entity) ->
			true
		;	assertz(referenced_entity_(Entity))
		).

	:- public(write_logtalk_entity_facts_to_graphML/3).
	write_logtalk_entity_facts_to_graphML(_ProjectPath, ProjectFilePaths, OutStream) :-
		reset_external_entities,
		member(File, ProjectFilePaths),
		{'$lgt_file_name'(logtalk, File, Directory, Basename, _SourceFile)},
		process(Basename, Directory, File, OutStream),
		fail.
	write_logtalk_entity_facts_to_graphML(_ProjectPath, _ProjectFilePaths, OutStream) :-
		output_external_entities(OutStream).
	write_logtalk_entity_facts_to_graphML(_,_,_).
	
	reset_external_entities :-
		retractall(included_entity_(_)),
		retractall(referenced_entity_(_)).		

	output_external_entities(_) :-
		retract(included_entity_(Entity)),
		retractall(referenced_entity_(Entity)),
		fail.
	output_external_entities(OutStream) :-
		retract(referenced_entity_(Entity)),
		utils4entities::entity_property(Entity, _, file(FileName, Directory)),
		atom_concat(Directory, FileName, File),
		(	current_object(Entity) ->
			print_name(object, Entity, Name),
			object_type(Entity, Type),
			graphML_api:write_file_as_element(OutStream, Name, File, Name, Type, [], [], Type)
		;	current_protocol(Entity) ->
			print_name(protocol, Entity, Name),
			graphML_api:write_file_as_element(OutStream, Name, File, Name, protocol, [], [], protocol)
		;	print_name(category, Entity, Name),
			graphML_api:write_file_as_element(OutStream, Name, File, Name, category, [], [], category)
		),
		fail.

	process(File, Path, FullPath, OutStream) :-
		protocol_property(Protocol, file(File, Path)),
		output_protocol(Protocol, FullPath, OutStream),
		assertz(included_entity_(Protocol)),
		fail.
	process(File, Path, FullPath, OutStream) :-
		object_property(Object, file(File, Path)),
		output_object(Object, FullPath, OutStream),
		assertz(included_entity_(Object)),
		fail.
	process(File, Path, FullPath, OutStream) :-
		category_property(Category, file(File, Path)),
		output_category(Category, FullPath, OutStream),
		assertz(included_entity_(Category)),
		fail.
	process(_, _, _).
	
	output_protocol(Protocol, File, OutStream) :-
		print_name(protocol, Protocol, Name),
		protocol_property(Protocol, public(Predicates)),
		exports_classification(Predicates, Protocol, Statics, Dynamics),
		graphML_api:write_file_as_element(OutStream, Name, File, Name, protocol, Statics, Dynamics, protocol),
		output_protocol_relations(Protocol, OutStream).
	
	output_object(Object, File, OutStream) :-
		print_name(object, Object, Name),
		object_property(Object, public(Predicates)),
		exports_classification(Predicates, Object, Statics, Dynamics),
		object_type(Object, Type),
		graphML_api:write_file_as_element(OutStream, Name, File, Name, Type, Statics, Dynamics, Type),
		output_object_relations(Object, OutStream).
	
	object_type(Object, Type) :-
		(	instantiates_class(Object, _)
		->	(	specializes_class(Object, _)
			-> 	Type = 'instance, class'
			;	Type = 'instance'
			)
		;	(	specializes_class(Object, _)
			-> 	Type = 'class'
			;	Type = 'prototype'
			)
		).
	
	output_category(Category, File, OutStream) :-
		print_name(category, Category, Name),
		category_property(Category, public(Predicates)),
		exports_classification(Predicates, Category, Statics, Dynamics),
		graphML_api:write_file_as_element(OutStream, Name, File, Name, category, Statics, Dynamics, category),
		output_category_relations(Category, OutStream).
	
	output_protocol_relations(Protocol, OutStream) :-
		extends_protocol(Protocol, ExtendedProtocol),
		print_name(protocol, Protocol, ProtocolName),
		print_name(protocol, ExtendedProtocol, ExtendedProtocolName),
		graphML_api:write_load_edge(OutStream, ProtocolName, ExtendedProtocolName, [], extends),
		remember_referenced_entity(ExtendedProtocol),
		fail.
	output_protocol_relations(_, _).
	
	output_object_relations(Object, OutStream) :-
		implements_protocol(Object, Protocol),
		print_name(object, Object, ObjectName),
		print_name(protocol, Protocol, ProtocolName),
		graphML_api:write_load_edge(OutStream, ObjectName, ProtocolName, [], implements),
		remember_referenced_entity(Protocol),
		fail.
	output_object_relations(Instance, OutStream) :-
		instantiates_class(Instance, Class),
		print_name(object, Instance, InstanceName),
		print_name(object, Class, ClassName),
		graphML_api:write_load_edge(OutStream, InstanceName, ClassName, [], instantiates),
		remember_referenced_entity(Class),
		fail.
	output_object_relations(Class, OutStream) :-
		specializes_class(Class, SuperClass),
		print_name(object, Class, ClassName),
		print_name(object, SuperClass, SuperClassName),
		graphML_api:write_load_edge(OutStream, ClassName, SuperClassName, [], specializes),
		remember_referenced_entity(SuperClass),
		fail.
	output_object_relations(Prototype, OutStream) :-
		extends_object(Prototype, Parent),
		print_name(object, Prototype, PrototypeName),
		print_name(object, Parent, ParentName),
		graphML_api:write_load_edge(OutStream, PrototypeName, ParentName, [], extends),
		remember_referenced_entity(Parent),
		fail.
	output_object_relations(Object, OutStream) :-
		imports_category(Object, Category),
		print_name(object, Object, ObjectName),
		print_name(category, Category, CategoryName),
		graphML_api:write_load_edge(OutStream, ObjectName, CategoryName, [], imports),
		remember_referenced_entity(Category),
		fail.
	output_object_relations(_, _).
	
	output_category_relations(Category, OutStream) :-
		extends_category(Category, ExtendedCategory),
		print_name(category, Category, CategoryName),
		print_name(category, ExtendedCategory, ExtendedCategoryName),
		graphML_api:write_load_edge(OutStream, CategoryName, ExtendedCategoryName, [], extends),
		remember_referenced_entity(ExtendedCategory),
		fail.
	output_category_relations(Category, OutStream) :-
		implements_protocol(Category, Protocol),
		print_name(category, Category, CategoryName),
		print_name(protocol, Protocol, ProtocolName),
		graphML_api:write_load_edge(OutStream, CategoryName, ProtocolName, [], implements),
		remember_referenced_entity(Protocol),
		fail.
	output_category_relations(Category, OutStream) :-
		complements_object(Category, Object),
		print_name(category, Category, CategoryName),
		print_name(object, Object, ObjectName),
		graphML_api:write_load_edge(OutStream, ObjectName, CategoryName, [], complements),
		remember_referenced_entity(Object),
		fail.
	output_category_relations(_, _).
	
	exports_classification([], _, [], []) :- !.
	exports_classification([Name/Arity|Tail], Entity, S, [Name/Arity|DTail]) :-
	    functor(H, Name, Arity),
	    catch(logtalk_adapter<<decode(H, Entity, _, _, _, _, Properties, declaration, _), _, fail),
	    member(dynamic, Properties),
	    !,
		exports_classification(Tail, Entity, S, DTail).
	exports_classification([E|Tail], Entity, [E|STail], D) :-
	    exports_classification(Tail, Entity, STail, D).
	
	print_name(object, Object, ObjectName) :-
		(	atom(Object) ->
			ObjectName = Object
		;	(	object_property(Object, info(Info)) ->
				parameter_names(Object, Info, Names)
			;	parameter_names(Object, [], Names)
			),
			Object =.. [Functor| _],
			ObjectName =.. [Functor| Names]
		).
	print_name(protocol, Protocol, Protocol).
	print_name(category, Category, CategoryName) :-
		(	atom(Category) ->
			CategoryName = Category
		;	(	category_property(Category, info(Info)) ->
				parameter_names(Category, Info, Names)
			;	parameter_names(Category, [], Names)
			),
			Category =.. [Functor| _],
			CategoryName =.. [Functor| Names]
		).
	
	parameter_names(Entity, Info, Names) :-
		(	member(parnames(Names), Info) ->
			true
		;	member(parameters(Parameters), Info) ->
			pairs::keys(Parameters, Names)
		;	Entity =.. [_| Names],
			variables_to_underscore(Names)
		).
	
	variables_to_underscore([]).
	variables_to_underscore([Arg| Args]) :-
		(	var(Arg) ->
			Arg = '_'
		;	true
		),
		variables_to_underscore(Args).
	
:- end_object.