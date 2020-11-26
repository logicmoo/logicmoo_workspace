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

:- module(pages_browser, [ make_form_urisel_page/4 ]).

:- use_module(library('http/http_dispatch')).
:- use_module(library('http/http_parameters')).

:- use_module(library(url)).

:- use_module('swilib/err').

:- use_module(pages_queries).
:- use_module(pages_util).
:- use_module(pages_planner).
:- use_module(mime_types).
:- use_module(rdf_convert).
:- use_module(rdfs_dtd).

:- use_module(dotgraph).
:- use_module(mimic).
:- use_module(rdf_dotgraph).
:- use_module(rdf_writer).
:- use_module(uris).
:- use_module(queries).

:- use_module(knowledgebase).

%% DEBUG
:- use_module('swilib/pretty').

/*

Page Types and their URIs

class             /item ? type=class kb=KB resource=Class 
property          /item ? type=property kb=KB resource=Property
object            /item ? type=object kb=KB resource=Object
data              [/data ...  for long literals ***]


classes           /toc ? type=classes kb=KB [#Class]
properties        /toc ? type=properties kb=KB [#Property]
namespaces        /toc ? type=namespaces kb=KB [#Namespace]
facts             /toc ? type=facts kb=KB 
                         [subject=Subject] [property=Property] [object=Object]

kb                /toc ? type=kb kb=KB

			  
knowledgebases    [? page for a single KB too?, genesis of a KB,
                   possibility to create, destroy, add, remove, closure
		   expression oriented (?)***]
           
help              /help ? [ path=Path ] other attributes
                  path and attributes of the referring page are submitted

system files      /static/Name
                  looked up in a special library directory
                  can be used e.g. for images, style sheets 

export            /export ? format=rdf kb=KB
                  [subject=Subject] [property=Property] [object=Object]

The values of the following fields are terms, encoded with
term_to_atom/2:
         kb
	 resource
	 subject
	 property
	 object

The labels #Class, #Property and #Namespace are terms, encoded with
term_to_atom/2.	 

The order of the fields specified here in URIs is not relevant, i.e.
attributes should be sorted to get a canonical form. None of the
specified fields can be supplied multiply.

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% The desired mime type seems to vary from client to client...
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rdf_mime_type('text/plain').
dtd_mime_type('text/plain').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Request Dispatch
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- http_handler_options(O),
   http_handler('/',                reply_index, O),
   http_handler('/index.html',      reply_index, O),
   http_handler('/item',            reply_item, O),
   http_handler('/form',            reply_form, O),
   http_handler('/command',         reply_command, O),
   http_handler('/toc',             reply_toc, O),
   http_handler('/once',            reply_file_once, O),
   http_handler(prefix('/static/'), reply_static, O).


reply_static(Request) :-
	memberchk(path(Path0), Request),
	sub_atom(Path0, 8, _, 0, Path),
	( document_file(Path, File) ->
	  true
	; msg('No path to document file: ~q.', [Path]),
	  throw(http_reply(not_found('')))
	),
	file_name_extension(_, Ext, File),
	( mime_extension(Ext, MimeType) ->
	  true
	; MimeType = 'text/plain'
	),
	( exists_file(File),
	  access_file(File, read) ->
  	  webserver_reply(file(File, Request), MimeType)
	; msg('File reply failed: ~q.', [File]),
	  throw(http_reply(not_found('')))
	).

reply_file_once(Request) :-
	get_form(Request, Form),
	formget(name=Name, Form),
	file_name_mime_type(Name, MimeType),
	webserver_reply(file_once(Name, Request), MimeType).

reply_item(Request) :-
	find_kb(Request, KB),
	get_form(Request, Form),
	formget(type=Type, Form),
	formget(resource=Resource, Form),
	( Type = rdf ->
	  rdf_mime_type(MimeType)
	; Type = plrdf ->
	  MimeType = 'text/plain'
	; MimeType = 'text/html'
	),
	( Type=class ->
	  make_item_class_page(KB, Resource, Page)
	; Type=property ->
	  make_item_property_page(KB, Resource, Page)
	; Type=kb ->
	  make_item_kb_page(KB, Resource, Page)
	; Type=dotgraph ->
	  make_item_dotgraph_page(KB, Resource, Page)
	; Type=solution ->
	  make_item_solution_page(KB, Resource, Page)
	; Type=rdf ->
	  make_item_rdf_page(KB, Resource, Page)
	; Type=plrdf ->
	  make_item_plrdf_page(KB, Resource, Page)
	; Type=query ->
	  make_item_query_page(KB, Resource, Page)
        ; make_item_object_page(KB, Resource, Page)
	),
	webserver_reply(Page, MimeType).

reply_form(Request) :-
	find_kb(Request, KB),
	get_form(Request, Form),
	formget(type=Type, Form),
	( Type=createkb ->
	  make_form_createkb_page(KB, Page)
	; Type=adddoc ->
	  make_form_adddoc_page(KB, Page)
	; Type=addns ->
	  make_form_addns_page(KB, Page)
        ; err('Unsupported form: ~q.', [Type])
	),
	webserver_reply(Page, 'text/html').
	
reply_index(Request) :-
	find_kb(Request, KB),
	make_toc_main_page(KB, Page),
	webserver_reply(Page, 'text/html').

reply_command(Request) :-

	find_kb(Request, KB),
	get_form(Request, Form),
	formget(type=Type, Form),
	( Type=createkb ->
	  make_command_createkb_page(KB, Form, Page)
	; Type=reloadkb ->
	  make_command_reloadkb_page(KB, Page)
	; Type=deletekb ->
	  make_command_deletekb_page(KB, Page)
	; Type=adddoc ->
	  make_command_adddoc_page(KB, Form, Page)
	; Type=addns ->
	  make_command_addns_page(KB, Form, Page)
	; Type=query ->
	  make_command_query_page(KB, Form, Page)
        ; err('Unsupported command: ~q.', [Type])
	),
	webserver_reply(Page, 'text/html').	

reply_toc(Request) :-
	find_kb(Request, KB),
	get_form(Request, Form),
	formget(type=Type, Form),
	( Type = rdf ->
	  rdf_mime_type(MimeType)
	; ( Type=rdfsdtd ) ->
	  dtd_mime_type(MimeType)  
	; Type = plrdf ->
	  MimeType = 'text/plain'
	; MimeType = 'text/html'
	),
	( Type=classes ->
	  make_toc_classes_page(KB, Page)
	; Type=properties ->
	  make_toc_properties_page(KB, Page)
	; Type=kbs ->
	  make_toc_kbs_page(KB, Page)
	; Type=rdf ->
	  make_kb_rdf_page(KB, Page)
	; Type=plrdf ->
	  make_kb_plrdf_page(KB, Page)
	; Type=dotgraph ->
	  make_kb_dotgraph_page(KB, Page)
	; Type=rdfsdtd ->
	  make_kb_rdfsdtd_page(KB, Page)
	; Type=namespaces ->
	  make_toc_namespaces_page(KB, Page)
	; Type=queries ->
	  make_toc_queries_page(KB, Page)
	; Type=main ->
	  make_toc_main_page(KB, Page)
	),
	webserver_reply(Page, MimeType).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Utilities
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

content_sequence([C], _, [C]) :-
	!.
content_sequence([C|Cs], Separator, [C, Separator|Cs1]) :-
	content_sequence(Cs, Separator, Cs1).
content_sequence([], _, []).

%%%%%%%%%%%%%%%%%%%%

file_name_mime_type(Name, MimeType) :-
	file_name_extension(_, Extension, Name),
	mime_extension(Extension, MimeType),
	!.
file_name_mime_type(_, 'text/plain').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Class Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_item_class_page(KB, Item, Page) :-

	make_item_page_general(KB, Item, class, 'Class', Page, Params),
	get_param(header_style_class, Params, 'browserHeaderClass'),
	get_param(footer_style_class, Params, 'browserFooterClass'),

        Lines =
	  ['Comments'=values(rdfs_comment):text,
           'Direct Superclasses'=pred(class_direct_superclasses):
	                                    class/[brief_comment],
           'Equal Classes'=pred(class_equal_classes):class,
           'Direct Subclasses'=pred(class_direct_subclasses):
	                                    class/[brief_comment],
	   'Effective Domain Properties'=
	      pred(class_effective_domain_properties):
                                            property/[brief_comment],
	   'Effective Range Properties'=
	             pred(class_effective_range_properties):
		                            property/[],
	   'Direct Instances'=pred(class_direct_instances):object ],


	make_view_refs(KB, class, Item, ViewRefs),
	( ViewRefs = [] ->
	  Lines1 = Lines
	; Lines1 = [(0-'Other Views'=xml(ViewRefs)) | Lines ]
	),
	make_atvs(KB, Item, [], Lines1, AVTable),
	get_param(av_table, Params, AVTable),   
        check_params(Params).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 	
%%%% Object Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	
make_item_object_page(KB, Item, Page) :-

	make_item_page_general(KB, Item, object, 'Object', Page, Params),
	get_param(header_style_class, Params, 'browserHeaderObject'),
	get_param(footer_style_class, Params, 'browserFooterObject'),

	L1 = [(20-'Direct Types'=pred(object_direct_types):class)|AVInput1],
	
	( atom(Item),
	  decompose_uri(Item, Scheme, _, _, _, _),
	  nonvar(Scheme) ->
	  L2 = [(30-'Reference'=xml([element(a, [href=Item], [Item])]))|L1]
        ; L2 = L1
	),

	make_view_refs(KB, object, Item, ViewRefs),
	( ViewRefs = [] ->
	  L3 = L2
	; L3 = [(10-'Other Views'=xml(ViewRefs))|L2]
	),

	Lines = [15-'Comments'=values(rdfs_comment):text|L3],

	object_direct_properties(KB, Item, Groups),

	( select(rdf_type-_, Groups, Groups1) -> true ; Groups1 = Groups ),
	( select(rdfs_comment-_, Groups1, Groups2) -> true ; Groups2=Groups1 ),

	object_direct_inverse_properties(KB, Item, InverseGroups),
	map_group_to_avline(Groups2, property, AVInput1),
	map_group_to_avline(InverseGroups, inverse_property, AVInput2),
	append(Lines, AVInput2, AVInput),

	make_atvs(KB, Item, [sort], AVInput, AVTable),

	get_param(av_table, Params, AVTable),   
        check_params(Params).

group_to_avline(Property-Values, Type, Property:Type=set(Values):object).

map_group_to_avline([X|Xs], T, [X1|Xs1]) :-
	group_to_avline(X, T, X1),
	map_group_to_avline(Xs, T, Xs1).
map_group_to_avline([], _, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Property Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


make_item_property_page(KB, Item, Page) :-

	make_item_page_general(KB, Item, property, 'Property', Page, Params),
	get_param(header_style_class, Params, 'browserHeaderProperty'),
	get_param(footer_style_class, Params, 'browserFooterProperty'),

	Lines =
	  ['Comments'=values(rdfs_comment):text,
	   'Direct Superproperties'=
	       pred(property_direct_superproperties):property/[brief_comment],
	   'Equal Properties'=
	       pred(property_equal_properties):property/[brief_comment],    
           'Direct Subproperties'=
	       pred(property_direct_subproperties):property/[brief_comment],
	   'Direct Domain Classes'=
	       pred(property_direct_domain_classes):class/[brief_comment],
	   'Direct Range Classes'=
	       pred(property_direct_range_classes):class/[brief_comment],
	   'Direct Extension'=pred(property_direct_extension):object ],


	make_view_refs(KB, property, Item, ViewRefs),
	( ViewRefs = [] ->
	  Lines1 = Lines
	; Lines1 = [(0-'Other Views'=xml(ViewRefs)) | Lines]
	),

	make_atvs(KB, Item, [], Lines1, AVTable),

	get_param(av_table, Params, AVTable),   
        check_params(Params).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_command_reloadkb_page(KB, Page) :-
	kb_id(KB, K),
	find_catalog_kb(KB, CatalogKB), 
	resource_pretty_name(CatalogKB, K, PrettyKB),
	item_uri(CatalogKB, kb, K, KBUri),

	reload_knowledgebase(KB, PrettyInfo),

	make_result_page(KB, 
	        'Success: Reload Knowledgebase',
		['Successfully reloaded  documents in knowledgebase ',
		 element(a, [href=KBUri], [PrettyKB]),
		 '. ',
		 element(br, [], []),
		 PrettyInfo],
		Page).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_command_deletekb_page(KB, Page) :-
	kb_id(KB, K),
	find_catalog_kb(KB, CatalogKB), 
	resource_pretty_name(CatalogKB, K, PrettyKB),

	delete_knowledgebase(KB),
	
	make_result_page(CatalogKB, 
			 'Success: Deleted Knowledgebase',
			 ['Successfully deleted knowledgebase ',
			  PrettyKB, '.'],
			 Page).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Form Create Knowledgebase Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


make_form_createkb_page(KB, Page) :-

	find_catalog_kb(KB, CatalogKB),

	make_meta_page_general(CatalogKB, createkb, 'Create Knowledgebase', 
	                       command, 'Command', Page, Params),

	get_param(header_style_class, Params, 'browserHeaderCommand'),
	get_param(footer_style_class, Params, 'browserFooterCommand'),

	find_page_template('permission_table.html', PermissionTable, _),

	kb_groups(CatalogKB, Groups),
	xml_select_item(CatalogKB, group, [], Groups, _, GroupSelect),

	find_page_template('submit.html', [Submit], SubmitParams),
	get_param(submit_value, SubmitParams, 'Create Knowledgebase'),

	make_atvs(CatalogKB, _, [], 
	           [ 'Identifier'=
                     xml([ element(input,
		                   [name=identifier, type=text, size=48],
				   [])]),
                     'Comment' =
	             xml([ element(textarea,
		                   [name=comment, rows=3, cols=48],
				   [' ']) ]),
		     'Group' = xml(GroupSelect),
		     'Permissions' = xml(PermissionTable)
                   ],
		  AVTable),

        AVForm = element(form, 
	                  [action='/command'],
		          [element(input, 
                                   [type=hidden, name=type, value=createkb],
				   []),
                           AVTable,
			   Submit]),

	get_param(av_table, Params, AVForm),   
        check_params(Params).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Command Create Knowledgebase Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_command_createkb_page(KB, Form, Page) :-
	kb_user(KB, Owner),
	kb_groups(KB, Groups),
	formget(identifier=Id, Form),
	formget(group=Group, Form),

	resolve_input_item(Id, Id1),
	
%%	resolve_input_item(Group, Group1),
%% USER/GROUP global uris as IDs???
%%	
	Group = Group1,
	formget(comment=Comment, Form),

	formget_default(perm_owner_r=POR, Form, n),
	formget_default(perm_owner_w=POW, Form, n),
	formget_default(perm_group_r=PGR, Form, n),
	formget_default(perm_group_w=PGW, Form, n),
	formget_default(perm_world_r=PWR, Form, n),
	formget_default(perm_world_w=PWW, Form, n),

	make_permissions(POR, POW, PGR, PGW, PWR, PWW, Permissions),

	canonicalize_item(Id1, Id2),
	make_kb(Id2, Owner, Groups, KB2),

	create_knowledgebase(KB2, [owner=Owner,
	                           group=Group1,
				   permissions=Permissions,
				   comment=Comment] ),

	find_catalog_kb(KB, CatalogKB), 
	resource_pretty_name(CatalogKB, Id2, PrettyKB),

	item_uri(CatalogKB, kb, Id2, KBUri),

	make_result_page(KB2, 
	        'Success: Create Knowledgebase',
		['Successfully created knowledgebase ',
		 element(a, [href=KBUri], [PrettyKB]),
		 '.'],
		Page).


resolve_input_item(Input, Resolved) :-
	trim_atom(Input, I1),
	( I1 = '' ->
          err('Empty input.')
	; true
	),
	decompose_uri(I1, Scheme, _, _, _, _),
	( var(Scheme) ->
          atom_concat('http://www.infraengine.com/user#', I1, Resolved)
	; Resolved = I1
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Form Add Documents Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%
%%%% *** This section to pages_util ?
%%%%

make_form_adddoc_page(KB, Page) :-
	known_rdf_sources(KB, Sources),
	Options = [sources=Sources, pretty='Add Documents'],
	make_form_urisel_page(KB, adddoc, Options, Page).

make_form_urisel_page(KB, Command, Options, Page) :-
	get_opt(multiple=Multiple, Options, true),
	get_opt(type_in=TypeIn, Options, true),
	get_opt(mini=Mini, Options, false),
	get_opt(sources=Sources, Options, []),
	get_opt(kbs=KBs, Options, []),
	get_opt(pretty=PrettyCommand, Options, '---'),
	get_opt(path=Path, Options, '/command'),

	make_meta_page_general(KB, Command, PrettyCommand, 
	                       command, 'Command', Mini, Page, Params),
	get_param(header_style_class, Params, 'browserHeaderCommand'),
	get_param(footer_style_class, Params, 'browserFooterCommand'),

	find_page_template('submit.html', [Submit], SubmitParams),
	get_param(submit_value, SubmitParams, PrettyCommand),

	kb_id(KB, KBId),
	term_to_atom(KBId, KBId1),

	( Multiple = true ->
	  MultipleAtts = [multiple=multiple]
	; MultipleAtts = []
	),

	( Sources = [] ->
	  SelectionRows = []
        ; length(Sources, Len),
	  ( Mini = true -> MaxSize = 8 ; MaxSize = 20 ),
	  Size is min(MaxSize, Len),
	  xml_select(uri, [size=Size | MultipleAtts], Sources, Select),
	  SelectionRows = ['Select' = xml(Select)]
        ),

	( KBs = [] ->
	  KBRows = []
	; kb_id(KB, ThisKBId),
	  find_catalog_kb(KB, KB1),
          xml_select_item(KB1, chosen_kb, [], KBs, ThisKBId, KBSelect),
	  KBRows = ['Knowledgebase' = xml(KBSelect)]
	),

	( TypeIn = true ->
	  Rows1 = [ 'URI'=
                     xml([ element(input,
		                   [name=uri, type=text, size=64],
				   [])])
		   | SelectionRows ]
        ; Rows1 = SelectionRows
	),

	append(KBRows, Rows1, Rows),
	

	make_atvs(KB, _, [], Rows, AVTable),

	( Rows = [] ->
	  AVForm = AVTable
	; AVForm = element(form, 
	                  [action=Path],
		          [element(input, 
                                   [type=hidden, name=type, value=Command],
				   []),
		           element(input, 
                                   [type=hidden, name=kb, value=KBId1],
				   []),
                           AVTable,
			   Submit])
        ),

	get_param(av_table, Params, AVForm),   
        check_params(Params).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Command Add Documents Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_command_adddoc_page(KB, Form, Page) :-
	findall(Uri, 
	        ( member(uri=Uri1, Form),
	          trim_atom(Uri1, Uri),
		  Uri \= '' ),
		Uris),		
        sort(Uris, Uris1),
	add_documents(KB, Uris1, PrettyInfo),
	
	kb_id(KB, KBId),
	item_uri(KB, kb, KBId, KBUri),
	kb_pretty(KB, KBPretty),

	make_href_list(Uris1, HRefs),
	( Uris1 = [] ->
	  Msg1 = ['No documents have been added to knowledgebase ']
        ; Uris1 = [_] ->
	  append(['Document '|HRefs],
	         [' has been added to knowledgebase '], Msg1)
	; append(['Documents '|HRefs],
                 [' have been added to knowledgebase '], Msg1)
	),
	append(Msg1, [element(a, [href=KBUri], [KBPretty]), '.'], Msg2),

	append(Msg2, [element(br, [], []), PrettyInfo], Msg),
	
	make_result_page(KB, 
	        'Success: Add Documents',
		Msg,
		Page).

make_href_list([Uri], [element(a, [href=Uri], [Uri])]) :-
	!.
make_href_list([Uri1, Uri2], 
               [element(a, [href=Uri1], [Uri1]), ' and ',
	        element(a, [href=Uri2], [Uri2])]) :-
	!.
make_href_list([Uri|Uris], [element(a, [href=Uri], [Uri]), ', '| Es]) :-
	make_href_list(Uris, Es).
make_href_list([], []).
	

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Result Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


make_result_page(KB, Title, Message, Page) :-
	
	make_meta_page_general(KB, meta, Title,
	                       result, 'Result', Page, Params),
	get_param(header_style_class, Params, 'browserHeaderCommand'),
	get_param(footer_style_class, Params, 'browserFooterCommand'),

	AVTable = element(table, [], 
	           [element(tr, [], 
		      [element(td, [class='browserResultMessage'],
		                   Message)])]),

	get_param(av_table, Params, AVTable),   
        check_params(Params).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Knowledgebase Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


make_item_kb_page(ContextKB, Item, Page) :-

	kb_user(ContextKB, User),
	kb_groups(ContextKB, Groups),
	make_kb(Item, User, Groups, KB),
	
	make_item_page_general(KB, Item, kb, 'Knowledgebase', Page, Params),
	get_param(header_style_class, Params, 'browserHeaderKB'),
	get_param(footer_style_class, Params, 'browserFooterKB'),

	form_uri(KB, adddoc, AdddocUri),
	form_uri(KB, addns, AddnsUri),
	command_uri(KB, reloadkb, ReloadKBUri),

	command_uri(KB, deletekb, DeleteKBUri),

	find_catalog_kb(KB, CatalogKB),

	form_proxy_uri(KB, ProxyUri),
	form_uri(CatalogKB, createkb, CreateKBUri),
	toc_uri(KB, kbs, KBsUri),

	item_uri_refs(KB, class, [rdfs_Resource], [], ResourceRef),

        Lines =
	  ['Commands'=xml([element(a, [href=AdddocUri,
	                               class=browserCommandRef],
				       ['Add Documents...']),
			   ', ',	       
		           element(a, [href=AddnsUri,
	                               class=browserCommandRef],
				       ['Add Namespace...']),
			   ', ',  		  
			   element(a, [href=ProxyUri,
			               class=browserCommandRef],
	                              ['Browse the Web...']),
                           ', ',				      
			   element(a, [href=ReloadKBUri,
	                               class=browserCommandRef],
				      ['Reload Documents']),
                           ', ',				      
			   element(a, [href=DeleteKBUri,
	                               class=browserCommandRef],
				      ['Delete']),
			   ', ',
			   element(a, [href=CreateKBUri,
	                               class=browserCommandRef],
				      ['Create Knowledgebase...']),
                           ', ',				      
			   element(a, [href=KBsUri],
				      ['Knowledgebases'])]),
           'Comments'=values(rdfs_comment):text,
           'Top Class'=xml(ResourceRef),
	   'Owner'=values(sys_owner):object,
           'Group'=values(sys_group):object,
	   'Permissions'=values(sys_permissions):object,
	   'Modification Time'=values(sys_modificationTime):object,
	   'Number of Classes'=pred(number_of_classes):object,
	   'Number of Properties'=pred(number_of_properties):object,
	   'Number of Triples'=pred(number_of_triples):object,
	   'Documents'=values(sys_document):object ],

	make_view_refs(CatalogKB, kb, Item, ViewRefs),
	content_view_refs(KB, CViewRefs),
	append(ViewRefs, [', '|CViewRefs], ViewRefs1),

	( ViewRefs1 = [] ->
	  Lines1 = Lines
	; Lines1 = [(0-'Other Views'=xml(ViewRefs1)) | Lines ]
	),



	make_atvs(CatalogKB, Item, [], Lines1, AVTable),


	get_param(av_table, Params, AVTable),   
        check_params(Params).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Known Sources ("History" feature)
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

known_rdf_sources(KB, Sources) :-
	find_library_kb(KB, LibraryKB),
	findall(T-D, ( fact(LibraryKB, D, rdf_type, sys_RdfDocument),
                       fact(LibraryKB, D, sys_accessed, T) ),
		     TDs ),
	sort(TDs, TDs1),
	reverse(TDs1, TDs2),
	map_value(TDs2, Sources1),
	std_rdf_sources(Sources2),
	subtract(Sources2, Sources1, Sources3),
	append(Sources1, Sources3, Sources).

std_rdf_sources([S1]) :-
	document_file('rdf/infraengine_schema.rdf', S1F),
	concat_atom(['file://', S1F], S1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Toc Namespaces Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_toc_namespaces_page(KB, Page) :-
	
	make_meta_page_general(KB, namespaces, 
	                           'Namespaces', meta, 'Meta', Page, Params),
	get_param(header_style_class, Params, 'browserHeaderNamespace'),
	get_param(footer_style_class, Params, 'browserFooterNamespace'),

	findall(N-A, fact(KB, N, sys_abbreviation, literal(A)), NAs),
	sort(NAs, NAs1),
	make_ns_rows(NAs1, -1, Rows),
	form_uri(KB, addns, AddnsUri),

	Lines =
	  ['Commands'=xml([element(a, [href=AddnsUri,
	                               class=browserCommandRef],
				      ['Add Namespace...'])]) ],

	make_atvs(KB, _, [], Lines, CmdTable),

	NSTable = element(table, [width='100%',
	                          border='0',
	                          cellspacing='0',
	                          cellpadding='2'], Rows),

        AVTable = element(table, [width='100%', border=0,
	                          cellspacing=0, cellpadding=0],
		    [element(tr, [],
		      [element(td, [], [CmdTable])]),
		     element(tr, [],
		      [element(td, [], [NSTable])])]),

	get_param(av_table, Params, AVTable),   
        check_params(Params).


make_ns_rows([N-A|Ns], I, Es) :-
	( I = 1 -> TRClass = browserOddRow ; TRClass = browserEvenRow ),
	Es = [element(tr, [class=TRClass],
	       [element(td, [width='15%',
	                     align=right,
			     class=browserStd],
	         [element(a, [name=Name], [A])]),
	        element(td, [class=browserStd], ['= ', N])]) | Es1 ],
	item_fragment_id(N, Name),
	I1 is I * -1,
	make_ns_rows(Ns, I1, Es1).
make_ns_rows([], _, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Toc Kowledgebases Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


make_toc_kbs_page(KB, Page) :-

	find_catalog_kb(KB, CatalogKB),
	
	make_meta_page_general(CatalogKB, kbs, 
	                       'Knowledgebases', meta, 'Meta', Page, Params),
	get_param(header_style_class, Params, 'browserHeaderKB'),
	get_param(footer_style_class, Params, 'browserFooterKB'),

	findall(K, fact(CatalogKB, K, rdf_type, sys_Knowledgebase), Ks),

	sort(Ks, Ks1),
	make_kb_rows(Ks1, KB, -1, Rows),

	form_uri(KB, createkb, CreatekbUri),

	Lines =
	  ['Commands'=xml([element(a, [href=CreatekbUri,
	                               class=browserCommandRef],
				      ['Create Knowledgebase...'])]) ],

	make_atvs(KB, _, [], Lines, CmdTable),

	KBTable = element(table, [width='100%',
	                          border='0',
	                          cellspacing='0',
	                          cellpadding='2'], Rows),

        AVTable = element(table, [width='100%', border=0,
	                          cellspacing=0, cellpadding=0],
		    [element(tr, [],
		      [element(td, [], [CmdTable])]),
		     element(tr, [],
		      [element(td, [], [KBTable])])]),

	get_param(av_table, Params, AVTable),   
        check_params(Params).


make_kb_rows([K|Ks], KB, I, Es) :-
	( I = 1 -> TRClass = browserOddRow ; TRClass = browserEvenRow ),
	Es = [element(tr, [class=TRClass],
	       [element(td, [class=browserStd], Refs)]) | Es1], 
       find_catalog_kb(KB, CatalogKB),	       
       item_uri_refs(CatalogKB, kb, [K], [brief_comment], Refs),
       I1 is I * -1,
       make_kb_rows(Ks, KB, I1, Es1).
make_kb_rows([], _, _, []).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Toc Main Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


make_toc_main_page(KB, Page) :-

	find_catalog_kb(KB, CatalogKB),

	make_meta_page_general(CatalogKB, main, 
	                       'Main', meta, 'Meta', Page, Params),


	get_param(header_style_class, Params, 'browserHeaderMain'),
	get_param(footer_style_class, Params, 'browserFooterMain'),

	toc_uri(CatalogKB, kbs, UriKBs),
	form_uri(CatalogKB, createkb, UriCreateKB),

	kb_user(CatalogKB, User),
	item_uri_refs(CatalogKB, object, [User], [], XMLUser),
	kb_groups(CatalogKB, Groups),
	item_uri_refs(CatalogKB, object, Groups, [], XMLGroups),

	  %%

	
	form_proxy_uri(KB, ProxyUri),

	Lines = ['Enter'=xml([element(a, [href=UriKBs],
	                                  ['Knowledgebases'])]),
		 'Commands'=xml([element(a, [href=ProxyUri,
			                     class=browserCommandRef],
	                                    ['Browse the Web...']),
			         ', ',		    
			         element(a, [href=UriCreateKB,
	                                     class=browserCommandRef],
					    ['Create Knowledgebase...'])]),
		 'You are user'=xml(XMLUser),
	         'You are member in groups'=xml(XMLGroups) ],
	make_atvs(CatalogKB, _, [], Lines, AVTable),
	get_param(av_table, Params, AVTable),   
        check_params(Params).


form_proxy_uri(KB, Uri) :-
	kb_id(KB, KB1),
	term_to_atom(KB1, KB2),
	www_form_encode(KB2, KB3),
	concat_atom(['/form_proxy?kb=', KB3], Uri).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Toc Classes Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_toc_classes_page(KB, Page) :-
	
	make_meta_page_general(KB, classes, 'Classes',
	                       meta, 'Meta', Page, Params),
	get_param(header_style_class, Params, 'browserHeaderClass'),
	get_param(footer_style_class, Params, 'browserFooterClass'),

	get_time(T1),
	msg('Computing class rows.'),

        graph_rows(KB, 'rdfs_Resource', 'rdfs_subClassOf', Rows),

	get_time(T2),
	DT is T2 - T1,
	kb_id(KB, Id),
	msg('Class rows ~q time: ~q.', [Id, DT]),

	gr_table_atts(TAtts),
	AVTable = element(table, TAtts, Rows),

	get_param(av_table, Params, AVTable),   
        check_params(Params).


graph_rows(KB, C, TP, Rs) :-
	graph_rows(KB, C, TP, 0, [], _, Rs, []).

map_graph_rows(KB, Cs, TP, Rs) :-
	map_graph_rows(KB, Cs, TP, 0, [], _, Rs, []).

graph_rows(KB, C, TP, N, Ls, Ls1, Rs, Rs1) :-
%%	write('.'), flush_output,
	N1 is N + 1,
	setof(EC, (EC = C ; tp_equal(KB, TP, C, EC)), ECs),
	( TP = rdfs_subPropertyOf -> RefType = property ; RefType = class ),
	item_uri_refs(KB, RefType, ECs, 
	              [brief_comment, anchor, anchor_exclude(Ls)], ECRefs),
	gr_row(ECRefs, N, Rs, Rs2),
	append(ECs, Ls, Ls2),
	( setof(SC, tp_direct_sub_of(KB, TP, SC, C), SCs) ->
          ( memberchk(C, Ls) ->
	    make_ref_rows(KB, C, N1, Rs2, Rs1),
	    Ls1 = Ls2
	  ; sort_pretty(KB, SCs, SCs1),
	    map_graph_rows(KB, SCs1, TP, N1, Ls2, Ls1, Rs2, Rs1)
	  )
       ; Rs1 = Rs2,
         Ls1 = Ls2
       ).

gr_row(Contents, N, Rs, Rs1) :-
	row_type_att(N, RowTypeAtt),
	gr_inner_table_atts(TAtts),
	gr_td_atts(TDAtts),
	gr_fill_td_atts(N, FillTDAtts),
	Rs = [ element(tr, [],
	       [ element(td, [], 
                 [ element(table, TAtts,
                   [ element(tr, [RowTypeAtt],
	             [ element(td, FillTDAtts, [' ']),
                       element(td, TDAtts, Contents) ]) ])])])
             | Rs1 ].

gr_fill_td_atts(N, [width=N1]) :-
	N2 is N * 50,
	term_to_atom(N2, N1).
gr_td_atts([class='browserStd']).
gr_table_atts([width='100%', border='0', cellspacing='0', cellpadding='0']).
gr_inner_table_atts([border='0', cellspacing='0', cellpadding='2']).

row_type_att(N, class='browserOddRow') :-
	0 =:= N rem 2,
	!.
row_type_att(_, class='browserEvenRow').

make_ref_rows(_KB, C, N, Rs, Rs1) :-
	item_fragment_id(C, Fragment),
	concat_atom(['#', Fragment], Ref),
	gr_row( [ element(a, [href=Ref], ['...']) ], N, Rs, Rs1 ).
		                          
map_graph_rows(KB, [C|Cs], TP, N, Ls, Ls1, Rs, Rs1) :-
	graph_rows(KB, C, TP, N, Ls, Ls2, Rs, Rs2),
	map_graph_rows(KB, Cs, TP, N, Ls2, Ls1, Rs2, Rs1).
map_graph_rows(_, [], _, _, Ls, Ls, Rs, Rs).

sort_pretty(KB, Items, Items1) :-
	map_add_pretty_name(Items, KB, Items2),
	sort(Items2, Items3),
	map_value(Items3, Items1).

map_add_pretty_name([X|Xs], KB, [(A:PN)-X|Xs1]) :-
	resource_pretty_name(KB, X, _, A, PN),
	map_add_pretty_name(Xs, KB, Xs1).
map_add_pretty_name([], _, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Form Add Namespace Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


make_form_addns_page(KB, Page) :-

	make_meta_page_general(KB, addns, 'Add Namespace', 
	                       command, 'Command', Page, Params),

	get_param(header_style_class, Params, 'browserHeaderCommand'),
	get_param(footer_style_class, Params, 'browserFooterCommand'),

	find_page_template('submit.html', [Submit], SubmitParams),
	get_param(submit_value, SubmitParams, 'Add Namespace'),

	kb_id(KB, KBId),
	term_to_atom(KBId, KBId1),
	
	( namespace_suggestion(KB, N) -> Sugg = [value=N] ; Sugg = [] ),


	make_atvs(KB, _, [], 
	           [ 'Abbreviation'=
                     xml([ element(input,
		                   [name=abbreviation, type=text, size=10],
				   [])]),
                    'Namespace'=
                     xml([ element(input,
		                   [name=namespace, type=text, size=48 | Sugg],
				   [])])
                   ],
		  AVTable),
		  
 
        AVForm = element(form, 
	                  [action='/command'],
		          [element(input, 
                                   [type=hidden, name=type, value=addns],
				   []),
		           element(input, 
                                   [type=hidden, name=kb, value=KBId1],
				   []),
                           AVTable,
			   Submit]),

	get_param(av_table, Params, AVForm),   
        check_params(Params).

namespace_suggestion(KB, N) :-
	setof(N1, A^fact(KB, N1, sys_abbreviation, literal(A)), Ns),
	fact(KB, P, _, _),
	atom(P),
	\+ (member(N2, Ns), sub_atom(P, 0, _, _, N2)),
	namespace_break(P, N, _).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Command Add Namespace Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


make_command_addns_page(KB, Form, Page) :-

	formget(abbreviation=A, Form),
	formget(namespace=N, Form),
	trim_atom(A, A1),
	trim_atom(N, N1),
	add_namespace(KB, N1, A1),
	kb_id(KB, KBId),
	item_uri(KB, kb, KBId, KBUri),
	kb_pretty(KB, KBPretty),
	toc_uri(KB, namespaces, N1, NsUri),
	KBRef = element(a, [href=KBUri], [KBPretty]),
	( A1 = '' -> A2 = '[default]' ; A2 = A1 ),
	NsRef = element(a, [href=NsUri], [A2, '=', N1]),
	
	Msg = ['Added namespace ', NsRef, ' to knowledgebase ', KBRef, '.'],

	make_result_page(KB, 
	        'Success: Add Namespace',
		Msg,
		Page).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Toc Properties Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_toc_properties_page(KB, Page) :-
	
	make_meta_page_general(KB, properties, 'Properties',
	                       meta, 'Meta', Page, Params),
	get_param(header_style_class, Params, 'browserHeaderProperty'),
	get_param(footer_style_class, Params, 'browserFooterProperty'),

	( setof(P, toplevel_property(KB, P), Ps ) ->
	  sort_pretty(KB, Ps, Ps1),
	  map_graph_rows(KB, Ps1, 'rdfs_subPropertyOf', Rows)
	; Rows = []
	),

	gr_table_atts(TAtts),
	AVTable = element(table, TAtts, Rows),

	get_param(av_table, Params, AVTable),   
        check_params(Params).


toplevel_property(KB, Prop) :-
	fact(KB, Prop, rdf_type, rdf_Property),
	\+ ( fact(KB, Prop, rdfs_subPropertyOf, Prop1),
	     Prop1 \= Prop ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Item RDF Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_item_rdf_page(KB, Resource, Page) :-
	findall(N-A, fact(KB, N, sys_abbreviation, literal(A)), NAs),
	object_triples(KB, Resource, Triples),
	triples_to_xml(Triples, NAs, Page).

make_item_plrdf_page(KB, Resource, Page) :-
	findall(N-A, fact(KB, N, sys_abbreviation, literal(A)), NAs),
	object_triples(KB, Resource, Triples),
	triples_to_mimic(Triples, NAs, Terms),
	Page = pp(mimic:pp_mimic_terms, Terms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Item Dotgraph Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_item_dotgraph_page(KB, Item, Page) :-
	findall(N-A, fact(KB, N, sys_abbreviation, literal(A)), _NAs),
	object_triples(KB, Item, Triples1),	
	findall( rdf(S,P,Item),
		 informative_fact(KB, S, P, Item),
		 Triples2 ),
	append(Triples1, Triples2, Triples3),
	triples_to_dotgraph(KB, Triples3, Dotgraph),
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

	Lines = [],
	make_view_refs(KB, dotgraph, Item, ViewRefs),
	( ViewRefs = [] ->
	  Lines1 = Lines
	; Lines1 = [(0-'Other Views'=xml(ViewRefs)) | Lines ]
	),
	make_atvs(KB, Item, [firsteven], Lines1, CmdTable),
	
	make_item_page_general(KB, Item, object, 'Graph', Page, Params),
	get_param(header_style_class, Params, 'browserHeaderObject'),
	get_param(footer_style_class, Params, 'browserFooterObject'),

	get_param(av_table, Params, AVTable),
	check_params(Params).

make_dotgraph_image(_KB, Dotgraph, ImageFile, Areas) :-
	MimeType = 'image/gif',
	process_dotgraph(Dotgraph, MimeType, ImageFile, Areas),
	webserver_register_file_once(ImageFile).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% KB RDF Page
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kb_content_triples(KB, Triples, NAs) :-
	findall(N-A, fact(KB, N, sys_abbreviation, literal(A)), NAs),
	findall(rdf(S, P, O),
		%% *** Literal handling is just for now
	        ( informative_fact(KB, S, P, O), S \= literal(_) ), Triples1),
	%% Ensure that there is a type fact for each subject, so plain
	%% striped syntax can be used:
	( setof(S1, P1^O1^member(rdf(S1, P1, O1), Triples1), Subjects) ->
	  true
	; Subjects = []
	),
	findall(rdf(S1, rdf_type, C),
		( member(S1, Subjects),
		  \+ informative_fact(KB, S1, rdf_type, _),
		  once(min_class(KB, S1, rdf_type, C))
		),
		Triples2),
	append(Triples2, Triples1, Triples).

make_kb_rdf_page(KB, Page) :-
	kb_content_triples(KB, Triples, NAs),
	triples_to_xml(Triples, NAs, Page).

make_kb_plrdf_page(KB, Page) :-
	kb_content_triples(KB, Triples, NAs),
	triples_to_mimic(Triples, NAs, Terms),
	Page = pp(mimic:pp_mimic_terms, Terms).

make_kb_dotgraph_page(KB, Page) :-
	kb_content_triples(KB, Triples, _),
	triples_to_dotgraph(KB, Triples, Dotgraph),
	make_dotgraph_page(KB, Dotgraph, Page).

make_kb_rdfsdtd_page(KB, Page) :-
	Page = pp(rdfs_dtd:print_dtd, KB).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	   
make_dotgraph_page(KB, Dotgraph, Page) :-
	MimeType = 'image/gif',
	process_dotgraph(Dotgraph, MimeType, ImageFile, Areas),
	webserver_register_file_once(ImageFile),

	kb_id(KB, K),
	term_to_atom(K, K1),	% *** see uri construction in pages_util...
	sformat(SrcUri1, '/once?kb=~w&name=~w', [K1, ImageFile]),
	string_to_atom(SrcUri1, SrcUri2),

	Page = [element(html, [],
		[element(head, [title='Testpage'], []),
		 element(body, [],
	          [element(h1, [], ['Testpage']),
		   element(map, [name=graph], Areas),
		   element(img, [alt='Graph',
				 src=SrcUri2,
				 border=0,
				 usemap='#graph'], [])])])].
	 
