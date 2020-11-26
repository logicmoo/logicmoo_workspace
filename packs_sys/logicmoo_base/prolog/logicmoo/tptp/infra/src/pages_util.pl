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

:- module(pages_util, [find_page_template/3,

		       trim_atom/2,
		       map_value/2,
		       get_opt/3,

		       get_form/2,
		       check_params/1,
		       get_param/3,
		       get_param_enforce/3,
		       formget/2,
		       formget_default/3,

		       find_kb/2,
		       find_kb/3,
		       find_library_kb/2,
		       find_catalog_kb/2,

		       item_uri/4,
		       toc_uri/3,
		       toc_uri/4,
		       form_uri/3,
		       command_uri/3,
		       once_uri/3,
		       item_fragment_id/2,
		       item_uri_ref/5,
		       item_uri_refs/5,

		       make_atvs/5,

		       make_item_page_general/6,
		       make_meta_page_general/7,
		       make_meta_page_general/8,

		       writeable_kb/2,
		       readable_kb/2,
		       kb_pretty/2,
		       
		       xml_select/4,
		       xml_select/5,
		       xml_select_item/6,

		       make_view_refs/4,
		       content_view_refs/2
		      ]).

:- use_module(webget).
:- use_module(config).
:- use_module('swilib/err').
:- use_module(pages_queries).
:- use_module(rdf_convert).
:- use_module(textutil).
:- use_module(xml_writer).

:- use_module(library('http/http_parameters')).
:- use_module(library(sgml)).
:- use_module(library(url)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Templates Pages
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% load_page_template(+Name, -Template, -Parameters)
%%%% 
%%%% Loads an html structure, replaces content and attribute values which
%%%% are atoms starting with '$' with variables. Those variables are
%%%% returned as list of Name=Var pairs in Parameters. Name is the the rest
%%%% of the atom, after the '$'. The same variable may occur multiple times
%%%% in the document.
%%%%
%%%% [ The XML-parser outputs harmless warnings if variables appear
%%%%   in positions where certain element types are required. ]
%%%%
load_page_template(Name, Template, Parameters) :-
	template_file(Name, FileName),
	dtd(html, DTD),
	load_structure(FileName, Structure, 
	               [dtd(DTD), dialect(sgml),
		        space(default), defaults(false)]),
        cleanup_whitespace(Structure, Structure1),
	make_template(Structure1, Template, [], Parameters).

make_template([element(T, A, C)|Cs], [element(T, A1, C1)|Cs1], Ps, Ps1) :-
	!,
	mt_atts(A, A1, Ps, Ps2),
	make_template(C, C1, Ps2, Ps3),
	make_template(Cs, Cs1, Ps3, Ps1).
make_template([Varspec|Cs], [Var|Cs1], Ps, Ps1) :-
	is_varspec(Varspec, Var, Ps, Ps2),
	!,
	make_template(Cs, Cs1, Ps2, Ps1).
make_template([C|Cs], [C|Cs1], Ps, Ps1) :-
	make_template(Cs, Cs1, Ps, Ps1).
make_template([], [], Ps, Ps).

mt_atts([A=V|AVs], [A=Var|AVs1], Ps, Ps1) :-
	is_varspec(V, Var, Ps, Ps2),
	!,
	mt_atts(AVs, AVs1, Ps2, Ps1).
mt_atts([AV|AVs], [AV|AVs1], Ps, Ps1) :-
	mt_atts(AVs, AVs1, Ps, Ps1).
mt_atts([], [], Ps, Ps).

is_varspec(X, Var, Ps, Ps1) :-
	atom(X),
	atom_prefix(X, '$'),
	!,
	atom_codes(X, [_|Codes]),
	reverse(Codes, Codes1),
	skip_whitespace(Codes1, Codes2),
	reverse(Codes2, Codes3),
	atom_codes(Name, Codes3),
	( memberchk(Name=Var, Ps) ->
	  Ps1 = Ps
	; Ps1 = [Name=Var|Ps]
	).

skip_whitespace([C|Codes], Codes1) :-
	code_type(C, space),
	!,
	skip_whitespace(Codes, Codes1).
skip_whitespace(Codes, Codes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Patch over the parsed html (with option space(default)), to
%%%% allow better xml writing. This might be not correct for HTML
%%%% in general, but suffient for our templates.
%%%%


cleanup_whitespace([' '|Cs], Cs1) :-
	!,
	clw(Cs, Cs1).
cleanup_whitespace(Cs, Cs1) :-
	clw(Cs, Cs1).

clw([element(T, A, C), ' ', element(T1, A1, C1)|Cs], Cs1) :-
	non_ws_element(T),
	non_ws_element(T1),
	!,
	clw([element(T, A, C), element(T1, A1, C1)| Cs], Cs1).
clw([' '], []) :-
	!.
clw([C|Cs], [C1|Cs1]) :-
	clw_1(C, C1),
	clw(Cs, Cs1).
clw([], []).

   
clw_1(element(T, A, Cs), element(T, A, Cs1)) :-
	!,
	cleanup_whitespace(Cs, Cs1).
clw_1(X, X).

non_ws_element(head).
non_ws_element(body).
non_ws_element(table).
non_ws_element(tbody).
non_ws_element(tr).
non_ws_element(td).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% General Utilities
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_value([_-V|Xs], [V|Xs1]) :-
	map_value(Xs, Xs1).
map_value([], []).

get_opt(KV, KVs, _) :-
	memberchk(KV, KVs),
	!.
get_opt(_=V, _, V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Trim Atom
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


trim_atom(Atom, Atom1) :-
	atom_codes(Atom, Codes),
	ta_1(Codes, Codes1),
	reverse(Codes1, Codes2),
	ta_1(Codes2, Codes3),
	reverse(Codes3, Codes4),
	atom_codes(Atom1, Codes4).

ta_1([C|Codes], Codes1) :-
	code_type(C, space),
	!,
	ta_1(Codes, Codes1).
ta_1(Codes, Codes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Request Accessors
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

formget(KV, Form) :-
	memberchk(KV, Form),
	!.
formget(K=_, _) :-
	err('Missing form parameter: ~q.', [K]).

formget_default(KV, Form, _) :-
	memberchk(KV, Form),
	!.
formget_default(_=V, _, V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Access Of Template Parameters
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_param_enforce(Param, Params, Value) :-
	memberchk(Param=Value, Params),
	!.
get_param_enforce(Param, Params, _) :-
	err('Parameter missing in template: ~q (among: ~q).', [Param, Params]).

%%%% 
%%%% get_param can be used
%%%% 
%%%% - to read the parameter (or obtain the variable if still unbound)
%%%% - to set the parameter 
%%%% - to default the parameter (since set takes only effect
%%%%   if it is still unbound)
%%%% 
%%%% If the parameter is not present, this predicate has no effect.
%%%% 
get_param(Param, Params, Value) :-
	memberchk(Param=Value, Params),
	!.
get_param(_, _, _).

check_params([P=V|_]) :-
	var(V),
	!,
	err('Unbound template parameter: ~q.', [P]).
check_params([_|PVs]) :-
	check_params(PVs).
check_params([]).

get_form(Request, Form) :-
	http_parameters(Request, [], [form_data(Form1)]),
	decoded_form(Form1, Form).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Form Field Encoding
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% decoded_form(+, -)
%%%% decoded_form(-, +)
%%%% 
decoded_form([A=V|AVs], [A=V1|AVs1]) :-
	coded_field(A),
	!,
	term_to_atom(V1, V),
	( var(V1) ->
	  err('Variable supplied as form argument ~q.', [A])
	; true
	),
	decoded_form(AVs, AVs1).
decoded_form([AV|AVs], [AV|AVs1]) :-
	decoded_form(AVs, AVs1).
decoded_form([], []).

coded_field(kb).
coded_field(resource).
coded_field(subject).
coded_field(property).
coded_field(object).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Find KBs
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_library_kb(KB, LibraryKB) :-
	kb_user(KB, User), 
	kb_groups(KB, Groups),
	make_kb(sys_library, User, Groups, LibraryKB).

find_catalog_kb(KB, CatalogKB) :-
	kb_user(KB, User), 
	kb_groups(KB, Groups),
	make_kb(sys_catalog, User, Groups, CatalogKB).

find_kb(Request, KB) :-
	http_parameters(Request, [], [form_data(Form)]),	
	!,
	find_kb(Request, Form, KB).

find_kb(Request, Form, KB) :-
	memberchk(user(User), Request),
	Groups = [User], % ???
	( ( memberchk(chosen_kb=KBId1, Form),
	    canonicalize_item(KBId1, KBId)
	  ; memberchk(kb=KBId, Form) ) ->
	  ( KBId = sys_generate ->
	    generate_kb(User, Groups, 'Generated by the system.', KB)
          ; make_kb(KBId, User, Groups, KB)
	  )
	; make_meta_kb(User, Groups, KB)
	),
	!.
find_kb(Request, _, _) :-
	err('Bad request - find_kb failed: ~q.', [Request]).

find_other_kb(KB, K, KB1) :-
	kb_user(KB, User),
	kb_groups(KB, Groups),
	make_kb(K, User, Groups, KB1).

kb_pretty(KB, KBPretty) :-
	kb_id(KB, KBId),
	find_catalog_kb(KB, CatalogKB),
	resource_pretty_name(CatalogKB, KBId, KBPretty).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Presentation Relevant Stuff
%%%% 
%%%% See also make_atvs.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

table_atts([border='0', cellspacing='0', cellpadding='5']).

atv_td_att_attributes([width='30%', class='browserAtt']).
atv_td_val_attributes([width='70%', class='browserVal']).

bind_general_params(Params) :-
	get_param(image_alt_logo, Params, 'infra engine'),
	get_param(image_logo, Params,
	                   'static/images/logo_small.gif'),
        get_param(stylesheet, Params,
	                   'static/stylesheet.css').
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Navigation Table
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_navigation_table(KB, FromType, FromItem, Table) :-

	table_atts(TableAtts),
	Table = element(table,
	                TableAtts, 
	                [ element(tr, [], Buttons) ]),
	( FromType = class ->
	  toc_uri(KB, classes, FromItem, ClassesUri)
	; toc_uri(KB, classes, ClassesUri)
	),
	( FromType = property ->
 	  toc_uri(KB, properties, FromItem, PropertiesUri)
	; toc_uri(KB, properties, PropertiesUri)
	),
	toc_uri(KB, namespaces, NamespacesUri),
	kb_id(KB, KBId),
	item_uri(KB, kb, KBId, KBUri),
	toc_uri(KB, main, MainUri),
	( ( FromType = meta ; FromType = command ; FromType = result) ->
          HelpFrag = FromItem
	; HelpFrag = FromType
	),
	help_uri(HelpFrag, HelpUri),
	toc_uri(KB, queries, QueryUri),	
	( FromItem = classes ->
	  make_disabled_navigation_button('Classes', ClassesButton)
        ; make_navigation_button('Classes', ClassesUri, ClassesButton)
	),
	( FromItem = properties ->
          make_disabled_navigation_button('Properties', PropertiesButton)
	; make_navigation_button('Properties', PropertiesUri,
	                         PropertiesButton)
	),
	( FromItem = namespaces ->
	 make_disabled_navigation_button('Namespaces', NamespacesButton)
	 ; make_navigation_button('Namespaces', NamespacesUri,
	                          NamespacesButton)
        ),
	( FromItem = main ->
	  make_disabled_navigation_button('Main', MainButton)
	; make_navigation_button('Main', MainUri, MainButton)
	),
	( FromItem = help ->
	  make_disabled_navigation_button('Help', HelpButton)
	; make_navigation_button('Help', HelpUri, HelpButton)
	),

	( FromItem = queries ->
          make_disabled_navigation_button('Queries', QueryButton)
	; make_navigation_button('Queries', QueryUri, QueryButton)
	),

	kb_pretty(KB, KBPretty),
	concat_atom(['Knowledgebase (', KBPretty, ')'], KBName),

	( FromType = kb ->
	  make_disabled_navigation_button(KBName, KBButton)
	; make_navigation_button(KBName, KBUri, KBButton)
	),

	Buttons = [ClassesButton,
	           PropertiesButton,
		   NamespacesButton,
		   QueryButton,
		   KBButton,
		   MainButton,
		   HelpButton].
	                      
make_navigation_button(Pretty, Uri, Button) :-
	Button =  element(td,
	                  [class='browserNaviButton'], 
	                  [element(a, [href=Uri, class='browserNaviRef'], 
			              [Pretty])]).

make_disabled_navigation_button(Pretty, Button) :-
	Button =  element(td,
	                  [class='browserNaviButton'], 
	                  [Pretty]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Page Construction
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_atvs(KB, Resource, Options, Specs, Table) :-
	( memberchk(firsteven, Options) -> FirstStyle = -1 ; FirstStyle = 1 ),
	map_make_atv(Specs, KB, Resource, Options, Rows),
	( memberchk(sort, Options) ->
	  sort(Rows, Rows1),
	  map_value(Rows1, Rows2)
	; Rows2 = Rows
	),
	map_set_style_class(Rows2, FirstStyle, Rows3),
	table_atts(TableAtts),
	Table = element(table, 
	                [class='browserAttVal', width='100%' | TableAtts],
			Rows3).
	

map_set_style_class([X|Xs], Y1, [X1|Xs1]) :-
	set_style_class(X, Y1, X1),
	Y1a is Y1 * -1,
	map_set_style_class(Xs, Y1a, Xs1).
map_set_style_class([], _, []).

set_style_class( element(tr, A, C), 1,
                 element(tr, [class='browserOddRow'|A], C) ) :-
	!.
set_style_class( element(tr, A, C), -1,
                 element(tr, [class='browserEvenRow'|A], C) ) :-
	!.
set_style_class(X, X).

map_make_atv([X|Xs], Y1, Y2, Os, [X1|Xs1]) :-
	make_atv(Y1, Y2, Os, X, X1),
	map_make_atv(Xs, Y1, Y2, Os, Xs1).
map_make_atv([], _, _, _, []).

make_atv(KB, Resource, Options,
        AttributeSpec=QuerySpec, TR) :-
	atv_td_att_attributes(AttAtts),
	atv_td_val_attributes(ValAtts),

	( QuerySpec = Query:Extras ->
	  ( Extras = (ResultRefType/LocalOptions) -> true
	  ; ResultRefType = Extras,
	    LocalOptions = []
	  ),
          ( Query=pred(QueryPredicate) ->
	    Call =.. [QueryPredicate, KB, Resource, Results],
	    call(Call)
          ; Query=values(Property) ->
	    findall(Val, fact(KB, Resource, Property, Val), Results)
	  ; Query=set(Results) ->
	    true
	  ),
	  item_uri_refs(KB, ResultRefType, Results, LocalOptions, Value)
	; QuerySpec = xml(Value) ->
          true
	; err('Bad atv query spec: ~q.', QuerySpec)
	),

	( AttributeSpec = AttributeItem:AttributeRefType ->
	  ( AttributeRefType = inverse_property ->
	    AttributeRefType1 = property
	  ; AttributeRefType1 = AttributeRefType
	  ),
	  item_uri_ref(KB, AttributeRefType1, AttributeItem, 
	               [sortkey(Sortkey1)], AttributeRefs1),
	  ( AttributeRefType = inverse_property ->
            Sortkey = inv(0, Sortkey1),
	    append(['inverse(' | AttributeRefs1], [')'], AttributeRefs)
          ; Sortkey = Sortkey1,
	    AttributeRefs = AttributeRefs1
	  )
        ; AttributeSpec = Sortkey-ASpec1, atom(ASpec1) ->
	  AttributeRefs = [ASpec1]
	; atom(AttributeSpec) ->
	  AttributeRefs = [AttributeSpec],
	  Sortkey = AttributeSpec
	; err('Bad atv attribute spec: ~q.', AttributeSpec)
	),    

	TR1 = element(tr, 
                       [],
	               [ element(td, AttAtts, AttributeRefs),
		         element(td, ValAtts, Value) ]),
        ( memberchk(sort, Options) ->
	  TR=Sortkey-TR1
        ; TR=TR1
	).

make_item_page_general(KB, Item, ItemType, PrettyItemType, Page, Params) :-
	find_page_template('item.html', Page, Params),
	bind_general_params(Params),

	resource_pretty_name(KB, Item, PrettyItem),
	format(atom(Title), 'InfraEngine - ~w', [PrettyItem]),
	get_param(item_title, Params, Title),

	item_uri_ref(KB, ItemType, Item, 
	             [to_ns_only([class='browserHeadingLink'])],
		     HeadingName),
	PrettyHeading = element(td, [class='browserHeading'], HeadingName),
	get_param(item_pretty_heading, Params, PrettyHeading),

	get_param(item_type, Params, PrettyItemType),

	make_navigation_table(KB, ItemType, Item, NavigationTable),
	get_param(navigation_table, Params, NavigationTable).


make_meta_page_general(KB, Item, PrettyItem, ItemType, PrettyItemType,
                       Page, Params) :-
	make_meta_page_general(KB, Item, PrettyItem, ItemType, PrettyItemType,
	                       false, Page, Params).

make_meta_page_general(KB, Item, PrettyItem, ItemType, PrettyItemType, Mini,
                       Page, Params) :-
	( Mini = true ->
	  find_page_template('item_mini.html', Page, Params),
	  HeadingClass='browserHeadingMini'
	; find_page_template('item.html', Page, Params),
	  HeadingClass='browserHeading'
	),
	bind_general_params(Params),
	format(atom(Title), 'InfraEngine - ~w', [PrettyItem]),
	get_param(item_title, Params, Title),
	get_param(item_title, Params, Title),
	PrettyHeading = element(td, [class=HeadingClass], [PrettyItem]),
	get_param(item_pretty_heading, Params, PrettyHeading),
	get_param(item_type, Params, PrettyItemType),
	make_navigation_table(KB, ItemType, Item, NavigationTable),
	get_param(navigation_table, Params, NavigationTable).

	  	  	   	   


	  



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% URI Construction
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 
%% Current version 1.15 of library(url) is of limited use here:
%% parse_url does not construct relative uri-references.
%% parse_url_search/3 in library(url) seems not to work in (-, +) mode,
%% but we can use www_form_encode/2.
%% 

item_uri(KB, Type, Resource, URI) :-
	kb_id(KB, KB1),
	term_to_atom(KB1, KB2),
	term_to_atom(Resource, Resource1),
	www_form_encode(KB2, KB3),
	www_form_encode(Resource1, Resource2),
	concat_atom(['/item?kb=', KB3,
	             '&resource=', Resource2,
		     '&type=', Type],
		    URI).

toc_uri(KB, Type, URI) :-
	kb_id(KB, KB1),
	term_to_atom(KB1, KB2),
	www_form_encode(KB2, KB3),
	concat_atom(['/toc?kb=', KB3, '&type=', Type], URI).

toc_uri(KB, Type, FromResource, URI) :-
	kb_id(KB, KB1),
	term_to_atom(KB1, KB2),
	www_form_encode(KB2, KB3),
	item_fragment_id(FromResource, Fragment),
	concat_atom(['/toc?kb=', KB3, '&type=', Type, '#', Fragment], URI).

item_fragment_id(Resource, Name) :-
	term_to_atom(Resource, Resource1),
	www_form_encode(Resource1, Name).

help_uri(Fragment, URI) :-
	concat_atom(['/static/doc/manual.html#', Fragment], URI).

form_uri(KB, Type, URI) :-
	kb_id(KB, KB1),
	term_to_atom(KB1, KB2),
	www_form_encode(KB2, KB3),
	concat_atom(['/form?kb=', KB3, '&type=', Type], URI).

command_uri(KB, Type, URI) :-
	kb_id(KB, KB1),
	term_to_atom(KB1, KB2),
	www_form_encode(KB2, KB3),
	concat_atom(['/command?kb=', KB3, '&type=', Type], URI).

once_uri(KB, File, URI) :-
	kb_id(KB, KB1),
	term_to_atom(KB1, KB2),
	www_form_encode(KB2, KB3),
	www_form_encode(File, File1),
	concat_atom(['/once?kb=', KB3, '&name=', File1], URI).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Reference Sequences
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

item_uri_refs(_, _, [], _, ['-']) :-
	!.
item_uri_refs(KB, Type, Resources, Options, Contents) :-
	map_item_uri_ref(Resources, KB, Type, Options, SortkeyAndRefs),
	sort(SortkeyAndRefs, SortkeyAndRefs1),
	map_valuemerge(SortkeyAndRefs1, Contents).

map_item_uri_ref([X|Xs], Y1, Y2, Options, [Pretty-Ref|Xs1]) :-
	item_uri_ref(Y1, Y2, X, [sortkey(Pretty)|Options], Ref),
	map_item_uri_ref(Xs, Y1, Y2, Options, Xs1).
map_item_uri_ref([], _, _, _, []).

map_valuemerge([_-Vs], Vs) :-
	!.
map_valuemerge([_-Vs|Xs], Xs1) :-
	append(Vs, [', '| Xs2], Xs1),
	map_valuemerge(Xs, Xs2).
map_valuemerge([], []).

%%%%
%%%% Works on arbitrary RDF items, i.e. resources and also literals.
%%%%
%%%% Result is a sequence of contents, e.g. one href [Item] or two
%%%% hrefs [Namespace, Item] or [Text] for literals.
%%%% 
item_uri_ref(KB, Type, literal(Literal), Options, Elements) :-
	!,
	( Type \= object, Type \= text -> 
          msg('Warning: literal ~q in ~q role.', [Literal, Type])
	; true
	),
	( atom(Literal), Type == text ->
	  Elements = [element(p, [], [Literal])]
	; atom(Literal) ->
          concat_atom(['"', Literal, '"'], Literal2),	    
	  Elements = [element(pre, [class=browserLiteral], [Literal2])]
        ; Literal = [_|_] ->
	  findall(A-N, fact(KB, N, sys_abbreviation, literal(A)), Ns),
	  with_output_to(atom(Literal1),
			 write_structure(Literal, [indent(4), namespaces(Ns)])),
	  concat_atom(['"', Literal1, '"'], Literal2),	    	    
	  Elements = [element(pre, [class=browserXMLLiteral], [Literal2])]
	; err('Bad literal: ~q.', [Literal])
	),
	( memberchk(sortkey(Literal2), Options) -> true ; true ).
item_uri_ref(KB, Type, Resource, Options, Elements) :-
	atom(Resource),
	!,
	item_uri(KB, Type, Resource, Uri),
	resource_pretty_name(KB, Resource, Namespace, NsAbbrev, Name),
	( memberchk(to_ns_only(NsRefAtts), Options) ->
	  NameRef = Name
	; NameRef = element(a, [href=Uri|AnchorAtts], [Name]),
	  NsRefAtts = []
	),
	( memberchk(anchor, Options),
	  \+ ( memberchk(anchor_exclude(AnchorExclude), Options),
	       memberchk(Resource, AnchorExclude) ) ->
	  item_fragment_id(Resource, Anchor), 
	  AnchorAtts = [name=Anchor]
	; AnchorAtts = []
	),
	( memberchk(sortkey(Sortkey), Options) -> true ; true ),
	( memberchk(brief_comment, Options),
	  find_brief_comment(KB, Resource, Comment) ->
	  Comments = [' ', element(span, [class='browserComment'], [Comment])]
	; Comments = []
	),
	( var(Namespace) ->
	  Elements = [ NameRef | Comments],
	  Sortkey = Name
	; toc_uri(KB, namespaces, Namespace, NsUri),
	  ( NsAbbrev = '' ->
	    Elements = [NameRef | Comments]
	  ; Elements = [ element(a, [title=Namespace, href=NsUri|NsRefAtts],
	                          [NsAbbrev]),
	               ':',
		       NameRef | Comments]
	  ),
          Sortkey = NsAbbrev:Name
        ).
item_uri_ref(KB, Type, blank(Id), Options, Elements) :- %% *** CHECK
	!,
	Resource = blank(Id),
	item_uri(KB, Type, Resource, Uri),
	resource_pretty_name(KB, Resource, Name),
	( memberchk(to_ns_only(NsRefAtts), Options) ->
	  NameRef = Name
	; NameRef = element(a, [href=Uri], [Name]),
	  NsRefAtts = []
	),
	( memberchk(sortkey(Sortkey), Options) -> true ; true ),
	( var(Namespace) ->
	  Elements = [ NameRef ],
	  Sortkey = skolem(Name,1,2)
	; toc_uri(KB, namespaces, Namespace, NsUri),
	  Elements = [ element(a, [href=NsUri|NsRefAtts], [NsAbbrev]),
	               ':',
		       NameRef ],
          Sortkey = NsAbbrev:skolem(Name,1,2)
        ).
item_uri_ref(KB, Type, pair(A, B), Options, Elements) :-
	!,
	( select(sortkey(SKA-SKB), Options, Options1) ->
	  OptionsA = [sortkey(SKA)|Options1],
	  OptionsB = [sortkey(SKB)|Options1]
	; OptionsA = Options,
	  OptionsB = Options
	),
	item_uri_ref(KB, Type, A, OptionsA, EA),
	item_uri_ref(KB, Type, B, OptionsB, EB),
	append(EB, [')'], EB1),
	append(['('| EA], [', '| EB1], Elements).
item_uri_ref(_, Type, Resource, _, _) :-
	err('Cannot make reference to ~q ~q.', [Type, Resource]).

find_brief_comment(KB, Resource, Comment) :-
	fact(KB, Resource, rdfs_comment, literal(Comment1)),
	!,
	config(pages_brief_comment_size, MaxLenAtom),
	term_to_atom(MaxLen, MaxLenAtom),
	abbreviate(Comment1, MaxLen, Comment).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Template Cache
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(page_template_cache/3).

%%%% 
%%%% for easier debugging of template pages:
%%%% 
:- retractall(page_template_cache(_, _, _)).

find_page_template(Name, Template, Params) :-
	page_template_cache(Name, Template, Params),
	!.
find_page_template(Name, Template, Params) :-
	load_page_template(Name, Template, Params),
	assert(page_template_cache(Name, Template, Params)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

writeable_kb(KB, KB1) :-
	find_catalog_kb(KB, CatalogKB),
	fact(CatalogKB, K1, rdf_type, sys_Knowledgebase),
	kb_user(KB, User),
	kb_groups(KB, Groups),
	make_kb(K1, User, Groups, KB1),
	has_write_permission(KB1).

readable_kb(KB, KB1) :-
	find_catalog_kb(KB, CatalogKB),
	fact(CatalogKB, K1, rdf_type, sys_Knowledgebase),
	kb_user(KB, User),
	kb_groups(KB, Groups),
	make_kb(K1, User, Groups, KB1),
	has_read_permission(KB1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Generate a KB
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_kb(User, Groups, Comment, GeneratedKB) :-
	kb_user(KB, User),
	kb_groups(KB, Groups),
	make_permissions(y, y, n, n, n,n, Permissions),
	gensym(sys_gen, Id),
	make_kb(Id, User, Groups, GeneratedKB),
	Owner = User,
	Group = User,
	create_knowledgebase(GeneratedKB, 
	                      [ owner=Owner,
			        group=Group,
				permissions=Permissions,
				comment=Comment] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% XML Helpers
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xml_select(Name, Attributes, Values, Result) :-
	xml_select(Name, Attributes, Values, _Selected, Result).
	
xml_select(Name, Attributes, Values, Selected,
                 [element(select, [name=Name|Attributes], Options)]) :-
	map_sel_option(Values, Selected, Options).

sel_option(Pretty-Value, Selected, 
           element(option, [value=Value|Sel], [Pretty])) :-
	!,
	( Value == Selected -> Sel = [selected=selected] ; Sel = [] ).
sel_option(Value, Selected, element(option, [value=Value|Sel], [Value])) :-
	( Value == Selected -> Sel = [selected=selected] ; Sel = [] ).

map_sel_option([X|Xs], S, [X1|Xs1]) :-
	sel_option(X, S, X1),
	map_sel_option(Xs, S, Xs1).
map_sel_option([], _, []).


xml_select_item(KB, Name, Attributes, Values, Selected,
                 [element(select, [name=Name|Attributes], Options)]) :-
	map_itemsel(Values, KB, Values1),
	( nonvar(Selected) ->
	  public_item(Selected, PublicSelected)
	; true
	),
	map_sel_option(Values1, PublicSelected, Options).

map_itemsel([X|Xs], Y1, [X1|Xs1]) :-
	itemsel(X, Y1, X1),
	map_itemsel(Xs, Y1, Xs1).
map_itemsel([], _, []).

itemsel(Pretty-Item, _, Pretty-PublicItem) :-
	!,
	public_item(Item, PublicItem).
itemsel(Item, KB, Pretty-PublicItem) :-
	resource_pretty_name(KB, Item, Pretty),
	public_item(Item, PublicItem).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


number_of_classes(KB, K, [literal(N)]) :-
	find_other_kb(KB, K, KB1),
	setof(X, fact(KB1, X, rdf_type, rdfs_Class), Xs),
	!,
	length(Xs, L),
	term_to_atom(L, N).
number_of_classes(_, _, [literal('0')]).

number_of_properties(KB, K, [literal(N)]) :-
	find_other_kb(KB, K, KB1),
	setof(X, fact(KB1, X, rdf_type, rdf_Property), Xs),
	!,
	length(Xs, L),
	term_to_atom(L, N).
number_of_properties(_, _, [literal('0')]).

number_of_triples(KB, K, [literal(N)]) :-
	find_other_kb(KB, K, KB1),
	findall(k, fact(KB1, _, _, _), Xs),
	!,
	length(Xs, L),
	term_to_atom(L, N).
number_of_triples(_, _, [literal('0')]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Views
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

content_view_refs(KB, Refs) :-
	toc_uri(KB, rdf, KBRDFUri),
	toc_uri(KB, plrdf, KBPLRDFUri),
	toc_uri(KB, dotgraph, KBDotgraphUri),
	toc_uri(KB, rdfsdtd, KBDTDRDFSUri),
	Refs = [element(a, [href=KBRDFUri], ['Content as RDF']),
		', ',
		element(a, [href=KBPLRDFUri], ['Content as PLRDF']),
		', ',
		element(a, [href=KBDotgraphUri], ['Content as Graph']),
		', ',
		element(a, [href=KBDTDRDFSUri], ['RDFS DTD'])
		].

make_view_refs(KB, SourceType, Item, Refs) :-
   
	item_uri(KB, rdf, Item, RDFUri),
	RDFRef = element(a, [href=RDFUri], ['RDF']),
	item_uri(KB, plrdf, Item, PLRDFUri),
	PLRDFRef = element(a, [href=PLRDFUri], ['PLRDF']),

	( SourceType=dotgraph ->
	  item_uri(KB, object, Item, ObjectUri),
	  ObjectRef = element(a, [href=ObjectUri], ['Object']),  
	  General = [ObjectRef, ', ', RDFRef, ', ', PLRDFRef]
        ; item_uri(KB, dotgraph, Item, DotgraphUri),
	  DotgraphRef = element(a, [href=DotgraphUri], ['Graph']),
	  General = [RDFRef, ', ', PLRDFRef, ', ', DotgraphRef]
	),
	
	( ( SourceType=class
	  ; SourceType=property
	  ; SourceType=kb
	  ; SourceType=solution
	  ; SourceType=query ) ->
	  item_uri(KB, object, Item, Uri),
	  Refs = [ element(a, [href=Uri], ['Object']), ', ' | General]
        ; ( SourceType=object
	  ; SourceType=dotgraph ) ->
          ( fact(KB, Item, rdf_type, rdfs_Class) ->
	    item_uri(KB, class, Item, Uri),
	    Refs = [ element(a, [href=Uri], ['Class']), ', ' | General]
	  ; fact(KB, Item, rdf_type, rdf_Property) ->
	    item_uri(KB, property, Item, Uri),
	    Refs = [ element(a, [href=Uri], ['Property']), ', ' | General]
	  ; fact(KB, Item, rdf_type, inf_Query) ->
	    item_uri(KB, query, Item, Uri),
	    Refs = [ element(a, [href=Uri], ['Query']), ', ' | General]
	  ; fact(KB, Item, rdf_type, inf_Solution) ->
	    item_uri(KB, solution, Item, Uri),
	    Refs = [ element(a, [href=Uri], ['Solution']), ', ' | General]
	  ; find_catalog_kb(KB, CatalogKB),
	    fact(CatalogKB, Item, rdf_type, sys_Knowledgebase) ->
	    item_uri(KB, kb, Item, Uri),
	    Refs = [ element(a, [href=Uri], ['Knowledgebase']), ', ' | General]
	  ; Refs = General
	  )
        ; Refs = []
	).

