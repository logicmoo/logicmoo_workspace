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
%%%% Pages Proxy
%%%%
%%%% Specializes Webproxy.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(pages_proxy, 
          [ bodytop/5,
	    stylesheet_link/2,
	    proxy_input_form/2]).

:- use_module(pages_util).
:- use_module(pages_browser).
:- use_module(uris).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Headers
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bodytop(KB, SrcPage, _Args, Uri, BodyTop) :-
	BodyTop =
	element(table, [width='100%', class='proxyTable'],
	         [element(tr, [], 
		      [element(td, [class='proxyStd'], Contents)])]),
        rdf_refs(SrcPage, Uri, Refs),
        ( contains_rdf(SrcPage) ->
	  Refs1 = [Uri|Refs]
	; Refs1 = Refs
	),
	( Refs1 = [] ->
          make_browse_top_page(KB, Page1)
	; ( setof(KB1, writeable_kb(KB, KB1), KBs) -> true ; KBs = [] ),
	  map_kb_id(KBs, KBs1),
          Options = [sources=Refs1, 
	             pretty='Add Documents',
		     mini=true,
		     type_in=false,
		     kbs=['Newly Generated'-sys_generate|KBs1]],
          make_form_urisel_page(KB, adddoc, Options, Page1)
	),
	once( xml_tagged(body, Page1, element(_, _, Contents) ) ).
	  

map_kb_id([X|Xs], [X1|Xs1]) :-
	kb_id(X, X1),
	map_kb_id(Xs, Xs1).
map_kb_id([], []).


% TODO: this page if it contains RDF


contains_rdf(Page) :-
	xml_tagged('rdf:rdf', Page, _),
	!.

rdf_refs(Page, BaseUri, Refs) :-
	findall(Ref, rdf_ref(Page, BaseUri, Ref), Refs),
	!.
rdf_refs(_, _, []).

rdf_ref(Page, BaseUri, Ref) :-
	xml_tagged(a, Page, element(_, Atts, _)),
	memberchk(href=Ref1, Atts),
	is_rdf_uri(Ref1),
	resolve_uri(BaseUri, Ref1, Ref).

is_rdf_uri(Ref) :-
	sub_atom(Ref, _, _, 0, '.rdf').
is_rdf_uri(Ref) :-
	sub_atom(Ref, _, _, 0, '.RDF').
is_rdf_uri(Ref) :-
	sub_atom(Ref, _, _, 0, '.rdfs').
is_rdf_uri(Ref) :-
	sub_atom(Ref, _, _, 0, '.RDFS').
is_rdf_uri(Ref) :-
	sub_atom(Ref, _, _, 0, '.plrdf').
is_rdf_uri(Ref) :-
	sub_atom(Ref, _, _, 0, '.PLRDF').

/*

	writeq(bodytop(KB-Args)),
	nl,
	term_to_atom(Args, Args1),
	toc_kb_ref(Args, KBRef),
	BodyTop = 
	  element(table, [width='100%', class='proxyTable'],
	           [element(tr, [], 
		     [element(td, [class='proxyStd'], ['Proxy | ',
		                       Args1, ' | ',
				       element(a, [href='/index.html'],
				                  ['Main']),
				       ' | ', KBRef
				       ])])]).

toc_kb_ref(Args, Ref) :-
	memberchk(kb=KBId, Args),
	!,
	term_to_atom(KBId, KBId1),
	concat_atom(['/toc?type=kb&kb=', KBId1], Uri),
	Ref = element(a, [href=Uri], ['Knowledgebase (', KBId, ')']).
toc_kb_ref(_, '---').
	
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stylesheet_link(_KB, element( link,
                          [ rel = stylesheet,
                            type = 'text/css',
                            href = 'static/stylesheet.css'],
                          [] ) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xml_tagged(Tag, [C|_], E) :-
	xml_tagged_1(Tag, C, E).
xml_tagged(Tag, [_|Cs], E) :-
	xml_tagged(Tag, Cs, E).

xml_tagged_1(Tag, element(Tag, A, C), element(Tag, A, C)).
xml_tagged_1(Tag, element(_, _, C), E) :-
	xml_tagged(Tag, C, E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


proxy_input_form(KB, Page) :-
	!,
	known_html_sources(KB, Sources),
	Options = [ path='/command_proxy',
	            sources=Sources,
		    multiple=false,
		    pretty='Browse the Web' ],
	make_form_urisel_page(KB, proxy, Options, Page).

known_html_sources(KB, Sources) :-
	find_library_kb(KB, LibraryKB),
	findall(T-D, ( fact(LibraryKB, D, rdf_type, sys_HtmlDocument),
                       fact(LibraryKB, D, sys_accessed, T) ),
		     TDs ),
	sort(TDs, TDs1),
	reverse(TDs1, TDs2),
	map_value(TDs2, Sources1),
	std_html_sources(Sources2),
	subtract(Sources2, Sources1, Sources3),
	append(Sources1, Sources3, Sources).

std_html_sources([S1,S2,S3]) :-
	document_file('doc/examples/examples.html', S1F),
	concat_atom(['file://', S1F], S1),
	document_file('doc/manual.html', S2F),
	concat_atom(['file://', S2F], S2),
	S3 = 'http://139.91.183.30:9090/RDF/Examples.html'.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_browse_top_page(KB, Page) :-
	kb_id(KB, Item),
	make_meta_page_general(KB, Item, 'Browse...', display, 
	                       'Command', true, Page, Params),
	get_param(header_style_class, Params, 'browserHeaderCommand'),
	get_param(av_table, Params, ''),   
        check_params(Params).


