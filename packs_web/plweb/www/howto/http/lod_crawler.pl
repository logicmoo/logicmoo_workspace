:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_head)).		% new

:- use_module(library(semweb/rdf_db)).
:- use_module(lod).

:- http_handler(root(.),	home,	  []).
:- http_handler(root(search),	search,	  []).
:- http_handler(root(resource),	resource, []).
:- http_handler(css('ptable.css'),  http_reply_file('ptable.css', []), []).
:- http_handler(css('sindice.css'), http_reply_file('sindice.css', []), []).

http:location(css, root(css), []).

%%	server(?Port)
%
%	Start the HTTP server, listening to Port.

server(Port) :-
	http_server(http_dispatch, [port(Port)]).

%%	home(+Request)
%
%	Privides the initial page of the LOD-crawler with a form
%	to search on http://sindice.com

home(_Request) :-
	reply_html_page(title('LOD Crawler'),
			[ h1(class(title), 'LOD Crawler'),
			  p(class(banner),
			    [ 'Welcome to the SWI-Prolog Linked Open Data ',
			      'crawler.  To start your experience, enter a ',
			      'search term such as "Amsterdam".'
			    ]),
			  \search_form
			]).

search_form -->
	{ http_link_to_id(search, [], Ref) },
	html([ \html_requires(css('sindice.css')),
	       form([id(search), action(Ref)],
		    [ input(name(q)),
		      input([type(submit), value('Search')])
		    ])
	     ]).


%%	search(+Request)
%
%	Submit the query to sindice and  show the result. Sindice almost
%	behaves  as  `dynamic  linked  open    data'  by  providing  the
%	search-results as properties of the query,   but not quite. This
%	means we must pick-up the  result-page   URI  from  the returned
%	named graph.

search(Request) :-
	http_parameters(Request, [q(Query, [])]),
	sindice_query(Query, 1, URL),
	lod_load(URL),
	(   rdf(URI, _, _, URL)		% work around Sindice bug
	->  resource_page(URI)
	).


%%	resource(+Request)
%
%	Explore a linked open-data resource. We distinguish three cases:
%
%	    1. If lod_load/1 succeeds, show the loaded triples
%	    2. If lod_load/1 indicates that the returned document
%	       has an unknown content-type, it appearenly is not LOD
%	       and we redirect the client to the web-page.
%	    3. Else, we re-throw the error, generating an error-page.

resource(Request) :-
	http_parameters(Request, [r(URI, [])]),
	catch(lod_load(URI), E, true),
	(   var(E)
	->  resource_page(URI)
	;   subsumes_term(error(domain_error(content_type, _), _), E)
	->  throw(http_reply(moved_temporary(URI)))
	;   throw(E)
	).


resource_page(URL) :-
	uri_label(URL, Label),
	findall(P-O, rdf(URL, P, O), Pairs0),
	sort(Pairs0, Pairs),
	group_pairs_by_key(Pairs, Grouped),
	reply_html_page(title('LOD Crawler --- ~w'-[Label]),
			[ h1(class(title), a(href(URL), Label)),
			  \property_table(Grouped)
			]).


property_table(Grouped) -->
	html([ \html_requires(css('ptable.css')),
	       table(class(properties),
		     [ \ptable_header
		     | \ptable_rows(Grouped)
		     ])
	     ]).

ptable_header -->
	html(tr([th('Predicate'), th('Object')])).

ptable_rows(Grouped) -->
	ptable_rows(Grouped, 1).

ptable_rows([], _) -->
	[].
ptable_rows([H|T], Row) -->
	{ Row1 is Row + 1 },
	ptable_row(H, Row),
	ptable_rows(T, Row1).

ptable_row(P-VL, Row) -->
	{ ( Row mod 2 =:= 0 -> Class = even ; Class = odd ) },
	html(tr(class(Class),
		[ td(class(predicate), \rlink(P)),
		  td(class(object),    ul(\vlist(VL)))
		])).

vlist([]) --> [].
vlist([H|T]) --> html(li(\vdiv(H))), vlist(T).

vdiv(literal(L)) --> !,
	{ text_of_literal(L, Text) },
	html(div(class(lvalue), Text)).
vdiv(R) -->
	html(div(class(rvalue), \rlink(R))).

rlink(P) -->
	{ uri_label(P, Label),
	  uri_iri(URI, P),
	  http_link_to_id(resource, [r=URI], HREF)
	},
	html(a(href(HREF), Label)).

%%	body(+Content)//
%
%	Define overall style. This hook into reply_html_page/2 is called
%	to translate the 2nd argument. It is searched for in the current
%	module as well as the user-module.
%
%	Redefining head//1 or body//1 is a   way to redefine the overall
%	page-style of all pages served.

body(Content) -->			% contents already provides a form
	{ sub_term(\search_form, Content)
	}, !,
	html(Content).
body(Content) -->			% add header with search-form
	html([ div(class(top), \search_form)
	     | Content
	     ]).
