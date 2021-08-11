:- module(lod,
	  [ lod_load/1,			% +URL
	    sindice_query/3,		% +Query, +Page, -URL
	    uri_label/2,		% +URI, -Label
	    text_of_literal/2		% +Literal, -Text
	  ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(uri)).
:- use_module(library(semweb/rdf_turtle)).
:- use_module(library(semweb/rdf_http_plugin)).

/** <module> Simple Linked Open Data query facility

*/

% Common RDF prefixes

:- rdf_register_ns(skos,	  'http://www.w3.org/2004/02/skos/core#').
:- rdf_register_ns(sindice,	  'http://sindice.com/vocab/search#').
:- rdf_register_ns(dbpprop,	  'http://dbpedia.org/property/').
:- rdf_register_ns(dbpedia,	  'http://dbpedia.org/resource/').
:- rdf_register_ns('dbpedia-owl', 'http://dbpedia.org/ontology/').
:- rdf_register_ns(dcterms,	  'http://purl.org/dc/terms/').
:- rdf_register_ns(foaf,	  'http://xmlns.com/foaf/0.1/').

%%	lod_load(+URL) is det.
%
%	Cached querying of Linked Open Data. First, we remove a possible
%	fragment identifier (#fragment), because   fragment  identifiers
%	are a client-side issue rather than a server-side issue.
%
%	@error	domain_error(content_type, 'RDF') is raised if the URL
%		contains no RDF data.  Note that rdf_load/1 already
%		raises this error if the MIME-type is incorrect.

lod_load(URI) :-
	url_sans_fragment(URI, URI2),
	(   rdf_graph(URI2)
	->  true
	;   rdf_load(URI2),
	    (	rdf_graph(URI2)
	    ->	true
	    ;	domain_error(content_type, 'RDF')
	    )
	).

url_sans_fragment(URI, URI2) :-
	uri_components(URI, Components),
	copy_components([scheme, authority, path, search],
			Components, Components2),
	uri_components(URI2, Components2).

copy_components([], _, _).
copy_components([H|T], In, Out) :-
	uri_data(H, In, Data),
	uri_data(H, Out, Data),
	copy_components(T, In, Out).


%%	sindice_query_url(+Query, +Page, -URL)
%
%	URL is the URL to send to Sindice  for Query. Query is a Sindice
%	_term_ query argument.

sindice_query(Query, Page, QueryURL) :-
	uri_query_components(Search, [q=Query, qt=term, page=Page]),
	sindice_host(Host),
	sindice_path(Path),
	uri_data(scheme, Components, http),
	uri_data(authority, Components, Host),
	uri_data(path, Components, Path),
	uri_data(search, Components, Search),
	uri_components(QueryURL, Components).

sindice_host('api.sindice.com').
sindice_path('/v3/search').


%%	uri_label(+URI, -Label:atom) is det.
%
%	Generate a display label for URI.

uri_label(URI, Label) :-
	display_label_property(P),
	rdf_has(URI, P, literal(Lit)),
	text_of_literal(Lit, Label), !.
uri_label(URI, Label) :-
	rdf_global_id(NS:Local, URI),
	Local \== '', !,
	atomic_list_concat([NS, Local], :, Label).
uri_label(URI, URI).


display_label_property(P) :- rdf_equal(dc:title, P).
display_label_property(P) :- rdf_equal(skos:prefLabel, P).
display_label_property(P) :- rdf_equal(rdfs:label, P).

%%	text_of_literal(+LiteralArg, -Text)
%
%	Text is the raw text of   a possibly typed of language-qualified
%	RDF literal.

text_of_literal(literal(Lit), Text) :- !,
	text_of_literal(Lit, Text).
text_of_literal(lang(_, Text), Text) :- !.
text_of_literal(type(_, Text), Text) :- !.
text_of_literal(Text, Text).
