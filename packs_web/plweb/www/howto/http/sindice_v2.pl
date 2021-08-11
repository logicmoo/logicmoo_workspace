:- module(sindice,
	  [ sindice/2			% +Query, -URI
	  ]).
:- use_module(library(rdf)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/http_open)).
:- use_module(library(uri)).
:- use_module(library(record)).

/** <module> Access the Sindice Semantic Web search engine

This module runs queries on the Sindice   Semantic Web search engine and
returns URIs that match the given query.

This module is based on sindice.pl from Yves Raimond and rewritten by
Jan Wielemaker.  Main changes:

    * Simplify the API and enhance code-reuse
    * Base URI handling on the new library(uri)
    * Use PlDoc comments
    * Do not use the RDF database.

@author Yves Raimond, C4DM, Queen Mary, University of London
@author Jan Wielemaker
@see    http://sindice.com/
@tbd	Sindice V2: http://sindice.com/developers/api#QueryLanguages
*/

:- record
	sindice(title:atom,
		link:atom,
		created:float).

:- rdf_register_ns(sindice, 'http://sindice.com/vocab/search#').
:- rdf_register_ns(dc,      'http://purl.org/dc/elements/1.1/').

%%	sindice(+Query, -Result) is nondet.
%
%	Result is a result-uri for Query on   Sindice.  Query is a value
%	that is acceptable for a sindice =term= query.

sindice(Query, Result) :-
	sindice_query_url(Query, 1, QueryURL),
	rdf_extra_headers(Options),
	setup_call_cleanup(http_open(QueryURL, In, Options),
			   load_rdf(In, Triples),
			   close(In)),
	v2_results(Triples, Result).

rdf_extra_headers(
	[ request_header('Accept' = 'text/rdf+xml,\
				     application/rdf+xml; q=0.9, \
				     text/turtle,\
				     application/x-turtle; q=0.8, \
				     */*; q=0.1')
	]).

v2_results(RDF, Result) :-
	rdf_equal(C_Result, sindice:'Result'), % Docs say Entry!
	rdf_equal(P_a, rdf:type),
	rdf_equal(P_title, dc:title),
	rdf_equal(P_link, sindice:link),
	rdf_equal(P_created, dc:created),

	member(rdf(Entry, P_a, C_Result), RDF),
	memberchk(rdf(Entry, P_title, literal(Title)), RDF),
	memberchk(rdf(Entry, P_link, Link), RDF),
	memberchk(rdf(Entry, P_created, L_Created), RDF),
	text_of(L_Created, CreatedText),
	parse_time(CreatedText, Created),
	make_sindice([title(Title), link(Link), created(Created)], Result).

text_of(literal(X), T) :- !,
	text_of(X, T).
text_of(lang(_, T), T) :- !.
text_of(type(_, T), T) :- !.
text_of(T, T).

%%	sindice_host(-Host)
%
%	Location of the Sindice server

sindice_host('api.sindice.com').
sindice_path('/v2/search').

%%	sindice_query_url(+Query, +Page, -URL)
%
%	URL is the URL to send to Sindice for Query. Query is one of:
%
%	    * uri(URI)
%	    * keyword(Keyword)


sindice_query_url(Query, Page, QueryURL) :-
	uri_query_components(Search, [q=Query, qt=term, page=Page]),
	sindice_host(Host),
	sindice_path(Path),
	uri_data(scheme, Components, http),
	uri_data(authority, Components, Host),
	uri_data(path, Components, Path),
	uri_data(search, Components, Search),
	uri_components(QueryURL, Components).



