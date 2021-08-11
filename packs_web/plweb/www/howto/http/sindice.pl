:- module(sindice,
	  [ sindice/2			% +Query, -URI
	  ]).
:- use_module(library(rdf)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/http_open)).
:- use_module(library(uri)).

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


%%	sindice(+Query, -Result) is nondet.
%
%	Result is a result-uri for Query on Sindice.  Query is one of:
%
%	    * uri(URI)
%	    * keyword(Keyword)

sindice(Query, Result) :-
	sindice_query_url(Query, QueryURL),
	rdf_extra_headers(Options),
	setup_call_cleanup(http_open(QueryURL, In, Options),
			   load_rdf(In, Triples),
			   close(In)),
	rdf_equal(rdfs:seeAlso, SeeAlso),
	member(rdf(_, SeeAlso, Result), Triples).

rdf_extra_headers(
	[ request_header('Accept' = 'text/rdf+xml,\
				     application/rdf+xml; q=0.9, \
				     text/turtle,\
				     application/x-turtle; q=0.8, \
				     */*; q=0.1')
	]).

%%	sindice_host(-Host)
%
%	Location of the Sindice server

sindice_host('sindice.com').
sindice_path('/query/lookup').

%%	sindice_query_url(+Query, -URL)
%
%	URL is the URL to send to Sindice for Query. Query is one of:
%
%	    * uri(URI)
%	    * keyword(Keyword)


sindice_query_url(Query, QueryURL) :-
	Query =.. [What, Value],
	uri_query_components(Search, [What=Value]),
	sindice_host(Host),
	sindice_path(Path),
	uri_data(scheme, Components, http),
	uri_data(authority, Components, Host),
	uri_data(path, Components, Path),
	uri_data(search, Components, Search),
	uri_components(QueryURL, Components).



