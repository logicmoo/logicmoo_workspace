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
%%%% Web Client
%%%% 
%%%% To use get_document with the client(wget) option, the wget program 
%%%% (version >= 1.8.1) must be installed.
%%%% 
%%%% Security relevant configuration parameters:
%%%% 
%%%% - config/webget_file_prefix
%%%% 
%%%%   Only file uris with paths starting with the specified prefix are
%%%%   obtained. If the specified prefix starts not with '/', no file uris
%%%%   are obtained. TODO: Check that no unwanted file name expansion
%%%%   happens in library predicates invoked.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 
% *** Todo: mime type handling, perhaps via magic
% 

:- module(webget, [get_document/3,
                   get_document/4]).

:- use_module('swilib/err').
:- use_module(config).
:- use_module(rdf_read).
:- use_module(rdf_convert).
:- use_module(uris).
:- use_module(mimic).

:- use_module(library(url)).
:- use_module(library(sgml)).
:- use_module(library(socket)).

%%%%
%%%% Options (have no effect for file: scheme URIs):
%%%%
%%%%     offline
%%%%         Only work offline, i.e. retrieve cached documents.
%%%% 
%%%%     noverify
%%%%         Return cached documents as found, without verifying
%%%%         their timestamp.
%%%% 
%%%%     nocache
%%%%         Do not return documents from the cache and do no
%%%%         leave documents in the cache.
%%%%
%%%%     client(wget)
%%%%         Which client implementation method should be used.
%%%%
%%%%     namespaces(Namespaces)
%%%%	     List of namespaces and abbreviations found in document
%%%%         is returned. Works currently only with 'rdf' type.
%%%%         See read_rdf/3.
%%%% 
get_document(Uri, Type, Content) :-
	get_document(Uri, Type, [], Content).
get_document(Uri, Type, Options, Content) :-
	msg('Getting document ~q', [Uri]),
	Options1 = [],
	append(Options, Options1, OptionsE),
	decompose_uri(Uri, Scheme, _Authority, Path, _, _),
	( var(Scheme) ->
	  weberror(document_not_found(Uri), 'Not an absolute URI: ~q.', [Uri])
	; true
	),
	( Scheme = file ->
	    %% 	    
	    %% Allow "file:" access to files in the document directory,
	    %% paths are verified by calling document_file.
	    %% 	
	  ( atom(Path),
	    document_file('', DocumentRoot),
	    sub_atom(Path, 0, Len, After, DocumentRoot),
	    sub_atom(Path, Len, After, 0, BasePath),  
	    document_file(BasePath, CacheFileName),
	    exists_file(CacheFileName) ->
	    true
	  ; weberror(document_not_found(Uri), 'Document not found: ~q', [Uri])
	  ),
	  Mode = file
	; Mode = web,
          ( memberchk(offline, OptionsE) ->
	    ( webcache_get(Uri, CacheFileName) ->
	      true
	    ; weberror(document_not_found(Uri),
		       'Document not found offline: ~q', [Uri])
	    )
	  ; memberchk(noverify, OptionsE),
	    webcache_get(Uri, CacheFileName) ->
	    true
	  ; ( memberchk(nocache, OptionsE) ->
	      CacheOptions = []
	    ; CacheOptions = [timestamping]
	    ),
	    config(webget_client, Client),
	    ( cache_web_page(Client, Uri, CacheFileName, CacheOptions) ->
              true
	    ; weberror(document_not_found(Uri), 'Document not found: ~q', [Uri])
	    )
	  )
	),
	( Type = lousy_html ->
	  load_lousy_html_file(CacheFileName, Content)
	; Type = html ->
	  load_html_file(CacheFileName, Content)
	; Type = xml ->
	  load_xml_file(CacheFileName, Content)
	  %% *** CHECK OPTIONS, concat_namespaces...
	; Type = sgml ->
	  load_sgml_file(CacheFileName, Content)
	; Type = rdf ->
	  ( memberchk(namespaces(Namespaces), OptionsE) ->
	    RdfOpts = [namespaces(Namespaces)]
	  ; RdfOpts = []
	  ),
	  read_rdf(CacheFileName, Content1, [base_uri(Uri)|RdfOpts]),
	  canonicalize_triples(Content1, Content)
	; Type = plrdf ->
	  ( memberchk(namespaces(Namespaces), OptionsE) ->
	    true
	  ; true
	  ),
	  mimic_file_triples(CacheFileName, Content, Namespaces)
	; Type = file ->
	  Content = file(CacheFileName)
	; weberror(document_not_found(Uri),
		   'Unsupported document type specified: ~q.', [Type])
	),
	( Mode = web, memberchk(nocache, OptionsE) ->
	  delete_file(CacheFileName)
	; true
	),
	msg('Finished getting document ~q', [Uri]).

load_lousy_html_file(File, Content) :-
	dtd(html, DTD),
	Options = [ dialect(sgml),
	            shorttag(false),
		    dtd(DTD),
		    syntax_errors(quiet),
		    max_errors(1000) ],
	load_structure(File, Content, Options).

find_cache_dir(CacheDir) :-
	config(webget_cache_dir, CacheDir1),
	expand_file_name(CacheDir1, [CacheDir|_]).

%%%% 
%%%% - Tested with wget 1.8.1
%%%% - This does not work for all uris
%%%% 
%%%% - Unfortunately wget does not allow arbitray dates as
%%%%   timestamps. Only files in its tree may be used as timestamps.
%%%% - Perhaps wget should offer a mode that allows abstracting
%%%%   from its path construction: e.g. print contents of given URIs to
%%%%   stdout or files while retrieving them from its cache or the web.
%%%% - Status returned by wget seems only relate to the last URI
%%%% 
wget_file_name(Uri, FileName) :-
	( parse_url(Uri, Parsed) ->
	  true
	; err('Cannot parse uri: ~q.', [Uri])
	),
	memberchk(host(Host), Parsed),
	memberchk(path(Path), Parsed),
	( memberchk(search(_), Parsed) ->
	  sub_atom(Uri, SB, _, SA, '?'),
	  SL1 is SA + 1,
	  sub_atom(Uri, SB, SL1, _, Search)
	; Search = ''
	),
	wget_canonic_search(Search, Search1),
	wget_canonic_path(Path, Path1),
	concat_atom([Host, Path1, Search1], FileName).

wget_canonic_search(S, S1) :-
	atom_codes(S, S2),
	cus_1(S2, S3),
	atom_codes(S1, S3).

cus_1([0'%, 0'2, 0'd|Cs], [0'-|Cs1]) :-
        !, 
        cus_1(Cs, Cs1).
cus_1([C|Cs], [C|Cs1]) :-
	cus_1(Cs, Cs1).
cus_1([], []).

wget_canonic_path(Path, Path1) :-
	atom_codes(Path, P2),
	cup_1(P2, P3),
	atom_codes(Path1, P3).

cup_1([0'/], [0'/, 0'i, 0'n, 0'd, 0'e, 0'x, 0'., 0'h, 0't, 0'm, 0'l]) :-
	!.
cup_1([0'~|Cs], [0'%, 0'7, 0'E|Cs1]) :-
        !, 
        cup_1(Cs, Cs1).
cup_1([C|Cs], [C|Cs1]) :-
	cup_1(Cs, Cs1).
cup_1([], []).

cache_web_page(wget, Uri, CacheFileName, Options) :-
	!,
	cache_web_page_with_wget(Uri, CacheFileName, Options).
cache_web_page(Client, _, _, _) :-
	err('No such Web client implementation: ~q.', [Client]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Web access using the wget program.
%%%%
%%%% Requires the wget program (version >= 1.8.1) installed.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cache_web_page_with_wget(Uri, CacheFileName, Options) :-
	tmp_file(wget_cache, CacheFileName),
	CanonicUris = [Uri],
	config(webget_wget_parameters, Params1),
	( memberchk(timestamping, Options) ->
	  Params2 = ['--timestamping', Params1]
	; Params2 = [Params1]
	),
	tmp_file('wget.log', LogFile),
        concat_atom(['--append-output=', LogFile], ParamAppendOutput),
	Params3 = [ParamAppendOutput,
	           '-O', CacheFileName|Params2],
	map_wget_uri_arg(CanonicUris, UriParams),
	append(Params3, UriParams, Params4),
	concat_atom(['umask 0077', ';', wget | Params4], ' ', Command),
	( shell(Command) ->
	  ( exists_file(CacheFileName) ->
	    webcache_set(Uri, CacheFileName)
	  ; fail
	  )
	; weberror(document_not_found(Uri), 'Command failed: ~q.', Command)
	).

map_wget_uri_arg([X|Xs], [X1|Xs1]) :-
	concat_atom(['"', X, '"'], X1),
	map_wget_uri_arg(Xs, Xs1).
map_wget_uri_arg([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic uri_cachefile/2.

webcache_get(Uri, File) :-
	once(uri_cachefile(Uri, File)),
	exists_file(File).

webcache_set(Uri, File) :-
	retractall(uri_cachefile(Uri, _)),
	assert(uri_cachefile(Uri, File)).

webcache_clear(Uri) :-
	retractall(uri_cachefile(Uri)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

weberror(document_not_found(Url), Format, Args) :-
	!,
	msg(Format, Args),
	throw(http_reply(not_found(Url))).
weberror(_, Format, Args) :-
	err(Format, Args).
	