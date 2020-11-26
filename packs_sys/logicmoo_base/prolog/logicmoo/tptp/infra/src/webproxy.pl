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
%%%% Web Proxy
%%%% 
%%%% General Proxy Stuff.
%%%% Special stuff is for now just imported from pages_proxy.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(webproxy, []).


:- use_module(library('http/http_dispatch')).
:- use_module(library('http/http_parameters')).
:- use_module(pages_proxy).

:- use_module(webget).
:- use_module(pages_util).
:- use_module(mime_types).
:- use_module(uris).
:- use_module(library(url)).

:- use_module(knowledgebase). % just the library for now ***
:- use_module('swilib/err').

%%
%% *** TODO: mime type by magic
%%           parse error handling
%%           more "heuristics" ? 
%%           redirection handling
%%


:- http_handler_options(O),
   http_handler('/form_proxy',     reply_form_proxy, O),
   http_handler('/command_proxy',  reply_command_proxy, O),
   http_handler(prefix('/proxy/'), reply_proxy, O).


reply_form_proxy(Request) :-
	find_kb(Request, KB),
	proxy_input_form(KB, Page),
	webserver_reply(Page, 'text/html').
	
reply_command_proxy(Request) :-
	find_kb(Request, KB),
	( get_form(Request, Form),
	  member(uri=Uri, Form),
	  trim_atom(Uri, Uri1),
	  Uri1 \= '' ->
	  decompose_uri(Uri1, S, _, _, _, _),
	  ( var(S) ->
	    concat_atom(['http://', Uri1], Uri2)
	  ; Uri2 = Uri1
	  ),
	  ( select(uri=_, Form, Args1) -> true ; Args1 = Form ),
	  ( select(type=_, Args1, Args) -> true ; Args = Args1 ),
	  proxy_reply(get(Args, Uri2), KB, [], Request, Page, MimeType)
	; proxy_input_form(KB, Page),
	  MimeType = 'text/html'
	),
	webserver_reply(Page, MimeType).

reply_proxy(Request) :-
	memberchk(path(Path), Request),
	sub_atom(Path, 0, B, L, '/proxy/'),
	sub_atom(Path, B, L, _, Cmd),
 	www_form_encode(Cmd1, Cmd),
	term_to_atom(Cmd2, Cmd1),
	( nonvar(Cmd2), Cmd2 = get(_, _) -> 
	  get(Args, _) = Cmd2,
	  ( \+ ground(Args) ->
	    err('Nonground parameters: ~q.', [Args])
	  ; true
	  ),
	  find_kb(Request, Args, KB)
	; err('Bad proxy path: ~q.', [Cmd2])
	),
	http_parameters(Request, [], [form_data(Form)]),
	( proxy_reply(Cmd2, KB, Form, Request, Page, MimeType) ->
	  webserver_reply(Page, MimeType),
	  true
	; err('Proxy reply failed.')
	).

proxy_reply(get(Args, Uri), KB, Form, Request, Page, MimeType) :-
	decompose_uri(Uri, S, A, P, Q, _F),
	( var(S) -> err('Relative URI: ~q.', [Uri]) ; true ),
	( var(P) -> P = '' ; true ),
	( Form = [] ->
	  Q1 = Q,
	  file_name_guess_mime_type(P, MimeType)
	; encode_form(Form, Q1),
	  MimeType = 'text/html'
	),
	( var(A) ->
	  concat_atom([S, ':', P], Uri1)
	; concat_atom([S, '://', A, P], Uri1)
	),
	( var(Q1) ->
	  Uri2 = Uri1
	; concat_atom([Uri1, '?', Q1], Uri2)
	),
	( MimeType = 'text/html' ->
	  get_document(Uri2, lousy_html, Page1),
	  %% get_document here is assumed to canonicalize tags to lowercase.
	  refine_document(KB, Page1, Args, Uri2, Page2),
	  add_to_library(Uri2, sys_HtmlDocument)
	; get_document(Uri2, file, Page2)
	),
	page_to_reply(Page2, Request, Page).

page_to_reply(file(Page), Request, file(Page, Request)) :-
	!.
page_to_reply(Page, _, Page).

encode_form(AVs, FormPart) :-
	encode_form_1(AVs, Atoms),
	concat_atom(Atoms, FormPart).

encode_form_1([A=V], [A, '=', V1]) :-
	!,
	www_form_encode(V, V1).
encode_form_1([A=V|AVs], [A, '=', V1, '&' | AVs1]) :-
	www_form_encode(V, V1),
	encode_form_1(AVs, AVs1).
encode_form_1([], []).

file_name_guess_mime_type(Name, MimeType) :-
	file_name_extension(_, Extension, Name),
	( mime_extension(Extension, MimeType)
	; guess_extension(Extension, MimeType)
	),
	!.
file_name_guess_mime_type(_, 'text/plain').

guess_extension('', 'text/html').
guess_extension('cgi', 'text/html').

map_fix_refs([X|Xs], Args, Y1, [X1|Xs1]) :-
	fix_refs(X, Args, Y1, X1),
	map_fix_refs(Xs, Args, Y1, Xs1).
map_fix_refs([], _, _, []).

fix_refs(element(T, A, C), Args, Base, element(T, A1, C1)) :-
	!,
	map_fix_refs_1(A, Args, Base, A1),
	map_fix_refs(C, Args, Base, C1).
fix_refs(X, _, _, X).

fix_refs_1(A=R, Args, B, A=R1) :-
	fix_ref_attribute(A),
	!,
	fix_ref(R, Args, B, R1).
fix_refs_1(X, _, _, X).

fix_ref_attribute(href).
fix_ref_attribute(action).
fix_ref_attribute(src).
fix_ref_attribute('HREF').
fix_ref_attribute('ACTION').
fix_ref_attribute('SRC').

fix_ref(R, Args, B, R1) :-
	resolve_uri(B, R, R2),
	decompose_uri(R2, _, _, P, _, _),
	( ( var(P)
          ; file_name_guess_mime_type(P, 'text/html')
	  ) ->
	  www_form_encode(R2, R3), % ???
	  R4 = get(Args, R3),
	  term_to_atom(R4, R5),
	  www_form_encode(R5, R6),
	  concat_atom(['/proxy/', R6], R1)
	; R1 = R2
	).

map_fix_refs_1([X|Xs], A, Y1, [X1|Xs1]) :-
	fix_refs_1(X, A, Y1, X1),
	map_fix_refs_1(Xs, A, Y1, Xs1).
map_fix_refs_1([], _, _, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Refine Document
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

refine_document(KB, Page, Args, BaseUri, Page1) :-
	map_fix_refs(Page, Args, BaseUri, Page2),
	bodytop(KB, Page, Args, BaseUri, Header),
	( xml_add_to_elem(Page2, body, Header, Page3) -> 
	  true
	; Page3 = Page2 
	),
	stylesheet_link(KB, Link),
	( xml_add_to_elem(Page3, head, Link, Page1) ->
	  true
	; xml_add_to_elem(Page3, html, element(head, [], [Link]), Page1) ->
	  true
	; Page1 = Page3
	).


xml_add_to_elem([C|Cs], Tag, H, [C1|Cs]) :-
	xml_add_to_elem_1(C, Tag, H, C1),
	!.
xml_add_to_elem([C|Cs], Tag, H, [C|Cs1]) :-
	xml_add_to_elem(Cs, Tag, H, Cs1).

xml_add_to_elem_1(C, Tag, H, C1) :-
	xml_add_to_elem_2(C, Tag, H, C1),
	!.
xml_add_to_elem_1(element(T, A, C), Tag, H, element(T, A, C1)) :-
	xml_add_to_elem(C, Tag, H, C1).

xml_add_to_elem_2(element(Tag, A, C), Tag, H, element(Tag, A, [H|C])).

