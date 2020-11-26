/*
* Copyright (C) 2007 Christoph Wernhard
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Http-Server
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(httpserver, [start_webserver/1,
		       webserver_reply/2,
		       http_handler_options/1,
		       webserver_register_file_once/1]).


:- use_module('swilib/err').
:- use_module(xml_writer).
:- use_module(config).

:- use_module(library('http/thread_httpd')).
%:- use_module(library('http/xpce_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/http_header')).
:- use_module(library('http/http_authenticate')).

start_webserver(Port) :-
	http_server(http_dispatch, [port(Port)]).

webserver_reply(file(File, Request), MimeType) :-
	!,
	http_reply_file(File, [mime_type(MimeType), cache(true)], Request).
webserver_reply(file_once(File, Request), MimeType) :-
	!,
	( retract(file_once(File)) ->
	  http_reply_file(File, [mime_type(MimeType), cache(false)], Request),
	  delete_file(File)
	; err('Auxiliary document not accessible: ~q.', [File])
	).
webserver_reply(Reply, MimeType) :-
	format('Content-type: ~w~n~n', [MimeType]),	
	write_reply_content(Reply, MimeType),
	flush_output.

write_reply_content(Reply, _) :-
	atom(Reply),
	!,
	write(Reply).
write_reply_content(Reply, _) :-
	Reply = [element(rdf:'RDF', _, _)],
	!,
	write_structure(Reply, [encoding('UTF-8'), indent(4)]).
write_reply_content(Reply, 'text/html') :-
	Reply = [element(_, _, _)],
	!,
	write(
	    '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">'),
	nl,
	write_structure(Reply, [space(html), indent(0)]).
write_reply_content(Reply, _) :-
	Reply = [element(_, _, _)],
	!,
	write_structure(Reply, []).
write_reply_content([e(X,Y,Z)], U) :-
	!,
	write_reply_content([element(X,Y,Z)], U).
write_reply_content(pp(PrettyPrinter, Term), _) :-
	!,
	( PrettyPrinter = Module:PrettyPrinter1 ->
	  Call =.. [PrettyPrinter1, Term],
	  call(Module:Call)
	; Call =.. [PrettyPrinter, Term],
	  call(Call)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% File Once
%%%% 
%%%% Useful for generated images that "belong" to another page.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic( file_once/2 ).

webserver_register_file_once(File) :-
	retractall( file_once(File) ),
	assert( file_once(File) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic http_handler_options/1.

%%%% 
%%%% Required at load time, since http_handler_options/1 might be
%%%% used during loading of further modules:
%%%% 
:- retractall(http_handler_options(_)),
   config(webserver_password_file, PwdFile),
   expand_file_name(PwdFile, [PwdFile1|_]),
   assert(http_handler_options([time_limit(300),
				authentication(basic(PwdFile1,
						     'InfraEngine Server'))])).


				
