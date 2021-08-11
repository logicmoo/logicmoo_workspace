/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(plweb,
	  [ server/0,
	    server/1
	  ]).

:- use_module(library(pldoc)).
:- use_module(library(doc_http)).
:- use_module(library(pldoc/doc_wiki)).
:- use_module(library(pldoc/doc_man)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/mimetype)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_parameters)).
:- use_module(library(settings)).
:- use_module(library(error)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(occurs)).
:- use_module(library(pairs)).
:- use_module(library(option)).
:- use_module(library(xpath)).
:- use_module(library(sgml)).
:- use_module(library(thread_pool)).
:- use_module(library(http/http_dirindex)).
:- use_module(library(debug)).
:- use_module(library(http/http_files)).

:- use_module(parms).
:- use_module(page).
:- use_module(download).
:- use_module(wiki).
:- use_module(http_cgi).
:- use_module(gitweb).
:- use_module(update).
:- use_module(autocomplete).
:- use_module(customise).
:- use_module(tests).
:- use_module(pack_info).

:- http_handler(root(.), serve_page(document_root),
		[prefix, priority(10), spawn(wiki)]).
:- http_handler(root('favicon.ico'), favicon,
		[priority(10)]).
:- http_handler(root('apple-touch-icon.png'), touch_icon, []).
:- http_handler(root(man), manual_file,
		[prefix, priority(10), spawn(wiki)]).
:- http_handler(root('.well-known/'),
		http_reply_from_files('.well-known', []), [prefix]).

:- create_prolog_flag(wiki_edit, true, []).

/** <module> Server for PlDoc wiki pages and SWI-Prolog website

@tbd	Turn directory listing into a library.
*/

		 /*******************************
		 *            SERVER		*
		 *******************************/

server :-
	server([]).

server(Options) :-
	with_mutex(plweb_init, server_init),
	doc_enable(true),
	setting(http:port, Port),
	setting(http:workers, Workers),
	merge_options(Options,
		      [ port(Port),
			workers(Workers)
		      ], HTTPOptions),
	http_server(http_dispatch, HTTPOptions),
	debug(plweb, 'Server was started at port ~d.', [Port]).

:- if(\+current_predicate(doc_enable/1)).
doc_enable(_).
:- endif.

:- dynamic
	server_init_done/0.

server_init :-
	server_init_done, !.
server_init :-
	asserta(server_init_done),
	load_settings('plweb.conf'),
	catch(make_directory_path(log), E,
	      print_message(warning, E)),
	update_pack_metadata_in_background,
	thread_create(index_wiki_pages, _,
		      [ alias('__index_wiki_pages'),
			detached(true)
		      ]),
	db_sync_thread.


:- multifile
	http_unix_daemon:http_server_hook/1.

http_unix_daemon:http_server_hook(Options) :-
	server(Options).

%%	favicon(+Request)
%
%	Serve /favicon.ico.

favicon(Request) :-
	http_reply_file(icons('favicon.ico'), [], Request).

%%	touch_icon(+Request)
%
%	Serve /apple-touch-icon.png.

touch_icon(Request) :-
	http_reply_file(icons('apple-touch-icon.png'), [], Request).


		 /*******************************
		 *	      SERVICES		*
		 *******************************/

%%	serve_page(+Alias, +Request)
%
%	HTTP handler for files below document-root.

serve_page(Alias, Request) :-
	memberchk(path_info(Relative), Request),
	Spec =.. [ Alias, Relative ],
	http_safe_file(Spec, []),
	find_file(Relative, File), !,
	serve_file(File, Request).
serve_page(Alias, Request) :-
	\+ memberchk(path_info(_), Request), !,
	serve_page(Alias, [path_info('index.html'),style(wiki(home))|Request]).
serve_page(_, Request) :-
	memberchk(path(Path), Request),
	existence_error(http_location, Path).

%%	find_file(+Relative, -File) is det.
%
%	Translate Relative into a File in the document-root tree. If the
%	given extension is .html, also look for   .txt files that can be
%	translated into HTML.
%	.frg files embed the contents of the body in the normal 1 col
%	layout format.
%	.hom files embed the contents of the body in the home page
%	format. Usually the home page fill will have nothing in it

find_file(Relative, File) :-
	file_name_extension(Base, html, Relative),
	source_extension(Ext),
	file_name_extension(Base, Ext, SrcFile),
	absolute_file_name(document_root(SrcFile),
			   File,
			   [ access(read),
			     file_errors(fail)
			   ]), !.
find_file(Relative, File) :-
	absolute_file_name(document_root(Relative),
			   File,
			   [ access(read),
			     file_errors(fail)
			   ]).
find_file(Relative, File) :-
	absolute_file_name(document_root(Relative),
			   File,
			   [ access(read),
			     file_errors(fail),
			     file_type(directory)
			   ]).

source_extension(hom).				% homepage embedded html
source_extension(txt).				% Markdown
source_extension(md).				% Markdown
source_extension(frg).				% embedded html


%%	serve_file(+File, +Request) is det.
%%	serve_file(+Extension, +File, +Request) is det.
%
%	Serve the requested file.

serve_file(File, Request) :-
	file_name_extension(_, Ext, File),
	debug(plweb, 'Serving ~q; ext=~q', [File, Ext]),
	serve_file(Ext, File, Request).

serve_file('',  Dir, Request) :-
	exists_directory(Dir), !,
	(   sub_atom(Dir, _, _, 0, /),
	    serve_index_file(Dir, Request)
	->  true
	;   http_reply_dirindex(Dir, [unsafe(true)], Request)
	).
serve_file(txt, File, Request) :-
	serve_file(md, File, Request).
serve_file(md, File, Request) :-
	http_parameters(Request,
			[ format(Format, [ oneof([raw,html]),
					   default(html)
					 ])
			]),
	Format == html, !,
	serve_wiki_file(File, Request).
serve_file(hom, File, Request) :-
	serve_embedded_hom_file(File, Request).
serve_file(frg, File, Request) :-
	serve_embedded_html_file(File, Request).
serve_file(_Ext, File, Request) :-	% serve plain files
	http_reply_file(File, [unsafe(true)], Request).

%%	serve_index_file(+Dir, +Request) is semidet.
%
%	Serve index.txt or index.html, etc. if it exists.

serve_index_file(Dir, Request) :-
        setting(http:index_files, Indices),
        member(Index, Indices),
	ensure_slash(Dir, DirSlash),
	atom_concat(DirSlash, Index, File),
        access_file(File, read), !,
        serve_file(File, Request).

ensure_slash(Dir, Dir) :-
	sub_atom(Dir, _, _, 0, /), !.
ensure_slash(Dir0, Dir) :-
	atom_concat(Dir0, /, Dir).

%%	serve_wiki_file(+File, +Request) is det.
%
%	Serve a file containing wiki text.

serve_wiki_file(File, Request) :-
	read_file_to_codes(File, String, []),
	setup_call_cleanup(
	    b_setval(pldoc_file, File),
	    serve_wiki(String, File, Request),
	    nb_delete(pldoc_file)).


%%	serve_wiki(+String, +File, +Request) is det.
%
%	Emit page from wiki content in String.

serve_wiki(String, File, Request) :-
	wiki_codes_to_dom(String, [], DOM0),
	extract_title(DOM0, Title, DOM),
	setup_call_cleanup(
	    b_setval(pldoc_options, [prefer(manual)]),
	    serve_wiki_page(Request, File, Title, DOM),
	    nb_delete(pldoc_options)).

serve_wiki_page(Request, File, Title, DOM) :-
	wiki_path(Request, File, WikiPath),
	title_text(Title, TitleString),
	reply_html_page(
	    wiki(WikiPath, Title),
	    [ title(TitleString)
	    ],
	    DOM).

%%	wiki_path(+Request, +File, -WikiPath) is det.
%
%	WikiPath is the canonical path to describe the wiki page File.

wiki_path(Request, File, WikiPath) :-
	memberchk(request_uri(Location), Request),
	atom_concat(/, WikiPath0, Location),
	normalize_extension(WikiPath0, File, WikiPath).

normalize_extension(Path, File, Path) :-
	file_name_extension(_, Ext, File),
	file_name_extension(_, Ext, Path), !.
normalize_extension(Path0, File, Path) :-
	source_extension(Ext),
	file_name_extension(_, Ext, File),
	file_name_extension(Base, html, Path0), !,
	file_name_extension(Base, Ext, Path).
normalize_extension(Dir, _, Index) :-
	sub_atom(Dir, _, _, 0, /), !,
	atom_concat(Dir, 'index.txt', Index).
normalize_extension(Path, _, Path).

%%	prolog:doc_directory(+Dir) is semidet.
%
%	Enable editing of wiki documents from the www directory.

:- multifile
	prolog:doc_directory/1.

prolog:doc_directory(Dir) :-
	absolute_file_name(document_root(.),
			   Root,
			   [ file_type(directory),
			     access(read)
			   ]),
	sub_atom(Dir, 0, _, _, Root).

%%	manual_file(+Request) is det.
%
%	HTTP handler for /man/file.{html,gif}

manual_file(Request) :-
	memberchk(path_info(Relative), Request),
	atom_concat('doc/Manual', Relative, Man),
	(   file_name_extension(_, html, Man)
	->  absolute_file_name(swi(Man),
			       ManFile,
			       [ access(read),
				 file_errors(fail)
			       ]), !,
	    reply_html_page(title('SWI-Prolog manual'),
			    \man_page(section(_,_,_,ManFile), []))
	;   !,
	    http_reply_file(swi(Man), [], Request)
	).
manual_file(Request) :-
	memberchk(path(Path), Request),
	existence_error(http_location, Path).


		 /*******************************
		 *	  EMBEDDED HTML		*
		 *******************************/

%%	serve_embedded_html_file(+File, +Request) is det.
%
%	Serve a .frg file, which is displayed as an embedded HTML file
%	in the 1 col content format, or a .hom file, which is displayed
%	as an embedded HTML file in the home page format

serve_embedded_html_file(File, Request) :-
	serve_embedded_html_file(wiki, File, Request).

serve_embedded_hom_file(File, Request) :-
	serve_embedded_html_file(homepage, File, Request).

serve_embedded_html_file(Style, File, _Request) :-
	load_html(File, DOM, []),
	xpath_chk(DOM, //body(self), element(_,_,Body)),
	xpath_chk(DOM, //head(self), element(_,_,Head)),
	reply_html_page(Style, Head, Body).


		 /*******************************
		 *     THREAD POOL HANDLING	*
		 *******************************/

:- multifile
	http:create_pool/1.

http:create_pool(Name) :-
	thread_pool(Name, Size, Options),
	thread_pool_create(Name, Size, Options).

thread_pool(wiki,     100, []).
thread_pool(download, 200, []).
thread_pool(cgi,       50, []).
thread_pool(complete,  20, []).
