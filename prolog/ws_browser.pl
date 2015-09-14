/*  Part of Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(ws_browser, [browse_server/1]).

:- use_module(library(apply)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).

:- http_handler(root(.),           list_files,  []). % /
:- http_handler(root(show_source), show_source, []). % /module?file=<file>

browse_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- multifile
    provides_method/1,
    fetch_module_files_hook/2,
    show_source_hook/2.

list_files(Request) :-
    print_message(information, format('Preparing to list files', [])),
    once(provides_method(DMethod)),
    http_parameters(Request,
                    [ meth(Method, [default(DMethod)])
                    ]),
    fetch_module_files_hook(Method, ModuleFiles),
    reply_html_page([% style(Style),
		     title('Browse Code')
		    ],
		    [h1('Modules'),
		     table([border(1)],
			   [\header,
			    \foldl(html_module_files(Method), ModuleFiles)
			   ])
		    ]),
    print_message(information, format('done', [])).

show_source(Request) :-
    once(provides_method(DMethod)),
    http_parameters(Request,
                    [meth(Method, [default(DMethod)]),
		     file(File, [])
                    ]),
    show_source_hook(Method, File),
    print_message(information, format('done', [])).

header -->
    html(tr([td(b('Module')),
	     td(b('File'))])).

html_module_files(Method, Module-Files) -->
    html(tr([td(Module),td(table([\foldl(html_file(Method), Files)]))])).

html_file(Method, File) -->
    html(tr([td(\html_link(Method, File))])).

html_link(Method, File) -->
    {http_link_to_id(show_source, [meth=Method, file=File], HREF)},
    html(a(href(HREF), File)).
