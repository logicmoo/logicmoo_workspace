/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(ws_browser, [browse_server/1]).

:- use_module(library(apply)).
:- use_module(library(transpose)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/thread_httpd)).

:- http_handler(root(.),           list_files,  []). % /
:- http_handler(root(show_source), show_source, []). % /module?file=<file>

browse_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- multifile
    provides_method/1,
    fetch_files_properties_hook/3,
    show_source_hook/2.

list_files(Request) :-
    print_message(information, format('Preparing to list files', [])),
    once(provides_method(DMethod)),
    http_parameters(Request,
                    [ meth(Method, [default(DMethod)])
                    ]),
    fetch_files_properties_hook(Method, Descs, FilesProps),
    reply_html_page([% style(Style),
                     title('Browse Code')
                    ],
                    [h1('Files'),
                     table([border(1)],
                           [\header(Descs),
                            \foldl(html_module_props(Method), FilesProps)
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

header(Descs) -->
    html(tr([td(b('File')),
             \foldl(desc, Descs)
            ])).

desc(Desc) --> html(td(b(Desc))).

html_module_props(Method, File-Props) -->
    {transpose(Props, PropsT)},
    html(tr([td(\html_file(Method, File)), \foldl(html_prop, PropsT)])).

html_prop(PropL) -->
    html(td(table(\foldl(prop, PropL)))).

prop(Prop) --> html(tr(td(Prop))).

html_file(Method, File) -->
    {http_link_to_id(show_source, [meth=Method, file=File], HREF)},
    html(a(href(HREF), File)).
