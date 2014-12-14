:- module(ws_browser, [browse_server/1]).

:- use_module(library(maplist_dcg)).
:- use_module(library(module_files)).
:- use_module(library(pldoc/doc_htmlsrc)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_open)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(uri)).

:- http_handler(root(.),           list_files,  []). % /
:- http_handler(root(show_source), show_source, []). % /module?file=<file>

browse_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- multifile
    fetch_module_files_hook/2,
    show_source_hook/2.

fetch_module_files_hook(live, ModuleFiles) :-
    findall(M-Files,
	    ( current_module(M),
	      module_files(M, Files)
	    ), ModuleFilesU),
    sort(ModuleFilesU, ModuleFiles).

list_files(Request) :-
    print_message(information, format('Preparing to list files', [])),
    http_parameters(Request,
                    [ meth(Method, [default(live)])
                    ]),
    fetch_module_files_hook(Method, ModuleFiles),
    reply_html_page([% style(Style),
		     title('Browse Code')
		    ],
		    [h1('Modules'),
		     table([border(1)],
			   [\header,
			    \maplist_dcg(html_module_files(Method), ModuleFiles)
			   ])
		    ]),
    print_message(information, format('done', [])).

show_source(Request) :-
    http_parameters(Request,
                    [meth(Method, [default(live)]),
		     file(File, [])
                    ]),
    show_source_hook(Method, File),
    print_message(information, format('done', [])).

show_source_hook(live, File) :-
    format('Content-type: text/html~n~n', []),
    source_to_html(File, stream(current_output), [format_comments(false)]).

header -->
    html(tr([td(b('Module')),
	     td(b('File'))])).

html_module_files(Method, Module-Files) -->
    html(tr([td(Module),td(table([\maplist_dcg(html_file(Method), Files)]))])).

html_file(Method, File) -->
    html(tr([td(\html_link(Method, File))])).

html_link(Method, File) -->
    {http_link_to_id(show_source, [meth=Method, file=File], HREF)},
    html(a(href(HREF), File)).
