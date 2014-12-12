:- module(ws_browse_code, [browse_code_server/1]).

:- use_module(library(maplist_dcg)).
:- use_module(library(module_files)).
:- use_module(library(pldoc/doc_htmlsrc)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)). % new                          
:- use_module(library(uri)).                     % new                          
:- use_module(library(http/http_open)).

:- http_handler(root(.),           list_files,  []). % /
:- http_handler(root(show_source), show_source, []). % /module?file=<file>

browse_code_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

fetch_module_files(live, ModuleFiles) :-
    findall(M-Files,
	    ( current_module(M),
	      module_files(M, Files)
	    ), ModuleFilesU),
    sort(ModuleFilesU, ModuleFiles).

list_files(Request) :-
    print_message(information, format('Preparing to list files', [])),
    http_parameters(Request,
                    [ csrc(Method, [default(live)])
                    ]),
    fetch_module_files(Method, ModuleFiles),
    reply_html_page([% style(Style),
		     title('Browse Code')
		    ],
		    [h1('Modules'),
		     table([border(1)],
			   [\header,
			    \maplist_dcg(html_module_files, ModuleFiles)
			   ])
		    ]),
    print_message(information, format('done', [])).

show_source(Request) :-
    http_parameters(Request,
                    [ % csrc(_, [default(live)]),
		      file(File, [])
                    ]),
    format('Content-type: text/html~n~n', []),
    source_to_html(File, stream(current_output), [format_comments(false)]).    

header -->
    html(tr([td(b('Module')),
	     td(b('File'))])).

html_module_files(Module-Files) -->
    html(tr([td(Module),td(table([\maplist_dcg(html_file, Files)]))])).

html_file(File) -->
    html(tr([td(\html_link(File))])).

html_link(File) -->
    {http_link_to_id(show_source, [file=File], HREF)},
    html(a(href(HREF), File)).
