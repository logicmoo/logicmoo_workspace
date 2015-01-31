:- module(ws_source, []).

:- reexport(library(ws_browser)).
:- use_module(library(pldoc/doc_htmlsrc)).
:- use_module(library(module_files)).

ws_browser:provides_method(live).

ws_browser:fetch_module_files_hook(live, ModuleFiles) :-
    findall(M-Files,
	    ( current_module(M),
	      module_files(M, Files)
	    ), ModuleFilesU),
    sort(ModuleFilesU, ModuleFiles).

ws_browser:show_source_hook(live, File) :-
    format('Content-type: text/html~n~n', []),
    source_to_html(File, stream(current_output),
		   [format_comments(false)]).
