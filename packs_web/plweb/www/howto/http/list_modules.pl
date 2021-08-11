:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- http_handler(root(list_modules), list_modules, []).

server(Port) :-
	http_server(http_dispatch, [port(Port)]).

list_modules(_Request) :-
	findall(M, current_module(M), List),
	sort(List, Modules),
	reply_html_page(title('Loaded Prolog modules'),
			[ h1('Loaded Prolog modules'),
			  table([ \header
				| \modules(Modules)
				])
			]).

header -->
	html(tr([th('Module'), th('File')])).

modules([]) -->	[].
modules([H|T]) --> module(H), modules(T).

module(Module) -->
	{ module_property(Module, file(Path)) }, !,
	html(tr([td(Module), td(Path)])).
module(Module) -->
	html(tr([td(Module), td(-)])).

