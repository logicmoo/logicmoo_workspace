:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- http_handler(root(hello_world), say_hi, []).

server(Port) :-
	http_server(http_dispatch, [port(Port)]).

say_hi(_Request) :-
	reply_html_page(title('Hello World'),
			[ h1('Hello World'),
			  p(['This example demonstrates generating HTML ',
			     'messages from Prolog'
			    ])
			]).
