:- use_module(library(http/http_server)).

:- initialization
    http_server([port(8080)]).

:- http_handler(root(.),
		http_redirect(moved, location_by_id(home_page)),
		[]).
:- http_handler(root(home), home_page, []).

home_page(_Request) :-
    reply_html_page(
	title('Demo server'),
	[ h1('Hello world!')
	]).
