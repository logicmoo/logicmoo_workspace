:- module(mudconsolestart, [http_mud_server/0,
			   http_mud_server/1]).
/* * module * Launch the http interaction, the 'mudconsole'

*/
:- use_module(logicmoo('mudconsole/mudconsole')).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_head)).
:- use_module(swi(library/http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(logicmoo(logicmoo_util/logicmoo_util_library)).
:- use_module(weblog('html_form/autocomplete')).
:- use_module(library(http/http_path), []).

% :- mc_start([title('Logicmoo MUD'), allow(_)]).

:- html_resource('http://fonts.googleapis.com/css?family=Henny+Penny|Sniglet|Nova+Square', [mime_type(text/css)]).
:- html_resource(webfonts,
		 [ virtual(true),
		   requires('http://fonts.googleapis.com/css?family=Henny+Penny|Sniglet|Nova+Square')
		 ]).

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(css, root(css), []).

http_mud_server :-
	debug(mudconsole, "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&", []),
	debug(mudconsole, "&				       &", []),
	debug(mudconsole, "&   MUD CONSOLE (after web console) &", []),
	debug(mudconsole, "&				       &", []),
	debug(mudconsole, "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&", []),
	http_mud_server([port(3020)]).

http_mud_server(Options) :-
	http_server(http_dispatch, Options).

:- http_handler(mudconsole(game), game_handler, []).

:- multifile
        body//2.

body(game, Body) -->
        html(body([ \html_requires(webfonts),
		    \html_requires('/css/mudconsole.css'),
		    div(id(top), h1('Logicmoo Game')),
                    div(id(content), Body)
                  ])).

game_handler(_Request) :-
	reply_html_page(game,
			title('Logicmoo Game'),
			\game_page).

game_page -->
	html([div(class(map_section), [
		   \id_div(map),
		   \id_div(inventory)
		  ]),
	    \id_div(output),
	    \id_div(error_area),
	    \input_area,
	    p(class(directions), 'Type into box above')
	]).

id_div(ID) -->
	html([
	    div(id(ID), &(nbsp))
	]).

input_area -->
	html([
	    div(id(input_area),
		input([type(text), id(inarea), value('Type here')]))
	]).
