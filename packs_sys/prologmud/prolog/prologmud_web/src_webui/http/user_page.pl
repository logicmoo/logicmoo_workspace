:- module(user_page, []).
/* * module * The web page the user interacts with

*/
:- multifile(style/1).
:- dynamic(style/1).

:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path), []).
:- use_module(library(http/http_server_files), []).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_head)).
:- use_module(swi(library/http/html_write)).
:- use_module(library(option)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_wrapper)).
:- if_file_exists(use_module(weblog(html_form/ajaxify))).

wire_in_weblog :-
  assertz(user:file_search_path(css, weblog('static/css'))),
  assertz(user:file_search_path(js, weblog('static/js'))),
  assertz(user:file_search_path(icons, weblog('static/icons'))).

:- wire_in_weblog.

:- multifile http:location/3.
:- dynamic   http:location/3.

% all URI locations to do with play are under some subtree.
% this means we can separate, for example, having apache forward via
% a rewrite rule only a subdomain to /mud and below.
%
http:location(mud, root(mud), []).

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

% these are static assets that belong to a specific MUD Game
user:file_search_path(js, O) :- expand_file_search_path(game('web/js'),O).
user:file_search_path(css, O):- expand_file_search_path(game('web/css'),O).
user:file_search_path(icons, O):- expand_file_search_path(game('web/icons'),O).
user:file_search_path(mud_code, O):- expand_file_search_path(game('web/prolog'),O).

% Someday these will be set up per-MUD
% these are static assets that belong to a specific MUD Server
user:file_search_path(js, '../src_assets/web/js').
user:file_search_path(css, '../src_assets/web/css').
user:file_search_path(icons, '../src_assets/web/icons').
user:file_search_path(mud_code, '../src_assets/web/prolog').

% These should remain. They are static assets served by the core
% server (eg. the javascript to make the page go, the fallback css, etc)
user:file_search_path(js, '../src_webui/http/web/js').
user:file_search_path(css, '../src_webui/http/web/css').
user:file_search_path(icons, '../src_webui/http/web/icons').


%
%  SECURITY - potential security hole.
%
:- if_file_exists(use_module(mud_code(mud_specific), [style/1])).
:- if_file_exists(use_module(logicmoo(model/substance))).

% The game page where players spend most of their time
:- http_handler(mud(.), mud_page, [id(mud), priority(10)]).

:- html_resource(jquery,
		 [ virtual(true),
		   requires(js('jquery-1.7.1.js'))
		 ]).
:- html_resource(js('jquery.form.js'),
		 [ requires(jquery)
		 ]).
:- html_resource(js('mud.js'),
		 [ requires(jquery),
		   requires(js('jquery.form.js'))
		 ]).
:- html_resource(mud,
		 [ virtual(true),
		   requires(js('mud.js'))
		 ]).

mud_page(_Request) :-
	actual_style(Style),
	reply_html_page(
	    Style,
	    [link([
		 rel('shortcut icon'),
		 type('image/x-icon'),
		 href('/icons/favicon.ico')])],
	    [
	     \map_section,
	     \description_section,
	     \stats_section,
	     \player_prompt,
	     \input_section
	    ]).

actual_style(Style) :- style(name(Style)),!.
actual_style(logicmoo).

:- multifile head/4, body/4.

% fallback style
head(logicmoo, Head) -->
	html(head([
		 title('LogicMOO'),
		 Head
	     ])).
body(logicmoo, Body) -->
	html(body([
		 h1('LogicMOO MUD'),
		 div(id(content), Body)
	     ])).

%%	map_section(?A:list, ?B:list) is det
%
%	generate the map
map_section -->
	{
	    \+ style(map),
	    !
        },
	[].
map_section -->
	{
	   http_current_player(P),
           substance(map_origin(OX, OY)),
           substance(map_size(X, Y))
        },
	html(div(id(map), table(\map_row(P, OX, OY, X, Y)))).

map_row(_, _, _, 0, _) --> !, [].
map_row(_, _, _, _, 0) --> !, [].
map_row(P, OX, OY, X, Y) -->
	{
           NOY is OY + 1,
           NY is Y - 1
        },
	html(tr(\map_cell(P, OY, OX, 0, X))),
	map_row(P, OX, NOY, X , NY).

map_cell(_, _, _, X, X) --> [].
map_cell(P, AbsY, OX, CurX, X) -->
	{
            CurX \= X,
	    NewX is CurX + 1,
            AbsX is OX + CurX,
            get_map_contents(P, AbsX, AbsY, Contents)
        },
	html(td(Contents)),
	map_cell(P, AbsY, OX, NewX, X).

get_map_contents(P, AbsX, AbsY, Contents) :-
        substance(cell(P, AbsX, AbsY, SemanticContent)),
	det_style(map_display(SemanticContent, Contents)).
get_map_contents(_, _, _, Contents) :-
	det_style(map_display(blank, Contents)).

det_style(map_display(Semantics, Contents)) :-
	style(map_display(Semantics, Contents)).
det_style(map_display(_, [div(class(blank), [&(nbsp)])])).

%%	http_current_player(-P:player) is det
%
%	binds to the current player
%	always succeeds, will make a player if need be
%
http_current_player(P) :-
	http_open_session(_, [renew(false)]),
	http_player_from_session(P).

http_player_from_session(P) :-
	http_session_data(player(P)), !.
http_player_from_session(P) :-
	http_current_request(Request),
	(   member(search(Opts), Request) ; Opts = []),
	substance(need_new_player(Opts, P)).

description_section -->
	html(p('someday this will be the description')).
stats_section -->
	html(p('someday this will be the stats')).
player_prompt -->
	html(p('someday this will be the player prompt')).
input_section -->
	html(form([input([type(text), id(nl), name(nl)], [])])).

:- listen(http_session(end(SessionID, _Peer)),
          byebye_player(SessionID)).

byebye_player(SessionID) :-
	http_current_session(SessionID, player(P)),
	substance(player_split(P)).
