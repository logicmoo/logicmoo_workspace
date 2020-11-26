:- module(cliopatria_binding, []).

:- throw('dont call cliopatria_binding!').

/* * module * Separate module so setting ends up in right place

% [Optionaly 1st run] tell where ClioPatria is located and restart

:-set_setting(cliopatria_binding:path, '../externals/ClioPatria'), save_settings('moo_settings.db').
*/

:- use_module(library(settings)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_head)).
:- use_module(swi(library/http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_path),[]).

:- multifile http:location/3.
:- dynamic   http:location/3.

% doesn't descend from root because that's being moved for cliopatria
http:location(cliopatria, root(cliopatria), [priority(100)]).


:- setting(path, atom, '../externals/ClioPatria', 'Path to root of cliopatria install').

% :- load_settings('moo_settings.db').

add_cliopatria_to_search_path :-
	setting(path, invalid),
	!,
	writeln('set the cliopatria path by querying set_setting(cliopatria_binding:path, \'c:/path/to/cliopatria\'), save_settings(\'moo_settings.db\').'),
	fail.
add_cliopatria_to_search_path :-
	setting(path, Path),
	asserta(user:file_search_path(cliopatria, Path)).


:- add_cliopatria_to_search_path.

:- ensure_loaded(logicmoo(dbase/dbase_rdf_entailment)).

