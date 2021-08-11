:- module(test_plweb,
	  [ test_links/0
	  ]).

:- use_module(library(ansi_term)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)). % HTTPS support.
:- use_module(library(uri)).

:- use_module(page).

/** <module> Test various aspects of plweb

@author Wouter Beek
@version 2016/05/02
*/

:- meta_predicate
	verbose(0, +, +).





test_links :-
	clause(plweb_page:menu(_,L), _),
	maplist(test_links, L).


test_links(_=L) :-
	is_list(L), !,
	maplist(test_links, L).
test_links(_=Uri) :-
	is_link(Uri), !,
	verbose(test_link(Uri), "Checking URL ‘~a’", [Uri]).
test_links(_).


test_link(Uri) :-
	http_get(Uri, _, [status_code(Code)]),
	(between(200, 299, Code) -> true ; throw(error(Code))).





% HELPERS %

%! is_link(@Term) is semidet.

is_link(Uri) :-
	atom(Uri),
	uri_components(Uri, uri_components(Scheme,Auth,_,_,_)),
	maplist(atom, [Scheme,Auth]).



%! verbose(:Goal_0, +Format, +Args) is det.

verbose(Goal_0, Format, Args) :-
	get_time(Start),
	format(Format, Args),
	(   catch(Goal_0, E, true)
	->  (   var(E)
	    ->  get_time(End),
	        Delta is End - Start,
	        ansi_format([fg(green)], "~`.t success (~2f sec.)~72|", [Delta])
	    ;   message_to_string(E, S),
	        ansi_format([fg(red)], "~`.t ERROR: ~w~72|", [S])
	    )
	;   ansi_format([fg(red)], "~`.t ERROR: (failed)~72|", [])
	),
	nl.
