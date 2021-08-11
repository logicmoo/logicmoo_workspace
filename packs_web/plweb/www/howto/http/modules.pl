:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).	% new
:- use_module(library(uri)).			% new

:- http_handler(root(.),      list_modules, []).
:- http_handler(root(module), list_module,  []).

server(Port) :-
	http_server(http_dispatch, [port(Port)]).

%%	list_modules(+Request)
%
%	Create a table of all available modules with their source-file

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
modules([H|T]) -->
	html(tr([td(\module_link(H)), td(\module_file(H))])),
	modules(T).

module_file(H) -->
	{ module_property(H, file(Path)) }, !,
	html(Path).
module_file(_) -->
	html(-).

module_link(H) -->
	{ href(list_module, [name=H], HREF) },
	html(a(href(HREF), H)).

%%	href(+HandleID, +Query, -HREF)
%
%	HREF is a link to a handler with HandleID and the given
%	list of Name=Value parameters.

href(HandleID, Query, HREF) :-
	http_location_by_id(HandleID, Location),
	uri_data(path, Components, Location),
	uri_query_components(String, Query),
	uri_data(search, Components, String),
	uri_components(HREF, Components).


%%	list_module(+Request)
%
%	List info for a given module.

list_module(Request) :-
	http_parameters(Request,
			[ name(Module, [])
			]),
	module_public_predicates(Module, Preds),
	reply_html_page(title('Module ~w'-[Module]),
			[ h2('Public predicates for module ~q'-[Module]),
			  \predicates_ul(Preds)
			]).

%%	predicates_ul(+Module)// is det.
%
%	Generate an HTML =ul= list of public predicates in Module.

predicates_ul(Preds) -->
	html(ul(\predicate_list(Preds))).

predicate_list([]) --> [].
predicate_list([H|T]) -->
	html(li('~q'-[H])),
	predicate_list(T).


%%	module_public_predicates(+Module, -PublicPreds)

module_public_predicates(Module, Preds) :-
	findall(H, predicate_property(Module:H, exported), HL),
	maplist(head_to_pi, HL, Preds0),
	sort(Preds0, Preds).

head_to_pi(Head, Name/Arity) :-
	functor(Head, Name, Arity).

