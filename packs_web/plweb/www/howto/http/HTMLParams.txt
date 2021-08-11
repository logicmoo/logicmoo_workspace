---++ Processing HTTP-parameters

In HTMLRules.txt, we used the HTML generation library to build a single
page dynamically.  In this page we will introduce two new primitives:

    1. Create a handler that processes parameters
    2. Create a URI to another handler dynamically

---+++ Creating links to another handler with parameters

First, the overall skeleton.  This is very much like the HTMLRules.txt
example, but we introduce new rules for making a link to a module and
including the file-name of a module.  Introducing such tiny rules and
managing a pool greatly boosts reusability in and maintainability of
the code. Started using =|?- server(5000).|=, the server is accessible
at http://localhost:5000/

==
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).	 % new
:- use_module(library(uri)).			 % new

:- http_handler(root(.),      list_modules, []). % /
:- http_handler(root(module), list_module,  []). % /module?name=<module>

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
==

Now comes ones of the new parts. Instead of including the name of the
module, we must create a link that, when clicked, invokes the
list_module(+Request) handler. We _can_ do this using the code below.
This example introduces two new primitives:

    1. Attribute-values can be of the form A+B, which evaluates to
       string-concatenation.
    2. Attribute-values can be of the form encode(X), which evaluates
       to %-encoding of X

==
	...,
	html(a(href('/list_module?name='+encode(Module)))
==

Although this type of coding is popular, it is not ideal because the
location of =|/list_module|= is hard-coded. We do not care about the
HTTP location, we want to *|call list_module(+Request)|*! The HTTP
infrastructure can tell us this location and provides the predicate
http_link_to_id/3 to compute a link based on the identifier of the
handler (by default the predicate name) and additional parameters.

==
module_link(H) -->
	{ http_link_to_id(list_module, [name=H], HREF) },
	html(a(href(HREF), H)).
==

Note that by using the URI encoding and decoding library(uri), we are
sure that the generated HREF is properly encoded.  The last step is
to define our handler for =|/list_module?name=<module>|=.


---+++ Processing parameters

Dealing with parameters is achieved through http_parameters/2. The first
argument is the Request passed by the server-library. The second
argument is a list of Name(ValueVar, Properties). The properties allow
for specifying the type, whether the parameter is optional, a default,
etc. See http_parameters/2.

==
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
==

The remainder of the code is similar to listing the modules.  However,
we do introduce some reusability principles:

    1. Create a predicate for each interesting computation
    (in this case generate a list of public predicates).

    2. Create a rule for reusable UI components.  predicates_ul//1
    writes an UL-list of predicates, something which is likely to
    be reusable in this application (list _undefined_, _private_, etc.
    predicates).

---+++ Summary

In this page we learned linking dynamically generated pages together and
passing parameters. The same infrastructure can of course be used to
generate forms as we discuss in HTMLForms.txt. We also learned how to
deal with reusability and maintenance by properly splitting the code
and by referring to HTTP paths by the predicate that implements them
rather than the exact location of the server.

---+++ What is still missing?

If you loaded the source and ran the demo, you might feel a bit
disappointed: it works flawlessly, but it looks very 1980's: totally
basic style and no fancy interaction. With the technology presented
so far, we can easily make it look 1990's: create reusable rules that
create tables-in-tables(-in-frames)! In HTMLStyle.txt, we discuss how to
do this state-of-the-art.

@see The source: modules.pl
@see Next: HTMLStyle.txt
