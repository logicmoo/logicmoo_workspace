---++ Rule definition facilities (html//1 rules)

In addition to the [[core translation][<HelloHTML.html>]] of Prolog
terms to HTML, the library(http/html_write) defines an escape mechanism
that allows you to write rules. In this page, we present a web-server
that lists all Prolog modules and the file from which they are loaded.
We start with the overall server and the skeleton of the page using the
code below. But, instead of filling out all the details in one giant
structure, we call rules (or macros) using the \<rule> construct.

The advantage of this approach is that the structure of the page
remains obvious.  In addition, the page-skeleton becomes _reusable_
by providing different parameters to the rules.

==
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
			  table([ \header	    % rule-invocation
				| \modules(Modules) % rule-invocation
				])
			]).
==

Now, we complete the definition of the skeleton by providing definitions
for the called rules. These definitions are
[[DCGs][http://cs.union.edu/~striegnk/learn-prolog-now/html/node54.html#lecture7]]
(Definite Clause Grammar rules). The _input_ arguments of the DCG rules
are parameters and the _output_ list is a list of HTML tokens created by
means of calling html//1. The argument of html//1 must obey the same
rules as the arguments of reply_html_page/2.

==
header -->
	html(tr([th('Module'), th('File')])).

modules([]) -->	[].
modules([H|T]) --> module(H), modules(T).

module(Module) -->
	{ module_property(Module, file(Path)) }, !,
	html(tr([td(Module), td(Path)])).
module(Module) -->
	html(tr([td(Module), td(-)])).
==

We managed to compensate for the disadvantages compared to writing
HTML/XML: we have _reusable_ skeletons and components: we can reuse
skeletons as in the top-box by passing arguments that fill the table
header and body and we can reuse components as defined in the bottom-box
in different tables.  _Reusability_ may not be a big issue for pages
that consists mainly of text, but dynamically generated pages often
contain lots of reusable elements!  The rule below shows one for
formatting integers in a cell as N,NNN,NNN...  This example illustrate
another type of value that can appear in the html//1 content: terms of
the form <format>-<arguments>, where _format_ is an atom and _arguments_
is a list of arguments that is passed to format/2.

==
	...
	html(tr([..., \int_cell(I), ...])),

int_cell(I) -->
	html(td(class(intcell), '~D'-[I])).
==


In the next example, we extend our program by making more use of rules
and dealing with HTTP-_parameters_ (i.e., ?name=value&...)

@see The source: list_modules.pl
@see Next: [[Processing HTTP-parameters][<HTMLParams.html>]]
