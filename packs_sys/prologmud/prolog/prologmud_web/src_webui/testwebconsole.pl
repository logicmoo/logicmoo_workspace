:- module(testmudconsole, [age/1, flag_table/0, simple_stuff/1]).

:- use_module('mudconsole/mudconsole').

:- use_module(swi(library/http/html_write)).

start_webclient:- mc_start([title('Logicmoo MUD'), allow(_)]).

%%	age(-Age)
%
%	Ask the user how old s/he is.

age(Age) :-
	mc_ask(
	    [ age(Age, between(0,120)) ],
	    [ p(['How old are you? ', input([name(age)])])
	    ]).

%%	flag_table
%
%	Show a table with all Prolog flags

flag_table :-
	findall(Flag-Value, current_prolog_flag(Flag,Value), Pairs),
	keysort(Pairs, Sorted),
	mc_html(mc_output, table(class(flags),
		      [ tr([th('Flag'), th('Value')])
		      | \rows(Sorted)
		      ]), [clear(true)]).

rows([]) --> [].
rows([H|T]) --> row(H), rows(T).

row(Flag-Value) -->
	html(tr([td(class(flag), Flag),
		 td(class(value), '~q'-[Value])
		])).


simple_stuff(X) :-
	mc_html(mc_output,  h1(X) , [clear(true)]).

