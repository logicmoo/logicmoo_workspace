:- module(i18n_test, [i18n_test/1]).

:- use_module(library(i18n/i18n_expansion)).
:- use_module(library(i18n/i18n_support)).

:- retractall(i18n_support:language(_)).	  % overriding language
:- retractall(i18n_support:i18n_resource_dir(_)). % overriding resource dir

i18n_support:i18n_resource_dir(Dir) :-
    context_module(M),
    current_module(M, F),
    directory_file_path(Dir, _, F).

i18n_support:language(es).	% Spanish

i18n_test(t1) :-
    A = ~hello,
    assertion(A == hola).

i18n_test(t2) :-
    A = hola,
    assertion(A == ~hello).
