:- begin_tests(i18n_2).

:- use_module(i18n_test).

:- use_module(library(i18n/i18n_expansion)).
:- use_module(library(i18n/i18n_support)).

:- retractall(i18n_support:language(_)). % overriding language
:- retractall(i18n_support:i18n_resource_dir(_)). % overriding resource dir

i18n_support:i18n_resource_dir(Dir) :-
    context_module(M),
    current_module(M, F),
    directory_file_path(Dir, _, F).

i18n_support:language(es).	% Spanish

:- init_i18n.

test(t1) :- test_t1.

test(t2) :- test_t2.

test(t3) :- test_t3.

test(t4) :- test_t4.

test(t5) :- test_t5.

test_t1 :-
    A = ~hello,
    assertion(A == hola).

test_t2 :-
    A = hola,
    assertion(A == ~hello).

% t3 and t4 are Ok, but documents an alternative semantic in the commented out
% lines: --EMM
test_t3 :-
    A = ~hello(B),
    % assertion(A == hola(B)).
    assertion(A == hello(B)).

test_t4 :-
    A = ~hello(B,world),
    % assertion(A == hola(B,mundo)).
    assertion(A == hello(B,mundo)).

test_t5 :-
    A = ~p(hello,B,world),
    assertion(A == p(hola,B,mundo)).

:- end_tests(i18n_2).
