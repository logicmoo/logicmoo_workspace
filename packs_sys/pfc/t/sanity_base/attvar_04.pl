#!/usr/bin/env swipl

%  was_module(sanity_ks_two,[]).

:- include(test_header).

:- ensure_loaded(library(attvar_reader)).

% Tests assertable attributed variables

:- debug_logicmoo(_).
:- nodebug_logicmoo(http(_)).
:- debug_logicmoo(logicmoo(_)).
 % :- mpred_trace_exec.

:- dynamic(sk_out/1).
:- dynamic(sk_in/1).

baseKB:rtArgsVerbatum(my_sk).

:- read_attvars(true).

% :- file_begin(pl).
my_sk(aVar([vn='Ex',sk='SKF-666'])).

:- must((my_sk(Ex),get_attr(Ex,sk,What),What=='SKF-666')).


