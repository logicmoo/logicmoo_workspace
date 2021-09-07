#!/usr/bin/env swipl

%  was_module(sanity_ks_two,[]).

:- include(test_header).

:- if(\+ current_module(attvar_reader)).
:- use_module(library(logicmoo/attvar_reader)).
:- endif.

% Tests assertable attributed variables

:- debug_logicmoo(_).
:- nodebug_logicmoo(http(_)).
:- debug_logicmoo(logicmoo(_)).
 % :- mpred_trace_exec.

:- dynamic(sk_out/1).
:- dynamic(sk_in/1).

% :- ain(baseKB:rtArgsVerbatum(my_sk)).

:- read_attvars(true).

% :- expects_dialect(swi).
my_sk(aVar([vn='Ex',sk='SKF-666'])).

:- mpred_test((my_sk(Ex),get_attr(Ex,sk,What),What=='SKF-666')).


