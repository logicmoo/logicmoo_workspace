
% :- asserta(mu:is_a_module).

:- set_prolog_flag(ec_loader, false).

:- use_module(library(nomic_mu)).

:- use_module(library(pfc)).
:- set_fileAssertMt(mu).

:- nodebug.

:- '$set_source_module'(mu).

:- op(1000, xfy, ('==>')).


setup_moo ==> {writeln(setup_moo)}.

:- begin_pfc.

setup_moo2 ==> {writeln(setup_moo2)}.

:- ensure_loaded(library(logicmoo_nlu/parser_lexical)).

:- module(mu).

