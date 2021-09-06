
% :- asserta(mu:is_a_module).

:- set_prolog_flag(ec_loader, false).

%:- expects_dialect(pfc).
:- use_module(library(nomic_mu)).

:- expects_dialect(pfc).
:- set_fileAssertMt(mu).

:- nodebug.

:- '$set_source_module'(mu).

:- op(1000, xfy, ('==>')).


setup_moo ==> {writeln(setup_moo)}.

%:- expects_dialect(pfc).

setup_moo2 ==> {writeln(setup_moo2)}.

:- ensure_loaded(library(logicmoo_nlu/parser_lexical)).

:- module(mu).

%:- ensure_loaded(adv_loader).


