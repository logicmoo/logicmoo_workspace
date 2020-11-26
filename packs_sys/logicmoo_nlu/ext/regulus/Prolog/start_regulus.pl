
% Top-level Regulus file

:- consult(regulus_config).

:- ensure_loaded(regulus_test).

:- compile(library(regulus2nuance)).

:- compile(library(regulus2dcg)).

:- compile(library(regulus_dcg_runtime)).


