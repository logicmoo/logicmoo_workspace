
% Top-level Regulus file (compiled version)

:- ensure_loaded(regulus_config).

:- ensure_loaded(regulus_test).

:- compile(library(regulus2nuance)).

:- load(library(regulus2nuance_main)).

:- load(library(regulus_declarations)).

:- load(library(regulus_expand)).

:- load(library(regulus_read)).

:- load(library(regulus_utilities)).

:- load(library(regulus_write_nuance)).

:- load(library(dcg_parsing)).

:- load(library(utilities)).

