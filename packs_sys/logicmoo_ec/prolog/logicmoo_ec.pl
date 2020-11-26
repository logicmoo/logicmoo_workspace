:- module(logicmoo_ec,[]).

% [Required] Load the Logicmoo Library Utils
:- ensure_loaded(library(logicmoo_common)).

:- reexport(logicmoo_planner).

:- use_module(library(logicmoo_lps)).
%:- use_module(library(logicmoo_dec)).
:- use_module(library(logicmoo_icl)).
:- use_module(library(logicmoo_rsasak)).
:- use_module(library(logicmoo_ocl)).
:- use_module(library(ec_planner/ec_reader)).
:- use_module(library(ec_planner/ec_lps_convert)).

%test_logicmoo_ec:- run_tests.


:- dynamic user:prolog_file_type/2.
:- multifile user:prolog_file_type/2.

user:prolog_file_type(pel, prolog).
user:prolog_file_type(e, prolog).




