:- module(logicmoo_dec,[test_logicmoo_dec/0]).

% [Required] Load the Logicmoo Library Utils
:- ensure_loaded(library(logicmoo_common)).

:- reexport(logicmoo_planner).

:- use_module(library(logicmoo_lps)).


test_logicmoo_dec:- 
   test_logicmoo_ec_sanity,
   test_logicmoo_ec_lps_reader.


%:- use_module(library(lps_corner)).
:- user:use_module(library(ec_planner/ec_lps_convert)).
:- user:reexport(library(ec_planner/ec_lps_convert)).

%:- use_module(library(eggdrop)).
%:- egg_go.

