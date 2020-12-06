:- module(logicmoo_icl,[test_logicmoo_icl/0]).

% [Required] Load the Logicmoo Library Utils
:- ensure_loaded(library(logicmoo_common)).

:- reexport(logicmoo_planner).

test_logicmoo_icl:- 
   test_logicmoo_icl_sanity,
   test_logicmoo_icl_lps_reader.

test_logicmoo_icl_lps_reader.
test_logicmoo_icl_sanity.


%:- use_module(library(lps_corner)).
:- user:use_module(library('code_icl/icl_int.tex')).
:- user:reexport(library('code_icl/icl_int.tex')).

%:- use_module(library(eggdrop)).
%:- egg_go.

