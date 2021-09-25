:- module(logicmoo_ec,[]).
/** <module> MODULE LOGICMOO EC
This module combines all LOGICMOO EventCalc modules into a unified interface for DEC LPS, PPDL. 
-@author Douglas R. Miles
-@license LGPL
*/

:- dynamic(ensure_lps/0).
:- multifile(ensure_lps/0).
% [Required] Load the Logicmoo Library Utils
:- ensure_loaded(library(logicmoo_common)).

%:- use_module(library(logicmoo_dec)).
:- use_module(library(logicmoo_icl)).
:- use_module(library(logicmoo_rsasak)).
:- use_module(library(logicmoo_ocl)).
:- use_module(library(ec_planner/ec_reader)).
:- use_module(library(ec_planner/ec_planner_dmiles)).

:- use_module(library(ec_planner/ec_lps_convert)).
:- use_module(library(ec_planner/lps_pddl_convert)).
:- listing(test_logicmoo_ec_lps_reader/0).

ensure_lps_now:- use_module(library(logicmoo_lps)).
:- listing(ensure_lps_now).

% :- ensure_lps_now.

:- reexport(logicmoo_planner).

%test_logicmoo_ec:- run_tests.

:- dynamic(user:prolog_file_type/2).
:- multifile(user:prolog_file_type/2).

user:prolog_file_type(pel, prolog).
user:prolog_file_type(e, prolog).

ensure_lps:- 
  current_module(swish) 
   -> ensure_lps_now
  ; fmt("Once SWISH is loaded, try: ?-ensure_lps.").

:- listing(ensure_lps/0).
:- ensure_lps.

%:- break.


