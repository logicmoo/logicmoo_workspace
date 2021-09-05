:- module(logicmoo_dec,[test_logicmoo_dec/0]).
/** <module> MODULE LOGICMOO DEC
This module holds Eric Mullers DEC reasoner E-file interface for LOGICMOO.
- @author Douglas R. Miles
- @license LGPL
*/

% [Required] Load the Logicmoo Library Utils
:- ensure_loaded(library(logicmoo_common)).

:- reexport(logicmoo_planner).


test_logicmoo_dec:- 
   test_logicmoo_ec_sanity,
   test_logicmoo_ec_lps_reader.


%:- use_module(library(lps_corner)).
%:- use_module(library(logicmoo_lps)).

%:- use_module(library(eggdrop)).
%:- egg_go.

