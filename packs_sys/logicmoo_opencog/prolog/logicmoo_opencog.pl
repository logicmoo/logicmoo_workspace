:- module(logicmoo_opencog,[test_logicmoo_opencog/0]).
/** <module> MODULE LOGICMOO OPENCOG
This replicates Eric Mullers DEC reasoner interface (for OpenCog).
- @author Douglas R. Miles
*/

% [Required] Load the Logicmoo Library Utils
:- ensure_loaded(library(logicmoo_common)).

:- reexport(opencog/atomspace).
:- reexport(opencog/atomeese).

:- use_module(library(logicmoo_ec)).
:- use_module(library(logicmoo_dec)).

test_logicmoo_opencog:- 
   test_logicmoo_opencog_sanity,
   test_logicmoo_opencog_lps_reader.

