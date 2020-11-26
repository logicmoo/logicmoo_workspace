/*************************************************************************

         name: update.pl 
      version: Apr 7, 1999, Nov 25, 1999
  description: uses ADL = algorithm definition language 
       author: Peter Bohlin, Staffan Larsson
 
*************************************************************************/

/*========================================================================
     Module Declaration
========================================================================*/

:- module(select,[select/0]).

:- use_module(library(tis),[check_condition/1,check_conditions/1,apply_operations/1]).
:- use_module(library(inoutput),[print_rule/1]).
:- use_module(library(error), [error/1]).

/*========================================================================
     Load rules
========================================================================*/

:- ensure_loaded(library(selection_rules)).

/*========================================================================
     Load the ADL interpreter
========================================================================*/

:- ensure_loaded(library(dme_adl)).

/*========================================================================
   The selection algorithm 
========================================================================*/

selection_algorithm( select ).
		 
/*========================================================================
   Main predicate
========================================================================*/

select :-
	selection_algorithm( Algorithm ),
	adl_exec( Algorithm ).
