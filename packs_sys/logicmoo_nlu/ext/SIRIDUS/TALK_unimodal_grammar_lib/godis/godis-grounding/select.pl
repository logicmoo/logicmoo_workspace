/*************************************************************************

         name: select.pl 
  description: uses ADL = algorithm definition language 
 
*************************************************************************/

/*========================================================================
     Module Declaration
========================================================================*/

:- module(select,[select/0]).

:- use_module(library(tis_access)).
:- use_module(library(tis_operators) ).

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


selection_algorithm( [
		      backupShared,
		      if  not ( in( $/private/agenda, A ) and q_raising_action( A ) )
		      then ( try select_action )
		      else [],
		      repeat ( select_icm orelse select_move ) ]  ).


/*========================================================================
   Main predicate
========================================================================*/

select :-
	selection_algorithm( Algorithm ),
	adl_exec( Algorithm ).
