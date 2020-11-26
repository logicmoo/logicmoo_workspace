/*************************************************************************

         name: update.pl 
      version: 
  description: uses ADL = algorithm definition language 
       author: Peter Bohlin, Staffan Larsson
 
*************************************************************************/

/*========================================================================
     Module Declaration
========================================================================*/

:- module(select,[select/0]).

:- use_module(trindikit(tkit_tis_access)).
:- use_module(trindikit(tkit_operators)).

/*========================================================================
     Load rules
========================================================================*/

:- ensure_loaded(library(selection_rules)).

/*========================================================================
     Load the ADL interpreter
========================================================================*/

:- ensure_loaded(trindikit(tkit_dme_adl)).

/*========================================================================
   The selection algorithm 
========================================================================*/

selection_algorithm( [ if empty($/private/agenda) then (try select_action),
		       try select_move ]  ).


/*========================================================================
   Main predicate
========================================================================*/

select :-
	selection_algorithm( Algorithm ),
	adl_exec( Algorithm ).
