/*************************************************************************

         name: update.pl 
      version: Apr 7, 1999, Nov 25, 1999
  description: uses ADL = algorithm definition language 
       author: Peter Bohlin, Staffan Larsson
 
*************************************************************************/

/*========================================================================
     Module Declaration
========================================================================*/

:- module(update,[update/0]).

:- use_module(library(tis),[check_condition/1,check_conditions/1,apply_operations/1, apply_operation/1]).

:- use_module(library(inoutput),[print_rule/1]).
:- use_module(library(error), [error/1]).

/*========================================================================
     Load rules
========================================================================*/

:- ensure_loaded(library(update_rules)).

/*========================================================================
     Load the ADL interpreter
========================================================================*/

:- ensure_loaded(library(dme_adl)).

/*========================================================================
   The update algorithm 
========================================================================*/

	
update_algorithm(
		 if ( latest_moves $== failed )
		then (repeat refill)
		else
		(   [ grounding,
		      repeat+ ( [ ( repeat accommodate ),
				  integrate ] ),
		      % ( repeat+  integrate ),
		      ( repeat manage_plan ),	  
		      ( if (latest_speaker $== usr)
		      then (repeat refill)
		      else store
		      )
		    ] ) ).

% main_update ==> [ (accommodate or database), (! integrate) ].

/*========================================================================
   Main predicate
========================================================================*/

update :-
	update_algorithm( Algorithm ),
	adl_exec( Algorithm ).
