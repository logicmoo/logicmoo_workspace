/*************************************************************************

         name: update.pl 
  description: IBiS1 update module
 
*************************************************************************/

/*========================================================================
     Module Declaration
========================================================================*/

:- module(update,[update/0]).

:- use_module(library(tis_access)).
:- use_module(library(tis_operators)).


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
%                 if not ($latest_moves == failed or in($latest_moves, no_move))
		 if not ($latest_moves == failed) 
                 then [
		       apply clear(/private/agenda),
		       getLatestMove,
		       try integrate,
%		       repeat [ integrate, findPlan ], 
		       try downdate_qud,
		       try load_plan,
		       repeat exec_plan
		      ]
		else []
		).

/*========================================================================
   Main predicate
========================================================================*/

update :-
	update_algorithm( Algorithm ),
	adl_exec( Algorithm ).
