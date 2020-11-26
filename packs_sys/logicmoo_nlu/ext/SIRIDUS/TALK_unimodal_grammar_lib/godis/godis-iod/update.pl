/*************************************************************************

         name: update.pl 
  description: uses ADL = algorithm definition language 
 
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

update_algorithm( [
		   if not ($latest_moves == failed) 
		   then [
			getLatestMoves,
			try backupSharedUsr,
			try irrelevantFollowup,
			try noFollowup,
			repeat(
			       [ repeat
			       ( [ integrate,			     
				   try downdate_issues,
				   try removeFindout,
				   try load_plan ]
			       orelse apply shift( /private/nim ) )
			       until fully_shifted( $/private/nim ),
				 apply shift( /private/nim ),
				 try select_action,
				 accommodate
			       ] ),
			apply cancel_shift( /private/nim ),
			repeat exec_plan,
			try downdate_qud,
			repeat downdate_issues, 
			repeat exec_plan
		       ]
		  else
		  [ ( failedFollowup orelse try unclearFollowup ),
		    apply clear( /shared/lu/moves )  ]
		  ] ).

/*========================================================================
   Main predicate
========================================================================*/

update :-
	update_algorithm( Algorithm ),
	adl_exec( Algorithm ).
