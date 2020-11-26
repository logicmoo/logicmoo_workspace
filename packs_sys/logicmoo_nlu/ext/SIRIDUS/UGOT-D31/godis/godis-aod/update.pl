/*************************************************************************

         name: update.pl 
  description: GoDiS-AOD update module.
               uses ADL = algorithm definition language 
      authors: Original code (June 2002) by Staffan Larsson (SL)
               Modifications by SL and David Hjelm (DH)
       
*************************************************************************/

/*========================================================================
     Module Declaration
========================================================================*/

:- module(update,[update/0]).
%trindikit 3 or trindikit4


%TRINDIKIT4 test
:- ( current_module(tkit_properties)->
       use_module(trindikit(tkit_tis_access)),
       use_module(trindikit(tkit_operators));
       use_module(library(tis_access)),
       use_module(library(tis_operators))).

/*========================================================================
     Load rules
========================================================================*/

:- ensure_loaded(library(update_rules)).

/*========================================================================
     Load the ADL interpreter
========================================================================*/

%TRINDIKIT4 test
:- ( current_module(tkit_properties)->
       ensure_loaded(trindikit(tkit_dme_adl));
       ensure_loaded(library(dme_adl))).

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
				 accommodate, try retract
			       ] ),
			apply cancel_shift( /private/nim ),
			repeat exec_plan,
			try downdate_qud,
			repeat downdate_issues,
			repeat exec_plan,
			repeat removeYesNo %SL021125
		       ]
		  else
		  [ ( failedFollowup orelse try unclearFollowup ),
		    try noContact, %DH 031201
		    apply clear( /shared/lu/moves ) ] % this really needed???
		  ] ).

/*========================================================================
   Main predicate
========================================================================*/

update :-
	update_algorithm( Algorithm ),
	adl_exec( Algorithm ).

