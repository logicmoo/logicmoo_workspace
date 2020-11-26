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

update_algorithm(
		 if not ($latest_moves == failed) 
                 then [ getLatestMoves,
			try irrelevantFollowup,
			repeat integrate,
			try load_plan,
			repeat exec_plan,
			repeat downdate_qud
		      ]
		else [ try unclearFollowup ]
		).

/*========================================================================
   Main predicate
========================================================================*/

update :-
	update_algorithm( Algorithm ),
	adl_exec( Algorithm ).
