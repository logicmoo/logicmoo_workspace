/*************************************************************************

         name: update.pl 
  description: GoDiS AOD update module.
               Uses ADL = algorithm definition language.
       author: Staffan Larsson
 
*************************************************************************/

/*========================================================================
     Module Declaration
========================================================================*/

:- module(select,[select/0]).

%TRINDIKIT4 test
:- ( current_module(tkit_properties)->
       use_module(trindikit(tkit_tis_access)),
       use_module(trindikit(tkit_operators));
       use_module(library(tis_access)),
       use_module(library(tis_operators))).

/*========================================================================
     Load rules
========================================================================*/

:- ensure_loaded(library(selection_rules)).

/*========================================================================
     Load the ADL interpreter
========================================================================*/

%TRINDIKIT4 test
:- ( current_module(tkit_properties)->
       ensure_loaded(trindikit(tkit_dme_adl));
       ensure_loaded(library(dme_adl))).

/*========================================================================
   The selection algorithm 
========================================================================*/


selection_algorithm( [
		      backupSharedSys,
		      if  not ( in( $/private/agenda, A ) and q_raising_action( A ) )
		      then ( try select_action )
		      else [],
		      % select ICM and moves for actions resulting from update
		      repeat ( select_icm orelse select_move ) ]  ).



/*========================================================================
   Main predicate
========================================================================*/

select :-
	selection_algorithm( Algorithm ),
	adl_exec( Algorithm ).

