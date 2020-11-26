/*************************************************************************

         name: control.pl 
      version: 
  description: Control module for GoDiS
       author: Peter Bohlin, Staffan Larsson
 
*************************************************************************/



/*========================================================================
     Module Declaration
========================================================================*/

:- module(control,[control/0]).

:- use_module(library(flags), [flag/2]).
:- use_module(library(tis),[check_condition/1]).
:- use_module(library(error),[error/1]).
:- use_module(library(inoutput),[print_state/0]).

/*----------------------------------------------------------------------
     TIS access restrictions
----------------------------------------------------------------------*/

read_access( [ program_state ] ).
write_access( [] ).

/*========================================================================
     Load the Control-ADL interpreter
========================================================================*/

:- ensure_loaded( library( control_adl ) ).


/*========================================================================
   The control algorithm 
========================================================================*/

control_algorithm( repeat (  [ select,
			       generate ,
			       output,
			       test( program_state $== run ),
			       update,
			       print_state,
			       test( program_state $== run ),
			       input,
			       interpret,
				%     print_state,
			       update,
			       print_state
			     ] )
		 ).
			    
