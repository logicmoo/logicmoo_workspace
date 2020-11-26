 
/*************************************************************************

         name: interface_variables.pl 
  description: Definitions of the interface variables
 
*************************************************************************/

:- module( module_interfaces, [ interface_variable_of_type/2 ] ).



/*----------------------------------------------------------------------
     The GoDiS interface variables
----------------------------------------------------------------------*/

interface_variable_of_type( input, string ).
interface_variable_of_type( output, string ).
interface_variable_of_type( latest_speaker, speaker ).
interface_variable_of_type( latest_moves, set(dmove) ).
interface_variable_of_type( next_moves, set(dmove) ).
interface_variable_of_type( program_state, program_state ).













