 
/*************************************************************************

         name: interface_variables.pl 
  description: Definitions of the interface variables
 
*************************************************************************/

:- module( module_interfaces, [ interface_variable_of_type/2 ] ).



interface_variable_of_type( input, string ).
interface_variable_of_type( output, string ).
interface_variable_of_type( latest_speaker, speaker ).
interface_variable_of_type( latest_moves, oqueue(dmove) ).
interface_variable_of_type( next_moves, oqueue(dmove) ).
interface_variable_of_type( program_state, program_state ).

interface_variable_of_type( score, real ).
interface_variable_of_type( timeout, real ).
interface_variable_of_type( language, language ).
%DH 031201
interface_variable_of_type( contact, bool ).
%interface_variable_of_type( reco_threshold1, real ).
%interface_variable_of_type( reco_threshold2, real ).













