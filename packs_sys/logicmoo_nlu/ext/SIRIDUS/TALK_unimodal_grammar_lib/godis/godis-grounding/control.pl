/*************************************************************************

         name: control.pl 
  description: Control algorithm for GoDiS-basic 
       
*************************************************************************/


:- ensure_loaded( library( control_operators) ).
:- ensure_loaded( library( tis_operators ) ).


/*========================================================================
   The control algorithm 
========================================================================*/

concurrent :- !,fail.
%concurrent.

control_algorithm([ input:init,
		    interpret:init,
		    generate:init,
		    output:init,
		    repeat (  [
			     select,
			     if not is_empty($next_moves)
			     then [
				   generate,
				   output,
				   update,
				   print_state ]
			     else [],
			     test( $program_state == run ),
			     input,
			     interpret,
			     update,
			     print_state
			     ] )
		  ] ) :- \+ concurrent.


control_algorithm((

 input: {
    init =>
	[ input:init,
          input:display_prompt ],
    new_data(in) =>
      [ input ],
    condition($program_state == quit) =>
      input:quit
 } |

 interpretation: {
   import interpret,

   init => interpret:init,  
   condition(is_set(input)) =>
      [ interpret, print_state ]
 } |

 dme: {
   import update,
   import select,
       
   init => [ select ],
   condition(not empty($latest_moves)) =>
    [
      update,
      if $latest_speaker == usr then
        select
      else
        []
    ]
 } |
		  
 generation: {

   init => generate:init,
   condition(not empty($next_moves)) =>
      generate
 } |

 output: {

   init => output:init,
	  
   condition(is_set(output)) =>
      output
 }

)) :- concurrent.
