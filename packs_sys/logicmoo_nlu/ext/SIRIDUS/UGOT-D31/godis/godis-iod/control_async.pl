/*************************************************************************

         name: control_async.pl 
  description: Control algorithm for IBiS 3,4; allows system to take
               several successive turns
 
*************************************************************************/


:- ensure_loaded( library( control_operators) ).
:- ensure_loaded( library( tis_operators ) ).


/*========================================================================
   The control algorithm 
========================================================================*/



control_algorithm((

 input: {
    condition(($program_state == init) and ($latest_moves == set([]))) =>
      [ input:init,
        input:display_prompt ],
    new_data(in) =>
      [ input,
        input:display_prompt ],
    condition(is_set(domain)) =>
      input:change_domain,
    condition($program_state == quit) =>
      input:quit
 } |

 interpretation: {
   import interpret,
		  
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
   condition(is_set(next_moves)) =>
      generate
 } |

 output: {
   condition(is_set(output)) =>
      output
 }

)).
