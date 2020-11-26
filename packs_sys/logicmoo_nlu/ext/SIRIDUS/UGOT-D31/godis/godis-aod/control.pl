/*************************************************************************

         name: control.pl 
  description: Control algorithm for GoDiS-IOD and AOD; allows system to take
               several successive turns
       author: Staffan Larsson
 
*************************************************************************/

%trindikit 3 or trindikit4
:- ( current_module(tkit_properties)->
       ensure_loaded(trindikit(tkit_operators));
       ensure_loaded( library( control_operators) ),
       ensure_loaded( library( tis_operators ) ) ).


/*========================================================================
   The control algorithm 
========================================================================*/

concurrent:-!,fail.
%concurrent.

control_algorithm( [
		    input:init,
		    interpret:init,
		    generate:init,
		    output:init,
		    repeat (  [
			       select,
			       if not is_empty($next_moves) % sys has something to say
			      then [ % system
				     print_state,
				     generate,
				     output,
				     update,
				     print_state ]
			      else [],
			       test( $program_state == run ),
	       % if system has nothing more to say, or QUD nonempty...
			       if is_empty($/private/agenda) or not is_empty($/shared/qud)
			      then [ % give turn to user...
				     input,
				     interpret,
				     update ]
			      else [], % ...otherwise start over
			       print_state
			      ] ),
		    input:quit,
		    output:quit
		   ] ) :- \+ concurrent.


%%% control_algorithm((

%%%  input: {
%%% %    condition((($program_state == init) and ( $latest_moves == oqueue([])))) =>
%%%     init =>
%%% 	[ input:init,
%%%           input:display_prompt ],
%%%     new_data(in) =>
%%%       [ input,
%%%         input:display_prompt ],
%%%     condition(is_set(domain)) =>
%%%       input:change_domain,
%%%     condition($program_state == quit) =>
%%%       input:quit
%%%  } |

%%%  interpretation: {
%%%    import interpret,

%%%    init => interpret:init,  
%%%    condition(is_set(input)) =>
%%%       [ interpret, print_state ]
%%%  } |

%%%  dme: {
%%%    import update,
%%%    import select,
       
%%%    init => [ select ],
%%%    condition(not empty($latest_moves)) =>
%%%     [
%%%       update,
%%%       if $latest_speaker == usr then
%%%         select
%%%       else
%%%         []
%%%     ]
%%%  } |
		  
%%%  generation: {

%%%    init => generate:init,
%%%    condition(not empty($next_moves)) =>
%%%       generate
%%%  } |

%%%  output: {

%%%    init => output:init,
	  
%%%    condition(is_set(output)) =>
%%%       output
%%%  }

%%% )) :- concurrent.
