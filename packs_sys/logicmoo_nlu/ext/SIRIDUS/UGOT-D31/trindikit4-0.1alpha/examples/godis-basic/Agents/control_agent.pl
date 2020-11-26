% Trindikit control agent for use with godis-basic and an active input module
% David Hjelm 0507

% load trindikit
:-use_module('../../../core/prolog/trindikit').

% load application-specific search paths
:-ensure_loaded(app_search_paths).

% the control algorithm 
control_algorithm( [ init => [ input:init,
			       interpret:init,
			       generate:init,
			       output:init|
			       SysTurn ] ,
		     
		    condition(is_set(input)) => [
				        interpret,
					update,
					print_state|
					SysTurn ]
		   ] ):-
	
	SysTurn = [ select,
		    if not is_empty($next_moves) then
			    [ generate,
			      output,
			      update,
			      print_state ] else
		            []
		  ].

run:-
	control_algorithm( CA ),
	setprop(control,yes),
	setprop(control-algorithm,CA),
	setprop(oaa-libdir,'$OAA_HOME/src/oaalib/prolog'),
	setprop(oaa-name, controller),
	setprop(oaa,yes),
	control.
			     
