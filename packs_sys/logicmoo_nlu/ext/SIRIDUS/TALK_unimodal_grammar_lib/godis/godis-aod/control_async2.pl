
:- ensure_loaded( library( control_operators) ).
:- ensure_loaded( library( tis_operators ) ).

% input runs as a separate (active) agent, not controlled by algorithm,
% except for when initializing and quitting

% interpret, update,select and generate run controlled by a single algorithm
% which triggers on setting of input variable.
% this algorithm consists of relevant parts from synchronous algorithm

% output triggers on setting of output variable


control_algorithm(A):-
  setof(Trig=>Algo,Trig=>Algo,A).

%initialization
init => [ input:init, output:init, generate:init, interpret:init,
          select,generate].

%quitting
condition( $program_state == quit) => 
        [ input:quit,output:quit, generate:quit, interpret:quit].

%interpretation, DME, generation
condition( is_set(input)) => 
  [ interpret,
    update,
    print_state,
    select,
    if not is_empty($latest_moves) then 
      [print_state,generate] 
   ].

%output
condition(not empty($output)) => [ output, update].
