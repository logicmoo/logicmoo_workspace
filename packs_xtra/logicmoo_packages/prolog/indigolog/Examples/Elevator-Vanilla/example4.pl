% This is like example1, except that sensing is used to find out which 
%    elevator call lights are on.
% run: ?- indigolog(control).
%
%  Respond to "look(i):" (where i is a floor) with either "on." or "off."
%  The elevator stops when it is parked and all the lights are off.

% Interface to the outside world via read and write
execute(A,Sr) :- ask_execute(A,Sr).
exog_occurs(_) :- fail.

fl(N) :- N=1; N=2; N=3; N=4; N=5; N=6.    % the elevator floors 

% Actions 
prim_action(down).              % elevator down one floor 
prim_action(up).                % elevator up one floor 
prim_action(off(N)) :- fl(N).   % turn off call button on floor n
prim_action(look(N)) :- fl(N).  % check call button on floor n 

% Fluents 
prim_fluent(floor).             % the floor the elevator is on (1 to 6)
prim_fluent(light(N)) :- fl(N). % call button of floor n is (on or off)

% Causal laws
causes_val(up,   floor, N, N is floor+1).
causes_val(down, floor, N, N is floor-1).
causes_val(off(N), light(N), off, true).   % Note: nothing puts light on

% Preconditions  of prim actions 
poss(down,    neg(floor=1)).
poss(up,      neg(floor=6)).
poss(off(N),  and(floor=N,light(N)=on)).
poss(look(_), true).

% Sensing axioms for primitive fluents. 
senses(look(N), light(N)).      % look(n) asks for the value of light(n) 

% Initial state: elevator is at floor 3, the button states are unknown 
initially(floor,3).

% Definitions of complex conditions 
proc(below_floor(N), floor<N).
proc(above_floor(N), floor>N).
proc(next_floor_to_serve(N), light(N)=on).

% Definitions of complex actions    
proc(go_floor(N), while(neg(floor=N), if(below_floor(N),up,down))).
proc(serve_a_floor, pi(n, [?(next_floor_to_serve(n)), go_floor(n), off(n)])).
proc(check_buttons, [look(1), look(2), look(3), look(4), look(5), look(6)]).

proc(control, 
  [ check_buttons, 
    while(or(some(n,light(n)=on), above_floor(1)), 
      if(some(n,light(n)=on), serve_a_floor, [down, check_buttons])) ]).
