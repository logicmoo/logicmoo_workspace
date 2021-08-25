% This is the original Golog elevator with no exogenous events, no sensing 
% Serve each floor whose call button is on initially, then park the elevator.
% run: ?- indigolog(control). 
%
% No user input is required.

% Interface to the outside world via read and write 
execute(A,Sr) :- ask_execute(A,Sr).
exog_occurs(_) :- fail.

fl(N) :- N=1; N=2; N=3; N=4; N=5; N=6.    % the 6 elevator floors

% Actions 
prim_action(down).              % elevator down one floor
prim_action(up).                % elevator up one floor
prim_action(off(N)) :- fl(N).   % turn off call button on floor n
prim_action(open).              % open elevator door
prim_action(close).             % close elevator door

% Fluents 
prim_fluent(floor).             % the floor the elevator is on (1 to 6)
prim_fluent(light(N)) :- fl(N). % call button of floor n (on or off)

% Causal laws 
causes_val(up,   floor, N, N is floor+1).
causes_val(down, floor, N, N is floor-1).
causes_val(off(N), light(N), off, true).  % Note: nothing turns a light on

% Preconditions of prim actions
poss(down,    neg(floor=1)).
poss(up,      neg(floor=6)).
poss(off(N),  and(floor=N,light(N)=on)).
poss(open, true).
poss(close, true).

% Initial state: elevator is at floor 3, and lights 2 and 5 are on
initially(floor,3).
initially(light(1), off).
initially(light(2), on).
initially(light(3), off).
initially(light(4), off).
initially(light(5), on).
initially(light(6), off).

% Definitions of complex conditions
proc(below_floor(N), floor<N).
proc(above_floor(N), floor>N).
proc(next_floor_to_serve(N), light(N)=on).

% Definitions of complex actions
proc(go_floor(N), while(neg(floor=N), if(below_floor(N),up,down))).
proc(serve_a_floor, pi(n, 
   [ ?(next_floor_to_serve(n)), go_floor(n), open, close, off(n) ])).
proc(control, 
   [ while( some(n,light(n)=on), serve_a_floor ),
     go_floor(1),
     open ] ).
