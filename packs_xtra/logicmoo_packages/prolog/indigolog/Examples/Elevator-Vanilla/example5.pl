% This is like example4, except that there is a single sensing action
%  that is assumed to return the state of all the lights.
% run: ?- indigolog(control).
%
%  Respond to "look:" with a list of 6 elements, each of which is 0 or 1
%  The elevator stops when it is parked and all the lights are off.

% Interface to the outside world via read and write 
execute(A,Sr) :- ask_execute(A,Sr).
exog_occurs(_) :- fail.

fl(N) :- N=1; N=2; N=3; N=4; N=5; N=6.    % the elevator floors

% Actions 
prim_action(down).              % elevator down one floor 
prim_action(up).                % elevator up one floor
prim_action(off(N)) :- fl(N).   % turn off call button on floor n
prim_action(look).              % check all call buttons 

% Fluents 
prim_fluent(floor).             % the floor the elevator is on (1 to 6)
prim_fluent(lights).            % call buttons of all floors (a list)

% Causal laws
causes_val(up,   floor, N, N is floor+1).
causes_val(down, floor, N, N is floor-1).
causes_val(off(N), lights, L, repl(lights,L,N,0)).   % No turnon action

% Preconditions  of prim actions
poss(down,    neg(floor=1)).
poss(up,      neg(floor=6)).
poss(off(N),  and(floor=N,lighton(N))).
poss(look, true).

% Sensing axioms for primitive fluents. 
senses(look, lights).           % ask for current value of all lights

% Initial state: elevator is at floor 3, the button states are unknown 
initially(floor,3).

% Definitions of complex conditions   
proc(below_floor(N), floor<N).
proc(above_floor(N), floor>N).
proc(next_floor_to_serve(N), lighton(N)).
proc(lighton(N), nth1(N,lights,1)).
proc(floor_waiting, member(1,lights)).

% List utility used above
repl([_|L],[X|L],1,X).
repl([Y|L1],[Y|L2],N,X) :- repl(L1,L2,M,X), N is M+1.

% nth([X|_],1,X).
% nth([_|L],N,X) :- nth(L,M,X), N is M+1.

% member(X,[X|_]).
% member(X,[_|L]) :- member(X,L).

% Definitions of complex actions
proc(go_floor(N), while(neg(floor=N), if(below_floor(N),up,down))).
proc(serve_a_floor, pi(n, [?(next_floor_to_serve(n)), go_floor(n), off(n)])).
proc(control, 
  [ look, 
    while(or(floor_waiting, above_floor(1)), 
      if(floor_waiting, serve_a_floor, [down,look])) ]).
