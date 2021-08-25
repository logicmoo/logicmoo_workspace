% This is just like example1, except that search is used to minimize
%   the mount of up and down motion that the elevator will need
% Serve each floor whose call button is on initially, then park the elevator.
% run: ?- indigolog(smart_control)  use search
% run: ?- indigolog(dumb_control)   the same but without search (fails)
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

% Definitions of complex actions
proc(go_floor(N), while(neg(floor=N), if(below_floor(N),up,down))).
proc(serve_floor(N), [ go_floor(N), open, close, off(N) ]).

proc(handle_reqs(Max),      % handle all elevator reqs in Max steps
    ndet(  [?(and(neg(some(n,light(n)=on)),Max>=floor-1)), go_floor(1), open],
            pi(n, pi(m, [ ?(and(light(n)=on, m is Max-abs(floor-n))),
                          ?(m > 0),
                          serve_floor(n),
                          handle_reqs(m) ] )))).

proc(minimize_motion(Max),  % iterative deepening search 
    ndet( handle_reqs(Max), pi(m, [?(m is Max+1), minimize_motion(m)]))).

proc(dumb_control, minimize_motion(0) ).           % always fails 
proc(smart_control, search(minimize_motion(0)) ).  % eventually succeeds
