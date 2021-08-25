%  This is the elevator that appears in the IJCAI-97 paper on ConGolog
%  It uses exogenous actions for temperature, smoke, and call buttons 
%  run: ?- indigolog(control).
%
%  Respond to "Exogenous action:" with either "nil." or with one of the
%    exogenous actions below, such as "on(5)." or "heat."
%  The elevator stops when it is parked and all lights are off.

% Interface to the outside world via read and write
execute(A,Sr) :- ask_execute(A,Sr).
exog_occurs(A) :- ask_exog_occurs(A).

fl(N) :- N=1; N=2; N=3; N=4; N=5; N=6.      % the elevator floors

% Actions 
prim_action(down).               % elevator down one floor
prim_action(up).                 % elevator up one floor
prim_action(toggle).             % toggle the fan 
prim_action(ring).               % ring the smoke alarm 
prim_action(off(N)) :- fl(N).    % turn off call button on floor n

exog_action(heat).               % increase temperature by 1
exog_action(cold).               % decrease temperature by 1
exog_action(smoke).              % smoke enters elevator 
exog_action(reset).              % smoke detector alarm is reset
exog_action(on(N)) :- fl(N).     % turn on call button on floor n 

% Fluents 
prim_fluent(floor).              % the floor the elevator is on (1 to 6)
prim_fluent(temp).               % the temperature in the elevator (number)
prim_fluent(fan).                % the fan (on or off)
prim_fluent(alarm).              % the smoke alarm (on or off)
prim_fluent(light(N)) :- fl(N).  % call button of floor n (on or off)


% Causal laws 
causes_val(up,   floor, N, N is floor+1).
causes_val(down, floor, N, N is floor-1).

causes_val(heat, temp, X, X is temp+1).
causes_val(cold, temp, X, X is temp-1).

causes_val(toggle, fan, on,  fan=off).
causes_val(toggle, fan, off, fan=on).

causes_val(on(N),  light(N), on,  true).
causes_val(off(N), light(N), off, true).

causes_val(smoke, alarm, on,  true).
causes_val(reset, alarm, off, true).

% Preconditions of prim actions 
poss(down,   neg(floor=1)).
poss(up,     neg(floor=6)).
poss(off(N), and(floor=N,light(N)=on)).
poss(toggle, true).
poss(ring,   true).

% Initial state  
initially(floor,3).	
initially(temp,2).
initially(fan,off).
initially(light(_),off).   % all lights off initially
initially(alarm,off).

% Definitions of complex conditions
proc(too_hot, temp>2).
proc(too_cold, -2>temp).
proc(below_floor(N), floor<N).
proc(above_floor(N), floor>N).
proc(next_floor_to_serve(N), light(N)=on).

% Definitions of complex actions
proc(go_floor(N), while(neg(floor=N), if(below_floor(N),up,down))).
proc(serve_floor(N), [go_floor(N), off(N)]).
proc(control, prioritized_interrupts(
        [interrupt(and(too_hot,fan=off), toggle),
         interrupt(and(too_cold,fan=on), toggle),
         interrupt(alarm=on, ring),
         interrupt(n, next_floor_to_serve(n), serve_floor(n)),
         interrupt(above_floor(1), down)])).

