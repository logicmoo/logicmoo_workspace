%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: ElevatorLego-BAT/elevator.pl
%
%       BAT axiomatization of the elevator example
%
%  AUTHOR : Sebastian Sardina (2001)
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  TYPE   : system independent code
%  TESTED : SWI Prolog 5.0.10 http://www.swi-prolog.org
%           ECLIPSE 5.4 http://www.icparc.ic.ac.uk/eclipse/
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             May 18, 2001
%
% This software was developed by the Cognitive Robotics Group under the
% direction of Hector Levesque and Ray Reiter.
% 
%        Do not distribute without permission.
%        Include this notice in any copy made.
% 
% 
%         Copyright (c) 2000 by The University of Toronto,
%                        Toronto, Ontario, Canada.
% 
%                          All Rights Reserved
% 
% Permission to use, copy, and modify, this software and its
% documentation for non-commercial research purpose is hereby granted
% without fee, provided that the above copyright notice appears in all
% copies and that both the copyright notice and this permission notice
% appear in supporting documentation, and that the name of The University
% of Toronto not be used in advertising or publicity pertaining to
% distribution of the software without specific, written prior
% permission.  The University of Toronto makes no representations about
% the suitability of this software for any purpose.  It is provided "as
% is" without express or implied warranty.
% 
% THE UNIVERSITY OF TORONTO DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
% SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
% FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TORONTO BE LIABLE FOR ANY
% SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
% RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
% CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%  A basic action theory (BAT) is described with:
%
% -- fun_fluent(fluent)     : for each functional fluent (non-ground)
% -- rel_fluent(fluent)     : for each relational fluent (non-ground)
%
%           e.g., rel_fluent(painted(C)).
%           e.g., fun_fluent(color(C)).
%
% -- prim_action(action)    : for each primitive action (ground)
% -- exog_action(action)    : for each exogenous action (ground)
%
%           e.g., prim_action(clean(C)) :- domain(C,country).
%           e.g., exog_action(painte(C,B)):- domain(C,country), domain(B,color).
%
% -- senses(action,fluent)  : for each sensing action
%
%           e.g, poss(check_painted(C),  painted(C)).
%
% -- poss(action,cond)      : when cond, action is executable
%
%           e.g, poss(clean(C),   and(painted(C),holding(cleanear))).
%
% -- initially(fluent,value): fluent has value in S0 (ground)
%
%          e.g., initially(painted(C), false):- domain(C,country), C\=3.
%                initially(painted(3), true).
%                initially(color(3), blue).
%
% -- causes_val(action,fluent,value,cond)
%          when cond holds, doing act causes functional fluent to have value
%
%            e.g., causes_val(paint(C2,V), color(C), V, C = C2).
%               or causes_val(paint(C,V), color(C), V, true).
%
% -- causes_true(action,fluent,cond)
%          when cond holds, doing act causes relational fluent to hold
% -- causes_false(action,fluent,cond)
%          when cond holds, doing act causes relational fluent to not hold
%
%            e.g., causes_true(paint(C2,_), painted(C), C = C2).
%               or causes_true(paint(C,_), painted(C), true).
%            e.g., causes_false(clean(C2),  painted(C), C = C2).
%               or causes_false(clean(C),  painted(C), true).
%
% -- sort(name,domain_of_sort).      : all sorts used in the domain
%
%        e.g., varsort(c, colors).
%              varsort(temp, temperature).
%              color([blue, green, yellow, red]).       
%              temperature([-10,0,10,20,30,40]).
%
%
% A high-level program-controller is described with:
%
% -- proc(name,P): for each procedure P 
% -- simulator(N,P): P is the N exogenous action simulator
%
% The interface for Lego is described with:
%
% -- actionNum(action, num)  
%         action has RCX code num
% -- simulateSensing(action)
%         sensing result for action should be asked to the user
% -- translateSensing(action, sensorValue, sensorResult) 
%         translate the sensorValue of action to sensorResult
% -- translateExogAction(codeAction, action) 
%         translateSensing action name into codeAction and vice-versa
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* DOMAINS/SORTS */
%fl([1,2,3,4,5,6,7,8,9,10]).       % possible floors 
fl([1,2,3,4,5,6]).       % possible floors 
dir([up,down]).                    % possible directions 
temperature([15,20,25,30,35]).     % possible temperatures

% There is nothing to do caching on (required becase cache/1 is static)
cache(_):-fail.

  /*  FLUENTS and CAUSAL LAWS */
rel_fluent(directionUp).           % Aiming up?
causes_true(turnaround,  directionUp, neg(directionUp)).
causes_false(turnaround, directionUp, directionUp).

fun_fluent(state).                 % Moving, stopped, or suspended (lost)
causes_val(go,              state, moving, true).
causes_val(freeze,          state, freezed, true).
causes_val(arrive,          state, stopped, true).
causes_val(getStuck,        state, stopped, true).
causes_val(stop_abnormally, state, suspended, true).


rel_fluent(closed(_)). 	           % floor is out of service 
causes_true(deny(N),   closed(N), true).
causes_false(allow(N), closed(N), true).

rel_fluent(light(_,_)).            % light buttons of each floor
causes_true(on(N,D),   light(N,D), true).
causes_false(off(N,D), light(N,D), true).
% Action check(N,D) gives the truth value of light(N,D)
senses(check(N,D), light(N,D)).
forget(check(N,D), light(N,D)).

rel_fluent(fan).                   % the fan is on or off 
causes_true(toggle,   fan, neg(fan)).
causes_false(toggle,  fan, fan).

rel_fluent(alarm).                 % the smoke alarm is on or off 
causes_true(smoke,   alarm,  true).
causes_false(reset,  alarm,  true).

fun_fluent(temp).                  % the temp of the elevator 
causes_val(heat,  temp, T, T is temp+5).
causes_val(cold,  temp, T, T is temp-5).
senses(thermo, temp).

fun_fluent(floor).                 % the floor the elevator is on 
causes_val(arrive, floor, N, or(and(directionUp,      N is floor + 1),
                                and(neg(directionUp), N is floor - 1))).

rel_fluent(talking).           % Should we talk loud?
causes_true(talk,    talking, true).
causes_false(shutup, talking, true).


  /*  ACTIONS and PRECONDITIONS*/
prim_action(go).                % go to next floor 
prim_action(freeze).            % stop moving
prim_action(turnaround).        % turn around to change the direction 
prim_action(off(N,D))  :- domain(N,fl), 
                          domain(D,dir). % turn off button N direction D
prim_action(open).              % open elevator door 
prim_action(close).             % close elevator door
prim_action(toggle).            % toggle the fan 
prim_action(ring).              % ring the smoke alarm 
prim_action(check(N,D)):- domain(N,fl), 
                          domain(D,dir). % sensing action to check lights 
prim_action(thermo).            % sensing action to check temperature 

prim_action(say(_)).            % say a message

exog_action(smoke).              % smoke enters elevator 
exog_action(reset).              % smoke detector alarm is reset 
exog_action(heat).               % temp=temp+5 
exog_action(cold).               % temp=temp-5 
exog_action(deny(N)) :- domain(N,fl).   % make floor N not available  
exog_action(allow(N)):- domain(N,fl).   % make floor N available  
exog_action(on(N,D)) :- domain(N,fl), 
                        domain(D,dir). % button (N,D) has been pushed  

exog_action(arrive).             % Arrive successfully and stop
exog_action(stop_abnormally).    % Stop because confused

exog_action(talk).
exog_action(shutup).

poss(go,     or( and(directionUp, floor < 10),
                 and(neg(directionUp), floor > 1)) ).
poss(off(N,_), floor = N).
poss(check(_,D), or( and(D=up, directionUp),
                     and(D=down, neg(directionUp))) ).
poss(thermo, true).
poss(open,   true).
poss(close,  true).
poss(toggle, true).
poss(ring,   true).
poss(freeze, state=moving).
poss(turnaround, state = stopped).
poss(say(_), true).

/* ABBREVIATIONS */
proc(too_hot,  temp >= 30).
proc(too_cold, temp < 30).
proc(below_floor(N), floor < N).
proc(above_floor(N), floor > N).



  /* INITIAL STATE: elevator is at floor 3, lights 2 and 5 are on */
initially(floor, 1). 
initially(fan,   false).
initially(alarm, false).
initially(closed(N),false) :- domain(N,fl).
initially(light(1,up),false).
%initially(light(N,D),false):- domain(N,fl), domain(D,dir).
%initially(temp,25).	% Temperature is unknown in S0 
initially(directionUp, true). 
initially(state, stopped).
initially(talking, true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definitions of complex actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
proc(serve(F,D), [open, close, off(F,D)]).

proc(floorOk(F), or(and(directionUp,neg(light(F,up))),
                    and(neg(directionUp),neg(light(F,down))))).

proc(start_up(X),   [search([star(turnaround),?(directionUp)]),
                     searchc(goto(X),'Computing path UP')] ).
proc(start_down(X), [search([star(turnaround),?(neg(directionUp))]),
                     searchc(goto(X),'Computing path DOWN')] ).

% Talk if talking is true
proc(talk(M), if(neg(talking), ?(true), say(M))).

proc(goto(X), 
   pi(f,[?(floor=f),
         if(f=X,[serve(f,up), serve(f,down)],
                pi(d,[?(or(d=up,d=down)),
                      check(f,d), 
                      branch(light(f,d)),
                      wndet(?(floorOk(f)), serve(f,d)),
                      go, sim(arrive), 
                      goto(X)]) )])).

proc(recover_position, 
     pi(m,
	[if(directionUp,?(reset(floor, 1,m)),?(reset(floor, -1,m))),
         say(m),
         ?(get(_))
        ])
    ). 

% reset(Location, Direction): auxiliary predicate
reset(Location, Direction, M) :-
    (Direction = 1 -> MDir=' going up.' ; MDir=' going down.'),
    concat_atom(['**** I got lost heading from waystation ', Location, 
                 'while ', MDir,
                 '. Please position me between waystations in ',
                 ' the correct direction, and type any key when ready: '],M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Main Routine
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% THIS IS THE MAIN PROCEDURE FOR INDIGOLOG
proc(main,  mainControl(N)) :- controller(N), !.
proc(main,  mainControl(1)). % default one



% Elevator with alarm
proc(mainControl(0), [prioritized_interrupts(
        [interrupt(alarm, ring),
         interrupt(state = moving, wait),
         interrupt(state = suspended, [recover_position, go]),
         interrupt(floor=1, [start_up(6), start_down(1)])] )]).

% Elevator with alarm and temperature and speech
proc(mainControl(1), [thermo, prioritized_interrupts(
        [interrupt(and(too_hot,neg(fan)), 
                   [talk('Too hot!. Turning on the fan'), toggle]),
         interrupt(and(too_cold,fan),
                   [talk('Too cold!. Turning off the fan'), toggle]),
         interrupt(alarm, search([star(freeze), ?(neg(state=moving)), ring])),
         interrupt(state = moving, wait),
         interrupt(state = suspended, [recover_position, go]),
         interrupt(state = freezed, go),
         interrupt(floor = 1, [talk('Planning to go up'),
                               start_up(6), 
                               talk('Planning to go down'),
                               start_down(1)])] )]).

% Elevator with alarm and temperature
proc(mainControl(2), [thermo, prioritized_interrupts(
        [interrupt(or(and(too_hot,neg(fan)),and(too_cold,fan)), toggle),
         interrupt(alarm, search([star(freeze), ?(neg(state=moving)), ring])),
         interrupt(state = moving, wait),
         interrupt(state = suspended, [recover_position, go]),
         interrupt(state = freezed, go),
         interrupt(floor = 1, [start_up(6), start_down(1)])] )]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Action/message mappings - numbers must correspond to NQC code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% actionNum(?Action, ?ActionNumber): Returns ActionNumber associated
%     with Action and vice versa. ActionNumber can be sent to the RCX
%     for execution of Action. It can be returned from the RCX to
%     report the occurrence of exogenous Action
actionNum(off(_,_), 0).
actionNum(recover_position, 0). % 0 is a general void action
actionNum(thermo, 0).

actionNum(turnaround, 1).
actionNum(go, 2).
actionNum(open, 3).
actionNum(close, 4).
actionNum(ring, 5).
actionNum(toggle, 5).
actionNum(freeze, 8).

actionNum(check(_,_), 7).
actionNum(arrive, 20).
actionNum(stop_abnormally, 21).
actionNum(reset, 22).

actionNum(say(M),   say(M, english)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translation of sensor values from RCX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% translateSensing(+Action, +SensorValue, SensingResult): Translate
%     the value SensorValue returned by the RCX sensor into a legal
%     SensingResult under Action



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: ElevatorLego-BAT/elevator.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%