%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: ElevatorSim-BAT/elevator.pl
%
%  AUTHOR : Sebastian Sardina (2001-2006)
%		(based on code previously written by Hector Levesque)
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  TYPE   : system independent code
%  TESTED : SWI Prolog 5.0.10 http://www.swi-prolog.org
%
%  This file contains 4 of the controllers from the original code
%  written by Hector Levesque for the 1st IndiGolog version:
%
%  mainControl(1) : (example2.pl in the original IndiGolog)
%  The dumb controller tries without search but commits too soon       
%
%  mainControl(2) : (example2.pl in the original IndiGolog)
%  The smart controller uses search to minimize the up-down motion     
%
%  mainControl(3) : (example3.pl in the original IndiGolog)
%  This is the elevator that appears in the IJCAI-97 paper on ConGolog 
%  It uses exogenous actions for temperature, smoke, and call buttons  
%
%  mainControl(4) : (example4.pl in the original IndiGolog)
%  This is the elevator with no exogenous events, but with sensing
%  actions for each call button of the elevator
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
:- dynamic controller/1.


/* DOMAINS-SORTS AVAILABLE */
fl([1,2,3,4,5,6,7,8,9,10]).       	% possible floors 
dir([up,down]).                    	% possible directions 
temperature([15,20,25,30,35]).     	% possible temperatures

% There is nothing to do caching on (required becase cache/1 is static)
cache(_):-fail.

floor(N) :- domain(N,fl).      /* N is a legal floor */


  /*  FLUENTS and CAUSAL LAWS */
fun_fluent(floor).              /* the floor the elevator is on */
causes_val(up,   floor, N, N is floor+1).
causes_val(down, floor, N, N is floor-1).

fun_fluent(temp).               /* the temperature of the elevator */
causes_val(heat, temp, X, X is temp+5).
causes_val(cold, temp, X, X is temp-5).

rel_fluent(fan).                   % the fan is on or off 
causes_true(toggle,   fan, neg(fan)).
causes_false(toggle,  fan, fan).

fun_fluent(alarm).              /* the smoke alarm is on or off */
causes_val(smoke, alarm, on,  true).
causes_val(resetAlarm, alarm, off, true).

fun_fluent(light(N)) :- floor(N).  /* call button of floor n is on or off */
causes_val(on(N),  light(N), on,  true).
causes_val(off(N), light(N), off, true).
senses(look(N), light(N)).      /* look(n) asks for the value of light(n) */


  /*  ACTIONS and PRECONDITIONS*/
prim_action(down).               /* elevator down one floor */
poss(down,   neg(floor=1)).

prim_action(up).                 /* elevator up one floor */
poss(up,     neg(floor=10)).

prim_action(toggle).             /* toggle the fan */
poss(toggle, true).

prim_action(ring).               /* ring the smoke alarm */
poss(ring,   true).

prim_action(off(N)) :- floor(N).    /* turn off call button on floor n */
poss(off(N), and(floor=N,light(N)=on)).

prim_action(open).		/* open door */
poss(open, true).

prim_action(close).             /* close door */
poss(close, true).

prim_action(close).             /* close door */
poss(close, true).

prim_action(look(N)) :- floor(N).  /* sensing action: check call button on floor n */
poss(look(_), true).



exog_action(heat).               /* increase temperature by 1 */
exog_action(cold).               /* decrease temperature by 1 */
exog_action(smoke).              /* smoke enters elevator */
exog_action(resetAlarm).              /* smoke detector alarm is reset */
exog_action(on(N)) :- floor(N).     /* turn on call button on floor n */  

prim_action(Act) :- exog_action(Act).
poss(Act, true) :- exog_action(Act).

/* ABBREVIATIONS */
proc(too_hot, temp>2).
proc(too_cold, -2>temp).
proc(below_floor(N), floor<N).
proc(above_floor(N), floor>N).
proc(next_floor_to_serve(N), light(N)=on).


/* INITIAL STATE: elevator is at floor 3, lights 2 and 6 are on */
initially(floor,2).	
initially(temp,2).
initially(fan,false).
initially(light(N),off) :- floor(N), N\=1, N\=3.
initially(light(3),on). 		   	
initially(light(1),on). 		   	
initially(alarm,off).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Definitions of complex actions 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% THIS IS THE MAIN PROCEDURE FOR INDIGOLOG
proc(main,  mainControl(N)) :- controller(N), !.
proc(main,  mainControl(3)). % default one

proc(go_floor(N), while(neg(floor=N), if(below_floor(N),up,down))).
proc(serve_floor(N), [go_floor(N), off(N)]).

proc(handle_reqs(Max),      /* handle all elevator reqs in Max steps */
    ndet(  [?(and(neg(some(n,light(n)=on)),Max>=floor-1)), go_floor(1), open],
            pi(n, pi(m, [ ?(and(light(n)=on, m is Max - abs(floor-n))),
                          ?(m > 0),
                          serve_floor(n),
                          handle_reqs(m) ] )))).




/*  This is the original elevator with no exogenous events, no sensing  */
/*  The smart controller uses search to minimize the up-down motion     */
/*  The dumb controller tries without search but commits too soon       */
proc(mainControl(1), dumb_control).
proc(mainControl(2), smart_control).

proc(minimize_motion(Max),  /* iterative deepening search */
    ndet( handle_reqs(Max), pi(m, [?(m is Max+1), minimize_motion(m)]))).

proc(dumb_control, minimize_motion(0) ).           /* always fails */
proc(smart_control, search(minimize_motion(0)) ).  /* eventually succeeds */


/*  This is the elevator that appears in the IJCAI-97 paper on ConGolog */
/*  It uses exogenous actions for temperature, smoke, and call buttons  */
proc(mainControl(3), prioritized_interrupts(
        [interrupt(and(too_hot,neg(fan)), toggle),
         interrupt(and(too_cold,fan), toggle),
         interrupt(alarm=on, ring),
         interrupt(n, next_floor_to_serve(n), serve_floor(n)),
         interrupt(above_floor(1), down)])).

/*  This is the elevator with no exogenous events, but with sensing   	*/
/*  actions for each call button of the elevator                      	*/
proc(mainControl(4), 
  [ check_buttons, 
    while(or(some(n,light(n)=on), above_floor(1)), 
      if(some(n,light(n)=on), serve_a_floor, [down, check_buttons])) ]).
proc(serve_a_floor, pi(n, [?(next_floor_to_serve(n)), go_floor(n), off(n)])).
proc(check_buttons, 
	[look(1), look(2), look(3), look(4), look(5), look(6), look(7), look(8), look(9), look(10)]).



proc(mainControl(5), searchn(minimize_motion(0),[]) ).  



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  INFORMATION FOR THE EXECUTOR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translations of domain actions to real actions (one-to-one)
actionNum(X,X).	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: ElevatorSim-BAT/elevator.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*

trans(searchn(minimize_motion(0),[]),[],E,H),transn(E,H,E2,H2,10),!,ttrans(E2,[on(3)|H2],EF,HF), final(EF,HF).


 trans(searchn(minimize_motion(0),[]),[],E,H),transn(E,H,E2,H2,10),!,trans(E2,[on(3)|H2],E3,H3), transn(E3,H3,E4,H4,5), ttrans(E4,[smoke|H4],EF,HF), final(EF,HF).


*/


