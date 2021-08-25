%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: ER1/er1test.pl
%
%       BAT axiomatization for controlling the Evolution ER1 robot
%
%  AUTHOR : Sebastian Sardina (2003)
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  TYPE   : system independent code
%  TESTED : SWI Prolog 5.0.10 http://www.swi-prolog.org
%           ECLIPSE 5.4 http://www.icparc.ic.ac.uk/eclipse/

%           For more information on Golog and some of its variants, see:
%               http://www.cs.toronto.edu/~cogrobo/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             November, 2003
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



   
% There is nothing to do caching on (required becase cache/1 is static)
cache(_):-fail.


/* DOMAINS/SORTS */
%fl([1,2,3,4,5,6,7,8,9,10]).       % possible floors 
fl([1,2,3,4,5,6]).       % possible floors 
dir([up,down]).                    % possible directions 
temperature([15,20,25,30,35]).     % possible temperatures


  /*  FLUENTS and CAUSAL LAWS */
fun_fluent(state).                 % Moving, stopped, or suspended (lost)
causes_val(moveFwd(_),      state, moving, true).
causes_val(turnLeft,        state, moving, true).
causes_val(turnRight,       state, moving, true).
causes_val(arrive,          state, stopped, true).
causes_val(getStuck,        state, stopped, true).
causes_val(freeze,          state, stopped, true).
causes_val(stop_abnormally, state, suspended, true).
causes_val(suspend,         state, suspended, true).
causes_val(resume,          state, stopped, true).

rel_fluent(sawObject(_)). 	   % An object has just been seen
causes_true(spotObject(O),    sawObject(O), true).
causes_false(lostObject(O),   sawObject(O), true).
causes_false(forgetObject(O), sawObject(O), true).

rel_fluent(objectLost(_)). 	   % We lost the object
causes_true(lostObject(O),    objectLost(O), true).
causes_false(forgetObject(O), objectLost(O), true).
causes_false(spotObject(O),   objectLost(O), true).

rel_fluent(silent).           % Should we talk or be silent?
causes_false(speakOn,  silent, true).
causes_true(speakOff,  slient, true).


rel_fluent(talking).          % Is the agent saying anything?
causes_true(say(_),        talking, true).
causes_true(say(_,_),      talking, true).
causes_false(finishSaying, talking, true).

rel_fluent(sensingObjects).   % Is the agent watching for objects?
causes_true(senseOn(objects),   sensingObjects, true).
causes_false(senseOff(objects), sensingObjects, true).


  /*  ACTIONS and PRECONDITIONS*/
prim_action(moveFwd(_)). 
prim_action(turnLeft).
prim_action(turnRight).
prim_action(freeze).
prim_action(forgetObject(_)).
prim_action(say(_)).
prim_action(say(_,_)).

prim_action(setObjectConfidence(_)).
prim_action(setLinearVelocity(_)).
prim_action(setPower(_,_)).
prim_action(setIR_oa(_,_)).
prim_action(senseOn(_)).
prim_action(senseOff(_)).

exog_action(finishSaying).          
exog_action(getStuck).
exog_action(arrive). 
exog_action(spotObject(_)).
exog_action(lostObject(_)).
exog_action(stop_abnormally).    % Stop because confused
exog_action(speakOn).
exog_action(speakOff).
exog_action(suspend).
exog_action(resume).

poss(moveFwd(_),       neg(state=moving)).
poss(turnLeft,         neg(state=moving)).
poss(turnRight,        neg(state=moving)).
poss(freeze,           true).
poss(forgetObject(O),  or(sawObject(O), objectLost(O))).
poss(say(_),           and(neg(talking),neg(silent))).
poss(say(_,_),         and(neg(talking),neg(silent))).

poss(setObjectConfidence(_), true).
poss(setLinearVelocity(_), true).
poss(setPower(_,_), true).
poss(setIR_oa(_,_), true).

poss(senseOn(_),  true).
poss(senseOff(_), true).


/* ABBREVIATIONS */


 /* INITIAL STATE: elevator is at floor 3, lights 2 and 5 are on */
initially(sawObject(_), false).
initially(objectLost(_), false).
initially(state, stopped).
initially(silent, false).
initially(sensingObjects, false).
initially(talking, false).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definitions of complex actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
proc(goSquare(Rot, M), 
     [moveFwd(M),
      if(Rot=right,turnRight,turnLeft),
      moveFwd(M),
      if(Rot=right,turnRight,turnLeft),
      moveFwd(M),
      if(Rot=right,turnRight,turnLeft),
      moveFwd(M),
      if(Rot=right,turnRight,turnLeft)
     ]).

proc(recognizeObject(O), talk(O)).

proc(talk(M), if(silent, ?(true), 
                 [?(neg(talking)), say(M,mike)]
                )
    ).

% Get a random integer between L and U
proc(getNumber(L,U,N),
     ?(random(L,U,N))
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Main Routine
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% THIS IS THE MAIN PROCEDURE FOR INDIGOLOG
proc(main,  mainControl(N)) :- controller(N), !.
proc(main,  mainControl(3)). % default one


% Robot travels a square of 2 meters
proc(mainControl(0), 
     [talk('ER1 square controller initiated!'),
      setLinearVelocity(20),
      prioritized_interrupts(
         [interrupt(or(state = moving, talking), wait),
          interrupt(true, [talk('Starting a new round'),
                           rndet(goSquare(right, 200), 
                                 [turnRight, goSquare(left,200), turnLeft]),
                           talk('Another round finished')
                          ])
         ]
                            )
     ]
    ).


% Robot travels a square of 2 meters while recognizing objects
proc(mainControl(2), 
     [talk('ER1 controller initiated successfully!'),
      setObjectConfidence(20),
      senseOn(objects),
      prioritized_interrupts(
         [interrupt(talking, wait),
          interrupt(o, sawObject(o), 
                    [talk(['Hey!, I have just seen the ',o]),
                     forgetObject(o)
                    ]),
          interrupt(state=moving, wait),
          interrupt(true, [talk('Starting a new round'),
                           rndet(goSquare(right, 200), 
                                 [turnRight, goSquare(left,200), turnLeft]),
                           talk('Another round finished')
                          ])
         ]
                            )
     ]
    ).

% Has suspension, OA, while recognizing and forgetting objects
proc(mainControl(3), 
     [talk('ER1 IndiGolog controller initiated successfully!'),
      setObjectConfidence(20),
      senseOn(objects),
      setPower(moving, 40),
      setIR_oa(30, 150),
      prioritized_interrupts(
         [interrupt(state=suspended,
                    [freeze,
                     talk(['Execution suspended']),
                     ?(neg(state=suspended))
                    ]),
          interrupt(or(state=suspended,talking), wait),
          interrupt(o, sawObject(o), 
                    [talk(['Hey!, I have just seen the ', o]),
                     forgetObject(o)
                    ]),
          interrupt(o, objectLost(o), 
                    [talk(['I have just lost the object ', o]),
                     forgetObject(o)
                    ]),
          interrupt(state=moving, wait),
          interrupt(true, [talk('Starting a new round'),
                           pi(n,[getNumber(10,30,n), setLinearVelocity(n)]),
                           rndet(goSquare(right, 200), 
                                 [turnRight, goSquare(left,200), turnLeft]),
                           talk('Another round finished')
                          ]),
          interrupt(true, wait)
         ])  % END OF INTERRUPTS
     ]
    ).


% Robot travels a square of 2 meters (NOT FINISHED: moveTo)
proc(mainControl(5), 
     [talk('ER1 square controller initiated!'),
      prioritized_interrupts(
         [interrupt(or(state = moving, talking), wait),
          interrupt(true, [talk('Starting a new round'),
                           pi(place,
                           pi(x,
                           pi(y, [getNextPlace(x,y,place),
                                  moveTo(x,y),
                                  talk(['I am traveling to ',place,' now'])
                                 ]
                             )))
                          ])
         ]
                            )
     ]
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: ER1/er1test.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%