%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE    : Examples/CLIMA/agent_clima.pl
%
%       BAT axiomatization of the CLIMA Agent 
%
%  AUTHOR : Sebastian Sardina (2007)
%  email  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/cogrobo
%  TYPE   : system independent code
%  TESTED : SWI Prolog 5.0.10 http://www.swi-prolog.org
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%
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
% -- causes_tt(action,fluent,value,cond)
%          when cond holds, doing act causes functional fluent to have value
%
%            e.g., causes_tt(paint(C2,V), color(C), V, C = C2).
%               or causes_tt(paint(C,V), color(C), V, true).
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

:- style_check(-discontiguous).
% :- style_check(-singleton).
% :- style_check(-atom).


% Indigolog caching: fluents that are heavily used should be cached 
cache(_):-fail, !.	% Do no caching.
%cache(locRobot(me)).
%cache(isPit(_)).
%cache(isGold(_)).

% roll always if possible; forced rolling if larger than 3.
roll_parameters(0,0,0).	   % roll-forward every single action


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  0 - DEFINITIONS OF DOMAINS/SORTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic gridsizeX/1, gridsizeY/1.

gridsizeX(100).
gridsizeY(100).
gridindexX(V) :- gridsizeX(S), S2 is S-1, !, get_integer(0,V,S2).
gridindexY(V) :- gridsizeY(S), S2 is S-1, !, get_integer(0,V,S2).
gridsize(X,Y) :- gridsizeX(X), gridsizeY(Y).

% This are the domains/sorts used in the application
direction(V) :- member(V, [up,down,left,right]).

all_direction(V) :- member(V, [n,s,r,l,ne,nw,se,sw,cur]).
location(loc(I,J)) :- gridindexX(I), gridindexY(J).
agent(A) :- A=me.
agent(A) :- teammember(A), \+ agentID(A).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  1 - ACTIONS AND PRECONDITIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prim_action(skip).
poss(skip, true).

prim_action(left).
poss(left, and(locRobot(me)=loc(X,_), X>0)).

prim_action(right).
poss(right, and(locRobot(me)=loc(X,_), X<gridSizeX)).

prim_action(up).
poss(up, and(locRobot(me)=loc(_,Y), Y>0)).

prim_action(down).
poss(down, and(locRobot(me)=loc(_,Y),  Y<gridSizeY)).

prim_action(pick).
poss(pick, and(isGold(locRobot(me))=true, neg(fullLoaded))).

prim_action(drop).
poss(drop, true).

prim_action(mark(_)).
poss(mark(_), true).

prim_action(unmark).
poss(unmark, true).


prim_action(tell(_Agent,_Message)).
poss(tell(_,_), true).
prim_action(broadcast(_Message)).
poss(broadcast(_), true).

prim_action(assumePit(_)).
poss(assumePit(_), true).

prim_action(assumeBlocked(_)).
poss(assumeBlocked(_), true).

prim_action(setState(_,_)).
poss(setState(_,TimeStamp), get_time(TimeStamp)).

prim_action(enterDungeon).
poss(enterDungeon, canEnterDungeon).

prim_action(initGrid).
poss(initGrid, true).

prim_action(setUrgentPlan(_Plan)).
poss(setUrgentPlan(Plan), assert(urgentExec(Plan))).


/* Exogenous Actions Available */
exog_action(simStart(_, _)).
exog_action(simEnd(_, _)).
exog_action(requestAction(_, _)).
exog_action(told(_,_)).
exog_action(connected(climaServer)).
exog_action(connected(messServer)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  2 - FUNCTIONAL FLUENTS AND CAUSAL LAWS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For compatibility with the form of BAT 
causes_val(A, F, V, C) :- causes(A, F, V, C).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FLUENTS USED TO MODEL THE WORLD STATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% playingGame: are we still playing in the simulation?
rel_fluent(playingGame).
causes_true(nothing, playingGame, true).
causes_false(bye, playingGame, true).

% are we already inside the grid?
rel_fluent(inDungeon).
causes_true(enterDungeon, inDungeon, true).
causes_false(simEnd(_, _), inDungeon, true).

% can we get into the grid?
rel_fluent(canEnterDungeon).
causes_true(simStart(_, _), canEnterDungeon, true).
causes_false(simEnd(_, _), canEnterDungeon, true).


% what is the actual grid size for the game
fun_fluent(gridSizeX).
causes(simStart(_, Data), gridSizeX, V, member(gsizeX(V), Data)).
fun_fluent(gridSizeY).
causes(simStart(_, Data), gridSizeY, V, member(gsizeY(V), Data)).
fun_fluent(gridSize).
causes(simStart(_, Data), gridSize, (X,Y), 
				and(member(gsizeY(Y), Data), member(gsizeX(X),Data)) ).


% where is the location of the depot
fun_fluent(depotX).
causes(simStart(_, Data), depotX, V, member(depotX(V), Data)).
fun_fluent(depotY).
causes(simStart(_, Data), depotY, V, member(depotY(V), Data)).
fun_fluent(locDepot).
causes(simStart(_, Data), locDepot, loc(X,Y), 
				and(member(depotY(Y), Data), member(depotX(X),Data))).


% locRobot(A): current location of agent A
fun_fluent(locRobot(A)) :- agent(A).
causes(up, 	locRobot(me), Y, up(locRobot(me),Y)).
causes(down, 	locRobot(me), Y, down(locRobot(me),Y)).
causes(left, 	locRobot(me), Y, left(locRobot(me),Y)).
causes(right, 	locRobot(me), Y, right(locRobot(me),Y)).
causes(requestAction(_, Data), locRobot(me), L,  sense_location(Data, L)).
causes(told(A, Data), locRobot(A), L,  sense_location(Data, L)).

% locRobotBefore: previous position of robot me before moving
fun_fluent(locRobotBefore).
causes(up,   locRobotBefore, V, V=locRobot(me)).
causes(down, locRobotBefore, V, V=locRobot(me)).
causes(left, locRobotBefore, V, V=locRobot(me)).
causes(right,locRobotBefore, V, V=locRobot(me)).
causes(setState(goingTo(_),_), locRobotBefore, V, locRobot(me)=V).

% locExpected: location the agent is expected to be (after moving)
fun_fluent(locExpected).
causes(up, 	locExpected, Y, up(locRobot(me),Y)).
causes(down, 	locExpected, Y, down(locRobot(me),Y)).
causes(left, 	locExpected, Y, left(locRobot(me),Y)).
causes(right, 	locExpected, Y, right(locRobot(me),Y)).



% robotState: sets the current state of the robot
fun_fluent(robotState).
causes(setState(State,Time), robotState, [State,Time], true).
causes(simStart(_,_), robotState, idle, true).


% isGold(L): whether there is gold at location L
fun_fluent(isGold(L)):- location(L).
causes(pick, isGold(L), false, locRobot(me)=L). 
causes(drop, isGold(L), true, locRobot(me)=L). 
causes(requestAction(_, Data), isGold(L), V, sense_gold(Data, L, V)).
causes(told(_, Data), isGold(L), V, sense_gold(Data, L, V)).
causes(told(_, gridInfo(gold, Data)), isGold(L), V, 
		and(member(isGold(List, V), Data), member(L,List))).


% isAgent(T): whether there is another agent of type T=enemy/ally at location L
rel_fluent(isAgent(L)):- location(L).
causes_true(requestAction(_, Data), isAgent(L),  
					and(sense_agent(Data, L, T), member(T,[enemy,ally]))).
causes_false(requestAction(_, Data), isAgent(L), sense_agent(Data, L, none)).
causes_true(told(_, Data), isAgent(L),  and(sense_agent(Data, L, T), member(T,[enemy,ally]))).
causes_false(told(_, Data), isAgent(L), sense_agent(Data, L, none)).


% hasGold: is the robot holding a gold brick?
fun_fluent(hasGold).
causes_val(requestAction(_, Data), hasGold, true, and(sense_items(Data,N),N>0)). 
causes_val(requestAction(_, Data), hasGold, false, and(sense_items(Data,N),N=0)). 
causes_val(drop, hasGold, false, true).
causes_val(simStart(_,_), hasGold, false, true).

% For CLIMA06 (OLD)
%causes_val(pick, hasGold, true, true).
%causes_val(requestAction(_, Data), hasGold, true,
%			and(sense_items(Data,N),
%			and(N<0,
%			and(lastAction=pick, sense_data(Data, gold, cur, false))))). 


% noGold: number of gold pieces we are carrying
fun_fluent(noGold).
%causes_val(pick, noGold, M2, and(noGold=M1,M2 is M1+1)).
causes_val(simStart(_,_), noGold, 0, true).
causes_val(drop, noGold, 0, true).
causes_val(requestAction(_, Data), noGold, N, and(sense_items(Data,N), N>=0)).
%causes_val(requestAction(_, Data), noGold, M2,
%		and(sense_items(Data,N),
%		and(N<0,
%		and(lastAction=pick, 
%		and(sense_data(Data, gold, cur, true),  % there is still gold here
%		and(noGold=M1,M2 is M1-1)))))).


% maxNoGold: a rigid fluent storing how many pieces of gold we can carry
fun_fluent(maxNoGold).
def_fluent(maxNoGold, 3, true).

fun_fluent(isBlocked(_Loc)).
%def_fluent(isBlocked(Loc), V, and(isPit(Loc)=R, blocked(R,V))).
def_fluent(isBlocked(Loc), V, myblocked(Loc,V)).
myblocked(Loc,V) :- currently(isPit(Loc),R), blocked(R,V).

blocked(true,true).
blocked(blocked,true).
blocked(false,false).
blocked(unknown,unknown).

% fullLoaded: the agent is carrying the maximum number of gold pieces
fun_fluent(fullLoaded).
def_fluent(fullLoaded, false, noGold < maxNoGold).
def_fluent(fullLoaded, true, noGold=maxNoGold).

fullLoaded(true) :- currently(noGold,3).
fullLoaded(false) :- currently(noGold,3).



% isPit(L): whether there is an object/pit at location L
fun_fluent(isPit(L)):- location(L).
causes(requestAction(_, Data), isPit(L), true, sense_obstacle(Data, L, true)).
causes(requestAction(_, Data), isPit(L), false, and(noObstacle(Data, L), neg(isPit(L)=blocked))).
/*causes(requestAction(_, Data), isPit(L), false, 
			and(sense_agent(Data, L, true), isPit(L)=unknown)).*/
causes(told(_, Data), isPit(L), V, sense_obstacle(Data, L, V)).
causes(told(_, Data), isPit(L), false, and(noObstacle(Data, L), neg(isPit(L)=blocked))).
causes(assumePit(L), isPit(L), true, and(neg(locDepot=L), isAgent(L))).
causes(assumeBlocked(L), isPit(L), blocked, neg(locDepot=L)).
causes(told(_, gridInfo(pit, Data)), isPit(L), V, 
		and(member(isPit(List, V), Data), member(L,List))).

%causes(simStart(_, _), isPit(L), possibly, location(L)).


noObstacle(Data, Loc) :-
	get_loc_info(Data, Loc, LocInfo),
	\+ member(unknown,LocInfo),
	\+ member(obstacle,LocInfo).


drop_at_depot(Data) :-
	member(cells(LCells), Data), 
	member(cell(cur, LCellProp), LCells),
	member(depot,LCellProp).

	


% deadline: next server deadline to submit action
fun_fluent(deadline).
causes(requestAction(_, Data), deadline, V, sense_deadline(Data, V)).


% A is an action that the agent can do in the CLIMA world
clima_action(A) :- member(A,[up,down,left,right,pick,drop,skip]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FLUENTS USED TO MODEL BEHAVIOR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% actionRequested: an action has been requested from game server and it is pending
rel_fluent(actionRequested).
causes_true(requestAction(_, _), actionRequested, true).
causes_false(A, actionRequested, clima_action(A)).
causes_false(simStart(_,_), actionRequested, true).

% brodcasted: have we already boradcasted the info that we got from the sensors?
rel_fluent(broadcasted).
causes_true(broadcast(_), broadcasted, true).
causes_false(requestAction(_, _), broadcasted, true).
causes_true(simStart(_, _), broadcasted, true).

% lastSensor: store the last sensing information obtained from game server
fun_fluent(lastSensor).
causes(requestAction(_, Data), lastSensor, Data, true).


% lastAction: store the last executed action
fun_fluent(lastAction).
causes(Action, lastAction, Action, clima_action(Action)).


% actionRequested: an action has been requested from game server and it is pending
% fun_fluent(lastActionFailed).
% def_fluent(lastActionFailed, unknown, neg(actionRequested)).
% def_fluent(lastActionFailed, V, 
% 	and(actionRequested,
% 	and(member(lastAction,[up,down,right,left]),
% 	or(and(locRobot(me)=locRobotBefore, V=true),
% 	   and(neg(locRobot(me)=locRobotBefore), V=false))))).
% def_fluent(lastActionFailed, V, 
% 	and(actionRequested,
% 	and(lastAction=pick,
% 		or(and(isGold(locRobot(me))=true, V=true),
% 	   	   and(neg(isGold(locRobot(me))=true), V=false))))).
% def_fluent(lastActionFailed, V, 
% 	and(actionRequested,
% 	and(lastAction=drop,
% 	or(and(and(neg(locRobot(me)=locDepot),neg(isGold(locRobot(me))=true)), V=true),
% 	   and(or(locRobot(me)=locDepot,isGold(locRobot(me))=true), V=false))))).



% visited(L): location L is visited already
rel_fluent(visited(L)) :- location(L).
causes_true(requestAction(_, Data), visited(L), sense_location(Data, L)).
causes_false(reset, visited(L), and(location(L), neg(L=locRobot(me)))).
causes_false(reset, visited(L), false, locRobot(me)=L).
%causes_false(simStart(_,_), visited(L), location(L)).

% noVisited(L): number of times location L has been 
fun_fluent(noVisited(L)) :- location(L).
causes(requestAction(_, Data), noVisited(L), V, 
		and(sense_location(Data, L), V is noVisited(L)+1)).
causes(reset, noVisited(L), 0, location(L)).
%causes(simStart(_,_), noVisited(L), 0, location(L)).


% are we entering the simulation in the middle?
rel_fluent(reconnecting).
causes_true(requestAction(_,Data), reconnecting, 
			and(neg(inDungeon),and(sense_step(Data,N), N>5))).
causes_false(tell(_, sendGridInfo), reconnecting, true).


% Agent has requested me information about the grid
rel_fluent(requestInfoGrid(_Agent)).
causes_true(told(Agent, sendGridInfo), requestInfoGrid(Agent), true).
causes_false(tell(Agent, gridInfo(_,_)), requestInfoGrid(Agent), true).


rel_fluent(restart).
causes_true(told(_,Data), restart, sense_step(Data,0)).
causes_false(told(_,Data), restart, and(sense_step(Data,N), N>5)).
causes_false(initGrid, restart, true).



% fun_fluent(diffSteps).
% causes(told(_,Data), diffSteps, V, and(sense_step(Data,N), V is N-diffSteps)).
% causes(initGrid, diffSteps, 0, true).
% 
% fun_fluent(curSteps).
% causes(told(_,Data), diffSteps, V, and(sense_step(Data,N), V is N-diffSteps)).
% causes(initGrid, diffSteps, 0, true).



% dummy fluent to force re-initializing the initial database
fun_fluent(restartGame).
causes(simStart(_,_), restartGame, true, resetInitialDB).
causes(initGrid, restartGame, true, resetInitialDB).




% No sensing actions in the domain. all sensing is done via exog actions
senses(_, _) :- fail.
senses(_, _, _, _, _) :- fail.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  3 - ABBREVIATIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Dir is the best direction to go if:
% 	- there is no pit & it has the lowest noVisited() score
% proc(bestAdjacent(Dir),
% 	some(n, 
% 	some(loc,
% 		and(apply(Dir, [locRobot(me), loc]), 
% 		and(isPit(loc)=false, 
% 		and(noVisited(loc)=n,
% 		    all(dir2,direction,
% 			or(dir2=Dir,
% 			   some(loc2,
% 				or(neg(apply(dir2,[locRobot(me),loc2])), % no loc2: out grid
% 			   	   and(apply(dir2,[locRobot(me),loc2]),
% 			  		or(neg(isPit(loc2)=false),noVisited(loc2)>=n)
% 				)))
% 			))
% 		)))
% 	))
% ).

bestAdjacent(Dir) :-
	currently(locRobot(me), LocRobotMe),
	apply(Dir, [LocRobotMe, Loc]),
	currently(isPit(Loc), false), 
	currently(noVisited(Loc), N),
	\+ (	member(Dir2, [up,down,left,right]),
		Dir2\=Dir,
		apply(Dir2, [LocRobotMe, Loc2]),
  		currently(isPit(Loc2), false),
		currently(noVisited(Loc2), N2), N2<N
	).
	


% proc(destinationGold(LocWithGold, Limit),
% 	and(isGold(LocWithGold)=true, 
% 	    some(dist,and(manhattanDistance(LocWithGold,locRobot(me),dist), dist<Limit))
% 	)	
% ).
proc(destinationGold(LocWithGold), closestGold(locRobot(me),LocWithGold)).



closestGold(LocRobot,LocWithGold,Limit) :-
	currently(isGold(LocWithGold), true),
	manhattanDistance(LocWithGold, LocRobot, Dist), 
	Dist =< Limit.

closestGold(LocRobot,LocWithGold) :-
	percentage_limit(5, Limit),
	closestGold(LocRobot,LocWithGold,Limit).
closestGold(LocRobot,LocWithGold) :-
	percentage_limit(10, Limit),
	closestGold(LocRobot,LocWithGold,Limit).
closestGold(LocRobot,LocWithGold) :-
	percentage_limit(20, Limit),
	closestGold(LocRobot,LocWithGold,Limit).
closestGold(LocRobot,LocWithGold) :-
	percentage_limit(40, Limit),
	closestGold(LocRobot,LocWithGold,Limit).
closestGold(LocRobot,LocWithGold) :-
	percentage_limit(80, Limit),
	closestGold(LocRobot,LocWithGold,Limit).
	

percentage_limit(Perc, Limit) :-
	gridsizeX(X),
	gridsizeY(Y),
	Limit is round((Perc*X*Y)/100).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Data is a list of the following form:
%
% [step(19), posX(13), posY(0), deadline(1143592298798), id('20'), items(2),
% cells([cell(cur, [agent(ally)]), cell(w, [empty]), cell(sw, [empty]), cell(s, [empty]), 
% cell(se, [gold]), cell(e, [obstacle])])]) which includes all the information received
% 
% which encoded all the information obtained in a requestAction() exogenous
% action from the game server. It provides sensing information relative 
% to the center position posX(X) posY(Y) (13,0 above)
%
% 
% The following predicates extract all the information from a Data as above:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% sensing of non-cells information: location, deadline, step number, id, items.
sense_location(Data, loc(X,Y)) :-  member(posX(X),Data), member(posY(Y),Data).
sense_deadline(Data, Deadline) :-  member(deadline(Deadline),Data).
sense_step(Data, Step) :-  member(step(Step),Data).
sense_id(Data, Id) :-  member(id(AtomId),Data), atom_number(AtomId,Id).
sense_items(Data, Items) :-  member(items(Items),Data).

get_loc_info(Data, Loc, LCellProp) :-
	sense_location(Data, LocRobot),
	member(cells(LCells), Data), 
	member(cell(CellID, LCellProp), LCells),
	apply(CellID, [LocRobot, Loc]).
	

% location Loc is a cell around the centre and V is true/false depending
% on whether the Obj (e.g., object, gold, enemy) was sensed in Loc
sense_data(Data, Obj, Loc, V) :-	% Loc is a relative position to the center in Data
	ground(Loc),
	member(Loc, [n,s,e,w,ne,nw,se,sw,cur]), !,
	sense_location(Data, LocCenter),
	apply(Loc, [LocCenter, Loc2]),
	sense_data(Data, Obj, Loc2, V).	
sense_data(Data, Obj, Loc, V) :-	% Loc is a veriable or a loc(_,_)
	sense_location(Data, LocRobot),
	member(cells(LCells), Data), 
	member(cell(CellID, LCellProp), LCells),
	apply(CellID, [LocRobot, Loc]),
	(member(Obj, LCellProp) -> V=true ; V=false).

% location Loc is a cell around the centre and V is true/false depending
% on whether gold was sensed in Loc
sense_gold(Data, Loc, V) :- sense_data(Data, gold, Loc, V).
sense_obstacle(Data, Loc, V) :- sense_data(Data, obstacle, Loc, V).

% Does Data says that there is an agent (T=enemy/ally) in Loc?
sense_agent(Data, Loc, T) :- 
	sense_location(Data, LocAgent),	
	sense_data(Data, agent(Type), Loc, V), 
	Loc\=LocAgent,
	(V=true -> T=Type ; T=none).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  4 - INITIAL STATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


	% Robot state
initially(locRobot(me),loc(0,0)).
initially(hasGold,false).
initially(noGold,0).
initially(inDungeon, false).
initially(canEnterDungeon, false).
initially(playingGame, true).
initially(gridSizeX, 99).
initially(gridSizeY, 99).
initially(gridSize, (99,99)).



	% Locations	
initially(isPit(R), unknown) :- location(R).
initially(isGold(R), unknown) :- location(R).
initially(visited(R), false):- location(R).
initially(noVisited(R), 0):- location(R).
initially(isAgent(L), false) :- location(L).

	% Others
initially(broadcasted,true).
initially(actionRequested,false).
initially(robotState, idle).
initially(reconnecting, false).



% Beliefs for the Boss agent
initially(restart, false).
initially(diffSteps, 0).
initially(requestInfoGrid(Agent), false) :- teammember(Agent).


% resets the initially/2 database	
resetInitialDB :- 
	initializeDB(isPit(_)),
	initializeDB(isGold(_)),
	initializeDB(visited(_)),
	initializeDB(noVisited(_)),
	initializeDB(isAgent(_)).

% Setup simulation with X and Y as the limits of the grid
setupSimulation(X,Y) :-
	retractall(gridsizeX(_)),
	retractall(gridsizeY(_)),
	assert(gridsizeX(X)),
	assert(gridsizeY(Y)).














%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  5 - MAIN ROUTINE CONTROLLERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%proc(main, if(some(x,clima_agentID(boss,x)), mainBoss, mainPlayer)).
proc(main, if(some(x,clima_agentID(boss,x)), mainBoss, mainControl(4))).

proc(mainBoss, mainControl(boss)).
proc(mainPlayer,
  	[gexec2(playingGame,
		[while(true, 
			[if(neg(inDungeon),
				[say('Waiting simulation to start'), 
			 	 while(neg(canEnterDungeon), wait), 	% wait for sim-start
			 	 ?(setupSimulation(gridSizeX, gridSizeY)),
				 say('Waiting for next request-action'),
			 	 while(neg(actionRequested), wait), 	% wait for sim-start
				 if(reconnecting,tell(boss,sendGridInfo),?(true)),
				 enterDungeon],
			?(true)),
			 say('Simulation started - Starting main controller'),
			 exogint(gexec2(inDungeon, mainControl(3,locDepot)),
							 abortCondition(locRobot(me))),
			 say('Simulation ended...')
			])
		]),
	say('BYE messages recevied. Tournament finished...')
	]		
).
abortCondition(Loc) :- 
	exists_pending_exog_event(requestAction(_,Data)),
	sense_location(Data,Loc2),
	Loc\=Loc2.
abortCondition(_) :-
	exists_pending_exog_event(simEnd(_,_)).



% Controller for the CLIMA agent:
%	1. Wait until a new action is requested from the game server
%	2. If not brodcasted yet, broadcast last sensing information received
%	2. If we have gold, then go to depot and drop gold
%	3. If there is gold in the current location, pick it up
%	4. If there is gold directly around us (e,w,n,s), move there right away
%	5. If there is gold indirectly around us (sw,nw,se,sw), move there in 2 steps
%	6. If there is a cell directly around us that we have not explored, go there
%	7. Otherwise, move random if possible
%	8. Otherwise, just do a skip action the turn
proc(mainControl(2, LocDepot),
	prioritized_interrupts(
	[interrupt(locRobot(me)=LocDepot, 
			[while(neg(actionRequested),wait),
			 if(locRobot(me)=LocDepot,
				[drop, pi(time,setState(dropped,time)), 
				 while(neg(actionRequested),wait),
				 wndet(safeRandomMove,?(true))], ?(true))
			]
		),
	  interrupt(plan, and(retract(urgentExec(plan)),
				report('Executing urgent plan')), plan),
	  interrupt(neg(actionRequested), if(broadcasted, wait, broadcast(lastSensor))),
	  interrupt(and(isGold(locRobot(me))=true,neg(fullLoaded)), pick), % gold here?
	  interrupt([(dir,direction), loc], 	% Gold around us?
			and(report(['THINK: ','Checking for gold around us: ',dir]), 
			and(neg(fullLoaded), 
			and(apply(dir, [locRobot(me), loc]), 
			and(isGold(loc)=true, report('Spotted gold around! Moving there...'))))), 
				pi(time,[setState(pickCloseGold,time), dir])
		),
	  interrupt(and(hasGold=true, report(['DECISION: ','We have gold! Planning to depot...'])), 
			travelTo(opt, LocDepot)),	% do we have gold? go back to depot?
	  interrupt(locWithGold, 	% plan to go to a particular gold
			and(report(['THINK: ','Looking for a close gold somewhere']),
			and(empty_nowhist,		% make sure now=[] (otherwise too slow)
			and(destinationGold(locWithGold),
			    report(['Going for gold at location: ',locWithGold])))),
			 travelTo(opt, locWithGold)
		),
	  interrupt((dir,direction), 
			and(report(['THINK: ','Looking for best adjacent cell...']),
			and(bestAdjacent(dir), 
				report(['DECISION: ','Moving to best adjacent cell: ',dir]))),
			 pi(result,try_movement(dir,result))),
	  interrupt(and(playingGame, report(['DECISION: ','Random movement...'])), safeRandomMove),
	  interrupt(and(playingGame, report(['DECISION: ','Cannot do anything, thus we skip...'])), skip)
	])  % END OF INTERRUPTS
).


proc(mainControl(3, LocDepot),
	prioritized_interrupts(
	[interrupt(locRobot(me)=LocDepot, 
			[while(neg(actionRequested),wait),
			 if(locRobot(me)=LocDepot,
				[drop, pi(time,setState(dropped,time)), 
				 while(neg(actionRequested),wait),
				 wndet(safeRandomMove,?(true))], ?(true))
			]
		),
	  interrupt(plan, retract(urgentExec(plan)), plan),
	  interrupt(neg(actionRequested), if(broadcasted, wait, broadcast(lastSensor))),
	  interrupt(and(isGold(locRobot(me))=true,neg(fullLoaded)), pick), % gold here?
	  interrupt([(dir,direction), loc], 	% Gold around us?
			and(neg(fullLoaded), 
			and(apply(dir, [locRobot(me), loc]),isGold(loc)=true)), 
				pi(time,[setState(pickCloseGold,time), dir])
		),
	  interrupt(hasGold=true, travelTo(opt, LocDepot)),	% do we have gold? go back to depot?
	  interrupt(locWithGold, 	% plan to go to a particular gold
			and(empty_nowhist, destinationGold(locWithGold)),
			 travelTo(opt, locWithGold)
		),
	  interrupt((dir,direction), bestAdjacent(dir), pi(result,try_movement(dir,result))),
	  interrupt(playingGame, safeRandomMove),
	  interrupt(playingGame, skip)
	])  % END OF INTERRUPTS
).



proc(mainControl(4),
	prioritized_interrupts(
	[interrupt(neg(actionRequested), wait),
	 interrupt(true, skip)
	])  % END OF INTERRUPTS
).







proc(mainControl(boss),
	prioritized_interrupts_simple(
	[interrupt((agent,teammember), requestInfoGrid(agent),
			pi([dataPitT,dataGold],
				[?(buildInfoData(dataPitT, pittrue)),
				  tell(agent,gridInfo(pit,dataPitT)),
				 %?(buildInfoData(dataPitF, pitfalse)),
				  %tell(agent,gridInfo(pit,dataPitF)),
				 ?(buildInfoData(dataGold, gold)),
				  tell(agent,gridInfo(gold,dataGold))
				]
			)),
	  interrupt(and(restart,report('Reinitializing database')), initGrid),
% 	  interrupt(and(diffSteps<(-10),report('Reinitializing database')), initGrid),
	  interrupt(true, wait)
	])  % END OF INTERRUPTS
).



buildInfoData([isPit(ListLoc,true)], pittrue) :-
	setof(Loc,currently(isPit(Loc),true),ListLoc).
buildInfoData([isPit(ListLoc,false)], pitfalse) :-
	setof(Loc,currently(isPit(Loc),false),ListLoc).
buildInfoData(Data,gold) :-
	setof(LocG,currently(isGold(LocG),true),ListTrueGold),
	Data=[isGold(ListTrueGold,true)].
buildInfoData([],_).



proc(goByPlanning(Method, Destination),
	pi([plan,myLocation,precPlan,planQual,time],
		[?(and(myLocation=locRobot(me),
		   and(bounded_pathfind(myLocation,Destination,Method,plan,planQual,8),
			report(['======PLAN FOUND: ',textual(plan)])))),
                setState(goingTo(Destination),time),	% set the agent state to goingTo(Destination)
		goal(locRobot(me)=Destination,	% success condition
			insist_movement(plan), 	% procedural plan to execute
			or(neg(robotState=[goingTo(Destination),time]),	% condition to fail
				neg(or(locRobot(me)=locRobotBefore,locRobot(me)=locExpected))),
			say(['Aborting plan to destination :',time])	% plan to recover from failure
			)
		]
	)
).
bounded_pathfind(Loc1,Loc2,Method,Plan,PlanQual,Sec) :-
catch(call_with_time_limit(Sec, pathfind(Loc1,Loc2,Method,Plan,PlanQual)),
	time_limit_exceeded,
      (report('!!!!!!!!!!!!!!!!! Bounded Planning aborted!'), fail)
).

% Take a step towards destination (mark current as blocked if we have to go back)
proc(goTowards(Destination),
	wndet(search(pi([(dir,[up,left,right,up]), curDist, futDist],
			[?(manhattanDistance(locRobot(me), Destination, curDist)), 
			?(actionRequested),
			 dir,
			?(manhattanDistance(locRobot(me), Destination, futDist)), 
			futDist < curDist
			]
			)),
		[assumeBlocked(locRobot(me)), 
		?(actionRequested),
		 wndet(pi([dir,result],[?(safeRandomDir(dir)),try_movement(dir,result)]), ?(true))]
	)
).	

% Execute sequential plan Plan by insisting on each individual action until success or failure
proc(insist_movement(Plan),
	ndet(?(Plan=[]),
	      	pi(result,
		[
		?(textual(Plan=[A|RestPlan])),
		?(actionRequested),	% block until next action is requested
		try_movement(A,result),
		if(result=ok,
			[say(['Action *',A,'* succeeded. Rest of plan: ',RestPlan]),
			 insist_movement(RestPlan)],
			say(['Action *',A,'* failed!. Dropping remaining plan...'])
		)
		])
	)
).


% Try to do movement A N times with Result (ok, failed, limit)
proc(try_movement(A,Result),
   	[?(apply(A,[locRobot(me), ExpectedLoc])),
		% Try 3 times action A, failing by executing assumePit(ExpectedLoc)
		% Abort if isPit(ExpectedLoc)=true
		% Succeed if locRobot(me)=ExpectedLoc 
	 try_action(4, A, isBlocked(ExpectedLoc)=true, locRobot(me)=ExpectedLoc,
		if(adj(locRobot(me),ExpectedLoc),assumePit(ExpectedLoc),?(true)), Result)
	]
).	


% Action A is tried N number of times to get SuccessCond (Result=ok) 
% Abort when AbortCond with Result=aborted 
% Fail with Result=failed if A has been tried N times already; execute PFailProg
proc(try_action(N,A,AbortCond,SuccCond,PFailProg,Result),
	[?(true), 	% Here there should be a condition to wait for evaluation exec of A
	if(AbortCond,[say('Abort condition applied.. aborting'),?(Result=aborted)],
		if(SuccCond, ?(Result=ok),
			if(N=1,[?(Result=failed),say('Gave up on action!'),PFailProg],
				pi(m,[A, ?(m is N-1), 	
				try_action(m,A,AbortCond,SuccCond,PFailProg,Result)])
			)
		)
	)
	]
).


% Say Text. For now it just prints the text in the console...
proc(say(Text), ?(report(Text))).
%proc(say(_Text), ?(true)).


% Just picks a direct adjacent direction where there is no obstacle and go
proc(safeRandomMove, search([randomMove,?(isPit(locRobot(me))=false)])).
safeRandomDir(Dir) :-
	currently(locRobot(me), LocRobot),
	direction(Dir),
	apply(Dir,[LocRobot,LocNew]),
	currently(isPit(LocNew),false).
	


	and(search([randomMove,?(isPit(locRobot(me))=false)])).
proc(randomMove,rpi(a,[up,down,left,right],a)).

% Solve P, if possible with C holding at the end
proc(search_pref(P,C), wndet(search([P,?(C)]),search(P))).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  6 - EXTRA AUXILIARLY PROGRAMS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

report(M) :- report_message(M).
empty_nowhist :- now([]).























%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  MAP TOOLS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Map Relative Definitions 
% To change the relative orientation of the grid, one has to only change this definitions 
% All the rest below should work out well and transparently of the grid orientation
up(loc(X,Y),loc(X,YN))    	:- YN is Y-1, location(loc(X,YN)).
down(loc(X,Y),loc(X,YN)) 	:- YN is Y+1, location(loc(X,YN)).
right(loc(X,Y),loc(XN,Y)) 	:- XN is X+1, location(loc(XN,Y)).
left(loc(X,Y),loc(XN,Y))  	:- XN is X-1, location(loc(XN,Y)).

% in case we got out of the grid
up(loc(X,Y),out)    	:- YN is Y-1, \+ location(loc(X,YN)).
down(loc(X,Y),out) 	:- YN is Y+1, \+ location(loc(X,YN)).
right(loc(X,Y),out) 	:- XN is X+1, \+ location(loc(XN,Y)).
left(loc(X,Y),out)  	:- XN is X-1, \+ location(loc(XN,Y)).
 

% to_up(Loc1,Loc2): Loc2 is towards direction "up" from Loc1
to_up(loc(_, Y1),loc(_, Y2)) 	:- gridindexY(Y1), gridindexY(Y2), Y2<Y1.
to_right(loc(X1, _),loc(X2, _))	:- gridindexX(X1), gridindexX(X2), X2>X1.
to_down(Loc1, Loc2) :- to_up(Loc2, Loc1).
to_left(Loc1, Loc2)	:- to_right(Loc2, Loc1).


% Directions: north, south, east, west, and combinations
n(L, L2) 	:- up(L, L2).
s(L, L2) 	:- down(L, L2).
e(L, L2)	:- right(L, L2).
w(L, L2)	:- left(L, L2).
ne(L, L2)	:- n(L, A), e(A, L2).
nw(L, L2)	:- n(L, A), w(A, L2).
sw(L, L2)	:- s(L, A), w(A, L2).
se(L, L2)	:- s(L, A), e(A, L2).
cur(L,L).

% to_north(Loc1,Loc2): Loc2 is towards direction "north" from Loc1
to_north(Loc1, Loc2) :- to_up(Loc1,Loc2).
to_east(Loc1, Loc2) :- to_right(Loc1, Loc2).
to_south(Loc1, Loc2) :- to_down(Loc1, Loc2).
to_west(Loc1, Loc2) :- to_left(Loc1, Loc2).
to_northwest(Loc1, Loc2) :- to_north(Loc1, Loc2), to_west(Loc1, Loc2).
to_northeast(Loc1, Loc2) :- to_north(Loc1, Loc2), to_east(Loc1, Loc2).
to_southwest(Loc1, Loc2) :- to_south(Loc1, Loc2), to_west(Loc1, Loc2).
to_southeast(Loc1, Loc2) :- to_south(Loc1, Loc2), to_east(Loc1, Loc2).


% rotateRight(R1, R2): R2 is the new direction from R1 after rotating clockwise
% rotateLeft(R1, R2): R2 is the new direction from R1 after rotating counter-clockwise
rotateRight(up,right).
rotateRight(right,down).
rotateRight(down,left).
rotateRight(left,up).
rotateLeft(R1, R2) :- rotateRight(R2,R1).

% oppdir(D1,D2): D2 is the oppositive movement to D1
oppdir(up,down).
oppdir(down,up).
oppdir(left,right).
oppdir(right,left).



% is loc(I,J) a valid location?
%valid_loc(loc(I,J)) :- domain(I,gridindexX), domain(J,gridindexY).
valid_loc(loc(I,J)) :- gridindexX(I), gridindexY(J).

% location R1 and R2 are adjacents
adj(R1,R2) :- (up(R1,R2) ; down(R1,R2) ; left(R1,R2) ; right(R1,R2)).

% adj/3: R2 is the adjacent square of R1 at direction D
adj(R1,R2,up)		:- up(R1,R2).
adj(R1,R2,down)  	:- down(R1,R2).	
adj(R1,R2,left)  	:- left(R1,R2).	
adj(R1,R2,right) 	:- right(R1,R2).	

% random adj
radj(L1,L2):-bagof(P,adj(L1,P),L),shuffle(L,RL),member(L2,RL). 

neighbor(L,L,0):-!. 
%neighbor(L1,L2,1):-!,bagof(P,adj(L1,P),L),shuffle(L,RL),member(L2,RL). 
neighbor(loc(I1,J1),loc(I2,J2),N):- 
	location(loc(I2,J2)),
	DiffI is I1-I2, DiffJ is J1-J2,
	abs(DiffI,AbsDiffI), abs(DiffJ,AbsDiffJ),
	N is AbsDiffI+AbsDiffJ.
	
% R2 is the next square of R1 in direction D
in_line(R1,_,R1).
in_line(R1,D,R2) :- adj(R1,R3,D), in_line(R3,D,R2).


% manhattanDistance(Loc1,Loc2,Distance): calculates Manhattan Distance between Loc1 and Loc2
manhattanDistance(loc(X1,Y1),loc(X2,Y2), Distance) :-
	DiffX is X1-X2, 
	DiffY is Y1-Y2,
	abs(DiffX,AbsDiffX), 
	abs(DiffY,AbsDiffY),
	Distance is AbsDiffX+AbsDiffY.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  PATH FINDING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pathfind(L1, L2, Type, Limit, Plan, Stats)
% Find a plan that takes you from L1 to L2 using path planning of type Type and
% with limit Limit. The heuristic, the cost model, the property used for 
% termination, and some statistics (Stats) are specified in the definition of the type. 

% pathfind(L1, L2, Type, inf, Plan, Stats)
% same as pathfind/6 but without a limit condition.
%
% pathfind_move/4 defines which locations to consider as part of a plan.
% Any valid location that is probably not a pit is ok to check as part of a plan.
% This is the same for all types of path finding. 

% SLOW BUT CORRECT ROBOUST VERSION
%
%pathfind_move(Start, End, _, D):- 
% 	%direction(D), 
% 	rdomain(D,[up,down,left,right]), 
% 	apply(D,[Start,End]),
% 	valid_loc(End),
% 	now(H),
% 	\+ holds(isPit(End)=true,H),
% 	\+ (holds(locDepot=End,H), holds(noGold=0,H)).	

% FAST: RELIES ON FULL PROGRESSION
%
pathfind_move(Start, End, _, D):- 
	rdomain(D,[up,down,left,right]), 
	apply(D,[Start,End]),
	valid_loc(End),
	\+ currently(isPit(End),true),
	\+ currently(isPit(End),blocked),
	\+ (currently(locDepot,End), currently(noGold,0)).	



% Pathfinding type safe(N): 
% Only go trhough unsafe places as long as this makes the path feasible or gives a shortcut 
% that will gain N moves. 
%
% Uses manhattan distance as the heuristic for the remaining path.
% Uses the following for computing the cost of the path found so far. 
% i)  The cost of each action is .99 so that there is preference in continuing a path rather
%     than searching for a new one with the same cost
% ii) The cost of an action that leads to an unsafe location is further increased by N
% Uses the number of possibly unsafe locations as termination condition
pathfind_f_function(loc(I,J), loc(I2,J2), safe(N), Cost, UpdCost, Assump, UpdAssump, Estimation):- 
	DiffI is I-I2, 
	DiffJ is J-J2,
	abs(DiffI,AbsDiffI), 
	abs(DiffJ,AbsDiffJ),
	now(H),
	(holds(isPit(loc(I,J))=false,H)-> 
		UpdAssump=Assump, 
		Demote=0 
	; 
		UpdAssump is Assump+1, 
		Demote is N
	),
	UpdCost is Cost+0.99+Demote,
	Estimation is AbsDiffI+AbsDiffJ.

pathfind(L1, L2, safe, Plan, Stats) :- 
	report(['Doing safe pathfind from location *',L1,'* to location *', L2,'*']),
	pathfind(L1, L2, safe(0), 1, Plan, Stats).
pathfind(L1, L2, opt, Plan, Stats)  :- 
	report(['Doing opt pathfind from location *',L1,'* to location *',L2,'*']),
	pathfind(L1, L2, safe(0), inf, Plan, Stats).

% Use pathfind(L1, L2, safe, Plan, Stats) to force going through safe locations only.
% Use pathfind(L1, L2, opt, Plan, Stats) to find the most optimistic plan.
% Use pathfind(L1, L2, safe(N), Limit, Plan, Stats) with high values of N and L>1 
% to avoid going through possibly unsafe locations unless it is necessary or it works 
% as a short cut and limit the possibly unsafe ones to be less than Limit.


% not ready yet
% type expl(N): a not-necessarily-shortest exporatory path that may go
% through a possibly unsafe place as long as this does not make the
% path longer than N moves.

% manhattan distance + plan length as the heuristic + promote
pathfind_f_function(loc(I,J), loc(I2,J2), expl1(N), CostSoFar, UpdatedCost, Estimation):- 
	DiffI is I-I2, 
	DiffJ is J-J2,
	abs(DiffI,AbsDiffI), 
	abs(DiffJ,AbsDiffJ),
	now(H),
	(holds(isPit(loc(I2,J2))=unknown,H) -> Promote is N; Promote=0),
	UpdatedCost is CostSoFar+1-Promote,
	Estimation is AbsDiffI+AbsDiffJ.


	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  INFORMATION FOR THE EXECUTOR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
actionNum(X,X).	% Translations of actions are one-to-one


		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Examples/CLIMA/agent_clima.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*

H=[
=========> EXOGENOUS EVENT:: Exog. Action * requestAction(1143592282090, [step(0), posX(13), posY(14), deadline(1143592286090), id('1'), cells([cell(cur, [agent(ally)]), cell(n, [empty]), cell(nw, [empty]), cell(w, [empty]), cell(sw, [empty]), cell(s, [gold]), cell(se, [empty]), cell(e, [empty]), cell(ne, [empty])])]) * occurred
random act

=========> EXOGENOUS EVENT:: Exog. Action * requestAction(1143592282639, [step(1), posX(13), posY(13), deadline(1143592286639), id('2'), cells([cell(cur, [agent(ally)]), cell(n, [empty]), cell(nw, [empty]), cell(w, [empty]), cell(sw, [empty]), cell(s, [empty]), cell(se, [empty]), cell(e, [empty]), cell(ne, [empty])])]) * occurred
>>>>>>>>>>>> ACTION EVENT:: Action * up * COMPLETED SUCCESSFULLY
random act
>>>>>>>>>>>> ACTION EVENT:: Action * up * COMPLETED SUCCESSFULLY

=========> EXOGENOUS EVENT:: Exog. Action * requestAction(1143592283396, [step(2), posX(13), posY(13), deadline(1143592287396), id('3'), cells([cell(cur, [agent(ally)]), cell(n, [empty]), cell(nw, [empty]), cell(w, [empty]), cell(sw, [empty]), cell(s, [empty]), cell(se, [empty]), cell(e, [empty]), cell(ne, [empty])])]) * occurred
random act

=========> EXOGENOUS EVENT:: Exog. Action * requestAction(1143592283907, [step(3), posX(13), posY(12), deadline(1143592287907), id('4'), cells([cell(cur, [agent(ally)]), cell(n, [empty]), cell(nw, [empty]), cell(w, [empty]), cell(sw, [empty]), cell(s, [empty]), cell(se, [empty]), cell(e, [empty])])]) * occurred
>>>>>>>>>>>> ACTION EVENT:: Action * up * COMPLETED SUCCESSFULLY
random act
>>>>>>>>>>>> ACTION EVENT:: Action * up * COMPLETED SUCCESSFULLY

=========> EXOGENOUS EVENT:: Exog. Action * requestAction(1143592284399, [step(4), posX(13), posY(11), deadline(1143592288399), id('5'), cells([cell(cur, [agent(ally)]), cell(n, [empty]), cell(nw, [empty]), cell(w, [empty]), cell(sw, [empty]), cell(s, [empty]), cell(se, [empty]), cell(e, [empty]), cell(ne, [empty])])]) * occurred
random act

=========> EXOGENOUS EVENT:: Exog. Action * requestAction(1143592284827, [step(5), posX(13), posY(10), deadline(1143592288827), id('6'), cells([cell(cur, [agent(ally)]), cell(n, [empty]), cell(nw, [empty]), cell(w, [empty]), cell(sw, [empty]), cell(s, [empty]), cell(se, [empty]), cell(e, [empty]), cell(ne, [empty])])]) * occurred
>>>>>>>>>>>> ACTION EVENT:: Action * up * COMPLETED SUCCESSFULLY
random act
>>>>>>>>>>>> ACTION EVENT:: Action * up * COMPLETED SUCCESSFULLY

=========> EXOGENOUS EVENT:: Exog. Action * requestAction(1143592285346, [step(6), posX(13), posY(9), deadline(1143592289346), id('7'), cells([cell(cur, [agent(ally)]), cell(n, [empty]), cell(nw, [empty]), cell(w, [empty]), cell(sw, [empty]), cell(s, [empty]), cell(se, [empty]), cell(e, [empty]), cell(ne, [empty])])]) * occurred
random act

=========> EXOGENOUS EVENT:: Exog. Action * requestAction(1143592285755, [step(7), posX(13), posY(8), deadline(1143592289755), id('8'), cells([cell(cur, [agent(ally)]), cell(n, [empty]), cell(nw, [empty]), cell(w, [empty]), cell(sw, [empty]), cell(s, [empty]), cell(se, [empty]), cell(e, [empty]), cell(ne, [empty])])]) * occurred
>>>>>>>>>>>> ACTION EVENT:: Action * up * COMPLETED SUCCESSFULLY
random act
>>>>>>>>>>>> ACTION EVENT:: Action * up * COMPLETED SUCCESSFULLY

=========> EXOGENOUS EVENT:: Exog. Action * requestAction(1143592286341, [step(8), posX(13), posY(7), deadline(1143592290341), id('9'), cells([cell(cur, [agent(ally)]), cell(n, [gold]), cell(nw, [empty]), cell(w, [empty]), cell(sw, [empty]), cell(s, [empty]), cell(se, [empty]), cell(e, [empty]), cell(ne, [empty])])]) * occurred
random act
>>>>>>>>>>>> ACTION EVENT:: Action * up * COMPLETED SUCCESSFULLY

=========> EXOGENOUS EVENT:: Exog. Action * requestAction(1143592286936, [step(9), posX(13), posY(6), deadline(1143592290936), id('10'), cells([cell(cur, [agent(ally), gold]), cell(n, [empty]), cell(nw, [empty]), cell(w, [empty]), cell(sw, [empty]), cell(s, [empty]), cell(e, [empty]), cell(ne, [empty])])]) * occurred
random act

=========> EXOGENOUS EVENT:: Exog. Action * requestAction(1143592287454, [step(10), posX(13), posY(5), deadline(1143592291454), id('11'), cells([cell(cur, [agent(ally)]), cell(n, [gold]), cell(nw, [empty]), cell(w, [empty]), cell(sw, [empty]), cell(s, [gold]), cell(se, [empty]), cell(e, [empty]), cell(ne, [empty])])]) * occurred
>>>>>>>>>>>> ACTION EVENT:: Action * up * COMPLETED SUCCESSFULLY
random act
>>>>>>>>>>>> ACTION EVENT:: Action * up * COMPLETED SUCCESSFULLY
DEBUG 0: Rolling down the river.......

=========> EXOGENOUS EVENT:: Exog. Action * requestAction(1143592290078, [step(11), posX(13), posY(4), deadline(1143592294078), id('12'), cells([cell(cur, [agent(ally), gold]), cell(n, [empty]), cell(nw, [empty]), cell(w, [empty]), cell(s, [empty]), cell(se, [empty]), cell(e, [empty]), cell(ne, [empty])])]) * occurred
DEBUG 0: done progressing the database!
random act
>>>>>>>>>>>> ACTION EVENT:: Action * up * COMPLETED SUCCESSFULLY

=========> EXOGENOUS EVENT:: Exog. Action * requestAction(1143592291004, [step(12), posX(13), posY(3), deadline(1143592295004), id('13'), cells([cell(cur, [agent(ally)]), cell(n, [empty]), cell(nw, [empty]), cell(w, [empty]), cell(sw, [empty]), cell(s, [gold]), cell(se, [empty]), cell(e, [empty]), cell(ne, [empty])])]) * occurred
random act

=========> EXOGENOUS EVENT:: Exog. Action * requestAction(1143592291450, [step(13), posX(13), posY(2), deadline(1143592295450), id('14'), cells([cell(cur, [agent(ally)]), cell(n, [empty]), cell(nw, [empty]), cell(w, [empty]), cell(sw, [empty]), cell(s, [empty]), cell(se, [empty]), cell(e, [empty]), cell(ne, [gold])])]) * occurred
>>>>>>>>>>>> ACTION EVENT:: Action * up * COMPLETED SUCCESSFULLY
random act
>>>>>>>>>>>> ACTION EVENT:: Action * up * COMPLETED SUCCESSFULLY

=========> EXOGENOUS EVENT:: Exog. Action * requestAction(1143592292130, [step(14), posX(13), posY(1), deadline(1143592296130), id('15'), cells([cell(cur, [agent(ally)]), cell(n, [empty]), cell(nw, [empty]), cell(w, [empty]), cell(sw, [empty]), cell(s, [empty]), cell(se, [empty]), cell(e, [gold])])]) * occurred
random act

=========> EXOGENOUS EVENT:: Exog. Action * requestAction(1143592292560, [step(15), posX(13), posY(0), deadline(1143592296560), id('16'), cells([cell(cur, [agent(ally)]), cell(w, [empty]), cell(s, [empty]), cell(se, [gold]), cell(e, [obstacle])])]) * occurred
>>>>>>>>>>>> ACTION EVENT:: Action * up * COMPLETED SUCCESSFULLY
random act
>>>>>>>>>>>> ACTION EVENT:: Action * down * COMPLETED SUCCESSFULLY

=========> EXOGENOUS EVENT:: Exog. Action * requestAction(1143592293406, [step(16), posX(13), posY(1), deadline(1143592297406), id('17'), cells([cell(cur, [agent(ally)]), cell(n, [empty]), cell(nw, [empty]), cell(w, [empty]), cell(s, [empty]), cell(se, [empty]), cell(e, [gold]), cell(ne, [obstacle])])]) * occurred
random act

=========> EXOGENOUS EVENT:: Exog. Action * requestAction(1143592293878, [step(17), posX(13), posY(0), deadline(1143592297878), id('18'), cells([cell(cur, [agent(ally)]), cell(w, [empty]), cell(sw, [empty]), cell(s, [empty]), cell(se, [gold]), cell(e, [obstacle])])]) * occurred
>>>>>>>>>>>> ACTION EVENT:: Action * up * COMPLETED SUCCESSFULLY
random act
>>>>>>>>>>>> ACTION EVENT:: Action * down * COMPLETED SUCCESSFULLY

=========> EXOGENOUS EVENT:: Exog. Action * requestAction(1143592294479, [step(18), posX(13), posY(1), deadline(1143592298479), id('19'), cells([cell(cur, [agent(ally)]), cell(n, [empty]), cell(nw, [empty]), cell(w, [empty]), cell(sw, [empty]), cell(s, [empty]), cell(se, [empty]), cell(e, [gold]), cell(ne, [obstacle])])]) * occurred
random act

=========> EXOGENOUS EVENT:: Exog. Action * requestAction(1143592294798, [step(19), posX(13), posY(0), deadline(1143592298798), id('20'), cells([cell(cur, [agent(ally)]), cell(w, [empty]), cell(sw, [empty]), cell(s, [empty]), cell(se, [gold]), cell(e, [obstacle])])]) * occurred
>>>>>>>>>>>> ACTION EVENT:: Action * up * COMPLETED SUCCESSFULLY
random act
>>>>>>>>>>>> ACTION EVENT:: Action * down * COMPLETED SUCCESSFULLY
DEBUG 0: Rolling down the river.......
DEBUG 0: done progressing the database!

=========> EXOGENOUS EVENT:: Exog. Action * simEnd(1143592302152, [id('21'), score(0), result(draw)]) * occurred

=========> EXOGENOUS EVENT:: Exog. Action * simStart(1143592311382, [id('22'), opponent(argentina), steps(20), gsizeX(25), gsizeY(25), depotX(0), depotY(1)]) * occurred


requestAction(1143595358607, [step(7), posX(11), posY(10), deadline(1143595362607), id('8'),cells([cell(cur, [agent(ally)]), cell(n, [gold]), cell(nw, [empty]), cell(w, [obstacle]), cell(sw, [empty]),cell(s, [empty]), cell(se, [empty]), cell(e, [empty]), cell(ne, [empty])])])


*/
