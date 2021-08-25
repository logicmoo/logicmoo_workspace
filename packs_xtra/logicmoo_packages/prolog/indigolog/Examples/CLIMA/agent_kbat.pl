%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE    : Examples/CLIMA/agent_clima.pl
%
%       Axiomatization of the Wumpus World 
%       under the BAT with possible values evaluator
%
%  AUTHOR : Stavros Vassos & Sebastian Sardina (2005)
%  email  : {ssardina,stavros}@cs.toronto.edu
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

% Indigolog caching: fluents that are heavily used should be cached 
cache(locRobot).
cache(isPit(_)).
cache(isGold(_)).
%cache(_):-fail.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  1 - ACTIONS AND PRECONDITIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prim_action(skip).
poss(skip, true).

prim_action(left).
poss(left, and(locRobot=loc(X,_), X>0)).

prim_action(right).
poss(right, and(locRobot=loc(X,_), X<gridSizeX)).

prim_action(up).
poss(up, and(locRobot=loc(_,Y), Y>0)).

prim_action(down).
poss(down, and(locRobot=loc(_,Y),  Y<gridSizeY)).

prim_action(pick).
poss(pick, and(isGold(locRobot)=true, noGold=0)).

prim_action(drop).
poss(drop, true).

prim_action(mark(_)).
poss(mark(_), true).

prim_action(unmark).
poss(unmark, true).

/* Exogenous Actions Available */
exog_action(simStart(_, _)).
exog_action(simEnd(_, _)).
exog_action(requestAction(_, _)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  2 - FUNCTIONAL FLUENTS AND CAUSAL LAWS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fun_fluent(actionRequested).
causes(requestAction(_, _), actionRequested, true, true).
causes(A, actionRequested, false, neg(A=requestAction(_, _))).

% fun_fluent(actionRequested).
% causes(requestAction(_, Data), actionRequested, (Deadline, Id), true).
% causes(A, actionRequested, false, neg(A=requestAction(_, _))).
% sense_reqaction(Data, Deadline, Id) :- 
% 	member(deadline(Deadlne),Data), 
% 	member(id(Id),Data).


% inDungeon: robot is inside the dungeon playing the game!
fun_fluent(inDungeon).
causes(simStart(_, _), inDungeon, true, true).
causes(simEnd(_, _), inDungeon, false, true).


fun_fluent(gridSizeX).
causes(simStart(_, Data), gridSizeX, V, member(gsizeX(V), Data)).
fun_fluent(gridSizeY).
causes(simStart(_, Data), gridSizeY, V, member(gsizeY(V), Data)).
fun_fluent(gridSize).
causes(simStart(_, Data), gridSize, (X,Y), 
				and(member(gsizeY(Y), Data), member(gsizeX(X),Data)) ).

fun_fluent(depotX).
causes(simStart(_, Data), depotX, V, member(depotX(V), Data)).
fun_fluent(depotY).
causes(simStart(_, Data), depotY, V, member(depotY(V), Data)).
fun_fluent(locDepot).
causes(simStart(_, Data), locDepot, loc(X,Y), 
				and(member(depotY(Y), Data), member(depotX(X),Data))).

% locRobot: current location of the robot 
fun_fluent(locRobot).
causes(up, 	locRobot, Y, up(locRobot,Y)).
causes(down, 	locRobot, Y, down(locRobot,Y)).
causes(left, 	locRobot, Y, left(locRobot,Y)).
causes(right, 	locRobot, Y, right(locRobot,Y)).
causes(requestAction(_, Data), locRobot, L,  sense_location(Data, L)).

sense_location(Data, loc(X,Y)) :- 
	member(posX(X),Data), 
	member(posY(Y),Data).


% isGold(L): whether there is gold at location L
fun_fluent(isGold(L)):- location(L).
causes(pick, isGold(L), false, locRobot=L). 
causes(pick, isGold(L), V, and(neg(locRobot=L),V=isGold(L))). 
causes(requestAction(_, Data), isGold(L), V, sense_gold(Data, L, V)).
causes(requestAction(_, Data), isGold(L), V, 
			and(location(L),
			and(neg(sense_gold(Data, L, V)),
				isGold(L)=V))
	).

sense_gold(Data, Loc, V) :-
	sense_location(Data, LocRobot),
	member(cells(LCells), Data), 
	member(cell(CellID, LCellProp), LCells),
	apply(CellID, [LocRobot, Loc]),
	(member(gold, LCellProp) -> V=true ; V=false).

% noGold: number of gold pices the robot is holding
fun_fluent(noGold).
causes(pick, noGold, V, V is noGold+1).
causes(drop, noGold, V, V is noGold-1).

% hasGold: is the robot holding a gold brick?
fun_fluent(hasGold).
causes(pick, hasGold, possibly, true).
causes(requestAction(_, Data), hasGold, true,
		and(hasGold=possibly, sense_gold(Data, locRobot, false))). 
causes(requestAction(_, Data), hasGold, false,
		and(hasGold=possibly, sense_gold(Data, locRobot, true))). 
causes(A, hasGold, true, 	and(hasGold=possibly, member(A,[up,down,right,left])) ). 
causes(A, hasGold, false, 	and(hasGold=possibly, member(A,[up,down,right,left])) ). 

causes(drop, hasGold, false, true).


% isPit(L): whether there is an object/pit at location L
fun_fluent(isPit(L)):- location(L).
causes(requestAction(_, Data), isPit(L), V, sense_pit(Data, L, V)).
causes(requestAction(_, Data), isGold(L), V, 
			and(location(L),
			and(neg(sense_pit(Data, L, V)),
				isPit(L)=V))
	).

sense_pit(Data, Loc, V) :-
	sense_location(Data, LocRobot),
	member(cells(LCells), Data), 
	member(cell(CellID, LCellProp), LCells),
	apply(CellID, [LocRobot, Loc]),
	(member(object, LCellProp) -> V=true ; V=false).



% visited(L): location L is visited already
fun_fluent(visited(L)) :- location(L).
causes(requestAction(_, Data), visited(L), true, sense_location(Data, L)).
causes(reset, visited(L), false, and(location(L), neg(L=locRobot))).
causes(reset, visited(L), true, locRobot=L).

fun_fluent(tries).
causes(reset, tries, V, V is tries+1).

% This clauses are not used so far as there are no sensing actions
settles(_, _, _, _, _) :- fail.
rejects(_, _, _, _, _) :- fail.
senses(_) :- fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  3 - ABBREVIATIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  4 - INITIAL STATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% Robot state
initially(locRobot,loc(0,0)).
initially(hasGold,false).
initially(noGold,0).
initially(inDungeon, false).
initially(gridSizeX, 99).
initially(gridSizeY, 99).
initially(gridSize, (99,99)).

	% Pits	
initially(isPit(R),true)		:- location(R), \+ R=loc(0,0).
initially(isPit(R),false)		:- location(R).
initially(isGold(R), true)	:- location(R).
initially(isGold(R), false)	:- location(R).

	% Others
initially(tries,1).
initially(visited(R), false):- location(R).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  5 - MAIN ROUTINE CONTROLLERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% THIS IS THE MAIN EXECUTOR
proc(main,  	[while(neg(inDungeon), [?(writeln('Waiting simulation to start')), wait]), 
			?(setupSimulation(gridSizeX, gridSizeY)), 
			mainControl(1)]).

setupSimulation(X,Y) :-
	retractall(gridsizeX(_)),
	retractall(gridsizeY(_)),
	assert(gridsizeX(X)),
	assert(gridsizeY(Y)).

% Controller for the Wumpus:
%	1. If agent knows where the wumpus is, she is in line with it and
%	   the wumpus may be alive, then aim to wumpus and shoot arrow
%	2. If agent knows that there is gold in the current square, pick it up
%	3. Otherwise: sense everything and take a randomWalk
%		If no randomWalk exists, go to loc(1,1) and climb
proc(mainControl(1),
   prioritized_interrupts(
         [interrupt(neg(actionRequested), wait),
	  interrupt(hasGold, [while(neg(locRobot=locDepot), goto(locDepot)), drop]),
          interrupt(isGold(locRobot)=true, pick),
          interrupt([(dir,direction), loc], 
          		and(apply(dir, [locRobot, loc]), isGold(loc)), dir),
          interrupt([(dir,[ne,nw,se,sw]), loc], 
          		and(apply(dir, [locRobot, loc]), isGold(loc)), 
          			search(star([pi((a,[up,down,left,right]),a), ?(locRobot=loc)], 6)) ),
          interrupt([(dir,direction),loc], 
          		and(apply(dir, [locRobot, loc]), 
          		and(neg(isPit(loc)), neg(visited(loc)))), dir),
	  interrupt(true, pi((a,[up,down,left,right]), a)),
	  interrupt(true, [writeln('Cannot do anything!'), skip])
         ])  % END OF INTERRUPTS
).

% proc(closestGold(Loc, LocGold), 
% 	pi(x,pi(y,pi(dist,[?(gridSizeX=x), ?(gridSizeY=y), ?(dist is x+y), closestGoldIter(Loc,LocGold,0,dist))))
% ).

proc(goto(Loc),
	[
% 	?(writeln('Performing step to depot!')),
	if(to_east(locRobot,Loc), right, 
	if(to_west(locRobot,Loc), left,
	if(to_south(locRobot,Loc), down,
	if(to_north(locRobot,Loc), up,
	if(to_northeast(locRobot,Loc), rndet(up,right),
	if(to_northwest(locRobot,Loc), rndet(up,left),
	if(to_southeast(locRobot,Loc), rndet(down,right), rndet(down,left) )))))))
	]
).	



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  6 - EXTRA AUXILIARLY PROGRAMS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

proc(move(D),  [search([star(turn,4),?(dirRobot=D),moveFwd])]).
proc(shoot(D), [search([star(turn,4),?(dirRobot=D),shootFwd])]).


% Think of a plan (random walk) to safely move to some unvisited location 
proc(newRandomWalk, 
	  search([
	  	  	rpi(y,location,[
	  	  		?(visited(y)=true),
	  	  		rpi(z,location,[
	  	  			?(adj(y,z)),
	  	  			?(visited(z)=false),
	  	  			?(or(aliveWumpus=false,neg(locWumpus=z))),
	  	  			?(isPit(z)=false),
	  	  			?(pathfind(locRobot,y,L)),
	  	  			L,
	  	  			rpi(w,direction,[move(w),?(locRobot=z)])
	  	  		])
	  	  	])
	      ],
		  "SEARCH FOR A (NEW) RANDOM WALK......")
	).


proc(goto(Loc), 
	  search([?(pathfind(locRobot,Loc,L)), L
			  ], ["PLANNING TO GO TO LOCATION: ",Loc])
	).


proc(goto1step(Loc), 
	  pi(x,direction,[move(x),?(locRobot=Loc)])
	).


proc(goodBorderPair(VLoc, NLoc), 
	[?(visited(VLoc)=true),
	?(adj(VLoc,NLoc)),
	?(visited(NLoc)=false),
	?(or(aliveWumpus=false,neg(locWumpus=NLoc))),
	?(isPit(NLoc)=false)]
	).


proc(explore_grid, 
	search(pi([s,q],[?(gridsize(s)), ?(q is 2*s-2), explore_limit(0,q)]))
).
proc(explore_limit(N,MAX), 
	  wndet(search([
	  	  	pi(y,[
	  	  		?(neighbor(locRobot,y,N)),
				?(visited(y)=true),
				%?(and(write('----->Y:'),writeln(y))),
	  	  		pi(z,[
	  	  			?(radj(y,z)),
	  	  			?(visited(z)=false),
	  	  			?(or(aliveWumpus=false,neg(locWumpus=z))),
	  	  			?(isPit(z)=false),
					%?(and(write('----->Z:'),writeln(z))),
	  	  			?(pathfind(locRobot,y,L)),
	  	  			L,
					%?(and(write('----->L:'),writeln(L))),
	  	  			rpi(w,direction,[move(w),
					%?(and(write('----->W:'),writeln(w))),
						?(locRobot=z)])
	  	  		])
	  	  	])
	      		],['SEARCH FOR A LOCATION ', N, ' STEPS AWAY......']),
		   search([?(and(M is N+1,M=<MAX)),explore_limit(M,MAX)])
		   )
	).


proc(explore_grid2, 
	search(pi([s,q],[?(gridsize(s)), ?(q is 2*s-2), explore_limit2(0,q)]))
).
proc(explore_limit2(N,MAX), 
	  wndet(search([
	  	  	pi(y,[
	  	  		?(neighbor(locRobot,y,N)),
				?(visited(y)=true),
	  	  		pi(z,[
	  	  			?(adj(y,z)),
	  	  			?(visited(z)=false),
	  	  			?(or(aliveWumpus=false,neg(locWumpus=z))),
	  	  			?(isPit(z)=false),
	  	  			?(pathfind(locRobot,y,L)),
	  	  			L,
	  	  			pi(w,direction,[move(w),?(locRobot=z)])
	  	  		])
	  	  	])
	      		],['SEARCH FOR A LOCATION ', N, ' STEPS AWAY......']),
		   search([?(and(M is N+1,M=<MAX)),explore_limit2(M,MAX)])
		   )
	).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  PROLOG SPECIFIC TOOLS USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Map Relative Definitions 
% In order to change the relative orientation of the grid, one has to only change this
% definitions. All the rest below should work out well and transparently of the grid orientation
up(loc(X,Y),loc(X,YN))    	:- YN is Y-1, location(loc(X,YN)). 
down(loc(X,Y),loc(X,YN)) 	:- YN is Y+1, location(loc(X,YN)).  
right(loc(X,Y),loc(XN,Y)) 	:- XN is X+1, location(loc(XN,Y)).  
left(loc(X,Y),loc(XN,Y))  	:- XN is X-1, location(loc(XN,Y)).  

% to_up(Loc1,Loc2): Loc2 is towards direction "up" from Loc1
to_up(loc(_, Y1),loc(_, Y2)) 	:- gridindexY(Y1), gridindexY(Y2), Y2<Y1.
to_right(loc(X1, _),loc(X2, _))	:- gridindexX(X1), gridindexX(X2), X2>X1.
to_down(Loc1, Loc2):- to_up(Loc2, Loc1).
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
to_north(Loc1, Loc2)	:- to_up(Loc1,Loc2).
to_east(Loc1, Loc2)	:- to_right(Loc1, Loc2).
to_south(Loc1, Loc2):- to_down(Loc1, Loc2).
to_west(Loc1, Loc2)	:- to_left(Loc1, Loc2).
to_northwest(Loc1, Loc2)	:- to_north(Loc1, Loc2), to_west(Loc1, Loc2).
to_northeast(Loc1, Loc2)	:- to_north(Loc1, Loc2), to_east(Loc1, Loc2).
to_southwest(Loc1, Loc2)	:- to_south(Loc1, Loc2), to_west(Loc1, Loc2).
to_southeast(Loc1, Loc2)	:- to_south(Loc1, Loc2), to_east(Loc1, Loc2).


% rotateRight(R1, R2): R2 is the new direction from R1 after rotating clockwise once
rotateRight(up,right).
rotateRight(right,down).
rotateRight(down,left).
rotateRight(left,up).

% is loc(I,J) a valid location?
valid_loc(loc(I,J)) :- domain(I,gridindexX), domain(J,gridindexY).

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

% Set up path finding. Here it will be used to find paths between locs
% Start and End, such that the path goes through locs visited before.
pathfind_move(Start, End, move(D)):- 
	direction(D), 
	apply(D,[Start,End]),
	now(H),
	holds(visited(End)=true,H).

% Set heuristic (manhattan distance)
pathfind_heuristic(loc(I,J), loc(I2,J2), H):- 
	DiffI is I-I2, 
	DiffJ is J-J2,
	abs(DiffI,AbsDiffI), 
	abs(DiffJ,AbsDiffJ),
	H is AbsDiffI+AbsDiffJ.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  INFORMATION FOR THE EXECUTOR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
actionNum(X,X).	% Translations of actions are one-to-one

		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Examples/CLIMA/agent_clima.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%