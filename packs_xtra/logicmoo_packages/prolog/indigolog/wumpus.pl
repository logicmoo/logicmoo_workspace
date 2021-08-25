%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILE    : Examples/Wumpus/wumpus.pl
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
%  A possible-value basic action theory (KBAT) is described with:
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- style_check(-discontiguous).
% :- style_check(-singleton).
% :- style_check(-atom).

/* IndiGolog caching: fluents that are heavily used should be cached */
cache(locWumpus).
cache(locRobot).
%cache(isPit(_)).
%cache(isGold(_)).
cache(_):-fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  0 - DEFINITIONS OF DOMAINS/SORTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic gridsize/1.

gridsize(8).
gridindex(V) :- 
	gridsize(S),
        between(1,S,V).
      /*
	findall(X,get_integer(1,X,S),L),
	member(V,L).*/
direction(V) :- member(V,[up,down,left,right]).
location(loc(I,J)) :- gridindex(I), gridindex(J).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  1 - ACTIONS AND PRECONDITIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prim_action(smell).
poss(smell, true).
senses(smell). 		% Perceived at a square iff the Wumpus is at this square or in its neighbourhood.

prim_action(senseBreeze).
poss(senseBreeze, true).
senses(senseBreeze). 	% Perceived at a square iff a isPit is in the neighborhood of this square.

prim_action(senseGold).
poss(senseGold, true).
senses(senseGold).	% Perceived at a square iff gold is in this square.

prim_action(shootFwd).
poss(shootFwd, hasArrow=true).

prim_action(pickGold).
poss(pickGold, isGold(locRobot)=true).

prim_action(setTemp(_)). % sets the value of a thinking fluent temp
poss(setTemp(_), true).

prim_action(moveFwd).
poss(moveFwd, neg(inTheEdge(locRobot,dirRobot))).

inTheEdge(loc(1,_),left).
inTheEdge(loc(X,_),right)	:- gridsize(X).
inTheEdge(loc(_,1),down).
inTheEdge(loc(_,X),up)		:- gridsize(X).

prim_action(turn).
poss(turn, true).

prim_action(climb).
poss(climb, true).

prim_action(enter).
poss(enter, true).

prim_action(reset).
poss(reset, true).

/* Exogenous Actions Available */
exog_action(scream).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  2 - FUNCTIONAL FLUENTS AND CAUSAL LAWS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% inDungeon: robot is inside the dungeon
fun_fluent(inDungeon).
causes(climb, inDungeon, false, locRobot=loc(1,1)).
causes(enter, inDungeon, true, true).

% locWumpus: locations of the Wumpus
fun_fluent(locWumpus).
rejects(smell, 1, locWumpus, Y, neg(adj(locRobot,Y))).
rejects(smell, 0, locWumpus, Y, adj(locRobot,Y)).

% locRobot: current location of the robot 
fun_fluent(locRobot).
causes(moveFwd, locRobot, Y, apply(dirRobot,[locRobot,Y])).

% dirRobot: direction of the robot (up, right, left, down)
fun_fluent(dirRobot).
causes(turn, dirRobot, Y, rotateRight(dirRobot,Y)).

% isGold(L): whether there is gold at location L
fun_fluent(isGold(L)):- location(L).
settles(senseGold, 1, isGold(L), true,  L=locRobot). 
settles(senseGold, 0, isGold(L), false, L=locRobot).
causes(pickGold, isGold(L), false, locRobot=L). 
causes(pickGold, isGold(L), V, and(neg(locRobot=L),V=isGold(L))). 

% noGold: number of gold pices the robot is holding
fun_fluent(noGold).
causes(pickGold, noGold, V, V is noGold+1).


% isPit(L): whether there is a pit at location L
fun_fluent(isPit(L)):- location(L).
settles(senseBreeze, 0, isPit(L), false, adj(locRobot,L)).
rejects(senseBreeze, 1, isPit(L), false,  
	and(adj(locRobot,L),
	    all(y,location, impl(and(adj(locRobot,y),neg(L=y)),isPit(y)=false))
	)
).

% hasArrow: whether the robot has an arrow to use
fun_fluent(hasArrow).
causes(shoot(_), hasArrow, false, true).

% aliveWumpus: wumpus is alive
fun_fluent(aliveWumpus).
causes(scream, aliveWumpus, false, true).
%causes(shootFwd, aliveWumpus, false, in_line(locRobot,dirRobot,locWumpus)).
%causes(shootFwd, aliveWumpus, true, 
%		   and(neg(in_line(locRobot,dirRobot,locWumpus)),aliveWumpus=true)
%		   ).

% visited(L): location L is visited already
fun_fluent(visited(L)) :- location(L).
causes(moveFwd, visited(L), true, apply(dirRobot,[locRobot,L])).
%"small" frame
causes(moveFwd, visited(L), V, and(neg(apply(dirRobot,[locRobot,L])),V=visited(L))).
causes(reset, visited(L), false,neg(locRobot=L)).
causes(reset, visited(L), true, locRobot=L).

%causes(moveFwd, locWumpus, V, or(V=locWumpus,and(adj(locRobot,L),V=L)))

fun_fluent(tries).
causes(reset, tries, V, V is tries+1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  3 - ABBREVIATIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  4 - INITIAL STATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% Robot state
initially(locRobot,loc(1,1)).
initially(dirRobot, right).
initially(hasArrow,true).
initially(noGold,0).
initially(inDungeon,true).
	% Wumpus state
initially(locWumpus,R):- location(R), \+ R=loc(1,1).
initially(aliveWumpus,true).
	% Pits	
initially(isPit(R),true)     :- location(R), \+ R=loc(1,1).
initially(isPit(R),false)    :- location(R).
initially(isGold(R),true)     :- location(R), \+ R=loc(1,1).
initially(isGold(R),false)    :- location(R).
	% Others
initially(tries,1).
initially(visited(R),true) :- R=loc(1,1).
initially(visited(R),false):- location(R), \+ R=loc(1,1).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  5 - MAIN ROUTINE CONTROLLERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% THIS IS THE MAIN PROCEDURE FOR INDIGOLOG
proc(main,  mainControl(N)) :- controller(N), !.
proc(main,  mainControl(4)). % default one


% Controller for the Wumpus:
%	1. If agent knows where the wumpus is, she is in line with it and
%	   the wumpus may be alive, then aim to wumpus and shoot arrow
%	2. If agent knows that there is gold in the current square, pick it up
%	3. Otherwise: sense everything and take a randomWalk
proc(mainControl(3),
   prioritized_interrupts(
         [interrupt([dir], and(aliveWumpus=true,
	 	    	    in_line(locRobot,dir,locWumpus)), [shoot(dir)] ),
	  interrupt(isGold(locRobot)=true, [pickGold]),
	  interrupt(inDungeon=true, [smell,senseBreeze,senseGold,
				wndet(newRandomWalk, [goto(loc(1,1)),climb])])
         ])  % END OF INTERRUPTS
).


% Controller for the Wumpus:
%	1. If agent knows where the wumpus is, she is in line with it and
%	   the wumpus may be alive, then aim to wumpus and shoot arrow
%	2. If agent knows that there is gold in the current square, pick it up
%	3. Otherwise: sense everything and take a randomWalk
%		If no randomWalk exists, go to loc(1,1) and climb
proc(mainControl(4),
   prioritized_interrupts(
         [interrupt([dir], and(aliveWumpus=true,
	 	    	    in_line(locRobot,dir,locWumpus)), [shoot(dir)] ),
	  interrupt(isGold(locRobot)=true, [pickGold]),
	  interrupt(inDungeon=true, 
	  	if(noGold>0,[goto(loc(1,1)),climb],
	  	    [smell,senseBreeze,senseGold,
		     wndet(explore_grid, [goto(loc(1,1)),climb])
		     ]))
         ])  % END OF INTERRUPTS
).

% This controller uses mainControl(4) and tries twice if no gold was obtained
proc(mainControl(5),
	[mainControl(4),
	 if(noGold>0, ?(true), [reset,enter,mainControl(4)])]
).


proc(mainControl(6),
   prioritized_interrupts(
         [interrupt([dir,r],and(locWumpus=r,
	 	    	and(aliveWumpus=true,
	 	    	    in_line(locRobot,dir,locWumpus))), [shoot(dir)] ),
	  interrupt(isGold(locRobot)=true, [pickGold]),
	  interrupt(inDungeon=true, 
	  	if(noGold>0,[goto(loc(1,1)),climb],
	  	    [smell,senseBreeze,senseGold,
		     wndet(explore_grid2, [goto(loc(1,1)),climb])
		     ]))
         ])  % END OF INTERRUPTS
).

% This controller uses mainControl(4) and tries twice if no gold was obtained

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

/* Map Definitions */
up(loc(X,Y),loc(X,YN))    :- YN is Y+1, location(loc(X,YN)). 
down(loc(X,Y),loc(X,YN))  :- YN is Y-1, location(loc(X,YN)).  
right(loc(X,Y),loc(XN,Y)) :- XN is X+1, location(loc(XN,Y)).  
left(loc(X,Y),loc(XN,Y))  :- XN is X-1, location(loc(XN,Y)).  

% rotateRight(R1, R2): R2 is the new direction from R1 after rotating clockwise once
rotateRight(up,right).
rotateRight(right,down).
rotateRight(down,left).
rotateRight(left,up).

valid_loc(loc(I,J)) :- domain(I,gridindex), domain(J,gridindex).

adj(R1,R2) :- (up(R1,R2) ; down(R1,R2) ; left(R1,R2) ; right(R1,R2)).

% adj/3: R2 is the adjacent square of R1 at direction D
adj(R1,R2,up)    :- up(R1,R2).
adj(R1,R2,down)  :- down(R1,R2).	
adj(R1,R2,left)  :- left(R1,R2).	
adj(R1,R2,right) :- right(R1,R2).	

%random adj
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
%  OLD STUFF
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

proc(mainControl(1), L) :-
reverse([climb, shootFwd, turn, moveFwd, moveFwd, moveFwd, turn, turn, senseGold, senseBreeze, smell, moveFwd, senseGold, senseBreeze, smell, moveFwd, turn, turn, turn, senseGold, senseBreeze, smell, moveFwd, turn, turn, turn, moveFwd, turn, turn, senseGold, senseBreeze, smell, moveFwd, turn, turn, turn, moveFwd, turn, turn, senseGold, senseBreeze, smell, moveFwd, turn, turn, turn, senseGold, senseBreeze, smell, moveFwd, turn, turn, turn, moveFwd, turn, turn, senseGold, senseBreeze, smell, moveFwd, senseGold, senseBreeze, smell, moveFwd, turn, turn, turn, senseGold, senseBreeze, smell],L).

proc(mainControl(2), L) :-
reverse([climb, moveFwd, turn, moveFwd, moveFwd, moveFwd, moveFwd, moveFwd, turn, turn, moveFwd, turn, moveFwd, turn, turn, turn, moveFwd, turn, pickGold, senseGold, senseBreeze, smell, moveFwd, turn, senseGold, senseBreeze, smell, moveFwd, senseGold, senseBreeze, smell, moveFwd, turn, senseGold, senseBreeze, smell, moveFwd, turn, turn, turn, moveFwd, turn, turn, senseGold, senseBreeze, smell, moveFwd, senseGold, senseBreeze, smell, moveFwd, turn, turn, turn, senseGold, senseBreeze, smell, moveFwd, turn, senseGold, senseBreeze, smell, moveFwd, turn, turn, turn, senseGold, senseBreeze, smell, moveFwd, turn, turn, turn, moveFwd, turn, turn, senseGold, senseBreeze, smell, moveFwd, senseGold, senseBreeze, smell, moveFwd, senseGold, senseBreeze, shootFwd, smell, moveFwd, turn, senseGold, senseBreeze, smell, moveFwd, turn, moveFwd, turn, turn, senseGold, senseBreeze, smell, moveFwd, turn, senseGold, senseBreeze, smell, moveFwd, turn, turn, turn, senseGold, senseBreeze, smell, moveFwd, turn, turn, turn, senseGold, senseBreeze, smell, moveFwd, senseGold, senseBreeze, smell, moveFwd, senseGold, senseBreeze, smell], L).

% "Thinking fluent" temp to be used in lessRandomWalk
fun_fluent(temp).
causes(setTemp(V), temp, V, true).

% Think of a plan (random walk) to safely move to some unvisited location 
proc(lessRandomWalk, 
	  search([
	      setTemp([]), 
	      star(
	      	[?(directions(D)),
	   		 rpi(y,D,[
	   			%?(write('-0-')),?(write(y)),?(write('\n')),
	   			pi(z,[
					?(apply(y,[locRobot,z])),
			  		?(valid_loc(z)),
					?(visited(z)=true),
					%?(write('-1-')),?(write(z)),?(write('\n')),
					%?(write('-2-')),?(write(temp)),?(write('\n')),
					?(\+(member(z,temp))), 
					%?(write('-3-')),?(write([z|temp])),?(write('\n')),
					setTemp([z|temp])
					%?(write('-4-')),?(write(temp)),?(write('\n')),
					%?(write('.'))
				]),
				move(y)])
			]
	      ),
		  moveSafely],
		  "SEARCH FOR A (LESS) RANDOM WALK......")
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  INFORMATION FOR THE EXECUTOR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translations of domain actions to real actions (one-to-one)
actionNum(X,X).	
	
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Examples/Wumpus/wumpus.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




now(5,H):- reverse([e(smell,0),smell,e(senseBreeze,0),senseBreeze,e(senseGold,0),senseGold,turn,turn,turn,moveFwd,e(smell,0),smell,e(senseBreeze,0),senseBreeze,e(senseGold,0),senseGold,moveFwd,e(smell,0),smell,e(senseBreeze,0),senseBreeze,e(senseGold,0),senseGold,moveFwd,e(smell,0),smell,e(senseBreeze,0),senseBreeze,e(senseGold,0),senseGold,turn,moveFwd,e(smell,1),smell,e(senseBreeze,0),senseBreeze,e(senseGold,0),senseGold,turn,moveFwd,e(smell,0),smell],H).







now(1,[climb, moveFwd, turn, turn, turn, moveFwd, turn, moveFwd, moveFwd, moveFwd, moveFwd, turn, turn, turn, scream, shootFwd, turn, moveFwd, moveFwd, turn, turn, moveFwd, turn, pickGold, e(senseGold, 1), senseGold, e(senseBreeze, 0), senseBreeze, e(smell, 0), smell, moveFwd, e(senseGold, 0), senseGold, e(senseBreeze, 0), senseBreeze,turn, turn, turn, moveFwd, turn, turn, e(senseGold, 0), senseGold, e(senseBreeze, 0), senseBreeze, e(smell, 1), smell, moveFwd, e(senseGold, 0), senseGold, e(senseBreeze, 0), senseBreeze, e(smell, 0), smell, moveFwd, turn, turn, turn, e(senseGold, 0), senseGold, e(senseBreeze, 0), senseBreeze, e(smell, 0), smell, moveFwd, turn, e(senseGold, 0), senseGold, e(senseBreeze, 0), senseBreeze, e(smell, 0), smell, moveFwd, turn, e(senseGold, 0), senseGold, e(smell, 0), smell, moveFwd, turn, turn, turn, e(senseGold, 0), senseGold, e(senseBreeze, 0), senseBreeze, e(smell, 0), smell, moveFwd, turn, turn, turn, e(senseGold, 0), senseGold, e(senseBreeze, 0), senseBreeze, e(smell, 0), smell, moveFwd, turn, e(senseGold, 0), senseGold, e(senseBreeze, 0), senseBreeze, e(smell, 0), smell, moveFwd, turn, turn, turn, e(senseGold, 0), senseGold, e(senseBreeze, 0), senseBreeze, e(smell, 0), smell, moveFwd, e(senseBreeze, 0), senseBreeze, e(smell, 0), smell, moveFwd, turn, turn, turn, e(senseGold, 0), senseGold, e(senseBreeze, 0), senseBreeze, e(smell, 0), smell, moveFwd, turn, turn, turn, e(senseGold, 0), senseGold, e(senseBreeze, 0), senseBreeze, e(smell, 0), smell, moveFwd, turn, e(senseGold, 0), senseGold, e(senseBreeze, 0), senseBreeze, e(smell, 0), smell, moveFwd, turn, turn, turn, e(senseGold, 0), senseGold, e(senseBreeze, 0), senseBreeze, e(smell, 0), smell]).



now(2,
[e(smell,0),smell,
e(senseBreeze,0),senseBreeze,
e(senseGold,0),senseGold,
turn,
turn,
turn,
moveFwd,
e(smell,0),smell,
e(senseBreeze,0),senseBreeze,
e(senseGold,0),senseGold,
turn,
moveFwd,
e(smell,0),smell,
e(senseBreeze,0),senseBreeze,
e(senseGold,0),senseGold,
turn,
turn,
turn,
moveFwd,
e(smell,0),smell,
e(senseBreeze,0),senseBreeze,
e(senseGold,0),senseGold,
turn,
turn,
turn,
moveFwd,
e(smell,0),smell,
e(senseBreeze,0),senseBreeze,
e(senseGold,0),senseGold,
turn,
moveFwd,
e(smell,0),smell,
e(senseBreeze,0),senseBreeze,
e(senseGold,0),senseGold,
turn,
moveFwd,
e(smell,0),smell,
e(senseBreeze,0),senseBreeze,
e(senseGold,0),senseGold,
turn,
turn,
turn,
moveFwd,
e(smell,0),smell,
e(senseBreeze,0),senseBreeze,
e(senseGold,0),senseGold,
moveFwd,
e(smell,1),smell,
e(senseBreeze,0),senseBreeze,
e(senseGold,0),senseGold,
turn,
turn,
moveFwd,
turn,
turn,
turn,
moveFwd,
e(smell,0),smell,
e(senseBreeze,0),senseBreeze,
e(senseGold,0),senseGold,
turn,
turn,
turn,
moveFwd,
e(smell,0),smell,
e(senseBreeze,0),senseBreeze,
e(senseGold,0),senseGold,
turn,
moveFwd,
e(smell,0),smell,
e(senseBreeze,0),senseBreeze,
e(senseGold,0),senseGold,
turn,
turn,
turn,
moveFwd,
e(smell,0),smell,
e(senseBreeze,0),senseBreeze,
e(senseGold,0),senseGold,
turn,
turn,
turn,
moveFwd,
e(smell,0),smell,
e(senseBreeze,0),senseBreeze,
e(senseGold,0),senseGold,
moveFwd,
e(smell,0),smell,
e(senseBreeze,0),senseBreeze,
e(senseGold,1),senseGold,
pickGold,
turn,
moveFwd,
turn,
turn,
moveFwd,
moveFwd,
turn,
shootFwd,
scream,
turn,
turn,
turn,
moveFwd,
moveFwd,
moveFwd,
moveFwd,
turn,
moveFwd,
turn,
turn,
turn,
moveFwd,
climb]).

%H = [moveFwd, turn, turn, e(senseGold, 0), senseGold, e(senseBreeze, 0), senseBreeze, e(smell, 0), smell]


now(6,H) :-
reverse([e(smell,0),smell,e(senseBreeze,0),senseBreeze,e(senseGold,0),senseGold,moveFwd,
e(smell,0),smell,e(senseBreeze,0),senseBreeze,e(senseGold,0),senseGold,moveFwd,e(smell,0),smell,
e(senseBreeze,0),senseBreeze,e(senseGold,0),senseGold,turn,turn,turn,moveFwd,e(smell,0),
smell,
e(senseBreeze,0),senseBreeze,e(senseGold,0),senseGold,moveFwd,e(smell,0),smell,e(senseBreeze,0),senseBreeze,e(senseGold,0),senseGold,turn,turn,turn,moveFwd,e(smell,0),smell,e(senseBreeze,0),senseBreeze,e(senseGold,0),senseGold],H).



