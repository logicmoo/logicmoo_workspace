%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: Delivery-BAT/delivery.pl
% Time-stamp: <03/12/26 20:33:09 ssardina>
%
%       BAT axiomatization of the delivery robot
%
%    LAST REVISED:                    (Sebastian Sardina) 
%    TESTED: SWI Prolog 5.2.8 under RedHat Linux 6.2-9.0
%            ECLIPSE 5.7 under RedHat Linux 6.2-9.0
%    DESCRIPTION: This is a controller for a delivery robot
%
%           For more information on Golog and some of its variants, see:
%               http://www.cs.toronto.edu/~cogrobo/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             May 15, 2001
%
% This software was developed by the Cognitive Robotics Group under the
% direction of Hector Levesque and Ray Reiter.
% 
%        Do not distribute without permission.
%        Include this notice in any copy made.
% 
% 
%         Copyright (c) 2000-2002 by The University of Toronto,
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
% This is a Golog delivery robot. It assumes clipping actions to start
% and stop motion behaviour, and exogenous requests for deliveries.
% There are no sensing actions (other than the exogenous ones).
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A - DOMAINS 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

office([central|L]) :- setof(station(X), member(X,[1,6,11,7,9,12,13,16]), L).
corridor(L) :- setof(station(X),member(X,[2,3,4,5,8,14,10,15]),L).
location(L) :- office(L1), corridor(L2), append(L1,L2,L).

direction([north,south,west,east]).
degree([90,180,-90,180,-180]).
priority([1,2,3,4,5]).

% Any place or space between places
anyLocation(A)      :- 
        location(P),
        setof(E, betweenPlaces(E), B), 
        append(P,B,A).
betweenLocation(P) :-   % A place between two places
        location(X1), 
        location(X2), 
        X1\=X2, 
        P = between(X1,X2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% B - FLUENTS AND SUCCESSOR STATE AXIOMS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Fluents that should be cached (they are used very often)
cache(direction).
cache(robotLocation).

%cache(_):-fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ROBOT MOVEMENTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Current direction of robot: east,south,north or south
fun_fluent(direction).           
causes_val(turnLeft,  direction, north, direction=east).
causes_val(turnLeft,  direction, west,  direction=north).
causes_val(turnLeft,  direction, south, direction=west).
causes_val(turnLeft,  direction, east,  direction=south).

causes_val(turnRight,  direction, north, direction=west).
causes_val(turnRight,  direction, west,  direction=south).
causes_val(turnRight,  direction, south, direction=east).
causes_val(turnRight,  direction, east,  direction=north).

causes_val(turnAround,  direction, north,  direction=south).
causes_val(turnAround,  direction, west,   direction=east).
causes_val(turnAround,  direction, south,  direction=north).
causes_val(turnAround,  direction, east,   direction=west).

causes_val(turn(D),           direction, V,      rotation(direction,V,D)).
causes_val(startLocalization, direction, north,  true).
causes_val(setLocation(_,D),  direction, D,      true).

% Current location of the robot
fun_fluent(robotLocation).          
causes_val(goNext,     robotLocation, P, 
	               and(getEdge(robotLocation,P2,direction,currentMap),
		           P=between(robotLocation,P2)) ).
causes_val(reachDest,  robotLocation, P, robotLocation=between(_,P)).
causes_val(turnAround, robotLocation, between(P2,P1), robotLocation=between(P1,P2)).
causes_val(setLocation(L,_),  robotLocation, L, true).
causes_val(startLocalization, robotLocation, station(2), true).

% Last location visited by the robot
fun_fluent(robotLastPlace).      
causes_val(goNext,    robotLastPlace, P, P=robotLocation).

% Where is the robot heading when it is moving
fun_fluent(robotDestination).    
causes_val(goNext,robotDestination,P,getEdge(robotLocation,P,direction,currentMap)).

% A segment between two stations is blocked
rel_fluent(blocked(_,_)).        
causes_true(getStuck,             blocked(P1,P2), robotLocation=between(P1,P2)). 
causes_true(getStuck,             blocked(P1,P2), robotLocation=between(P2,P1)). 
causes_false(clearRoute(P1,P2),   blocked(P1,P2), true). 
causes_false(clearRoute(P1,P2),   blocked(P2,P1), true). 


% Should we talk loud?
rel_fluent(talking).           
causes_true(talk,    talking, true).
causes_false(shutup, talking, true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ROBOT STATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Current state of the robot
fun_fluent(robotState).          
causes_val(goNext,          robotState, moving,      true). 
causes_val(searchPath,      robotState, moving,      true). 
causes_val(reachDest,       robotState, reached,     robotState=moving). 
causes_val(stop_abnormally, robotState, lost,        true). 
causes_val(freezeRobot,     robotState, frozen,      true). 
causes_val(resetRobot,      robotState, idle,        true). 
causes_val(dropOff,         robotState, waitingPush, true). 
causes_val(pushGo,          robotState, readyGo,     robotState=waitingPush). 
causes_val(getStuck,        robotState, stuck,       true). 

% Current state of the robot
rel_fluent(robotLost).          
causes_true(stop_abnormally,    robotLost, true).
causes_false(setLocation(_,_),  robotLost, true).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HANDLING OF REQUESTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Robot has started to get an order
rel_fluent(startOrder).          

% Sender of the package holding
fun_fluent(sender).              

% Recipient of the package holding
fun_fluent(recipient).           
causes_val(dropOff,    recipient, unknown,  holdingShip).
senses(readRecipient,  recipient).

% The robot is holding a package
rel_fluent(holdingShip).         
causes_true(pickUp,          holdingShip, serviceAccepted(robotLocation)).
causes_false(dropOff,        holdingShip, true).

% Customer requested service
rel_fluent(askedService(_)).     
causes_true(orderShipment(C,_), askedService(C), true).
causes_false(ackOrder(C),       askedService(C), true).
causes_false(declineOrder(C),   askedService(C), true).

% Priority requested from customer
fun_fluent(orderPrio(_)).        
causes_val(orderShipment(C,P),  orderPrio(C), P, true).

% Service was accepted for customer
rel_fluent(serviceAccepted(_)).        
causes_false(declineOrder(C), serviceAccepted(C), true).
causes_true(ackOrder(C),      serviceAccepted(C), true).
causes_false(pickUp,          serviceAccepted(C), robotLocation=C).

% Service to customer is suspended
rel_fluent(suspended(_)).        
causes_true(suspend(C),  suspended(C), true).
causes_false(enable(C),  suspended(C), true).

% Number of packages in mailbox of customer
fun_fluent(mailBox(_)).          
causes_val(emptyMailBoxes,    mailBox(C), 0, customer(C)).
causes_val(dropOff,           mailBox(C), N, and(robotLocation=central,
                                             and(recipient=C,
                                                 N is mailBox(C)+1)) ).
% The mailbox for customer is full
rel_fluent(mailBoxFull(_)).      

% Customer is in his office
rel_fluent(inOffice(_)).         
causes_true(in(C),      inOffice(C), true).
causes_false(out(C),    inOffice(C), true).
senses(senseDoor(C),    inOffice(C)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOCALIZATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prim_action(startLocalization).
poss(startLocalization, true).

prim_action(addNode(N)) :- domain(N,location).
poss(addNode(_),  robotLost).

prim_action(addEdge(_)).
poss(addEdge(_),  robotLost).

prim_action(addNEdge(_)).
poss(addNEdge(_), robotLost).

prim_action(addCounter).
poss(addCounter, robotLost).

prim_action(setCurrentMap(_)).
poss(setCurrentMap(_), robotLost).

fun_fluent(counterStations).           
causes_val(startLocalization,  counterStations, 2, true).
causes_val(addCounter,         counterStations, X, X is counterStations+1).

fun_fluent(currentMap).           
causes_val(startLocalization,  currentMap, 
      graph([station(1),station(2)],[edge(station(1),station(2),south),
	                             edge(station(2),station(1),north)],[]), true).
causes_val(setCurrentMap(G),   currentMap, G, true).
causes_val(A,                  currentMap, G, 
                  and(neg(A=startLocalization),
                  and(neg(some(g,A=setCurrentMap(g))), realWorldGraph(G))) ).


causes_val(addNode(N),  currentMap, NewMap, add_node(N,currentMap,NewMap)).
causes_val(addEdge(E),  currentMap, NewMap, add_edge(E,currentMap,NewMap)).
causes_val(addNEdge(E), currentMap, NewMap, add_nedge(E,currentMap,NewMap)).

rel_fluent(stationVisited(_)).
causes_true(reachDest,           stationVisited(S), robotDestination=S).
causes_false(startLocalization,  stationVisited(S), location(S)).


% Check if there is a line below the path sensor (sensing action)
prim_action(senseLine).   
senses(senseLine, lineBelow).
poss(senseLine, true).
rel_fluent(lineBelow).

proc(localize,
	[startLocalization,
         addCounter,  % Counter=3
	 %
	 while(neg(some(l,some(d,uniquePlace(l,d)))), localizeOneStep),
	 pi(loc,
	 pi(deg,
	 pi(dir,
	     [?(uniquePlace(loc,deg)),
	      ?(rotation(direction,dir,deg)),
	      setLocation(loc,dir)
             ]
           ))) 
        ]
).

% The current map is a rotated version of the real one (it has to be rotated Deg)
% The robot is at location Loc in the real-world
proc(uniquePlace(Loc,Deg),
	some(realWorld,
	        and(realWorldGraph(realWorld),
                    uniqueLocation(currentMap,realWorld, robotLocation, Loc, Deg)
	           )
	     )
).

proc(localizeOneStep,
	[discoverStation,
         pi(station, 
	 pi(path,
	     [?(and(getNode(station,currentMap),neg(stationVisited(station)))),
	      ?(path_graph_short(robotLocation,station,currentMap,10,path)),
	      pi(x, pi(restpath, [?(path=[x|restpath]), traversePath(restpath)]))
	      ])),
	 cleanMap]
).

proc(discoverStation,
	[turn(-90),
	senseLine,
	if(lineBelow,addStation,addNonStation),
	turn(90),
	%
	turn(90),
	senseLine,
	if(lineBelow,addStation,addNonStation),
	turn(-90),
	%
	moveFwd,
	senseLine,
	if(lineBelow,addStation,addNonStation),
	moveBack]
).


proc(addNonStation,
	pi(reverseDir,
	   [addNEdge(edge(robotLocation,_,direction)),
	    ?(rotation(direction,reverseDir,180)),
	    addNEdge(edge(_,robotLocation,reverseDir))]
	    )
).

proc(addStation,
	pi(c,
	pi(reverseDir,
	   [?(c=counterStations),
	    addNode(station(c)),
	    addEdge(edge(robotLocation,station(c),direction)),
	    ?(rotation(direction,reverseDir,180)),
	    addEdge(edge(station(c),robotLocation,reverseDir)),
	    addCounter]
	    ))
).

proc(cleanMap,
	pi(realWorld, 
	pi(newMap, [?(realWorldGraph(realWorld)),
	            ?(cleanGraph(currentMap, realWorld, newMap)),
		    setCurrentMap(newMap)]
	  ))
).






% Loc is the real location of NodeG1 in graph G2 and 
% G1 has to be rotated Deg to fit graph G1
uniqueLocation(G1, G2, NodeG1, Loc, Deg) :-
	% First, get all possible mappings: G1->G2
	findall((M,D), sub_graph_rot(G2, G1, M, D), [(Map,Deg)]),
	member((Loc,NodeG1), Map).

           
% Clean graph G1 w.r.t. graph G2 (i.e., remove known repeated nodes in G1)
cleanGraph(G1, G2, GNew) :-
	% First, get all possible mappings: G1->G2
	findall(M, D^sub_graph_rot(G2, G1, M, D), LMaps), 
	setof((Node1,Node2), (getNode(Node1,G1),  % get all Node1=Node2 in G1
	                      getNode(Node2,G1), 
		              Node1\=Node2,
		              \+ not_equal_nodes(Node1,Node2,LMaps)), LEqualNodes),
	remove_equal_nodes(LEqualNodes, G1, GNew).
cleanGraph(G1,_,G1).


% Node1 is not the same as Node2 if there is a Map in LMaps such that
% Node1 is mapped to a different node than Node2
not_equal_nodes(Node1, Node2, LMaps) :-
	member(Map, LMaps),
	member((MapNode1,Node1), Map),
	member((MapNode2,Node2), Map),
	MapNode1\=MapNode2.


remove_equal_nodes([], G, G).
remove_equal_nodes([(N1,N2)|Tail], G, GNew) :-
	getNode(N1,G),getNode(N2,G),!,
	combine_nodes(N1,N2,G,GNew2),
	remove_equal_nodes(Tail, GNew2, GNew).
remove_equal_nodes([_|Tail], G, GNew) :-
	remove_equal_nodes(Tail, G, GNew).
	

	

 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% C - ACTIONS and PRECONDITIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HANDLING OF REQUESTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Acknowledge order of C
prim_action(ackOrder(C))      :- domain(C,office).	
poss(ackOrder(C), neg(serviceAccepted(C)) ).

% Deny order to C
prim_action(declineOrder(C))  :- domain(C,office).   
poss(declineOrder(_), 	true).

% Suspend service to C
prim_action(suspend(C))       :- domain(C,office).   
poss(suspend(C), and(neg(inOffice(C)), serviceAccepted(C)) ).
      
% Restart service to C
prim_action(enable(C))        :- domain(C,office).   
poss(enable(_), true).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ROBOT MOVEMENTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Pick up shipment 
prim_action(pickUp).                            
poss(pickUp, robotState=readyGo).

% Drop shipment
prim_action(dropOff).                           
poss(dropOff, and(robotState=frozen, 
	          or(inOffice(robotLocation), and(robotLocation=central,
		                                  mailBox(recipient)<3)) 
	         ) 
).

% Aborts navigation 
prim_action(resetRobot).	                
poss(resetRobot, true).

% Start going to next station
prim_action(goNext).                            
poss(goNext, or(some(a,some(b,robotLocation=between(a,b))),
	        some(next, and(getEdge(robotLocation,next,direction,currentMap),
	                   neg(blocked(robotLocation,next))))) ).

% Move back/forward a litle bit
prim_action(moveBack).                           
poss(moveBack, true).

prim_action(moveFwd).                           
poss(moveFwd, true).

% Move 90 degrees to left
prim_action(turnLeft).	                        
poss(turnLeft, isEdgeLeft).

% Move 90 degrees to right
prim_action(turnRight).	                        
poss(turnRight, isEdgeRight).

% Turn D degress
prim_action(turn(D)) :- domain(D,degree).
poss(turn(D), or(D=90,or(D=180,or(-90=D,-180=D))) ).

% Move 180 degrees
prim_action(turnAround).                        
poss(turnAround, and(neg(isEdgeLeft), neg(isEdgeRight)) ).

% Set robotLocation to L and direction to D when the robot is lost
prim_action(setLocation(L,D)) :- domain(L, location), domain(D, direction).
poss(setLocation(_,_), robotLost).

% Do not move
prim_action(freezeRobot).	                
poss(freezeRobot, true).

% Read the recipient of the pack that we are holding (sensing action)
prim_action(readRecipient).	                
poss(readRecipient, holdingShip).

% Check if C is in office (sensing action)
prim_action(senseDoor(C)) :- domain(C,office).   
poss(senseDoor(C), robotLocation=C).

% Say a message
prim_action(say(_)).            
poss(say(_), true).

% Ring the bell
prim_action(ring).            
poss(ring, true).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% D - EXOGENOUS ACTIONS OR EVENTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
exog_action(debug).                    

% Empty all mailboxes
exog_action(emptyMailBoxes).                    

% Stop because totally confused
exog_action(stop_abnormally).                   

% Destination reached successfully
exog_action(reachDest).               	        

% Button has been pressed
exog_action(pushGo).               	        

% Could not get to destination, the is blocked
exog_action(getStuck).                          

% The route P1-P2 is clear
exog_action(clearRoute(P1,P2)) :- domain(P1, location), domain(P2, location).

% Customer C is in/out office
exog_action(in(C))  :- domain(C, office).             
exog_action(out(C)) :- domain(C, office).   	        

% New order has arrived from Sender with Priority
exog_action(orderShipment(Sender,Prio)) :- 	 
        domain(Sender, office), 
	domain(Prio, priority).

% Talk loud or shutup
exog_action(talk).
exog_action(shutup).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% E - INITIAL STATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The robot should first acknowledge Misha and start going to his office.

% Initially the robot is idle at the central office heading west 

% Initial location and direction of the robot
initially(robotLocation, central).
%initially(robotLocation, station(1)).
initially(direction, east).


% Initial state of the robot
initially(robotState, idle).
initially(robotLost, false).
initially(holdingShip, false).	
initially(talking, true).	

initially(startOrder,false). % No order has been started to be served

%% Initial configuration of customers' requests

% Customer in office 1 is in, the other customers are "unknown"
initially(inOffice(central), true).  

% Customer at office 6 (with prio 12) is the only one who has requested service 
initially(askedService(station(6)), true).	
initially(orderPrio(station(6)), 12).
initially(askedService(C), false) :- 
        domain(C,office), 
        \+ initially(askedService(C), true).

% Everybody has their mailbox empty except for customer 13 with 3 packages.
initially(mailBox(station(13)), 3).	 
initially(mailBox(C), 0) :- 
        domain(C, office), C\=13.

% No customer is suspended and has an accepted service
initially(suspended(C), false) :- domain(C,office).	
initially(serviceAccepted(C), false)   :- domain(C,office).

% The initial recipient is "unknown"
initially(recipient,unknown).


initially(currentMap, G) :-  realWorldGraph(G).
initially(counterStations,5).


% Initial state of the map
%initially(blocked(station(4), station(14)), true).
%initially(blocked(station(14), station(4)), true).
%initially(blocked(P1,P2), false)  :- 
%        domain(P1, location),
%        domain(P2, location),
%        \+ initially(blocked(P1,P2), true).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% F - ABBREVIATIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
% The best customer to serve is the one that has to be served, its not
% currently suspended and there is no other customer to be served and not
% suspended with higher priority
proc(bestCustToServe(C), 
     and(serviceAccepted(C),
     and(neg(suspended(C)),
         neg(some(c,and(serviceAccepted(c),
                    and(neg(suspended(c)), orderPrio(c)>orderPrio(C)))))
        ))).



% Is there an edge to the left/right of the current location and direction?
proc(isEdgeLeft,
	   some(dir, some(x, 
	                    and(rotation(direction,dir,-90),
			        getEdge(robotLocation,x,dir,currentMap))))
).

proc(isEdgeRight,
	   some(dir, some(x, 
	                    and(rotation(direction,dir,90),
			        getEdge(robotLocation,x,dir,currentMap))))
).

                      

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% G - DEFINITIONS OF COMPLEX ACTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Execute program E in an interative deeping fashion up to Max times
% Stop when condition C holds
proc(minimization(E,C,Max), minimize(E,C,Max,0)).
proc(minimize(E,C,Max,N),
     wndet(search([exec(E,N),?(C)]), 
           [?(N<Max),pi(n2,[?(n2 is N+1), minimize(E,C,Max,n2)])])).

% Execute program E exactly N consecutive times
proc(exec(E,N), 
     wndet(?(N=0), [E,pi(n2,[?(n2 is N-1),exec(E,n2)])]) ).

% Handle a new order by accepting it if possible, otherwise decline it
proc(handleNewOrder(C), wndet(ackOrder(C), declineOrder(C)) ).

% If we want to go to Dest, the next station is Next 
% (uses the knowledge from the Prolog clause path/3)
proc(nextStation(Dest,Next),
   ?(path_graph_short(robotLocation,Dest,currentMap,10,[_,Next|_])) ).

% Go to Loc with the minimal number of steps
proc(goToLocation(Loc),
	pi(path,
	   [?(path_graph_short(robotLocation,Loc,currentMap,10,path)) ,
	    pi(x, pi(restpath, [?(path=[x|restpath]), traversePath(restpath)]))
           ])
).

% Traverse a sequence of stations
proc(traversePath(Path),
	pi(next, 
	pi(rest, [?(Path=[next|rest]),
	           turnToAim(next),
		   goNext,
		   sim(reachDest),
		   if(rest=[], ?(true), traversePath(rest))
	         ]
	 ))
).

% Turn to aim station Next
proc(turnToAim(Next),
   [star(ndet(turnLeft,ndet(turnRight,turnAround)),2), 
    ?(getEdge(robotLocation,Next,direction,currentMap))
   ]
).


% SERVE PROCEDURE: Serve a customer that needs service
% Used for both picking up a package at some office and
% dropping off a package that the agent is holding.
% serve should be used with a conditional search since it solution depend
% on the customer being at his office.
%
% Choose a customer that needs to be served (package to pickup or to drop-off),
% go to customer office, serve customer in a possible way (3 ways: pickup/drop-off,
% suspend, leave in mailbox), and, finally, reset to idle
proc(serve, 
pi(c,[wndet(?(and(holdingShip,c=recipient)), ?(bestCustToServe(c))),
      goToLocation(c), 
      commit, 
      senseDoor(c), 
      branch(inOffice(c)),
      wndet(search(service), wndet(suspend(c), [goToLocation(central), service])),
      resetRobot]
)).

% Drop package and pick up new package
proc(service, [freezeRobot, dropOff, sim(pushGo), pickUp]).

% Ask the user to reposition the robot because he is totally lost
proc(recover_position, 
	[say('I got lost! I will try to find where I am...'),
	 searchPath,    % search for any black line
	 localize
        ]
).

prim_action(searchPath).
poss(searchPath, robotLost).

% manual_localization(Location, Direction): auxiliary predicate
manual_localization(Location, Direction, M) :-
    (Direction = 1 -> MDir=' going up.' ; MDir=' going down.'),
    concat_atom(['I got lost heading from waystation ', Location, 
                 ' while ', MDir,
                 '.. Please position me between waystations in ',
                 ' the correct direction, and type any key when ready.'],M).

%%%%%%%%%%%%%%%%%%%%%%%%%
%  Main Routine
%%%%%%%%%%%%%%%%%%%%%%%%%

proc(mainControl(2), [prioritized_interrupts(
    [interrupt(or(robotState = moving, robotState = waitingPush), 
               wait),
     interrupt(true, localize)]
    )]
).

  
proc(mainControl(1), [prioritized_interrupts(
    [interrupt(robotState = lost, 
               [resetRobot, recover_position, goNext]),
     interrupt(n, askedService(n), 
               handleNewOrder(n)),
     interrupt(or(robotState = moving, robotState = waitingPush), 
               wait),
     interrupt(robotState = stuck, 
               [resetRobot, 
                abort(startOrder), 
                moveBack, 
		turnAround, 
                goNext, 
                resetRobot]),
     interrupt(c, and(serviceAccepted(c), suspended(c)), 
               declineOrder(c)),
     interrupt(holdingShip, 
               [wndet(?(neg(recipient=unknown)), readRecipient),
                search([gexec(startOrder,searchc(serve))])]),
     interrupt(c, and(robotState = idle, serviceAccepted(c)), 
               [say(['Trying to serve ',c]),
                wndet(search([gexec(startOrder, searchc(serve))]), 
                      [say(['Sorry it is not safe to serve ',c]),declineOrder(c)])
               ]), 
     interrupt(and(neg(robotLocation=central), neg(holdingShip)), 
               [say('Wrapping up to central office...'),
                search(pi(c,[nextStation(central,c), turnToAim(c), goNext])),
                resetRobot]), 
     interrupt(true, [say('Waiting at central station....'), wait])]
     )]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% H - ACTION/MESSAGE MAPPINGS - NUMBERS MUST CORRESPOND TO NQC CODE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% actionNum(?Action, ?ActionNumber): Returns ActionNumber associated
%     with Action and vice versa. ActionNumber can be sent to the RCX
%     for execution of Action. It can be returned from the RCX to
%     report the occurrence of exogenous Action

% This action have no impact on the RCX
%% actionNum(readRecipient, 0).
%% actionNum(enable(_), 0).
%% actionNum(suspend(_), 0).
%% actionNum(declineOrder(_), 0).
%% actionNum(ackOrder(_), 0).
%% actionNum(recover_position, 0).

% These actions should be sent to the RCX for action
actionNum(turnAround,   1).
actionNum(turnLeft,     2).
actionNum(turnRight,    3).
actionNum(pickUp,       4).
actionNum(dropOff,      5).
actionNum(goNext,       6).
actionNum(moveBack,     7).
actionNum(moveFwd,      8).
actionNum(freezeRobot,  9).
%actionNum(senseDoor(_), 10).
actionNum(resetRobot,   11).
actionNum(ring,         12).
actionNum(senseLine,    13).
actionNum(searchPath,   14).

% Exogenous actions
actionNum(reachDest,       20).
actionNum(stop_abnormally, 21).
actionNum(pushGo,          22).
actionNum(getStuck,        23).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% I - Translation of sensor values from RCX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% translateSensorValue(+Action, +SensorValue, SensingResult): Translate
%     the value SensorValue returned by the RCX sensor into a legal
%     SensingResult under Action

%translateSensorValue(A, SensorValue, SensorResult):- 
%	A=senseDoor(_), 
%	SensorValue>25-> SensorResult=true ; SensorResult=false.
%translateSensorValue(_, SensorValue, SensorValue).  % For all the other actions






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% J - OTHERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:- include('map-circle').   % Include the map




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAP AND PATH TOOLS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% sub_graph_rot(G1, G2, Map): 
%   the possibly incomplete graph G2 is a rotated subgraph of G1 under mapping Map
sub_graph_rot(G1, G2, Map, D) :-
	member(D,[0,90,180,270]),
	rotate_graph(G2, D, RG2),
	sub_graph(G1,RG2,Map).

% rotate_graph(G1, D, G2): graph G2 is graph G1 rotated D degress clockwise
rotate_graph(graph(Nodes, Edges), D, graph(RNodes, REdges)) :-
	rotate_graph(graph(Nodes, Edges,[]), D, graph(RNodes, REdges,[])).
rotate_graph(graph(Nodes, Edges, NEdges), D, graph(Nodes, REdges,RNEdges)) :-
	maplist(rotate_edge(D),Edges,REdges),
	maplist(rotate_edge(D),NEdges,RNEdges).

% Rotate the orientation of an edge some Degrees
rotate_edge(Degrees, edge(S,D,O), edge(S,D,RO)) :-
	rotation(O, RO, Degrees).

% rotation(X,Y,D) : Y is D clockwise degrees from X
rotation(X, Y, D) :-
	rotate_clock(X,Y,DC),
	(D=DC ; D is (360-DC)*(-1)).

rotate_clock(X, X, 0).
rotate_clock(X, Y, 90) :-
	rot(X, Y, 90).
rotate_clock(X, Y, 180) :-
	rot(X, Z, 90),
	rot(Z, Y, 90).
rotate_clock(X, Y, 270) :-
	rot(X, Z, 90),
	rot(Z, W, 90),
	rot(W, Y, 90).

rot(north,east,90).
rot(east,south,90).
rot(south,west,90).
rot(west,north,90).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEFINITION OF THE REAL WORLD GRAPH
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% edge definition for the map CIRCLE
nodes(circle,L)    :- location(L).
edge(circle,X,Y,D) :- edge1(circle,X,Y,D). 
edge(circle,X,Y,D) :- rotation(D2,D,180), edge1(circle,Y,X,D2).

edge1(circle, central,	        station(2),     north).
edge1(circle, station(2),	station(3),     east).
edge1(circle, station(3),	station(4),     south).
edge1(circle, station(4),	station(5),     south).
edge1(circle, central,	        station(15),    south).
edge1(circle, station(15),	station(5),     east).

edge1(circle, station(2),	station(6),	west).

edge1(circle, central,	        station(8),     west).
edge1(circle, station(8),	station(7),     north).
edge1(circle, station(8),	station(9),     south).

edge1(circle, station(3),	station(10),	east).
edge1(circle, station(10),	station(11),	east).
edge1(circle, station(10),	station(14),	south).
edge1(circle, station(14),	station(13),	east).
edge1(circle, station(13),	station(12),	north).
edge1(circle, station(13),	station(16),	south).
edge1(circle, station(16),	station(5),	west).


/* World topology dictated by edge/3:


     6 ------------2------------3---------10---------11
                   |            |         |
                   |            |         |
                   |            |         |
                   |            |         |
     7             |            |         |          12
     |             |            |         |          |
     |             |            |         |          | 
     |             |            |         14---------13
     |             |            |         |          |
     |             |            |         |          |
     8 ---------central         |         |          |
     |             |            |         |          |
     |             |            |         |          |
     |             |            4---------|          |
     |             |            |                    |
     |             |            |                    |
     |            15------------5--------------------16
     |                                             
     |                                             
     9             
|*/


map_graph(Id, graph(Nodes, Edges)) :-
	nodes(Id, Nodes),
	setof(edge(S,D,O),O2^(edge(Id,S,D,O) ; 
                              edge(Id,D,S,O2), rotation(O2,O,180)),Edges).

realWorldGraph(G) :- map_graph(circle, G).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OLDDDDDDDDDDDDDDD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Generic facts about connectivity: 
% connected(Id, X, Y, D):-
%              X and Y are connected with Y at direction D to X in Id
connected(Id, between(P1,P2),P,D):- !,
	( connected(Id, P1,P2,D), P=P2 
	; 
	  connected(Id, P2,P1,D),P=P1 
	).
connected(Id, X, Y, west) :- edge(Id, X, Y, west)  ; edge(Id, Y, X, east).
connected(Id, X, Y, east) :- edge(Id, X, Y, east)  ; edge(Id, Y, X, west).
connected(Id, X, Y, north):- edge(Id, X, Y, north) ; edge(Id, Y, X, south).
connected(Id, X, Y, south):- edge(Id, X, Y, south) ; edge(Id, Y, X, north).

% C1 and C2 are connected in some way
connected(Id, C1, C2):- connected(Id, C1, C2, _) ; connected(Id, C2, C1, _).


% hasLeft(X,D) : at X there is a route on the left when aiming D
% hasRight(X,D): at X there is a route on the right when aiming D
hasLeft(Id, X, D) :- rotation(D, D2, -90), connected(Id, X,_, D2).
hasRight(Id, X, D):- rotation(D, D2, 90),  connected(Id, X,_, D2).


% Dist is the minimal distance between P1 and P2
mindist(P1, P2, Dist) :- mindist2(P1,P2,Dist,1).

mindist2(P1,P2,Limit,Limit) :- length(Path,Limit), path(P1,P2,Path), !.
mindist2(P1,P2,Dist,Limit)  :- L2 is Limit+1,  mindist2(P1,P2,Dist,L2).


% path(X, Y, Path): Path is a list of locations to pass through
%   in order to get from X to Y

% We'll strip off the first element as it will be X
path(X, Y, Path) :- path1(X, [Y], [X|Path]). 

path1(X, [X | Path], [X | Path]).

path1(X, [Y | Path1], Path) :-
    connected(Y, Z, _),
    \+ member(Z, Path1),
    path1(X, [Z, Y | Path1], Path).

% in_path(X, Y, Z): Z is the next element in a path from X to Y
in_path(X, Y, Z) :-
	path(X, Y, [Z|_]).


% P is the shortest path in G from X to Y (and with length less than Limit)
path_plan_short(X, Y, G, Limit, P) :- path_plan_short(X,Y,G,0,Limit, P).

path_plan_short(X,Y,G,N,_,P) :- path_plan(X,Y,G,N,P), !.
path_plan_short(X,Y,G,N,L,P) :-
	L\=0,
	L2 is L-1, N2 is N+1, path_plan_short(X,Y,G,N2,L2,P).


% path_graph(X, Y, Id, L, P): P is a path of length L from X to Y in map Id
path_plan(X, Y, Id, L, [X|LV]) :- 
	length(LV, L), % Build a list LV of variables of length L
	path1_plan(Id, Y, [X|LV]). 

path1_plan(_, X, [X]).
path1_plan(Id, X, [Y | Path1]) :- 
	edge(Id,Z,Y,_),  % We can go from Y to Z with an edge in E
%	append(GList, VList, Path1),
%	ground(GList), 
%	\+ member(Z, Path1),
	Path1=[Z|_],
	path1_plan(Id, X, Path1).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Delivery-BAT/delivery.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*

4 ?-  now(H), trans(searchc(serve),H,E,H).

Action (h for help) ? abort
% Execution Aborted
5 ?- now(H), trans(search(goToLocation(station(12))),H,E,R).

H = [e(readRecipient, station(12)), readRecipient, pickUp, pushGo, dropOff, freezeRobot,
e(senseDoor(station(...)), true), senseDoor(station(...)), reachDest|...]
E = followpath([[], pi(x, pi(restpath, [? ([station(...)|...]=[x|...]), traversePath(restpath)]))], [[[], pi(x, pi(restpath, [? ([...|...]=[...|...]), traversePath(restpath)]))], [e(readRecipient, station(12)), readRecipient, pickUp, pushGo, dropOff, freezeRobot|...], [[[], traversePath([station(...)|...])]], [e(readRecipient, station(12)), readRecipient, pickUp, pushGo|...], [[[[]|...]]], [e(readRecipient, station(...)), readRecipient|...], [[...]], [...|...]|...])
R = [e(readRecipient, station(12)), readRecipient, pickUp, pushGo, dropOff, freezeRobot,
e(senseDoor(station(...)), true), senseDoor(station(...)), reachDest|...]

Yes


?- now(H).

H = [e(readRecipient, station(12)), readRecipient, pickUp, pushGo, dropOff, freezeRobot, e(senseDoor(station(...)), true), senseDoor(station(...)), reachDest|...] [write]

H = [e(readRecipient, station(12)), readRecipient, pickUp, pushGo, dropOff, freezeRobot, e(senseDoor(station(6)), true), senseDoor(station(6)), reachDest, goNext, turnLeft, reachDest, goNext, turnLeft, e(startOrder, true), say(['Trying to serve ', station(6)]), ackOrder(station(6))]


assert(now([e(readRecipient, station(12)), readRecipient, pickUp, pushGo, dropOff, freezeRobot, e(senseDoor(station(6)), true), senseDoor(station(6)), reachDest, goNext, turnLeft, reachDest, goNext, turnLeft, e(startOrder, true), say(['Trying to serve ', station(6)]), ackOrder(station(6))])).


assert(graph(graph([station(1), station(2), station(3)], [edge(station(1), station(2), south), edge(station(2), station(1), north), edge(station(2), station(3), south), edge(station(3), station(2), north)], [])),1).


assert(graph(graph([station(1), station(2), station(3), station(4)], [edge(station(4), station(2), south), edge(station(2), station(4), north), edge(station(1), station(2), south), edge(station(2), station(1), north), edge(station(2), station(3), south), edge(station(3), station(2), north)], []),2)).

*/
