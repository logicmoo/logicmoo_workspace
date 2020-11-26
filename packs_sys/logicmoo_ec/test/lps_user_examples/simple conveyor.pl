:- expects_dialect(lps).

% This version works correctly.

maxTime(160).
fluents  
pouring(_Receptacle), 
contents(_Receptacle, _Level), 
location(_Object, _Position),
moving(_Vector).

initially 
contents(bottle, 0), 
contents(container, 0), 
contents(tank1, 100), 
contents(tank2, 100),
contents(heaven, 1000),
location(bottle, 160),
location(tank1, 160),
location(tank2, 240),
location(container, 400).

actions  
openValve(_Receptacle),  
closeValve(_Receptacle), 
pourChunk(_Receptacle1, _Receptacle2), 
startConveyor(_Vector),
stopConveyor,
turnConveyor/1.

% This constraint prevents multiple, unwanted closing of the valve.
% It can be replaced by an extra condition pouring(Receptacle1) at T3
% in the clause defining the macroaction pour/3.
% 
false closeValve(R), not pouring(R). 

% These constraints are no longer necessary.
% __________________________________________
% false pourChunk(_, _), turnConveyor(_). 
% false pourChunk(Container1, Container2), location(Container1, Place1),
% location(Container2, Place2), Place1 \= Place2. 
% 
% false openValve(R), pouring(R). 
% false openValve(R), moving(_). 
%_________________________________________

% Here startConveyor and stopConveyor are external actions.
% location(bottle, StopPlace) is an external observation of a fluent value.
%
makeLocation(bottle, Station) from T to T if 
	location(bottle, Place) at T, location(Station, Place) at T.

makeLocation(bottle, Station) from T1 to T4 if 
	location(bottle, Place1) at T1, location(Station, Place2) at T1, 
	Vector is Place2 - Place1, 
	stopPlace(Vector, Place2, Stop),
	startConveyor(Vector) from T1,
	location(bottle, Stop) at T3,
	moving(Vector) at T3, % Needed for the logic to be correct.
	stopConveyor from T3 to T4.
	
stopPlace(Vector, Place2, Stop):-
    conveyorSpeed(S), Vector > 0, Stop is Place2 - S.
stopPlace(Vector, Place2, Stop):- 
    conveyorSpeed(S),Vector < 0, Stop is Place2 + S.

% Here the external actions and fluent are simulated internally.
% This is not necessary if the actions  are simply performed externally 
% and the fluent is observed externally.
% __________________________________________________

startConveyor(Vector) initiates moving(Vector).
stopConveyor terminates moving(_).

if moving(Vector) at T, Vector > 0
then turnConveyor(clockwise) from T.

if moving(Vector) at T, Vector < 0
then turnConveyor(counterClockwise) from T.

turnConveyor(counterClockwise) updates Place to NewPlace in location(bottle, Place) if
	conveyorSpeed(S), NewPlace is Place-S.

turnConveyor(clockwise) updates Place to NewPlace in location(bottle, Place) if
	conveyorSpeed(S), NewPlace is Place+S.

conveyorSpeed(10). % our conveyor moves this distance per LPS cycle

%_______________________________________________
% end of simulation of conveyor.

if 	contents(bottle, 0) at T1, 
	location(tank1, Place) at T1,location(bottle,Place) at T1,
	not pouring(_) at T1 % needed for refraction.
then  
   	pour(tank1, bottle, 50) from T1 to T2,
	makeLocation(bottle, tank2) from T2 to T3, 
   	pour(tank2, bottle, 50) from T3 to T4, 
	makeLocation(bottle, container) from T4 to T5, 
  	pour(bottle,container, 100) from T5 to T6, 
	makeLocation(bottle, tank1) from T6.


pour(Receptacle1, Receptacle2, Quantity) from T1 to T4 if
	contents(Receptacle2, OldLevel) at T1, 
	contents(Receptacle1, Supply) at T1, Supply >= Quantity,
	DesiredLevel is  Quantity + OldLevel,
	valveRate(R),
	StopLevel is DesiredLevel - R,
	openValve(Receptacle1) from T1 to T2,
%	pouring(Receptacle1) at T3,
	contents(Receptacle2, StopLevel) at T3,
	closeValve(Receptacle1) from T3 to T4.

% Here the external actions and fluent are simulated internally.
% This is not necessary if the actions  are simply performed externally 
% and the fluent is observed externally.
% __________________________________________________

openValve(Receptacle) initiates pouring(Receptacle).
closeValve(Receptacle) terminates pouring(Receptacle).

if pouring(Receptacle1) at T1, 
location(Receptacle1, Position1) at T1,
location(Receptacle2, Position1) at T1,
Receptacle2 \= Receptacle1
then pourChunk(Receptacle1, Receptacle2) from T1 to T2.	

pourChunk(_, Receptacle2) updates Old to New in contents(Receptacle2, Old) if 
	valveRate(Quantity), New is Old + Quantity.
pourChunk(Receptacle1, _) updates Old to New in contents(Receptacle1, Old) if 
	valveRate(Quantity), New is Old - Quantity.

valveRate(10). % our receptacle's output valves debit quantity of liquid per LPS cycle

%_______________________________________________
% end of simulation of pouring.
    
% helper intensional fluent for 2D display; 
% you need to use the sample(..) option to force LPS to remember it
locatedContents(Item,Pos,Level) at T if 
	location(Item,Pos) at T, contents(Item,Level) at T.


d(locatedContents(bottle,Pos,Level),[type:rectangle,  
	fillColor:yellow, 
    from:[X1,60], to:[X2,Height]]) :- 
   Height is 60 + Level/4,
	X1 is Pos-10, X2 is Pos+10.

d(locatedContents(tank1,Pos,Level),[type:rectangle,  
	fillColor:yellow, 
    from:[130, 120], to:[190,Height]]) :- 
    Height is 120 + Level/4.

d(locatedContents(tank2,Pos,Level),[type:rectangle,  
	fillColor:yellow, 
    from:[210, 120], to:[270,Height]]) :- 
    Height is 120 + Level/4.

d(location(bottle,Pos),[type:rectangle,  
%    fillColor:yellow, 
    from:[X1,60], to:[X2,100], strokeColor:blue]) :- 
    X1 is Pos-10, X2 is Pos+10.
   
d(timeless,[[type:line, strokeWidth: 2, strokeColor:black, from:[100,60], to:[400, 60]],
[type:circle, strokeWidth: 2, strokeColor:black, center:[100,40], radius:20],
[type:circle, strokeWidth: 2, strokeColor:black, center:[400,40], radius:20], 
[type:rectangle, fillColor:white, from:[130,120], to:[190,150], strokeColor:blue],
[type:rectangle, fillColor:white, from:[210,120], to:[270,150], strokeColor:blue],
[type:line, strokeWidth: 2, strokeColor:black, from:[100,20], to:[400, 20]]]).

/** <examples>
?- go(Timeline).
?- go.
?- go(Timeline,[sample([locatedContents(_,_,_)])]).
?- gov.
*/

