:- expects_dialect(lps).

maxTime(90).
fluents  
pouring(_Receptacle), 
contents(_Receptacle, _Level), 
empty/1, 
location(_Object, _Position).

initially 
contents(bottle, 0), 
contents(container, 0), 
contents(tank1, 100), 
contents(tank2, 100),
location(bottle, 160),
location(tank1, 160),
location(tank2, 240),
location(container, 360).

actions  
openValve(_Receptacle),  
closeValve(_Receptacle), 
pourChunk(_Receptacle1, _Receptacle2), 
turnConveyor/1.


% This shouldn't be needed.
% Remove it and see what happens.
false pourChunk(_, _), turnConveyor(_).

% This shouldn't be needed.
% Remove it and see what happens.
% 
false pourChunk(Container1, Container2), location(Container1, Place1),
location(Container2, Place2), Place1 \= Place2.


makeLocation(bottle, Station) from T if 
	location(bottle, Place1) at T, location(Station, Place2) at T, 
	Vector is Place2 - Place1, 
	moveConveyor(Vector) from T.

moveConveyor(0) from T to T.
moveConveyor(Vector) from T1 to T3 if 
	Vector > 0, turnConveyor(clockwise) from T1 to T2,
	NewVector is Vector -10, moveConveyor(NewVector) from T2 to T3.
moveConveyor(Vector) if 
	Vector < 0, turnConveyor(counterClockwise),
	NewVector is Vector +10, moveConveyor(NewVector).

turnConveyor(counterClockwise) updates Place to NewPlace in location(bottle, Place) if
	NewPlace is Place-10.

turnConveyor(clockwise) updates Place to NewPlace in location(bottle, Place) if
	NewPlace is Place+10.

empty(Receptacle) if contents(Receptacle, 0).

if empty(bottle) at T1, location(bottle,160) at T1
then  
    pour(tank1, bottle, 50) from T1 to T2,
	makeLocation(bottle, tank2) from T2 to T3, 
    pour(tank2, bottle, 50) from T3 to T4, 
	makeLocation(bottle, container) from T4 to T5, 
    pour(bottle,container, 100) from T5 to T6, 
	makeLocation(bottle, tank1) from T6.


pour(Receptacle1, Receptacle2, Quantity) from T1 to T4 if
	contents(Receptacle2, OldLevel) at T1, 
	DesiredLevel is  Quantity + OldLevel,
	valveRate(R),
	StopLevel is DesiredLevel - R,
	openValve(Receptacle1) from T1 to T2,
	contents(Receptacle2, StopLevel) at T3,
	closeValve(Receptacle1) from T3 to T4.


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
    

% helper intensional fluent for 2D display; 
% you need to use the sample(..) option to force LPS to remember it
locatedContents(Item,Pos,Level) at T if 
	location(Item,Pos) at T, contents(Item,Level) at T.


d(locatedContents(bottle,Pos,Level),[type:rectangle,  
	fillColor:yellow, 
    from:[X1,60], to:[X2,Height]]) :- 
   Height is 60 + Level/4,
	X1 is Pos-10, X2 is Pos+10.

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
