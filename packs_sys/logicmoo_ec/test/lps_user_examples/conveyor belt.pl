:- expects_dialect(lps).

maxTime(30).
fluents normalOperation, started, contents/2, empty/1, location/2.
initially normalOperation, 
contents(bottle, 0), 
contents(container, 0), 
contents(tank1, 100), 
contents(tank2, 100),
location(bottle, 2),
location(tank1, 2),
location(tank2, 5),
location(container, 7).

actions pour/3, turnConveyor/1, waitCycle.

wait(0) from T to T.
wait(Cycles) from T1 to T2
if Cycles > 0, waitCycle from T1 to T, RemainingCycles is Cycles - 1, 
	wait(RemainingCycles) from T to T2.

% This shouldn't be needed.
% Remove it and see what happens.
false pour(_, _, _), turnConveyor(_).

% This shouldn't be needed.
% Remove it and see what happens.
false pour(Container1, Container2, _), location(Container1, Place1),
location(Container2, Place2), Place1 \= Place2.

makeLocation(bottle, Station) from T if 
location(bottle, Place1) at T, location(Station, Place2) at T, 
Vector is Place2 - Place1, 
moveConveyor(Vector) from T.

moveConveyor(0) from T to T.

moveConveyor(Vector) from T1 to T3 if Vector > 0, turnConveyor(clockwise) from T1 to T2,
NewVector is Vector -1, moveConveyor(NewVector) from T2 to T3.

moveConveyor(Vector) if Vector < 0, turnConveyor(counterClockwise),
NewVector is Vector +1, moveConveyor(NewVector).

turnConveyor(counterClockwise) updates Place to NewPlace in location(bottle, Place) if
NewPlace is Place-1.

turnConveyor(clockwise) updates Place to NewPlace in location(bottle, Place) if
NewPlace is Place+1.

empty(Receptacle) if contents(Receptacle, 0).

pour(Receptacle1, Receptacle2, Quantity) 
updates Old to New in contents(Receptacle2, Old) if New is Old + Quantity.

pour(Receptacle1, Receptacle2, Quantity) 
updates Old to New in contents(Receptacle1, Old) if New is Old - Quantity.
                                      


/* This reactive rule has explicit time.
 * But the version with implicit time works equally well.
 * Before introducing "wait".
*/
if normalOperation at T1,
empty(bottle) at T1, not started at T1
then  initiate started from T1, makeLocation(bottle, tank1) from T1 to T2, 
pour(tank1, bottle, 5) from T2 to T3,
wait(3) from T3 to T4,
makeLocation(bottle, tank2) from T4 to T5,
pour(tank2, bottle, 5) from T5 to T6, 
wait(3) from T6 to T7,
makeLocation(bottle, container) from T7 to T8,
pour(bottle, container, 10) from T8 to T9, 
wait(3) from T9 to T10,
makeLocation(bottle, tank1) from T10 to T11,
terminate started from T11.

/*
if normalOperation,
empty(bottle), not started
then  initiate started, makeLocation(bottle, tank1), 
pour(tank1, bottle, 5), wait(3),
makeLocation(bottle, tank2),
pour(tank2, bottle, 5), wait(3),
makeLocation(bottle, container),
pour(bottle, container, 10), 
makeLocation(bottle, tank1), wait(3),
terminate started.
*/

d(location(bottle,Pos),[type:rectangle,  
    fillColor:yellow, 
    from:[X1,60], to:[X2,100], strokeColor:blue]) :- 
    X1 is 100+ Pos * 30, X2 is 110+Pos*30.

d(timeless,[[type:line, strokeWidth: 2, strokeColor:black, from:[100,60], to:[400, 60]],
[type:circle, strokeWidth: 2, strokeColor:black, center:[100,40], radius:20],
[type:circle, strokeWidth: 2, strokeColor:black, center:[400,40], radius:20], 
[type:rectangle, fillColor:white, from:[130,120], to:[190,150], strokeColor:blue],
[type:rectangle, fillColor:white, from:[210,120], to:[270,150], strokeColor:blue],
[type:line, strokeWidth: 2, strokeColor:black, from:[100,20], to:[400, 20]]]).

/** <examples>
?- go(Timeline).
?- go.
*/
